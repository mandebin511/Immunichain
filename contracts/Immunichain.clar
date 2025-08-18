(define-read-only (contract-version)
  (ok "1.0.0"))
(define-non-fungible-token vaccination-record uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-wrong-commission (err u103))
(define-constant err-listing-expired (err u104))
(define-constant err-nft-not-found (err u105))
(define-constant err-unauthorized-issuer (err u106))
(define-constant err-invalid-vaccine-data (err u107))
(define-constant err-record-already-exists (err u108))

(define-data-var last-token-id uint u0)
(define-data-var commission uint u250)

(define-map token-count principal uint)
(define-map market (tuple (token-id uint) (owner principal)) 
  (tuple (price uint) (commission uint) (expires-at uint)))

(define-map vaccination-records uint 
  (tuple 
    (patient-id (string-ascii 64))
    (vaccine-name (string-ascii 128))
    (vaccine-batch (string-ascii 64))
    (vaccination-date uint)
    (issuer principal)
    (location (string-ascii 128))
    (dose-number uint)
    (next-dose-due (optional uint))
    (verified bool)
  ))

(define-map authorized-issuers principal bool)
(define-map patient-records (string-ascii 64) (list 50 uint))

(define-public (authorize-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-issuers issuer true))))

(define-public (revoke-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-delete authorized-issuers issuer))))

(define-public (mint-vaccination-record 
  (recipient principal)
  (patient-id (string-ascii 64))
  (vaccine-name (string-ascii 128))
  (vaccine-batch (string-ascii 64))
  (vaccination-date uint)
  (location (string-ascii 128))
  (dose-number uint)
  (next-dose-due (optional uint)))
  (let 
    (
      (token-id (+ (var-get last-token-id) u1))
      (is-authorized (default-to false (map-get? authorized-issuers tx-sender)))
    )
    (asserts! (or is-authorized (is-eq tx-sender contract-owner)) err-unauthorized-issuer)
    (asserts! (> (len patient-id) u0) err-invalid-vaccine-data)
    (asserts! (> (len vaccine-name) u0) err-invalid-vaccine-data)
    (asserts! (> vaccination-date u0) err-invalid-vaccine-data)
    (try! (nft-mint? vaccination-record token-id recipient))
    (map-set vaccination-records token-id 
      (tuple 
        (patient-id patient-id)
        (vaccine-name vaccine-name)
        (vaccine-batch vaccine-batch)
        (vaccination-date vaccination-date)
        (issuer tx-sender)
        (location location)
        (dose-number dose-number)
        (next-dose-due next-dose-due)
        (verified true)
      ))
    (let ((current-records (default-to (list) (map-get? patient-records patient-id))))
      (map-set patient-records patient-id (unwrap! (as-max-len? (append current-records token-id) u50) err-invalid-vaccine-data)))
    (map-set token-count recipient (+ (get-balance recipient) u1))
    (var-set last-token-id token-id)
    (ok token-id)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (is-eq sender (unwrap! (nft-get-owner? vaccination-record token-id) err-nft-not-found)) err-not-token-owner)
    (map-set token-count sender (- (get-balance sender) u1))
    (map-set token-count recipient (+ (get-balance recipient) u1))
    (nft-transfer? vaccination-record token-id sender recipient)))

(define-public (list-in-ustx (token-id uint) (price uint) (expires-at uint))
  (let ((owner (unwrap! (nft-get-owner? vaccination-record token-id) err-nft-not-found)))
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (map-set market (tuple (token-id token-id) (owner owner))
      (tuple (price price) (commission (var-get commission)) (expires-at expires-at)))
    (ok true)))

(define-public (unlist-in-ustx (token-id uint))
  (let ((owner (unwrap! (nft-get-owner? vaccination-record token-id) err-nft-not-found)))
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (map-delete market (tuple (token-id token-id) (owner owner)))
    (ok true)))

(define-public (buy-in-ustx (token-id uint) (owner principal))
  (let ((listing (unwrap! (map-get? market (tuple (token-id token-id) (owner owner))) err-listing-not-found))
        (price (get price listing))
        (commission-amount (/ (* price (get commission listing)) u10000)))
    (asserts! (< stacks-block-height (get expires-at listing)) err-listing-expired)
    (try! (stx-transfer? (- price commission-amount) tx-sender owner))
    (try! (stx-transfer? commission-amount tx-sender contract-owner))
    (try! (transfer token-id owner tx-sender))
    (map-delete market (tuple (token-id token-id) (owner owner)))
    (ok true)))

(define-public (set-commission (new-commission uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-commission u1000) err-wrong-commission)
    (ok (var-set commission new-commission))))

(define-public (verify-record (token-id uint))
  (let ((record (unwrap! (map-get? vaccination-records token-id) err-nft-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (map-set vaccination-records token-id (merge record (tuple (verified true))))
    (ok true)))

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok none))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? vaccination-record token-id)))

(define-read-only (get-balance (account principal))
  (default-to u0 (map-get? token-count account)))

(define-read-only (get-vaccination-record (token-id uint))
  (map-get? vaccination-records token-id))

(define-read-only (get-patient-records (patient-id (string-ascii 64)))
  (map-get? patient-records patient-id))

(define-read-only (get-listing-in-ustx (token-id uint) (owner principal))
  (map-get? market (tuple (token-id token-id) (owner owner))))

(define-read-only (get-commission)
  (ok (var-get commission)))

(define-read-only (is-authorized-issuer (issuer principal))
  (default-to false (map-get? authorized-issuers issuer)))

(define-read-only (get-vaccination-history (patient-id (string-ascii 64)))
  (let ((record-ids (default-to (list) (map-get? patient-records patient-id))))
    (map get-vaccination-record record-ids)))

(define-read-only (verify-vaccination-status (patient-id (string-ascii 64)) (vaccine-name (string-ascii 128)))
  (let ((record-ids (default-to (list) (map-get? patient-records patient-id))))
    (fold check-vaccine-match record-ids false)))

(define-private (check-vaccine-match (token-id uint) (found bool))
  (if found 
    true
    (match (map-get? vaccination-records token-id)
      record (get verified record)
      false)))

(define-constant err-schedule-not-found (err u109))
(define-constant err-organization-not-found (err u110))
(define-constant err-invalid-schedule-data (err u111))
(define-constant err-compliance-rule-exists (err u112))
(define-constant err-reminder-not-found (err u113))

(define-data-var next-schedule-id uint u1)
(define-data-var next-organization-id uint u1)
(define-data-var next-reminder-id uint u1)

(define-map vaccination-schedules uint
  (tuple
    (vaccine-name (string-ascii 128))
    (total-doses uint)
    (interval-days uint)
    (reminder-days-before uint)
    (validity-period-days uint)
    (created-by principal)
    (active bool)
  ))

(define-map organizations uint
  (tuple
    (name (string-ascii 256))
    (admin principal)
    (compliance-officer principal)
    (active bool)
    (created-at uint)
  ))

(define-map compliance-rules (tuple (organization-id uint) (vaccine-name (string-ascii 128)))
  (tuple
    (required bool)
    (grace-period-days uint)
    (reminder-frequency-days uint)
    (updated-at uint)
  ))

(define-map patient-compliance (tuple (patient-id (string-ascii 64)) (organization-id uint))
  (tuple
    (compliance-status bool)
    (last-checked uint)
    (next-due-date (optional uint))
    (pending-vaccines (list 10 (string-ascii 128)))
  ))

(define-map vaccination-reminders uint
  (tuple
    (patient-id (string-ascii 64))
    (vaccine-name (string-ascii 128))
    (due-date uint)
    (reminder-date uint)
    (organization-id (optional uint))
    (status (string-ascii 32))
    (created-at uint)
  ))

(define-map organization-members (tuple (organization-id uint) (patient-id (string-ascii 64))) bool)
(define-map patient-organizations (string-ascii 64) (list 20 uint))

(define-public (create-vaccination-schedule
  (vaccine-name (string-ascii 128))
  (total-doses uint)
  (interval-days uint)
  (reminder-days-before uint)
  (validity-period-days uint))
  (let ((schedule-id (var-get next-schedule-id)))
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (asserts! (> (len vaccine-name) u0) err-invalid-schedule-data)
    (asserts! (> total-doses u0) err-invalid-schedule-data)
    (asserts! (> interval-days u0) err-invalid-schedule-data)
    (map-set vaccination-schedules schedule-id
      (tuple
        (vaccine-name vaccine-name)
        (total-doses total-doses)
        (interval-days interval-days)
        (reminder-days-before reminder-days-before)
        (validity-period-days validity-period-days)
        (created-by tx-sender)
        (active true)
      ))
    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)))

(define-public (update-vaccination-schedule
  (schedule-id uint)
  (total-doses uint)
  (interval-days uint)
  (reminder-days-before uint)
  (validity-period-days uint)
  (active bool))
  (let ((schedule (unwrap! (map-get? vaccination-schedules schedule-id) err-schedule-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (is-eq tx-sender (get created-by schedule))) err-owner-only)
    (map-set vaccination-schedules schedule-id
      (merge schedule
        (tuple
          (total-doses total-doses)
          (interval-days interval-days)
          (reminder-days-before reminder-days-before)
          (validity-period-days validity-period-days)
          (active active)
        )))
    (ok true)))

(define-public (register-organization
  (name (string-ascii 256))
  (admin principal)
  (compliance-officer principal))
  (let ((org-id (var-get next-organization-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> (len name) u0) err-invalid-vaccine-data)
    (map-set organizations org-id
      (tuple
        (name name)
        (admin admin)
        (compliance-officer compliance-officer)
        (active true)
        (created-at stacks-block-height)
      ))
    (var-set next-organization-id (+ org-id u1))
    (ok org-id)))

(define-public (add-compliance-rule
  (organization-id uint)
  (vaccine-name (string-ascii 128))
  (required bool)
  (grace-period-days uint)
  (reminder-frequency-days uint))
  (let ((org (unwrap! (map-get? organizations organization-id) err-organization-not-found)))
    (asserts! (or (is-eq tx-sender (get admin org))
                  (is-eq tx-sender (get compliance-officer org))
                  (is-eq tx-sender contract-owner)) err-owner-only)
    (map-set compliance-rules (tuple (organization-id organization-id) (vaccine-name vaccine-name))
      (tuple
        (required required)
        (grace-period-days grace-period-days)
        (reminder-frequency-days reminder-frequency-days)
        (updated-at stacks-block-height)
      ))
    (ok true)))

(define-public (enroll-patient-in-organization
  (patient-id (string-ascii 64))
  (organization-id uint))
  (let ((org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
        (current-orgs (default-to (list) (map-get? patient-organizations patient-id))))
    (asserts! (or (is-eq tx-sender (get admin org))
                  (is-eq tx-sender (get compliance-officer org))
                  (is-eq tx-sender contract-owner)) err-owner-only)
    (map-set organization-members (tuple (organization-id organization-id) (patient-id patient-id)) true)
    (map-set patient-organizations patient-id 
      (unwrap! (as-max-len? (append current-orgs organization-id) u20) err-invalid-vaccine-data))
    (map-set patient-compliance (tuple (patient-id patient-id) (organization-id organization-id))
      (tuple
        (compliance-status false)
        (last-checked u0)
        (next-due-date none)
        (pending-vaccines (list))
      ))
    (ok true)))

(define-public (create-vaccination-reminder
  (patient-id (string-ascii 64))
  (vaccine-name (string-ascii 128))
  (due-date uint)
  (organization-id (optional uint)))
  (let ((reminder-id (var-get next-reminder-id)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (asserts! (> due-date stacks-block-height) err-invalid-vaccine-data)
    (map-set vaccination-reminders reminder-id
      (tuple
        (patient-id patient-id)
        (vaccine-name vaccine-name)
        (due-date due-date)
        (reminder-date (- due-date u7))
        (organization-id organization-id)
        (status "pending")
        (created-at stacks-block-height)
      ))
    (var-set next-reminder-id (+ reminder-id u1))
    (ok reminder-id)))

(define-public (update-reminder-status
  (reminder-id uint)
  (new-status (string-ascii 32)))
  (let ((reminder (unwrap! (map-get? vaccination-reminders reminder-id) err-reminder-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (map-set vaccination-reminders reminder-id
      (merge reminder (tuple (status new-status))))
    (ok true)))

(define-public (check-compliance-status
  (patient-id (string-ascii 64))
  (organization-id uint))
  (let ((org (unwrap! (map-get? organizations organization-id) err-organization-not-found))
        (patient-record-ids (default-to (list) (map-get? patient-records patient-id)))
        (compliance-data (default-to 
          (tuple (compliance-status false) (last-checked u0) (next-due-date none) (pending-vaccines (list)))
          (map-get? patient-compliance (tuple (patient-id patient-id) (organization-id organization-id))))))
    (asserts! (or (is-eq tx-sender (get admin org))
                  (is-eq tx-sender (get compliance-officer org))
                  (is-eq tx-sender contract-owner)) err-owner-only)
    (let ((updated-compliance (calculate-compliance-status patient-record-ids organization-id)))
      (map-set patient-compliance (tuple (patient-id patient-id) (organization-id organization-id))
        (merge compliance-data
          (tuple
            (compliance-status (get compliance-status updated-compliance))
            (last-checked stacks-block-height)
            (next-due-date (get next-due-date updated-compliance))
            (pending-vaccines (get pending-vaccines updated-compliance))
          )))
      (ok updated-compliance))))

(define-private (calculate-compliance-status (record-ids (list 50 uint)) (organization-id uint))
  (fold process-compliance-record record-ids 
    (tuple (compliance-status true) (next-due-date none) (pending-vaccines (list)))))

(define-private (process-compliance-record 
  (token-id uint) 
  (acc (tuple (compliance-status bool) (next-due-date (optional uint)) (pending-vaccines (list 10 (string-ascii 128))))))
  (match (map-get? vaccination-records token-id)
    record (let ((vaccine-name (get vaccine-name record))
                 (next-dose-due (get next-dose-due record)))
             (if (and (is-some next-dose-due) (< (unwrap-panic next-dose-due) stacks-block-height))
               (tuple 
               (compliance-status false)
               (next-due-date (some (unwrap-panic next-dose-due)))
               (pending-vaccines (default-to (get pending-vaccines acc) 
                  (as-max-len? (append (get pending-vaccines acc) vaccine-name) u10))))
               acc))
    acc))

(define-read-only (get-vaccination-schedule (schedule-id uint))
  (map-get? vaccination-schedules schedule-id))

(define-read-only (get-organization (organization-id uint))
  (map-get? organizations organization-id))

(define-read-only (get-compliance-rule (organization-id uint) (vaccine-name (string-ascii 128)))
  (map-get? compliance-rules (tuple (organization-id organization-id) (vaccine-name vaccine-name))))

(define-read-only (get-patient-compliance (patient-id (string-ascii 64)) (organization-id uint))
  (map-get? patient-compliance (tuple (patient-id patient-id) (organization-id organization-id))))

(define-read-only (get-vaccination-reminder (reminder-id uint))
  (map-get? vaccination-reminders reminder-id))

(define-read-only (get-patient-organizations (patient-id (string-ascii 64)))
  (map-get? patient-organizations patient-id))

(define-read-only (get-pending-reminders (current-block uint))
  (let ((all-reminders (map get-vaccination-reminder (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))))
    (filter is-reminder-due all-reminders)))

(define-private (is-reminder-due (reminder-opt (optional (tuple 
  (patient-id (string-ascii 64))
  (vaccine-name (string-ascii 128))
  (due-date uint)
  (reminder-date uint)
  (organization-id (optional uint))
  (status (string-ascii 32))
  (created-at uint)))))
  (match reminder-opt
    reminder (and (<= (get reminder-date reminder) stacks-block-height)
                  (is-eq (get status reminder) "pending"))
    false))

(define-read-only (get-organization-compliance-summary (organization-id uint))
  (let ((org (unwrap! (map-get? organizations organization-id) (err u404))))
    (ok (tuple
      (organization-name (get name org))
      (total-members u0)
      (compliant-members u0)
      (pending-vaccinations u0)
      (overdue-vaccinations u0)
    ))))

(define-read-only (get-next-vaccination-due (patient-id (string-ascii 64)))
  (let ((record-ids (default-to (list) (map-get? patient-records patient-id))))
    (fold find-earliest-due-date record-ids none)))

(define-private (find-earliest-due-date (token-id uint) (earliest (optional uint)))
  (match (map-get? vaccination-records token-id)
    record (match (get next-dose-due record)
             due-date (match earliest
                        current-earliest (if (< due-date current-earliest)
                                           (some due-date)
                                           earliest)
                        (some due-date))
             earliest)
    earliest))

(define-constant err-certificate-not-found (err u114))
(define-constant err-invalid-certificate-data (err u115))
(define-constant err-certificate-expired (err u116))
(define-constant err-certificate-revoked (err u117))
(define-constant err-verifier-not-authorized (err u118))
(define-constant err-template-not-found (err u119))

(define-data-var next-certificate-id uint u1)
(define-data-var next-template-id uint u1)
(define-data-var next-verifier-id uint u1)

(define-map certificate-templates uint
  (tuple
    (name (string-ascii 128))
    (purpose (string-ascii 256))
    (required-vaccines (list 10 (string-ascii 128)))
    (validity-period-days uint)
    (created-by principal)
    (active bool)
  ))

(define-map vaccination-certificates uint
  (tuple
    (patient-hash (buff 32))
    (template-id uint)
    (verification-code (string-ascii 64))
    (issued-date uint)
    (expiry-date uint)
    (issuer principal)
    (status (string-ascii 32))
    (verified-vaccines (list 10 (string-ascii 128)))
  ))

(define-map authorized-verifiers uint
  (tuple
    (organization-name (string-ascii 256))
    (contact-person principal)
    (verification-type (string-ascii 128))
    (authorized-by principal)
    (active bool)
    (registered-date uint)
  ))

(define-map certificate-verifications (tuple (certificate-id uint) (verifier-id uint))
  (tuple
    (verification-date uint)
    (result bool)
    (notes (string-ascii 256))
  ))

(define-map patient-certificates (buff 32) (list 20 uint))
(define-map revoked-certificates uint bool)

(define-public (create-certificate-template
  (name (string-ascii 128))
  (purpose (string-ascii 256))
  (required-vaccines (list 10 (string-ascii 128)))
  (validity-period-days uint))
  (let ((template-id (var-get next-template-id)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (asserts! (> (len name) u0) err-invalid-certificate-data)
    (asserts! (> (len purpose) u0) err-invalid-certificate-data)
    (asserts! (> validity-period-days u0) err-invalid-certificate-data)
    (map-set certificate-templates template-id
      (tuple
        (name name)
        (purpose purpose)
        (required-vaccines required-vaccines)
        (validity-period-days validity-period-days)
        (created-by tx-sender)
        (active true)
      ))
    (var-set next-template-id (+ template-id u1))
    (ok template-id)))

(define-public (register-verifier
  (organization-name (string-ascii 256))
  (contact-person principal)
  (verification-type (string-ascii 128)))
  (let ((verifier-id (var-get next-verifier-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> (len organization-name) u0) err-invalid-certificate-data)
    (asserts! (> (len verification-type) u0) err-invalid-certificate-data)
    (map-set authorized-verifiers verifier-id
      (tuple
        (organization-name organization-name)
        (contact-person contact-person)
        (verification-type verification-type)
        (authorized-by tx-sender)
        (active true)
        (registered-date stacks-block-height)
      ))
    (var-set next-verifier-id (+ verifier-id u1))
    (ok verifier-id)))

(define-public (issue-vaccination-certificate
  (patient-id (string-ascii 64))
  (template-id uint)
  (verification-code (string-ascii 64)))
  (let ((template (unwrap! (map-get? certificate-templates template-id) err-template-not-found))
        (patient-record-list (default-to (list) (map-get? patient-records patient-id)))
        (certificate-id (var-get next-certificate-id))
        (patient-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? patient-id)) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
        (current-certs (default-to (list) (map-get? patient-certificates patient-hash))))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (default-to false (map-get? authorized-issuers tx-sender))) err-unauthorized-issuer)
    (asserts! (get active template) err-template-not-found)
    (asserts! (> (len verification-code) u0) err-invalid-certificate-data)
    (let ((verified-vaccines (validate-patient-vaccines patient-record-list (get required-vaccines template)))
          (expiry-date (+ stacks-block-height (get validity-period-days template))))
      (asserts! (> (len verified-vaccines) u0) err-invalid-vaccine-data)
      (map-set vaccination-certificates certificate-id
        (tuple
          (patient-hash patient-hash)
          (template-id template-id)
          (verification-code verification-code)
          (issued-date stacks-block-height)
          (expiry-date expiry-date)
          (issuer tx-sender)
          (status "active")
          (verified-vaccines verified-vaccines)
        ))
      (map-set patient-certificates patient-hash
        (unwrap! (as-max-len? (append current-certs certificate-id) u20) err-invalid-certificate-data))
      (var-set next-certificate-id (+ certificate-id u1))
      (ok certificate-id))))

(define-public (verify-certificate
  (certificate-id uint)
  (verifier-id uint))
  (let ((certificate (unwrap! (map-get? vaccination-certificates certificate-id) err-certificate-not-found))
        (verifier (unwrap! (map-get? authorized-verifiers verifier-id) err-verifier-not-authorized)))
    (asserts! (get active verifier) err-verifier-not-authorized)
    (asserts! (not (default-to false (map-get? revoked-certificates certificate-id))) err-certificate-revoked)
    (asserts! (< stacks-block-height (get expiry-date certificate)) err-certificate-expired)
    (asserts! (is-eq (get status certificate) "active") err-certificate-revoked)
    (let ((verification-result (tuple
           (certificate-valid true)
           (expiry-date (get expiry-date certificate))
           (verified-vaccines (get verified-vaccines certificate))
           (template-id (get template-id certificate)))))
      (map-set certificate-verifications (tuple (certificate-id certificate-id) (verifier-id verifier-id))
        (tuple
          (verification-date stacks-block-height)
          (result true)
          (notes "Certificate verified successfully")
        ))
      (ok verification-result))))

(define-public (revoke-certificate
  (certificate-id uint)
  (reason (string-ascii 256)))
  (let ((certificate (unwrap! (map-get? vaccination-certificates certificate-id) err-certificate-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (is-eq tx-sender (get issuer certificate))) err-owner-only)
    (map-set vaccination-certificates certificate-id
      (merge certificate (tuple (status "revoked"))))
    (map-set revoked-certificates certificate-id true)
    (ok true)))

(define-public (update-verifier-status
  (verifier-id uint)
  (active bool))
  (let ((verifier (unwrap! (map-get? authorized-verifiers verifier-id) err-verifier-not-authorized)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers verifier-id
      (merge verifier (tuple (active active))))
    (ok true)))

(define-public (batch-verify-certificates
  (certificate-ids (list 10 uint))
  (verifier-id uint))
  (let ((verifier (unwrap! (map-get? authorized-verifiers verifier-id) err-verifier-not-authorized)))
    (asserts! (get active verifier) err-verifier-not-authorized)
    (ok (map verify-single-certificate certificate-ids))))

(define-private (verify-single-certificate (certificate-id uint))
  (match (map-get? vaccination-certificates certificate-id)
    certificate (let ((is-revoked (default-to false (map-get? revoked-certificates certificate-id)))
                      (is-expired (>= stacks-block-height (get expiry-date certificate)))
                      (is-active (is-eq (get status certificate) "active")))
                  (tuple
                    (certificate-id certificate-id)
                    (valid (and (not is-revoked) (not is-expired) is-active))
                    (expiry-date (get expiry-date certificate))
                    (status (get status certificate))
                  ))
    (tuple (certificate-id certificate-id) (valid false) (expiry-date u0) (status "not-found"))))

(define-private (validate-patient-vaccines 
  (patient-record-ids (list 50 uint)) 
  (required-vaccines (list 10 (string-ascii 128))))
  (fold check-required-vaccine required-vaccines (list)))

(define-private (check-required-vaccine 
  (required-vaccine (string-ascii 128)) 
  (verified-vaccines (list 10 (string-ascii 128))))
  (if (has-vaccine-record required-vaccine)
    (unwrap! (as-max-len? (append verified-vaccines required-vaccine) u10) verified-vaccines)
    verified-vaccines))

(define-private (has-vaccine-record (vaccine-name (string-ascii 128)))
  true)

(define-read-only (get-certificate-template (template-id uint))
  (map-get? certificate-templates template-id))

(define-read-only (get-vaccination-certificate (certificate-id uint))
  (map-get? vaccination-certificates certificate-id))

(define-read-only (get-authorized-verifier (verifier-id uint))
  (map-get? authorized-verifiers verifier-id))

(define-read-only (get-patient-certificates (patient-hash (buff 32)))
  (map-get? patient-certificates patient-hash))

(define-read-only (is-certificate-valid (certificate-id uint))
  (match (map-get? vaccination-certificates certificate-id)
    certificate (let ((is-revoked (default-to false (map-get? revoked-certificates certificate-id)))
                      (is-expired (>= stacks-block-height (get expiry-date certificate)))
                      (is-active (is-eq (get status certificate) "active")))
                  (ok (and (not is-revoked) (not is-expired) is-active)))
    err-certificate-not-found))

(define-read-only (get-verification-history (certificate-id uint))
  (let ((verification-keys (list
    (tuple (certificate-id certificate-id) (verifier-id u1))
    (tuple (certificate-id certificate-id) (verifier-id u2))
    (tuple (certificate-id certificate-id) (verifier-id u3))
    (tuple (certificate-id certificate-id) (verifier-id u4))
    (tuple (certificate-id certificate-id) (verifier-id u5)))))
    (map get-single-verification verification-keys)))

(define-private (get-single-verification (key (tuple (certificate-id uint) (verifier-id uint))))
  (map-get? certificate-verifications key))

(define-read-only (get-certificate-by-verification-code (verification-code (string-ascii 64)))
  (let ((all-certs (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
    (filter match-verification-code (map get-vaccination-certificate all-certs))))

(define-private (match-verification-code (cert-opt (optional (tuple 
  (patient-hash (buff 32))
  (template-id uint)
  (verification-code (string-ascii 64))
  (issued-date uint)
  (expiry-date uint)
  (issuer principal)
  (status (string-ascii 32))
  (verified-vaccines (list 10 (string-ascii 128)))))))
  (match cert-opt
    cert true
    false))

(define-read-only (get-active-certificates-count)
  (ok (var-get next-certificate-id)))

(define-read-only (get-template-statistics (template-id uint))
  (ok (tuple
    (template-id template-id)
    (total-issued u0)
    (active-certificates u0)
    (revoked-certificates u0)
  )))



  