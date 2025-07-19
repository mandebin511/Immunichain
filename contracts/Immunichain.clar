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