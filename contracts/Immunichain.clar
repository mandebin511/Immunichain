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