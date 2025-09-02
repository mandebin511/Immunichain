;; Vaccine Adverse Event Reporting System (VAERS)
;; Tracks and analyzes adverse events following vaccination

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var next-follow-up-id uint u1)
(define-data-var reporting-enabled bool true)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-EVENTS-PER-PATIENT u50)
(define-constant MAX-FOLLOW-UPS-PER-EVENT u10)
(define-constant SEVERITY-MILD u1)
(define-constant SEVERITY-MODERATE u2)
(define-constant SEVERITY-SEVERE u3)
(define-constant SEVERITY-LIFE-THREATENING u4)

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED u200)
(define-constant ERR-EVENT-NOT-FOUND u201)
(define-constant ERR-INVALID-SEVERITY u202)
(define-constant ERR-INVALID-EVENT-DATA u203)
(define-constant ERR-REPORTING-DISABLED u204)
(define-constant ERR-DUPLICATE-REPORT u205)
(define-constant ERR-FOLLOW-UP-NOT-FOUND u206)

;; Data Maps
(define-map adverse-events uint
    {
        patient-id: (string-ascii 64),
        vaccination-record-id: uint,
        vaccine-name: (string-ascii 128),
        vaccine-batch: (string-ascii 64),
        event-description: (string-ascii 500),
        severity-level: uint,
        onset-days-after-vaccination: uint,
        reporter-type: (string-ascii 32),
        reporter-principal: principal,
        medical-attention-required: bool,
        hospitalization-required: bool,
        reported-date: uint,
        status: (string-ascii 32),
        recovery-status: (string-ascii 32)
    }
)

(define-map event-symptoms uint
    {
        symptoms: (list 10 (string-ascii 128)),
        duration-days: uint,
        resolution-date: (optional uint)
    }
)

(define-map follow-up-reports uint
    {
        event-id: uint,
        follow-up-date: uint,
        reporter-principal: principal,
        status-update: (string-ascii 32),
        additional-notes: (string-ascii 500),
        outcome-changed: bool
    }
)

(define-map patient-event-history (string-ascii 64) (list 50 uint))
(define-map vaccine-event-stats (string-ascii 128) 
    {
        total-reports: uint,
        mild-events: uint,
        moderate-events: uint,
        severe-events: uint,
        life-threatening-events: uint,
        total-recoveries: uint
    }
)

(define-map authorized-reporters principal bool)
(define-map event-reviewers uint principal)

;; Public Functions

;; Enable/disable reporting system
(define-public (toggle-reporting (enabled bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
        (var-set reporting-enabled enabled)
        (ok true)
    )
)

;; Authorize healthcare provider to report events
(define-public (authorize-reporter (reporter principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
        (map-set authorized-reporters reporter true)
        (ok true)
    )
)

;; Report adverse event
(define-public (report-adverse-event
    (patient-id (string-ascii 64))
    (vaccination-record-id uint)
    (vaccine-name (string-ascii 128))
    (vaccine-batch (string-ascii 64))
    (event-description (string-ascii 500))
    (severity-level uint)
    (onset-days-after-vaccination uint)
    (reporter-type (string-ascii 32))
    (medical-attention-required bool)
    (hospitalization-required bool)
    (symptoms (list 10 (string-ascii 128)))
    (duration-days uint))
    (let
        (
            (event-id (var-get next-event-id))
            (is-authorized (or 
                (is-eq tx-sender CONTRACT-OWNER)
                (default-to false (map-get? authorized-reporters tx-sender))
            ))
            (current-events (default-to (list) (map-get? patient-event-history patient-id)))
        )
        (asserts! (var-get reporting-enabled) (err ERR-REPORTING-DISABLED))
        (asserts! is-authorized (err ERR-NOT-AUTHORIZED))
        (asserts! (and (>= severity-level u1) (<= severity-level u4)) (err ERR-INVALID-SEVERITY))
        (asserts! (> (len event-description) u0) (err ERR-INVALID-EVENT-DATA))
        (asserts! (> (len vaccine-name) u0) (err ERR-INVALID-EVENT-DATA))
        
        ;; Store main event record
        (map-set adverse-events event-id
            {
                patient-id: patient-id,
                vaccination-record-id: vaccination-record-id,
                vaccine-name: vaccine-name,
                vaccine-batch: vaccine-batch,
                event-description: event-description,
                severity-level: severity-level,
                onset-days-after-vaccination: onset-days-after-vaccination,
                reporter-type: reporter-type,
                reporter-principal: tx-sender,
                medical-attention-required: medical-attention-required,
                hospitalization-required: hospitalization-required,
                reported-date: stacks-block-height,
                status: "under-review",
                recovery-status: "ongoing"
            }
        )
        
        ;; Store symptom details
        (map-set event-symptoms event-id
            {
                symptoms: symptoms,
                duration-days: duration-days,
                resolution-date: none
            }
        )
        
        ;; Update patient history
        (map-set patient-event-history patient-id 
            (unwrap! (as-max-len? (append current-events event-id) u50) (err ERR-INVALID-EVENT-DATA)))
        
        ;; Update vaccine statistics
        (unwrap! (update-vaccine-stats vaccine-name severity-level) (err ERR-INVALID-EVENT-DATA))
        
        (var-set next-event-id (+ event-id u1))
        (ok event-id)
    )
)

;; Submit follow-up report
(define-public (submit-follow-up
    (event-id uint)
    (status-update (string-ascii 32))
    (additional-notes (string-ascii 500))
    (outcome-changed bool))
    (let
        (
            (event (unwrap! (map-get? adverse-events event-id) (err ERR-EVENT-NOT-FOUND)))
            (follow-up-id (var-get next-follow-up-id))
            (is-authorized (or 
                (is-eq tx-sender CONTRACT-OWNER)
                (default-to false (map-get? authorized-reporters tx-sender))
                (is-eq tx-sender (get reporter-principal event))
            ))
        )
        (asserts! is-authorized (err ERR-NOT-AUTHORIZED))
        
        (map-set follow-up-reports follow-up-id
            {
                event-id: event-id,
                follow-up-date: stacks-block-height,
                reporter-principal: tx-sender,
                status-update: status-update,
                additional-notes: additional-notes,
                outcome-changed: outcome-changed
            }
        )
        
        ;; Update event status if specified
        (if (> (len status-update) u0)
            (map-set adverse-events event-id 
                (merge event { recovery-status: status-update }))
            true
        )
        
        (var-set next-follow-up-id (+ follow-up-id u1))
        (ok follow-up-id)
    )
)

;; Update event status (for authorized reviewers)
(define-public (update-event-status
    (event-id uint)
    (new-status (string-ascii 32))
    (recovery-status (string-ascii 32)))
    (let
        (
            (event (unwrap! (map-get? adverse-events event-id) (err ERR-EVENT-NOT-FOUND)))
            (is-authorized (or 
                (is-eq tx-sender CONTRACT-OWNER)
                (default-to false (map-get? authorized-reporters tx-sender))
            ))
        )
        (asserts! is-authorized (err ERR-NOT-AUTHORIZED))
        
        (map-set adverse-events event-id 
            (merge event 
                { 
                    status: new-status,
                    recovery-status: recovery-status
                }
            ))
        
        ;; Update vaccine stats if recovery status changed
        (if (is-eq recovery-status "recovered")
            (unwrap! (increment-recovery-count (get vaccine-name event)) (err ERR-INVALID-EVENT-DATA))
            true
        )
        
        (ok true)
    )
)

;; Mark symptoms as resolved
(define-public (resolve-symptoms
    (event-id uint)
    (resolution-date uint))
    (let
        (
            (event (unwrap! (map-get? adverse-events event-id) (err ERR-EVENT-NOT-FOUND)))
            (symptoms (unwrap! (map-get? event-symptoms event-id) (err ERR-EVENT-NOT-FOUND)))
            (is-authorized (or 
                (is-eq tx-sender CONTRACT-OWNER)
                (is-eq tx-sender (get reporter-principal event))
                (default-to false (map-get? authorized-reporters tx-sender))
            ))
        )
        (asserts! is-authorized (err ERR-NOT-AUTHORIZED))
        
        (map-set event-symptoms event-id 
            (merge symptoms { resolution-date: (some resolution-date) }))
        
        (ok true)
    )
)

;; Read-only Functions

(define-read-only (get-adverse-event (event-id uint))
    (map-get? adverse-events event-id)
)

(define-read-only (get-event-symptoms (event-id uint))
    (map-get? event-symptoms event-id)
)

(define-read-only (get-follow-up-report (follow-up-id uint))
    (map-get? follow-up-reports follow-up-id)
)

(define-read-only (get-patient-event-history (patient-id (string-ascii 64)))
    (map-get? patient-event-history patient-id)
)

(define-read-only (get-vaccine-safety-stats (vaccine-name (string-ascii 128)))
    (default-to
        {
            total-reports: u0,
            mild-events: u0,
            moderate-events: u0,
            severe-events: u0,
            life-threatening-events: u0,
            total-recoveries: u0
        }
        (map-get? vaccine-event-stats vaccine-name)
    )
)

(define-read-only (is-authorized-reporter (reporter principal))
    (default-to false (map-get? authorized-reporters reporter))
)

(define-read-only (get-reporting-status)
    (ok (var-get reporting-enabled))
)

(define-read-only (get-severe-events-by-vaccine (vaccine-name (string-ascii 128)))
    (let ((stats (get-vaccine-safety-stats vaccine-name)))
        (ok (+ (get severe-events stats) (get life-threatening-events stats)))
    )
)

(define-read-only (calculate-safety-profile (vaccine-name (string-ascii 128)))
    (let
        (
            (stats (get-vaccine-safety-stats vaccine-name))
            (total-events (get total-reports stats))
            (severe-events (+ (get severe-events stats) (get life-threatening-events stats)))
        )
        (ok {
            vaccine-name: vaccine-name,
            total-events: total-events,
            severe-event-rate: (if (> total-events u0) (/ (* severe-events u10000) total-events) u0),
            recovery-rate: (if (> total-events u0) (/ (* (get total-recoveries stats) u10000) total-events) u0),
            safety-score: (if (> total-events u0) (- u10000 (/ (* severe-events u10000) total-events)) u10000)
        })
    )
)

;; Private Functions

(define-private (update-vaccine-stats (vaccine-name (string-ascii 128)) (severity uint))
    (let
        (
            (current-stats (get-vaccine-safety-stats vaccine-name))
            (updated-stats (merge current-stats
                {
                    total-reports: (+ (get total-reports current-stats) u1),
                    mild-events: (if (is-eq severity u1) (+ (get mild-events current-stats) u1) (get mild-events current-stats)),
                    moderate-events: (if (is-eq severity u2) (+ (get moderate-events current-stats) u1) (get moderate-events current-stats)),
                    severe-events: (if (is-eq severity u3) (+ (get severe-events current-stats) u1) (get severe-events current-stats)),
                    life-threatening-events: (if (is-eq severity u4) (+ (get life-threatening-events current-stats) u1) (get life-threatening-events current-stats))
                }
            ))
        )
        (map-set vaccine-event-stats vaccine-name updated-stats)
        (ok true)
    )
)

(define-private (increment-recovery-count (vaccine-name (string-ascii 128)))
    (let
        (
            (current-stats (get-vaccine-safety-stats vaccine-name))
            (updated-stats (merge current-stats
                {
                    total-recoveries: (+ (get total-recoveries current-stats) u1)
                }
            ))
        )
        (map-set vaccine-event-stats vaccine-name updated-stats)
        (ok true)
    )
)
