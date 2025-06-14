(define-data-var donation-pool uint u0)
(define-data-var total-donors uint u0)
(define-data-var next-cycle-block uint u0)

(define-constant voting-duration u10080) ;; ~1 week
(define-constant cycle-interval u2100) ;; ~2 weeks
(define-constant donation-ratio u80) ;; 80% of yield donated

(define-map stacked-delegates principal (tuple (amount uint)))
(define-map proposals uint (tuple (recipient principal) (votes uint)))
(define-map matching-pool principal (tuple (amount uint) (threshold uint)))
(define-map recipient-history principal uint)
(define-map reputation principal uint)

(define-non-fungible-token reputation-token uint)

;; Donate STX to the pool
(define-public (donate (amount uint))
  (begin
    (asserts! (> amount u0) (err u100))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set donation-pool (+ (var-get donation-pool) amount))
    (var-set total-donors (+ (var-get total-donors) u1))
    (ok amount)
  )
)

;; Delegate STX for stacking (simplified)
(define-public (delegate-stack (amount uint))
  (begin
    (asserts! (> amount u0) (err u101))
    (map-set stacked-delegates tx-sender { amount: amount })
    (ok true)
  )
)

;; Submit a proposal
(define-public (submit-proposal (proposal-id uint) (recipient principal))
  (begin
    ;; Removed invalid is-principal check
    (map-set proposals proposal-id { recipient: recipient, votes: u0 })
    (ok true)
  )
)

;; Vote on proposal (1 vote = 1 reputation point)
(define-public (vote (proposal-id uint) (weight uint))
  (begin
    (asserts! (> weight u0) (err u102))
    (let ((rep (default-to u0 (map-get? reputation tx-sender))))
      (asserts! (>= rep weight) (err u103))
      (let ((prop (map-get? proposals proposal-id)))
        (match prop
          proposal-data (let ((updated-votes (+ (get votes proposal-data) weight)))
              (map-set proposals proposal-id {
                recipient: (get recipient proposal-data),
                votes: updated-votes
              })
              (ok true))
          (err u104))
      )
    )
  )
)

;; Execute cycle donation
(define-public (execute-cycle (proposal-id uint))
  (let (
    (current-block burn-block-height)
    (last-cycle (var-get next-cycle-block))
  )
    (asserts! (>= current-block last-cycle) (err u105))
    (let ((prop (map-get? proposals proposal-id)))
      (match prop
        proposal-data (let ((recipient (get recipient proposal-data))
                (yield (/ (var-get donation-pool) u10)) ;; assume 10% as yield
                (donation (/ (* (/ (var-get donation-pool) u10) donation-ratio) u100))
              )
            ;; check recipient not funded recently
            (let ((last-received (default-to u0 (map-get? recipient-history recipient))))
              (asserts! (< (- current-block last-received) (* u3 cycle-interval)) (err u106))
              (try! (stx-transfer? donation (as-contract tx-sender) recipient))
              (map-set recipient-history recipient current-block)
              (var-set next-cycle-block (+ current-block cycle-interval))
              (ok donation))
        )
        (err u107)
      )
    )
  )
)

;; Donate with matching requirement
(define-public (donate-matching (amount uint) (threshold uint))
  (begin
    (asserts! (> threshold amount) (err u108))
    (map-set matching-pool tx-sender { amount: amount, threshold: threshold })
    (ok true)
  )
)

;; Unlock matching donation when threshold is reached
(define-public (unlock-matching (donor principal))
  (let ((match-data (map-get? matching-pool donor)))
    (match match-data
      data (begin
          (asserts! (>= (var-get donation-pool) (get threshold data)) (err u109))
          (try! (stx-transfer? (get amount data) donor (as-contract tx-sender)))
          (map-delete matching-pool donor)
          (ok true)
        )
      (err u110)
    )
  )
)

;; Mint non-transferable reputation token (for donors)
(define-public (mint-reputation (amount uint))
  (begin
    (asserts! (> amount u0) (err u111))
    (let ((existing (default-to u0 (map-get? reputation tx-sender))))
      (map-set reputation tx-sender (+ existing amount))
      (ok true)
    )
  )
)
