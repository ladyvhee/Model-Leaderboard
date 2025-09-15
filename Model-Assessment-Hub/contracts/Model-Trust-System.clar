;; Enhanced Decentralized AI Model Registry and Ranking Platform
;; A comprehensive smart contract enabling AI model registration, community-driven evaluation,
;; reputation-based scoring, and decentralized governance of AI model quality rankings.
;; Features include staking mechanisms, weighted voting, category-based organization,
;; and anti-spam protection through economic incentives.

;; CONSTANTS AND CONFIGURATION
;; Contract governance
(define-constant contract-administrator-principal tx-sender)

;; Enhanced error codes for comprehensive error handling
(define-constant ERR-UNAUTHORIZED-ACCESS-DENIED (err u100))
(define-constant ERR-MODEL-REGISTRATION-NOT-FOUND (err u101))
(define-constant ERR-DUPLICATE-EVALUATION-SUBMISSION (err u102))
(define-constant ERR-INVALID-RATING-SCORE-PROVIDED (err u103))
(define-constant ERR-MODEL-ALREADY-EXISTS (err u104))
(define-constant ERR-INSUFFICIENT-STAKE-AMOUNT (err u105))
(define-constant ERR-INVALID-MODEL-CATEGORY-TYPE (err u106))
(define-constant ERR-MODEL-STATUS-DEACTIVATED (err u107))
(define-constant ERR-WITHDRAWAL-TIME-LOCK-ACTIVE (err u108))
(define-constant ERR-INVALID-STRING-LENGTH-PROVIDED (err u109))
(define-constant ERR-INVALID-COMMENT-LENGTH-EXCEEDED (err u110))

;; Economic parameters for platform operations
(define-constant minimum-registration-stake-amount u1000000) ;; 1 STX in microSTX
(define-constant stake-withdrawal-lockup-period u100) ;; blocks before withdrawal allowed
(define-constant reputation-scoring-weight-multiplier u100)
(define-constant maximum-allowed-rating-score u10)
(define-constant minimum-allowed-rating-score u1)

;; Platform configuration limits
(define-constant maximum-models-per-category-limit u10)
(define-constant model-name-maximum-character-length u100)
(define-constant model-description-maximum-character-length u500)
(define-constant evaluation-comment-maximum-character-length u200)
(define-constant category-name-maximum-character-length u30)
(define-constant ipfs-content-hash-character-length u64)

;; Supported AI model categories - Fixed to use consistent string-ascii 30
(define-constant supported-artificial-intelligence-model-categories 
  (list 
    "natural-language-processing"
    "computer-vision"
    "recommendation-systems"
    "reinforcement-learning"
    "generative-models"
    "speech-recognition"
    "time-series-analysis"
    "other-category"))

;; STATE VARIABLES
(define-data-var next-available-model-registration-identifier uint u1)
(define-data-var total-registered-models-count uint u0)
(define-data-var platform-initialization-completed-status bool false)

;; DATA STRUCTURES AND STORAGE MAPS
;; Comprehensive AI model registry storage
(define-map registered-artificial-intelligence-models
  { model-unique-identifier: uint }
  {
    model-display-name-text: (string-ascii 100),
    detailed-model-description: (string-utf8 500),
    model-creator-principal-address: principal,
    assigned-model-category: (string-ascii 30),
    content-ipfs-hash-reference: (string-ascii 64),
    accumulated-evaluation-vote-count: uint,
    cumulative-weighted-score-total: uint,
    calculated-average-rating-score: uint,
    required-registration-stake-amount: uint,
    registration-block-height-timestamp: uint,
    current-model-active-status: bool,
    last-updated-block-height: uint
  }
)

;; Community voting and evaluation tracking system
(define-map community-model-evaluation-records
  { evaluator-principal-address: principal, target-model-unique-identifier: uint }
  {
    assigned-evaluation-rating-score: uint,
    evaluation-submission-timestamp: uint,
    optional-evaluator-feedback-comment: (optional (string-utf8 200)),
    evaluator-reputation-points-at-vote-time: uint
  }
)

;; Participant economic stake tracking and management
(define-map participant-economic-stake-balances
  { participant-principal-address: principal }
  { 
    total-staked-amount-balance: uint,
    active-model-stake-identifiers: (list 50 uint)
  }
)

;; Dynamic category-based model ranking leaderboards
(define-map category-based-model-leaderboards
  { category-unique-identifier: (string-ascii 30) }
  { 
    ranked-model-identifier-list: (list 10 uint),
    leaderboard-last-update-timestamp: uint,
    category-total-model-count: uint
  }
)

;; Participant reputation and platform activity tracking
(define-map participant-reputation-activity-profiles
  { participant-principal-address: principal }
  {
    accumulated-reputation-points-total: uint,
    total-evaluations-submitted-count: uint,
    total-models-contributed-count: uint,
    quality-evaluation-score-average: uint,
    account-creation-block-height: uint
  }
)

;; Model performance analytics and trending data
(define-map model-performance-analytics-data
  { model-unique-identifier: uint }
  {
    weekly-evaluation-vote-count: uint,
    monthly-evaluation-vote-count: uint,
    trending-popularity-score: uint,
    quality-consistency-rating-score: uint
  }
)

;; READ-ONLY QUERY FUNCTIONS
;; Retrieve complete model registration information
(define-read-only (fetch-complete-model-details (target-model-unique-identifier uint))
  (map-get? registered-artificial-intelligence-models { model-unique-identifier: target-model-unique-identifier })
)

;; Get specific user's evaluation for target model
(define-read-only (fetch-user-model-evaluation (evaluator-principal-address principal) (target-model-unique-identifier uint))
  (map-get? community-model-evaluation-records { evaluator-principal-address: evaluator-principal-address, target-model-unique-identifier: target-model-unique-identifier })
)

;; Retrieve participant's comprehensive reputation profile
(define-read-only (fetch-participant-reputation-profile (participant-principal-address principal))
  (default-to 
    { 
      accumulated-reputation-points-total: u0, 
      total-evaluations-submitted-count: u0, 
      total-models-contributed-count: u0,
      quality-evaluation-score-average: u0,
      account-creation-block-height: u0
    }
    (map-get? participant-reputation-activity-profiles { participant-principal-address: participant-principal-address })
  )
)

;; Get category-specific leaderboard rankings
(define-read-only (fetch-category-based-leaderboard (category-unique-identifier (string-ascii 30)))
  (map-get? category-based-model-leaderboards { category-unique-identifier: category-unique-identifier })
)

;; Query comprehensive platform statistics and metrics
(define-read-only (fetch-platform-wide-statistics)
  {
    total-registered-models: (var-get total-registered-models-count),
    next-model-registration-id: (var-get next-available-model-registration-identifier),
    platform-initialization-status: (var-get platform-initialization-completed-status)
  }
)

;; Validate model category membership against supported list
(define-read-only (validate-supported-model-category (category-unique-identifier (string-ascii 30)))
  (or 
    (is-eq category-unique-identifier "natural-language-processing")
    (is-eq category-unique-identifier "computer-vision")
    (is-eq category-unique-identifier "recommendation-systems")
    (is-eq category-unique-identifier "reinforcement-learning")
    (is-eq category-unique-identifier "generative-models")
    (is-eq category-unique-identifier "speech-recognition")
    (is-eq category-unique-identifier "time-series-analysis")
    (is-eq category-unique-identifier "other-category")
  )
)

;; Calculate reputation-weighted evaluation scoring algorithm
(define-read-only (compute-reputation-weighted-evaluation-score 
  (base-evaluation-rating-score uint) 
  (evaluator-accumulated-reputation-points uint))
  (let ((reputation-weight-calculation-factor (+ u1 (/ evaluator-accumulated-reputation-points reputation-scoring-weight-multiplier))))
    (* base-evaluation-rating-score reputation-weight-calculation-factor)
  )
)

;; Retrieve participant's total economic stake balance
(define-read-only (fetch-participant-total-stake-balance (participant-principal-address principal))
  (default-to u0 
    (get total-staked-amount-balance 
      (map-get? participant-economic-stake-balances { participant-principal-address: participant-principal-address })))
)

;; Check model's current active operational status
(define-read-only (verify-model-current-active-status (target-model-unique-identifier uint))
  (match (fetch-complete-model-details target-model-unique-identifier)
    model-registration-record (get current-model-active-status model-registration-record)
    false
  )
)

;; Get models within specific category with pagination support
(define-read-only (fetch-models-within-category (category-unique-identifier (string-ascii 30)))
  (match (fetch-category-based-leaderboard category-unique-identifier)
    leaderboard-data-record (get ranked-model-identifier-list leaderboard-data-record)
    (list)
  )
)

;; INPUT VALIDATION HELPER FUNCTIONS
;; Validate evaluation comment length if provided by user
(define-private (validate-evaluation-comment-length (comment-optional-parameter (optional (string-utf8 200))))
  (match comment-optional-parameter
    some-comment-text (and (> (len some-comment-text) u0) (<= (len some-comment-text) evaluation-comment-maximum-character-length))
    true  ;; None is always considered valid
  )
)

;; Validate model identifier within acceptable bounds
(define-private (validate-model-identifier-bounds (model-registration-id uint))
  (and (> model-registration-id u0) (< model-registration-id (var-get next-available-model-registration-identifier)))
)

;; CORE PLATFORM FUNCTIONALITY FUNCTIONS
;; Register new AI model with comprehensive validation and staking
(define-public (register-artificial-intelligence-model 
  (model-display-name-text (string-ascii 100))
  (detailed-model-description (string-utf8 500))
  (assigned-model-category (string-ascii 30))
  (content-ipfs-hash-reference (string-ascii 64))
)
  (let (
    (new-model-registration-identifier (var-get next-available-model-registration-identifier))
    (current-block-height-timestamp stacks-block-height)
    (registrant-principal-address tx-sender)
  )
    (asserts! (validate-supported-model-category assigned-model-category) ERR-INVALID-MODEL-CATEGORY-TYPE)
    (asserts! (> (len model-display-name-text) u0) ERR-INVALID-STRING-LENGTH-PROVIDED)
    (asserts! (> (len detailed-model-description) u0) ERR-INVALID-STRING-LENGTH-PROVIDED)
    (asserts! (is-eq (len content-ipfs-hash-reference) ipfs-content-hash-character-length) ERR-INVALID-STRING-LENGTH-PROVIDED)
    
    ;; Economic requirements validation and enforcement
    (asserts! (>= (stx-get-balance registrant-principal-address) minimum-registration-stake-amount) ERR-INSUFFICIENT-STAKE-AMOUNT)
    
    ;; Execute stake transfer to contract escrow
    (try! (stx-transfer? minimum-registration-stake-amount registrant-principal-address (as-contract tx-sender)))
    
    ;; Register model in platform system
    (map-set registered-artificial-intelligence-models
      { model-unique-identifier: new-model-registration-identifier }
      {
        model-display-name-text: model-display-name-text,
        detailed-model-description: detailed-model-description,
        model-creator-principal-address: registrant-principal-address,
        assigned-model-category: assigned-model-category,
        content-ipfs-hash-reference: content-ipfs-hash-reference,
        accumulated-evaluation-vote-count: u0,
        cumulative-weighted-score-total: u0,
        calculated-average-rating-score: u0,
        required-registration-stake-amount: minimum-registration-stake-amount,
        registration-block-height-timestamp: current-block-height-timestamp,
        current-model-active-status: true,
        last-updated-block-height: current-block-height-timestamp
      }
    )
    
    ;; Update participant stake tracking records
    (update-participant-stake-tracking-record registrant-principal-address minimum-registration-stake-amount new-model-registration-identifier)
    
    ;; Update participant reputation profile
    (increment-participant-reputation-points registrant-principal-address "model-registration")
    
    ;; Update platform-wide system counters
    (var-set next-available-model-registration-identifier (+ new-model-registration-identifier u1))
    (var-set total-registered-models-count (+ (var-get total-registered-models-count) u1))
    
    ;; Refresh category-specific leaderboard rankings
    (refresh-category-leaderboard-rankings assigned-model-category)
    
    (ok new-model-registration-identifier)
  )
)

;; Submit community model evaluation with anti-spam protection mechanisms
(define-public (submit-community-model-evaluation 
  (target-model-unique-identifier uint) 
  (assigned-evaluation-rating-score uint) 
  (optional-evaluator-feedback-comment (optional (string-utf8 200))))
  (let (
    (target-model-registration-record (unwrap! (fetch-complete-model-details target-model-unique-identifier) ERR-MODEL-REGISTRATION-NOT-FOUND))
    (current-block-height-timestamp stacks-block-height)
    (evaluator-principal-address tx-sender)
    (evaluator-reputation-activity-profile (fetch-participant-reputation-profile evaluator-principal-address))
    (weighted-evaluation-score-calculation (compute-reputation-weighted-evaluation-score 
                                           assigned-evaluation-rating-score 
                                           (get accumulated-reputation-points-total evaluator-reputation-activity-profile)))
    ;; VALIDATION: Check inputs before processing
    (validated-feedback-comment (begin 
                                 (asserts! (validate-evaluation-comment-length optional-evaluator-feedback-comment) ERR-INVALID-COMMENT-LENGTH-EXCEEDED)
                                 optional-evaluator-feedback-comment))
  )
    ;; Comprehensive validation checks and requirements
    (asserts! (and (>= assigned-evaluation-rating-score minimum-allowed-rating-score) 
                   (<= assigned-evaluation-rating-score maximum-allowed-rating-score)) ERR-INVALID-RATING-SCORE-PROVIDED)
    (asserts! (is-none (fetch-user-model-evaluation evaluator-principal-address target-model-unique-identifier)) ERR-DUPLICATE-EVALUATION-SUBMISSION)
    (asserts! (get current-model-active-status target-model-registration-record) ERR-MODEL-STATUS-DEACTIVATED)
    
    ;; Record evaluation submission in platform system
    (map-set community-model-evaluation-records
      { evaluator-principal-address: evaluator-principal-address, target-model-unique-identifier: target-model-unique-identifier }
      {
        assigned-evaluation-rating-score: assigned-evaluation-rating-score,
        evaluation-submission-timestamp: current-block-height-timestamp,
        optional-evaluator-feedback-comment: validated-feedback-comment,
        evaluator-reputation-points-at-vote-time: (get accumulated-reputation-points-total evaluator-reputation-activity-profile)
      }
    )
    
    ;; Update model statistics with reputation-weighted scoring
    (let (
      (updated-evaluation-vote-count (+ (get accumulated-evaluation-vote-count target-model-registration-record) u1))
      (updated-weighted-score-total (+ (get cumulative-weighted-score-total target-model-registration-record) weighted-evaluation-score-calculation))
      (updated-calculated-average-rating (/ updated-weighted-score-total updated-evaluation-vote-count))
    )
      (map-set registered-artificial-intelligence-models
        { model-unique-identifier: target-model-unique-identifier }
        (merge target-model-registration-record {
          accumulated-evaluation-vote-count: updated-evaluation-vote-count,
          cumulative-weighted-score-total: updated-weighted-score-total,
          calculated-average-rating-score: updated-calculated-average-rating,
          last-updated-block-height: current-block-height-timestamp
        })
      )
    )
    
    ;; Update evaluator's reputation profile
    (increment-participant-reputation-points evaluator-principal-address "eval-submission")
    
    ;; Refresh category-specific leaderboard rankings
    (refresh-category-leaderboard-rankings (get assigned-model-category target-model-registration-record))
    
    (ok true)
  )
)

;; Withdraw model registration stake with time-lock protection
(define-public (withdraw-model-registration-stake (target-model-unique-identifier uint))
  (let (
    (target-model-registration-record (unwrap! (fetch-complete-model-details target-model-unique-identifier) ERR-MODEL-REGISTRATION-NOT-FOUND))
    (current-block-height-timestamp stacks-block-height)
  )
    ;; Authorization and timing validation requirements
    (asserts! (is-eq tx-sender (get model-creator-principal-address target-model-registration-record)) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (> current-block-height-timestamp 
                 (+ (get registration-block-height-timestamp target-model-registration-record) stake-withdrawal-lockup-period)) 
              ERR-WITHDRAWAL-TIME-LOCK-ACTIVE)
    
    ;; Execute stake return to model creator
    (try! (as-contract (stx-transfer? 
                        (get required-registration-stake-amount target-model-registration-record) 
                        tx-sender 
                        (get model-creator-principal-address target-model-registration-record))))
    
    ;; Update participant stake tracking records
    (reduce-participant-stake-tracking-record tx-sender (get required-registration-stake-amount target-model-registration-record))
    
    ;; Deactivate model registration status
    (map-set registered-artificial-intelligence-models
      { model-unique-identifier: target-model-unique-identifier }
      (merge target-model-registration-record { 
        current-model-active-status: false,
        last-updated-block-height: current-block-height-timestamp
      })
    )
    
    (ok true)
  )
)

;; ADMINISTRATIVE AND UTILITY FUNCTIONS
;; Administrative model deactivation with proper validation
(define-public (deactivate-model-administrative-action (target-model-unique-identifier uint))
  (let (
    ;; VALIDATION: Check model identifier before processing
    (validated-model-registration-id (begin 
                                      (asserts! (validate-model-identifier-bounds target-model-unique-identifier) ERR-MODEL-REGISTRATION-NOT-FOUND)
                                      target-model-unique-identifier))
    (target-model-registration-record (unwrap! (fetch-complete-model-details validated-model-registration-id) ERR-MODEL-REGISTRATION-NOT-FOUND))
    (current-block-height-timestamp stacks-block-height)
  )
    ;; Administrative authorization validation
    (asserts! (is-eq tx-sender contract-administrator-principal) ERR-UNAUTHORIZED-ACCESS-DENIED)
    
    ;; Execute model deactivation using validated identifier
    (map-set registered-artificial-intelligence-models
      { model-unique-identifier: validated-model-registration-id }
      (merge target-model-registration-record { 
        current-model-active-status: false,
        last-updated-block-height: current-block-height-timestamp
      })
    )
    
    (ok true)
  )
)

;; Platform initialization with comprehensive category setup
(define-public (initialize-platform-categories)
  (begin
    ;; Initialize all supported AI model categories
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "natural-language-processing" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "computer-vision" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "recommendation-systems" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "reinforcement-learning" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "generative-models" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "speech-recognition" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "time-series-analysis" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    (map-set category-based-model-leaderboards 
      { category-unique-identifier: "other-category" } 
      { ranked-model-identifier-list: (list), leaderboard-last-update-timestamp: stacks-block-height, category-total-model-count: u0 })
    
    ;; Mark platform initialization as completed
    (var-set platform-initialization-completed-status true)
    (ok true)
  )
)

;; INTERNAL HELPER FUNCTIONS
;; Update participant stake tracking records with proper handling
(define-private (update-participant-stake-tracking-record 
  (participant-principal-address principal) 
  (additional-stake-amount uint) 
  (model-registration-identifier uint))
  (let (
    (current-stake-tracking-record (map-get? participant-economic-stake-balances { participant-principal-address: participant-principal-address }))
  )
    (match current-stake-tracking-record
      some-existing-record 
        (let (
          (existing-total-stake-balance (get total-staked-amount-balance some-existing-record))
          (existing-model-stake-identifiers (get active-model-stake-identifiers some-existing-record))
        )
          (map-set participant-economic-stake-balances
            { participant-principal-address: participant-principal-address }
            {
              total-staked-amount-balance: (+ existing-total-stake-balance additional-stake-amount),
              active-model-stake-identifiers: (unwrap-panic (as-max-len? (append existing-model-stake-identifiers model-registration-identifier) u50))
            }
          )
        )
      ;; Create new tracking record if none exists
      (map-set participant-economic-stake-balances
        { participant-principal-address: participant-principal-address }
        {
          total-staked-amount-balance: additional-stake-amount,
          active-model-stake-identifiers: (list model-registration-identifier)
        }
      )
    )
  )
)

;; Reduce participant stake balance with proper validation
(define-private (reduce-participant-stake-tracking-record 
  (participant-principal-address principal) 
  (withdrawn-stake-amount uint))
  (let (
    (current-stake-tracking-record (map-get? participant-economic-stake-balances { participant-principal-address: participant-principal-address }))
  )
    (match current-stake-tracking-record
      some-existing-record 
        (let (
          (current-total-stake-balance (get total-staked-amount-balance some-existing-record))
        )
          (map-set participant-economic-stake-balances
            { participant-principal-address: participant-principal-address }
            (merge some-existing-record {
              total-staked-amount-balance: (- current-total-stake-balance withdrawn-stake-amount)
            })
          )
        )
      ;; Do nothing if no tracking record exists
      false
    )
  )
)

;; Increment participant reputation points based on activity type
(define-private (increment-participant-reputation-points 
  (participant-principal-address principal) 
  (platform-activity-type (string-ascii 20)))
  (let (
    (current-reputation-activity-profile (fetch-participant-reputation-profile participant-principal-address))
    (current-block-height-timestamp stacks-block-height)
  )
    (if (is-eq platform-activity-type "model-registration")
      (map-set participant-reputation-activity-profiles
        { participant-principal-address: participant-principal-address }
        {
          accumulated-reputation-points-total: (+ (get accumulated-reputation-points-total current-reputation-activity-profile) u5),
          total-evaluations-submitted-count: (get total-evaluations-submitted-count current-reputation-activity-profile),
          total-models-contributed-count: (+ (get total-models-contributed-count current-reputation-activity-profile) u1),
          quality-evaluation-score-average: (get quality-evaluation-score-average current-reputation-activity-profile),
          account-creation-block-height: (if (is-eq (get account-creation-block-height current-reputation-activity-profile) u0) 
                                          current-block-height-timestamp 
                                          (get account-creation-block-height current-reputation-activity-profile))
        })
      (map-set participant-reputation-activity-profiles
        { participant-principal-address: participant-principal-address }
        {
          accumulated-reputation-points-total: (+ (get accumulated-reputation-points-total current-reputation-activity-profile) u1),
          total-evaluations-submitted-count: (+ (get total-evaluations-submitted-count current-reputation-activity-profile) u1),
          total-models-contributed-count: (get total-models-contributed-count current-reputation-activity-profile),
          quality-evaluation-score-average: (get quality-evaluation-score-average current-reputation-activity-profile),
          account-creation-block-height: (if (is-eq (get account-creation-block-height current-reputation-activity-profile) u0) 
                                          current-block-height-timestamp 
                                          (get account-creation-block-height current-reputation-activity-profile))
        })
    )
  )
)

;; Refresh category-specific leaderboard rankings with enhanced logic
(define-private (refresh-category-leaderboard-rankings (category-unique-identifier (string-ascii 30)))
  (let (
    (current-leaderboard-data-record (fetch-category-based-leaderboard category-unique-identifier))
    (current-block-height-timestamp stacks-block-height)
  )
    ;; Enhanced leaderboard update logic - simplified for demonstration
    (match current-leaderboard-data-record
      some-existing-data 
        (map-set category-based-model-leaderboards
          { category-unique-identifier: category-unique-identifier }
          {
            ranked-model-identifier-list: (get ranked-model-identifier-list some-existing-data),
            leaderboard-last-update-timestamp: current-block-height-timestamp,
            category-total-model-count: (+ (get category-total-model-count some-existing-data) u1)
          })
      ;; Create new leaderboard if none exists
      (map-set category-based-model-leaderboards
        { category-unique-identifier: category-unique-identifier }
        {
          ranked-model-identifier-list: (list),
          leaderboard-last-update-timestamp: current-block-height-timestamp,
          category-total-model-count: u1
        })
    )
  )
)