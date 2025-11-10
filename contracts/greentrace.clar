
;; title: greentrace
;; version: 1.0.0
;; summary: Supply chain carbon footprint tracking with NFT verification
;; description: Enables manufacturers, logistics providers, and retailers to record
;; carbon emissions data for products throughout the supply chain. Consumers can
;; verify product carbon footprints via QR-linked NFTs and purchase offsets.

;; traits
;;

;; token definitions
(define-non-fungible-token carbon-product-nft uint)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-value (err u104))
(define-constant err-not-manufacturer (err u105))
(define-constant err-product-finalized (err u106))

;; Roles
(define-constant role-manufacturer u1)
(define-constant role-logistics u2)
(define-constant role-retailer u3)

;; data vars
(define-data-var next-product-id uint u1)
(define-data-var carbon-offset-rate uint u1000000) ;; microSTX per kg CO2

;; data maps

;; Registered participants in the supply chain
(define-map participants
    principal
    {
        role: uint,
        name: (string-ascii 100),
        verified: bool,
        registration-block: uint
    }
)

;; Product carbon footprint data
(define-map products
    uint ;; product-id
    {
        manufacturer: principal,
        product-name: (string-ascii 100),
        production-carbon: uint, ;; grams of CO2
        total-carbon: uint, ;; total accumulated carbon in grams
        finalized: bool,
        created-block: uint,
        qr-code: (string-ascii 64) ;; hash of QR code
    }
)

;; Transport/logistics emissions per product
(define-map logistics-entries
    { product-id: uint, entry-id: uint }
    {
        provider: principal,
        transport-carbon: uint, ;; grams of CO2
        distance: uint, ;; km
        transport-mode: (string-ascii 50),
        timestamp: uint
    }
)

;; Track number of logistics entries per product
(define-map product-logistics-count
    uint ;; product-id
    uint ;; count
)

;; Retail carbon disclosure
(define-map retail-listings
    { product-id: uint, retailer: principal }
    {
        listed-carbon: uint,
        listed-at: uint,
        disclosed: bool
    }
)

;; Consumer carbon offsets purchased
(define-map consumer-offsets
    principal
    {
        total-offset: uint, ;; grams of CO2 offset
        total-spent: uint ;; microSTX spent
    }
)

;; Consumer carbon budgets
(define-map consumer-budgets
    principal
    {
        monthly-budget: uint, ;; grams of CO2
        current-usage: uint, ;; grams of CO2 this period
        period-start: uint ;; block height
    }
)

;; public functions

;; Register as a supply chain participant
(define-public (register-participant (role uint) (name (string-ascii 100)))
    (let
        (
            (caller tx-sender)
        )
        ;; Check if already registered
        (asserts! (is-none (map-get? participants caller)) err-already-exists)
        ;; Validate role
        (asserts! (or (is-eq role role-manufacturer) 
                      (is-eq role role-logistics) 
                      (is-eq role role-retailer)) err-invalid-value)
        
        ;; Register participant
        (ok (map-set participants caller {
            role: role,
            name: name,
            verified: false,
            registration-block: block-height
        }))
    )
)

;; Verify a participant (contract owner only)
(define-public (verify-participant (participant principal))
    (let
        (
            (participant-data (unwrap! (map-get? participants participant) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (ok (map-set participants participant
            (merge participant-data { verified: true })
        ))
    )
)

;; Register a new product with manufacturing carbon cost
(define-public (register-product 
    (product-name (string-ascii 100))
    (production-carbon uint)
    (qr-code (string-ascii 64)))
    (let
        (
            (caller tx-sender)
            (product-id (var-get next-product-id))
            (participant-data (unwrap! (map-get? participants caller) err-unauthorized))
        )
        ;; Check if caller is a verified manufacturer
        (asserts! (is-eq (get role participant-data) role-manufacturer) err-not-manufacturer)
        (asserts! (get verified participant-data) err-unauthorized)
        (asserts! (> production-carbon u0) err-invalid-value)
        
        ;; Create product record
        (try! (nft-mint? carbon-product-nft product-id caller))
        (map-set products product-id {
            manufacturer: caller,
            product-name: product-name,
            production-carbon: production-carbon,
            total-carbon: production-carbon,
            finalized: false,
            created-block: block-height,
            qr-code: qr-code
        })
        
        ;; Initialize logistics count
        (map-set product-logistics-count product-id u0)
        
        ;; Increment product ID
        (var-set next-product-id (+ product-id u1))
        
        (ok product-id)
    )
)

;; Add logistics/transport emissions to a product
(define-public (add-logistics-entry
    (product-id uint)
    (transport-carbon uint)
    (distance uint)
    (transport-mode (string-ascii 50)))
    (let
        (
            (caller tx-sender)
            (participant-data (unwrap! (map-get? participants caller) err-unauthorized))
            (product-data (unwrap! (map-get? products product-id) err-not-found))
            (entry-count (default-to u0 (map-get? product-logistics-count product-id)))
        )
        ;; Check if caller is a verified logistics provider
        (asserts! (is-eq (get role participant-data) role-logistics) err-unauthorized)
        (asserts! (get verified participant-data) err-unauthorized)
        (asserts! (not (get finalized product-data)) err-product-finalized)
        (asserts! (> transport-carbon u0) err-invalid-value)
        
        ;; Add logistics entry
        (map-set logistics-entries 
            { product-id: product-id, entry-id: entry-count }
            {
                provider: caller,
                transport-carbon: transport-carbon,
                distance: distance,
                transport-mode: transport-mode,
                timestamp: block-height
            }
        )
        
        ;; Update product total carbon
        (map-set products product-id
            (merge product-data {
                total-carbon: (+ (get total-carbon product-data) transport-carbon)
            })
        )
        
        ;; Increment logistics count
        (map-set product-logistics-count product-id (+ entry-count u1))
        
        (ok entry-count)
    )
)

;; Finalize product carbon footprint (manufacturer only)
(define-public (finalize-product (product-id uint))
    (let
        (
            (caller tx-sender)
            (product-data (unwrap! (map-get? products product-id) err-not-found))
        )
        ;; Only manufacturer can finalize
        (asserts! (is-eq caller (get manufacturer product-data)) err-unauthorized)
        (asserts! (not (get finalized product-data)) err-product-finalized)
        
        ;; Finalize product
        (ok (map-set products product-id
            (merge product-data { finalized: true })
        ))
    )
)

;; Retailer lists product with carbon disclosure
(define-public (list-product-retail (product-id uint))
    (let
        (
            (caller tx-sender)
            (participant-data (unwrap! (map-get? participants caller) err-unauthorized))
            (product-data (unwrap! (map-get? products product-id) err-not-found))
        )
        ;; Check if caller is a verified retailer
        (asserts! (is-eq (get role participant-data) role-retailer) err-unauthorized)
        (asserts! (get verified participant-data) err-unauthorized)
        (asserts! (get finalized product-data) err-product-finalized)
        
        ;; Create retail listing
        (ok (map-set retail-listings
            { product-id: product-id, retailer: caller }
            {
                listed-carbon: (get total-carbon product-data),
                listed-at: block-height,
                disclosed: true
            }
        ))
    )
)

;; Consumer purchases carbon offset
(define-public (purchase-offset (carbon-amount uint))
    (let
        (
            (caller tx-sender)
            (cost (* carbon-amount (var-get carbon-offset-rate)))
            (current-offsets (default-to 
                { total-offset: u0, total-spent: u0 } 
                (map-get? consumer-offsets caller)))
        )
        (asserts! (> carbon-amount u0) err-invalid-value)
        
        ;; Transfer STX for offset (to contract owner)
        (try! (stx-transfer? cost caller contract-owner))
        
        ;; Update consumer offsets
        (ok (map-set consumer-offsets caller {
            total-offset: (+ (get total-offset current-offsets) carbon-amount),
            total-spent: (+ (get total-spent current-offsets) cost)
        }))
    )
)

;; Set consumer carbon budget
(define-public (set-carbon-budget (monthly-budget uint))
    (let
        (
            (caller tx-sender)
        )
        (asserts! (> monthly-budget u0) err-invalid-value)
        
        (ok (map-set consumer-budgets caller {
            monthly-budget: monthly-budget,
            current-usage: u0,
            period-start: block-height
        }))
    )
)

;; Track consumer purchase (updates carbon usage)
(define-public (track-consumer-purchase (product-id uint))
    (let
        (
            (caller tx-sender)
            (product-data (unwrap! (map-get? products product-id) err-not-found))
            (budget-data (unwrap! (map-get? consumer-budgets caller) err-not-found))
        )
        (asserts! (get finalized product-data) err-product-finalized)
        
        ;; Update usage
        (ok (map-set consumer-budgets caller
            (merge budget-data {
                current-usage: (+ (get current-usage budget-data) (get total-carbon product-data))
            })
        ))
    )
)

;; Update carbon offset rate (owner only)
(define-public (set-offset-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-rate u0) err-invalid-value)
        (ok (var-set carbon-offset-rate new-rate))
    )
)

;; read only functions

;; Get participant info
(define-read-only (get-participant (participant principal))
    (ok (map-get? participants participant))
)

;; Get product carbon footprint
(define-read-only (get-product-footprint (product-id uint))
    (ok (map-get? products product-id))
)

;; Get logistics entry
(define-read-only (get-logistics-entry (product-id uint) (entry-id uint))
    (ok (map-get? logistics-entries { product-id: product-id, entry-id: entry-id }))
)

;; Get product logistics count
(define-read-only (get-logistics-count (product-id uint))
    (ok (default-to u0 (map-get? product-logistics-count product-id)))
)

;; Get retail listing
(define-read-only (get-retail-listing (product-id uint) (retailer principal))
    (ok (map-get? retail-listings { product-id: product-id, retailer: retailer }))
)

;; Get consumer offsets
(define-read-only (get-consumer-offsets (consumer principal))
    (ok (map-get? consumer-offsets consumer))
)

;; Get consumer budget
(define-read-only (get-consumer-budget (consumer principal))
    (ok (map-get? consumer-budgets consumer))
)

;; Get current offset rate
(define-read-only (get-offset-rate)
    (ok (var-get carbon-offset-rate))
)

;; Get NFT owner
(define-read-only (get-owner (product-id uint))
    (ok (nft-get-owner? carbon-product-nft product-id))
)

;; Get NFT URI (returns QR code hash)
(define-read-only (get-token-uri (product-id uint))
    (ok (some (get qr-code (unwrap! (map-get? products product-id) err-not-found))))
)

;; Get last token ID
(define-read-only (get-last-token-id)
    (ok (- (var-get next-product-id) u1))
)

;; private functions

;; NFT transfer function (required by trait)
(define-public (transfer (product-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-unauthorized)
        (nft-transfer? carbon-product-nft product-id sender recipient)
    )
)