;; Implementation Planning Contract
;; Manages the planning and resource allocation for improvement implementations

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_NOT_FOUND (err u301))
(define-constant ERR_INVALID_STATUS (err u302))
(define-constant ERR_INSUFFICIENT_BUDGET (err u303))
(define-constant ERR_INVALID_TIMELINE (err u304))

;; Data structures
(define-map implementation-plans
  { plan-id: uint }
  {
    improvement-id: uint,
    coordinator: principal,
    title: (string-ascii 100),
    total-budget: uint,
    allocated-budget: uint,
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20),
    creation-block: uint,
    team-size: uint
  }
)

(define-map plan-milestones
  { plan-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    target-block: uint,
    budget-allocation: uint,
    status: (string-ascii 20),
    completion-block: (optional uint)
  }
)

(define-map resource-allocations
  { plan-id: uint, resource-type: (string-ascii 50) }
  {
    amount: uint,
    unit: (string-ascii 20),
    allocated-block: uint,
    status: (string-ascii 20)
  }
)

(define-map team-assignments
  { plan-id: uint, member: principal }
  {
    role: (string-ascii 50),
    assignment-block: uint,
    status: (string-ascii 20)
  }
)

(define-data-var next-plan-id uint u1)
(define-data-var total-plans uint u0)
(define-data-var max-team-size uint u20)

;; Public functions

(define-public (create-implementation-plan
  (improvement-id uint)
  (coordinator principal)
  (title (string-ascii 100))
  (total-budget uint)
  (duration-blocks uint))
  (let (
    (plan-id (var-get next-plan-id))
    (start-block block-height)
    (end-block (+ start-block duration-blocks))
  )
    (asserts! (> duration-blocks u0) ERR_INVALID_TIMELINE)
    (asserts! (> total-budget u0) ERR_INSUFFICIENT_BUDGET)

    (map-set implementation-plans
      { plan-id: plan-id }
      {
        improvement-id: improvement-id,
        coordinator: coordinator,
        title: title,
        total-budget: total-budget,
        allocated-budget: u0,
        start-block: start-block,
        end-block: end-block,
        status: "planning",
        creation-block: block-height,
        team-size: u0
      }
    )

    (var-set next-plan-id (+ plan-id u1))
    (var-set total-plans (+ (var-get total-plans) u1))

    (print { event: "plan-created", id: plan-id, coordinator: coordinator })
    (ok plan-id)
  )
)

(define-public (add-milestone
  (plan-id uint)
  (milestone-id uint)
  (title (string-ascii 100))
  (description (string-ascii 300))
  (target-block uint)
  (budget-allocation uint))
  (let (
    (plan (unwrap! (map-get? implementation-plans { plan-id: plan-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get coordinator plan)) ERR_UNAUTHORIZED)
    (asserts! (>= target-block (get start-block plan)) ERR_INVALID_TIMELINE)
    (asserts! (<= target-block (get end-block plan)) ERR_INVALID_TIMELINE)

    (map-set plan-milestones
      { plan-id: plan-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        target-block: target-block,
        budget-allocation: budget-allocation,
        status: "planned",
        completion-block: none
      }
    )

    ;; Update allocated budget
    (let (
      (new-allocated (+ (get allocated-budget plan) budget-allocation))
    )
      (asserts! (<= new-allocated (get total-budget plan)) ERR_INSUFFICIENT_BUDGET)
      (map-set implementation-plans
        { plan-id: plan-id }
        (merge plan { allocated-budget: new-allocated })
      )
    )

    (print { event: "milestone-added", plan-id: plan-id, milestone-id: milestone-id })
    (ok true)
  )
)

(define-public (allocate-resource
  (plan-id uint)
  (resource-type (string-ascii 50))
  (amount uint)
  (unit (string-ascii 20)))
  (let (
    (plan (unwrap! (map-get? implementation-plans { plan-id: plan-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get coordinator plan)) ERR_UNAUTHORIZED)

    (map-set resource-allocations
      { plan-id: plan-id, resource-type: resource-type }
      {
        amount: amount,
        unit: unit,
        allocated-block: block-height,
        status: "allocated"
      }
    )

    (print { event: "resource-allocated", plan-id: plan-id, resource: resource-type, amount: amount })
    (ok true)
  )
)

(define-public (assign-team-member
  (plan-id uint)
  (member principal)
  (role (string-ascii 50)))
  (let (
    (plan (unwrap! (map-get? implementation-plans { plan-id: plan-id }) ERR_NOT_FOUND))
    (current-team-size (get team-size plan))
  )
    (asserts! (is-eq tx-sender (get coordinator plan)) ERR_UNAUTHORIZED)
    (asserts! (< current-team-size (var-get max-team-size)) ERR_UNAUTHORIZED)

    (map-set team-assignments
      { plan-id: plan-id, member: member }
      {
        role: role,
        assignment-block: block-height,
        status: "assigned"
      }
    )

    ;; Update team size
    (map-set implementation-plans
      { plan-id: plan-id }
      (merge plan { team-size: (+ current-team-size u1) })
    )

    (print { event: "team-member-assigned", plan-id: plan-id, member: member, role: role })
    (ok true)
  )
)

(define-public (update-plan-status (plan-id uint) (new-status (string-ascii 20)))
  (let (
    (plan (unwrap! (map-get? implementation-plans { plan-id: plan-id }) ERR_NOT_FOUND))
  )
    (asserts! (or (is-eq tx-sender (get coordinator plan)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)

    (map-set implementation-plans
      { plan-id: plan-id }
      (merge plan { status: new-status })
    )

    (print { event: "plan-status-updated", plan-id: plan-id, status: new-status })
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-implementation-plan (plan-id uint))
  (map-get? implementation-plans { plan-id: plan-id })
)

(define-read-only (get-milestone (plan-id uint) (milestone-id uint))
  (map-get? plan-milestones { plan-id: plan-id, milestone-id: milestone-id })
)

(define-read-only (get-resource-allocation (plan-id uint) (resource-type (string-ascii 50)))
  (map-get? resource-allocations { plan-id: plan-id, resource-type: resource-type })
)

(define-read-only (get-team-assignment (plan-id uint) (member principal))
  (map-get? team-assignments { plan-id: plan-id, member: member })
)

(define-read-only (get-total-plans)
  (var-get total-plans)
)

(define-read-only (get-next-plan-id)
  (var-get next-plan-id)
)
