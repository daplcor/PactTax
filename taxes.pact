(namespace "free")
(define-keyset "free.taxes-gov" (read-keyset "taxes-gov"))

(module taxes GOV

   (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.taxes-gov" ))
   )

   (defcap MOVEFUNDS ()
    (enforce-guard (keyset-ref-guard "free.taxes-gov" ))
    (compose-capability (BANK))
   )

   (defcap PAYMENT (account:string year:string amount:decimal) @event true)

   (defcap USERPAY (account:string)
    (enforce-guard (at 'guard (coin.details account)))
   )
    
   (defschema tax-schema
     @doc "Schema for storing tax account data"
     account:string
     guard:guard
     payments:[object:{payment-schema}]
   )

   (deftable taxes:{tax-schema})

   (defschema payment-schema
     @doc "Schema for storing payment data"
     year:string
     payment:decimal
    )
   
   (defschema cost-schema
     @doc "Stores cost values"
     year:string
     cost:decimal
   )

   (deftable costing:{cost-schema})

; #######################################
;             Main Functions
; #######################################

  (defun update-costs:string (year:string cost:decimal)
    @doc "Provides costs for the service"
        (with-capability (GOV)
           (write costing year
            { "year": year, "cost": cost}
           )
        )
  )

  (defun get-costs:[object:{cost-schema}] ()
    (select costing (constantly true))
  )

  (defun get-account:[object:{payment-schema}] (account:string)
   (at 'payments (read taxes account))
  )

  (defun get-ac ()
  (select taxes (constantly true))
  )

  (defun process-payment:bool (year:string account:string guard:guard)
    @doc "Process a payment for generating tax information"
    (let* (
          (costdata (get-payment-year-amount year))
          (cost:decimal (at 'cost costdata))
          (bank:string (get-BANK-account))
          )
      (enforce (validate-principal guard account) "Invalid Account Type")
      (with-capability (USERPAY account)
      (coin.transfer account bank cost)
      (with-default-read taxes account
        ;  { 'account: "", 'guard: "", 'payments: []}
        ;  { 'account:=acc, 'guard:=g, 'payments:=p}
        { 'payments: []}
        { 'payments:=p}
        (let ((newp (+ p [{'year: year, 'payment: cost }])))
          (write taxes account
              { "account": account, "guard": guard, "payments": newp}
          )
        )  
      )
     )
     (emit-event (PAYMENT account year cost))    
    )
  )

 (defun get-payment-amount:decimal (year:string)
  (at 'cost (read costing year))
 )

 (defun get-payment-year-amount:object (year:string)
  (read costing year)
 )

 (defun transfer-funds:string (account:string guard:guard amount:decimal)
   @doc "Transfer funds from the BANK account"
   (with-capability (MOVEFUNDS)
    (install-capability (coin.TRANSFER (get-BANK-account) account amount))
    (coin.transfer-create (get-BANK-account) account guard amount)
   )
 )

; #############################################
;                 Wallet Account
; #############################################


(defcap BANK ()
@doc "Checks to make sure the guard for the given account name is satisfied"
true
)

(defun require-BANK ()
@doc "The function used when building the user guard for managed accounts"
(require-capability (BANK))
)

(defun create-BANK-guard ()
@doc "Creates the user guard"
(create-user-guard (require-BANK))
)

(defun get-BANK-account ()
(create-principal (create-BANK-guard))
)

(defun initwallet ()
 (with-capability (GOV)
    (coin.create-account (get-BANK-account) (create-BANK-guard))
 )
)

)

(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table taxes)
(create-table costing)
(initwallet)
])