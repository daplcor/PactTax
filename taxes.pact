(namespace "free")
(define-keyset "free.taxes-gov" (read-keyset "taxes-gov"))

(module taxes GOV

   (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.taxes-gov" ))
   )

   (defcap PAYMENT (account:string year:integer amount:decimal) @event true)
    
   (defschema tax-schema
     @doc "Schema for storing tax account data"
     account:string
     guard:guard
     payments:[object:{payment-schema}]
   )

   (deftable taxes:{tax-schema})

   (defschema payment-schema
     @doc "Schema for storing payment data"
     year:integer
     payment:decimal
    )
    
    (defschema bank-info
        @doc "Stores string values"
        value:string
    )
    
    (deftable bankInfo:{bank-info})

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

  (defun get-costs ()
    (select costing (constantly true))
  )

  (defun get-account (account:string)
   (at 'payments (read taxes account))
  )


  (defun process-payment (year:string account:string guard:guard)
    @doc "Process a payment for generating tax information"
    (let* (
        (costdata (get-payment-year-amount year))
        (cost:decimal (at 'cost costdata))
        (bank:string (get-BANK-account))
    )
    (enforce (validate-principal guard account) "Invalid Account Type")
    (coin.transfer account bank cost)
    (write taxes account
        { "account": account, "guard": guard, "payments": [{'payment: cost, 'year: year}]}
        )
    )
  )

 (defun get-payment-amount:decimal (year:string)
  (at 'cost (read costing year))
 )

 (defun get-payment-year-amount (year:string)
  (read costing year)
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
(create-table bankInfo)
(create-table costing)
(initwallet)
])