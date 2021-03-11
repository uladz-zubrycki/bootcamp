### Description

```
  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  object Homework {
    case class PaymentCard( /* Add parameters as needed */ )
    sealed trait ValidationError
    object ValidationError {
      ??? // Add errors as needed
    }
    object PaymentCardValidator {
      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]
      def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String,
      ): AllErrorsOr[PaymentCard] = ???
    }
  }
```