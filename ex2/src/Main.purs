module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (fromFoldable)
import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.String (null, length, toLower, toUpper)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid, unV)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- EXERCISE 2 - REPLACE `?solution` WITH YOUR SOLUTION!

-- | Validate that an input string contains some mix of upper- and lowercase
-- | characters
validateContainsMixedCase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsMixedCase input = ?solution

--------------------------------------------------------------------------------
main :: Eff (console :: CONSOLE) Unit
main = do
  let validTest = validateContainsMixedCase "Hello!"
  logShow (printValidation validTest)
  
  log "\n"
  
  let noUppercaseTest = validateContainsMixedCase "hello!"
  logShow (printValidation noUppercaseTest)
  
  log "\n"
  
  let noLowercaseTest = validateContainsMixedCase "HELLO!"
  logShow (printValidation noLowercaseTest)

--------------------------------------------------------------------------------
-- TYPES AND FUNCTIONS FOR PRIMITIVE VALIDATIONS

-- | Type of validation errors encountered when validating primitive input
data InvalidPrimitive 
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | NoLowercase String
  | NoUppercase String

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V (NonEmptyList InvalidPrimitive) String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input
  
-- | Validates that an input string conforms to some regular expression that 
-- | checks for valid email addresses
validateEmailRegex :: String -> V (NonEmptyList InvalidPrimitive) String
validateEmailRegex input
  | test emailRegex input = pure input
  | otherwise = invalid (singleton (InvalidEmail input))

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMinimumLength input minLength
  | (length input) < minLength = invalid (singleton (TooShort (length input) minLength))
  | otherwise = pure input
  
-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMaximumLength input maxLength
  | (length input) < maxLength = invalid (singleton (TooLong (length input) maxLength))
  | otherwise = pure input
  
-- | Validate that an input string is within the minimum and maximum given `Int`
-- | lengths
validateLength
  :: String
  -> Int
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateLength input minLength maxLength =
     validateMinimumLength input minLength
  *> validateMaximumLength input maxLength

-- | Validate that an input string contains at least one lowercase character
validateContainsLowercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsLowercase input
  | (toUpper input) == input = invalid (singleton (NoLowercase input))
  | otherwise = pure input
  
-- | Validate that an input string contains at least one uppercase character
validateContainsUppercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsUppercase input
  | (toLower input) == input = invalid (singleton (NoUppercase input))
  | otherwise = pure input

---------------------------------------------------------------------------------
-- !!! BOILERPLATE TO MAKE EVERYTHING A LITTLE EASIER TO WORK WITH            !!!
---------------------------------------------------------------------------------

-- | Helper function to print validations
printValidation 
  :: forall err
   . Show err
  => V (NonEmptyList err) String
  -> String
printValidation = 
  unV (show <<< fromFoldable) (\result -> "Valid: " <> result)

-- | Derive a `Generic` instance for `InvalidPrimitive` so we can get a `Show` 
-- | instance to print to the console.
derive instance genericInvalidPrimitive :: Generic InvalidPrimitive _

-- | Derive `show` for `InvalidPrimitive` using the `Generic` instance.
instance showInvalidPrimitive :: Show InvalidPrimitive where
  show = genericShow
  
-- | Utility function to unsafely construct a regular expression from a pattern
-- | string.
-- |
-- | This will fail at runtime with an error if the pattern string is invalid.
unsafeRegexFromString :: String -> Regex
unsafeRegexFromString str = unsafePartial (fromRight (regex str noFlags))

-- | Regular expression for email address validation.
emailRegex :: Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"