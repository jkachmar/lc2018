module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (fromFoldable)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.String (length, null, toUpper)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Validation.Semigroup (V, invalid, unV)

--------------------------------------------------------------------------------
-- EXERCISE 1.1 - REPLACE `?solution` WITH YOUR SOLUTION!

-- | Validate that an input string is not empty
validateNonEmpty 
  :: String 
  -> V (NonEmptyList InvalidPrimitive) String
validateNonEmpty input = ?solution

--------------------------------------------------------------------------------
main :: Eff (console :: CONSOLE) Unit
main = do
  let validTest = validateNonEmpty "Hello!"
  logShow (printValidation validTest)
  
  log "\n"
  
  let invalidTest = validateNonEmpty ""
  logShow (printValidation invalidTest)

--------------------------------------------------------------------------------
-- TYPES AND FUNCTIONS DEFINED EARLIER IN THE PRESENTATION

-- | Type of validation errors encountered when validating primitive input
data InvalidPrimitive 
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | NoLowercase String
  | NoUppercase String

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMinimumLength input minLength
  | (length input) < minLength = invalid (singleton (TooShort (length input) minLength))
  | otherwise = pure input

-- | Validate that an input string contains at least one lowercase element
validateContainsLowercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsLowercase input
  | (toUpper input) == input = invalid (singleton (NoLowercase input))
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
