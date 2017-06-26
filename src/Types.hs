{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Country(..)
  , FixerResponse(..)
  , FixerDate(..)
  ) where

import Data.Aeson
import Data.Map.Lazy (Map)
import Data.Monoid (mconcat)
import qualified Data.Text as Text
import GHC.Generics

data Country
  = AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BHD
  | BIF
  | BMD
  | BND
  | BOB
  | BOV
  | BRL
  | BSD
  | BTN
  | BWP
  | BYN
  | BZD
  | CAD
  | CDF
  | CHE
  | CHF
  | CHW
  | CLF
  | CLP
  | CNY
  | COP
  | COU
  | CRC
  | CUC
  | CUP
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ERN
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GHS
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | IQD
  | IRR
  | ISK
  | JMD
  | JOD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KPW
  | KRW
  | KWD
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | LYD
  | MAD
  | MDL
  | MGA
  | MKD
  | MMK
  | MNT
  | MOP
  | MRO
  | MUR
  | MVR
  | MWK
  | MXN
  | MXV
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | OMR
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SDG
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | SSP
  | STD
  | SVC
  | SYP
  | SZL
  | THB
  | TJS
  | TMT
  | TND
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | USD
  | USN
  | UYI
  | UYU
  | UZS
  | VEF
  | VND
  | VUV
  | WST
  | XAF
  | XAG
  | XAU
  | XBA
  | XBB
  | XBC
  | XBD
  | XCD
  | XDR
  | XOF
  | XPD
  | XPF
  | XPT
  | XSU
  | XTS
  | XUA
  | XXX
  | YER
  | ZAR
  | ZMW
  | ZWL
  deriving (Show, Read, Generic, Ord, Eq)

instance FromJSON Country

instance ToJSON Country

instance FromJSONKey Country where
  fromJSONKey = FromJSONKeyText (read . Text.unpack)

instance ToJSONKey Country

data FixerResponse = FixerResponse
  { base :: Country
  , date :: String
  , rates :: Map Country Float
  } deriving (Generic, Eq, Show)

instance FromJSON FixerResponse

instance ToJSON FixerResponse

-- Prefer to use existing types. `Data.Time` has a `Day` type that does
-- exactly this.
data FixerDate = FixerDate
  { year :: Int
  , month :: Int
  , day :: Int
  }

-- Show instances should almost always be derived, and used to generate
-- valid Haskell code. They should not be for pretty-printing or displaying
-- code to the user.
instance Show FixerDate where
  show x = mconcat [(show $ year x), "-", (show $ month x), "-", (show $ day x)]
