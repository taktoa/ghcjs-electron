{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A wrapper over the Electron locale API, as documented
-- <https://electron.atom.io/docs/api/locales here>.
module GHCJS.Electron.Locales
  ( LocaleID, Locale (..), LocaleData (..)
  , getLocaleID, getLocale, toLocaleData, parseLocale, localeMap
  ) where

import           Data.Maybe      (fromJust)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Data.JSString   (JSString)
import qualified Data.JSString   as JSString

import           Data.Data       (Data)
import           GHC.Generics    (Generic)

foreign import javascript safe
  "$r = require('electron').app.getLocale();"
  appGetLocale :: IO JSString

-- | Get the current 'LocaleID'.
getLocaleID :: IO LocaleID
getLocaleID = Text.pack . JSString.unpack <$> appGetLocale

-- | Get the current 'Locale'.
getLocale :: IO (Maybe Locale)
getLocale = parseLocale <$> getLocaleID

-- | Get the 'LocaleData' corresponding to the given 'Locale'.
toLocaleData :: Locale -> LocaleData
toLocaleData = fromJust . flip Map.lookup localeMap

-- | Get the 'Locale' corresponding to the given IETF language tag, assuming
-- that such a 'Locale' exists in the 'localeMap'.
parseLocale :: LocaleID -> Maybe Locale
parseLocale = flip Map.lookup
              (Map.fromList ((\(loc, ld) -> (_localeDataID ld, loc))
                             <$> Map.toList localeMap))

-- | An IETF language tag, as specified in RFC 5646 and RFC 4647.
type LocaleID = Text

-- | A 'LocaleData' contains two pieces of information:
-- 1. The locale ID, as an IETF language tag.
-- 2. The name of the locale, as English text.
data LocaleData
  = MkLocaleData
    { _localeDataID   :: !LocaleID
    , _localeDataName :: !Text
    }
  deriving (Eq, Ord, Show, Generic)

-- | The type of locales.
data Locale
  = Locale_AF    -- ^ Afrikaans
  | Locale_AN    -- ^ Aragonese
  | Locale_AR_AE -- ^ Arabic (U.A.E.)
  | Locale_AR_IQ -- ^ Arabic (Iraq)
  | Locale_AR    -- ^ Arabic (Standard)
  | Locale_AR_BH -- ^ Arabic (Bahrain)
  | Locale_AR_DZ -- ^ Arabic (Algeria)
  | Locale_AR_EG -- ^ Arabic (Egypt)
  | Locale_AR_JO -- ^ Arabic (Jordan)
  | Locale_AR_KW -- ^ Arabic (Kuwait)
  | Locale_AR_LB -- ^ Arabic (Lebanon)
  | Locale_AR_LY -- ^ Arabic (Libya)
  | Locale_AR_MA -- ^ Arabic (Morocco)
  | Locale_AR_OM -- ^ Arabic (Oman)
  | Locale_AR_QA -- ^ Arabic (Qatar)
  | Locale_AR_SA -- ^ Arabic (Saudi Arabia)
  | Locale_AR_SY -- ^ Arabic (Syria)
  | Locale_AR_TN -- ^ Arabic (Tunisia)
  | Locale_AR_YE -- ^ Arabic (Yemen)
  | Locale_AS    -- ^ Assamese
  | Locale_AST   -- ^ Asturian
  | Locale_AZ    -- ^ Azerbaijani
  | Locale_BE    -- ^ Belarusian
  | Locale_BG    -- ^ Bulgarian
  | Locale_BN    -- ^ Bengali
  | Locale_BR    -- ^ Breton
  | Locale_BS    -- ^ Bosnian
  | Locale_CA    -- ^ Catalan
  | Locale_CE    -- ^ Chechen
  | Locale_CH    -- ^ Chamorro
  | Locale_CO    -- ^ Corsican
  | Locale_CR    -- ^ Cree
  | Locale_CS    -- ^ Czech
  | Locale_CV    -- ^ Chuvash
  | Locale_DA    -- ^ Danish
  | Locale_DE    -- ^ German (Standard)
  | Locale_DE_AT -- ^ German (Austria)
  | Locale_DE_CH -- ^ German (Switzerland)
  | Locale_DE_DE -- ^ German (Germany)
  | Locale_DE_LI -- ^ German (Liechtenstein)
  | Locale_DE_LU -- ^ German (Luxembourg)
  | Locale_EL    -- ^ Greek
  | Locale_EN_AU -- ^ English (Australia)
  | Locale_EN_BZ -- ^ English (Belize)
  | Locale_EN    -- ^ English
  | Locale_EN_CA -- ^ English (Canada)
  | Locale_EN_GB -- ^ English (United Kingdom)
  | Locale_EN_IE -- ^ English (Ireland)
  | Locale_EN_JM -- ^ English (Jamaica)
  | Locale_EN_NZ -- ^ English (New Zealand)
  | Locale_EN_PH -- ^ English (Philippines)
  | Locale_EN_TT -- ^ English (Trinidad & Tobago)
  | Locale_EN_US -- ^ English (United States)
  | Locale_EN_ZA -- ^ English (South Africa)
  | Locale_EN_ZW -- ^ English (Zimbabwe)
  | Locale_EO    -- ^ Esperanto
  | Locale_ET    -- ^ Estonian
  | Locale_EU    -- ^ Basque
  | Locale_FA    -- ^ Farsi
  | Locale_FA_IR -- ^ Persian/Iran
  | Locale_FI    -- ^ Finnish
  | Locale_FJ    -- ^ Fijian
  | Locale_FO    -- ^ Faeroese
  | Locale_FR_CH -- ^ French (Switzerland)
  | Locale_FR_FR -- ^ French (France)
  | Locale_FR_LU -- ^ French (Luxembourg)
  | Locale_FR_MC -- ^ French (Monaco)
  | Locale_FR    -- ^ French (Standard)
  | Locale_FR_BE -- ^ French (Belgium)
  | Locale_FR_CA -- ^ French (Canada)
  | Locale_FUR   -- ^ Friulian
  | Locale_FY    -- ^ Frisian
  | Locale_GA    -- ^ Irish
  | Locale_GD_IE -- ^ Gaelic (Irish)
  | Locale_GD    -- ^ Gaelic (Scots)
  | Locale_GL    -- ^ Galacian
  | Locale_GU    -- ^ Gujurati
  | Locale_HE    -- ^ Hebrew
  | Locale_HI    -- ^ Hindi
  | Locale_HR    -- ^ Croatian
  | Locale_HT    -- ^ Haitian
  | Locale_HU    -- ^ Hungarian
  | Locale_HY    -- ^ Armenian
  | Locale_ID    -- ^ Indonesian
  | Locale_IS    -- ^ Icelandic
  | Locale_IT_CH -- ^ Italian (Switzerland)
  | Locale_IT    -- ^ Italian (Standard)
  | Locale_IU    -- ^ Inuktitut
  | Locale_JA    -- ^ Japanese
  | Locale_KA    -- ^ Georgian
  | Locale_KK    -- ^ Kazakh
  | Locale_KM    -- ^ Khmer
  | Locale_KN    -- ^ Kannada
  | Locale_KO    -- ^ Korean
  | Locale_KO_KP -- ^ Korean (North Korea)
  | Locale_KO_KR -- ^ Korean (South Korea)
  | Locale_KS    -- ^ Kashmiri
  | Locale_KY    -- ^ Kirghiz
  | Locale_LA    -- ^ Latin
  | Locale_LB    -- ^ Luxembourgish
  | Locale_LT    -- ^ Lithuanian
  | Locale_LV    -- ^ Latvian
  | Locale_MI    -- ^ Maori
  | Locale_MK    -- ^ FYRO Macedonian
  | Locale_ML    -- ^ Malayalam
  | Locale_MO    -- ^ Moldavian
  | Locale_MR    -- ^ Marathi
  | Locale_MS    -- ^ Malay
  | Locale_MT    -- ^ Maltese
  | Locale_MY    -- ^ Burmese
  | Locale_NB    -- ^ Norwegian (Bokmal)
  | Locale_NE    -- ^ Nepali
  | Locale_NG    -- ^ Ndonga
  | Locale_NL    -- ^ Dutch (Standard)
  | Locale_NL_BE -- ^ Dutch (Belgian)
  | Locale_NN    -- ^ Norwegian (Nynorsk)
  | Locale_NO    -- ^ Norwegian
  | Locale_NV    -- ^ Navajo
  | Locale_OC    -- ^ Occitan
  | Locale_OM    -- ^ Oromo
  | Locale_OR    -- ^ Oriya
  | Locale_SQ    -- ^ Albanian
  | Locale_TLH   -- ^ Klingon
  | Locale_ZH_TW -- ^ Chinese (Taiwan)
  | Locale_ZH    -- ^ Chinese
  | Locale_ZH_CN -- ^ Chinese (PRC)
  | Locale_ZH_HK -- ^ Chinese (Hong Kong)
  | Locale_ZH_SG -- ^ Chinese (Singapore)
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Generic)

-- | This allows you to get the 'LocaleData' associated with a given 'Locale'.
--
-- Note that 'localeMap' should satisfy the following law:
--
-- >>> sort (Map.keys localeMap) == [minBound .. maxBound]
--
-- In other words, every constructor of Locale should have a corresponding value
-- in this finite map; thus making 'Map.lookup' a total function.
localeMap :: Map Locale LocaleData
localeMap = Map.fromList
            [ Locale_AF    ~> ld "af"    "Afrikaans"
            , Locale_AN    ~> ld "an"    "Aragonese"
            , Locale_AR_AE ~> ld "ar-AE" "Arabic (U.A.E.)"
            , Locale_AR_IQ ~> ld "ar-IQ" "Arabic (Iraq)"
            , Locale_AR    ~> ld "ar"    "Arabic (Standard)"
            , Locale_AR_BH ~> ld "ar-BH" "Arabic (Bahrain)"
            , Locale_AR_DZ ~> ld "ar-DZ" "Arabic (Algeria)"
            , Locale_AR_EG ~> ld "ar-EG" "Arabic (Egypt)"
            , Locale_AR_JO ~> ld "ar-JO" "Arabic (Jordan)"
            , Locale_AR_KW ~> ld "ar-KW" "Arabic (Kuwait)"
            , Locale_AR_LB ~> ld "ar-LB" "Arabic (Lebanon)"
            , Locale_AR_LY ~> ld "ar-LY" "Arabic (Libya)"
            , Locale_AR_MA ~> ld "ar-MA" "Arabic (Morocco)"
            , Locale_AR_OM ~> ld "ar-OM" "Arabic (Oman)"
            , Locale_AR_QA ~> ld "ar-QA" "Arabic (Qatar)"
            , Locale_AR_SA ~> ld "ar-SA" "Arabic (Saudi Arabia)"
            , Locale_AR_SY ~> ld "ar-SY" "Arabic (Syria)"
            , Locale_AR_TN ~> ld "ar-TN" "Arabic (Tunisia)"
            , Locale_AR_YE ~> ld "ar-YE" "Arabic (Yemen)"
            , Locale_AS    ~> ld "as"    "Assamese"
            , Locale_AST   ~> ld "ast"   "Asturian"
            , Locale_AZ    ~> ld "az"    "Azerbaijani"
            , Locale_BE    ~> ld "be"    "Belarusian"
            , Locale_BG    ~> ld "bg"    "Bulgarian"
            , Locale_BN    ~> ld "bn"    "Bengali"
            , Locale_BR    ~> ld "br"    "Breton"
            , Locale_BS    ~> ld "bs"    "Bosnian"
            , Locale_CA    ~> ld "ca"    "Catalan"
            , Locale_CE    ~> ld "ce"    "Chechen"
            , Locale_CH    ~> ld "ch"    "Chamorro"
            , Locale_CO    ~> ld "co"    "Corsican"
            , Locale_CR    ~> ld "cr"    "Cree"
            , Locale_CS    ~> ld "cs"    "Czech"
            , Locale_CV    ~> ld "cv"    "Chuvash"
            , Locale_DA    ~> ld "da"    "Danish"
            , Locale_DE    ~> ld "de"    "German (Standard)"
            , Locale_DE_AT ~> ld "de-AT" "German (Austria)"
            , Locale_DE_CH ~> ld "de-CH" "German (Switzerland)"
            , Locale_DE_DE ~> ld "de-DE" "German (Germany)"
            , Locale_DE_LI ~> ld "de-LI" "German (Liechtenstein)"
            , Locale_DE_LU ~> ld "de-LU" "German (Luxembourg)"
            , Locale_EL    ~> ld "el"    "Greek"
            , Locale_EN_AU ~> ld "en-AU" "English (Australia)"
            , Locale_EN_BZ ~> ld "en-BZ" "English (Belize)"
            , Locale_EN    ~> ld "en"    "English"
            , Locale_EN_CA ~> ld "en-CA" "English (Canada)"
            , Locale_EN_GB ~> ld "en-GB" "English (United Kingdom)"
            , Locale_EN_IE ~> ld "en-IE" "English (Ireland)"
            , Locale_EN_JM ~> ld "en-JM" "English (Jamaica)"
            , Locale_EN_NZ ~> ld "en-NZ" "English (New Zealand)"
            , Locale_EN_PH ~> ld "en-PH" "English (Philippines)"
            , Locale_EN_TT ~> ld "en-TT" "English (Trinidad & Tobago)"
            , Locale_EN_US ~> ld "en-US" "English (United States)"
            , Locale_EN_ZA ~> ld "en-ZA" "English (South Africa)"
            , Locale_EN_ZW ~> ld "en-ZW" "English (Zimbabwe)"
            , Locale_EO    ~> ld "eo"    "Esperanto"
            , Locale_ET    ~> ld "et"    "Estonian"
            , Locale_EU    ~> ld "eu"    "Basque"
            , Locale_FA    ~> ld "fa"    "Farsi"
            , Locale_FA_IR ~> ld "fa-IR" "Persian/Iran"
            , Locale_FI    ~> ld "fi"    "Finnish"
            , Locale_FJ    ~> ld "fj"    "Fijian"
            , Locale_FO    ~> ld "fo"    "Faeroese"
            , Locale_FR_CH ~> ld "fr-CH" "French (Switzerland)"
            , Locale_FR_FR ~> ld "fr-FR" "French (France)"
            , Locale_FR_LU ~> ld "fr-LU" "French (Luxembourg)"
            , Locale_FR_MC ~> ld "fr-MC" "French (Monaco)"
            , Locale_FR    ~> ld "fr"    "French (Standard)"
            , Locale_FR_BE ~> ld "fr-BE" "French (Belgium)"
            , Locale_FR_CA ~> ld "fr-CA" "French (Canada)"
            , Locale_FUR   ~> ld "fur"   "Friulian"
            , Locale_FY    ~> ld "fy"    "Frisian"
            , Locale_GA    ~> ld "ga"    "Irish"
            , Locale_GD_IE ~> ld "gd-IE" "Gaelic (Irish)"
            , Locale_GD    ~> ld "gd"    "Gaelic (Scots)"
            , Locale_GL    ~> ld "gl"    "Galacian"
            , Locale_GU    ~> ld "gu"    "Gujurati"
            , Locale_HE    ~> ld "he"    "Hebrew"
            , Locale_HI    ~> ld "hi"    "Hindi"
            , Locale_HR    ~> ld "hr"    "Croatian"
            , Locale_HT    ~> ld "ht"    "Haitian"
            , Locale_HU    ~> ld "hu"    "Hungarian"
            , Locale_HY    ~> ld "hy"    "Armenian"
            , Locale_ID    ~> ld "id"    "Indonesian"
            , Locale_IS    ~> ld "is"    "Icelandic"
            , Locale_IT_CH ~> ld "it-CH" "Italian (Switzerland)"
            , Locale_IT    ~> ld "it"    "Italian (Standard)"
            , Locale_IU    ~> ld "iu"    "Inuktitut"
            , Locale_JA    ~> ld "ja"    "Japanese"
            , Locale_KA    ~> ld "ka"    "Georgian"
            , Locale_KK    ~> ld "kk"    "Kazakh"
            , Locale_KM    ~> ld "km"    "Khmer"
            , Locale_KN    ~> ld "kn"    "Kannada"
            , Locale_KO    ~> ld "ko"    "Korean"
            , Locale_KO_KP ~> ld "ko-KP" "Korean (North Korea)"
            , Locale_KO_KR ~> ld "ko-KR" "Korean (South Korea)"
            , Locale_KS    ~> ld "ks"    "Kashmiri"
            , Locale_KY    ~> ld "ky"    "Kirghiz"
            , Locale_LA    ~> ld "la"    "Latin"
            , Locale_LB    ~> ld "lb"    "Luxembourgish"
            , Locale_LT    ~> ld "lt"    "Lithuanian"
            , Locale_LV    ~> ld "lv"    "Latvian"
            , Locale_MI    ~> ld "mi"    "Maori"
            , Locale_MK    ~> ld "mk"    "FYRO Macedonian"
            , Locale_ML    ~> ld "ml"    "Malayalam"
            , Locale_MO    ~> ld "mo"    "Moldavian"
            , Locale_MR    ~> ld "mr"    "Marathi"
            , Locale_MS    ~> ld "ms"    "Malay"
            , Locale_MT    ~> ld "mt"    "Maltese"
            , Locale_MY    ~> ld "my"    "Burmese"
            , Locale_NB    ~> ld "nb"    "Norwegian (Bokmal)"
            , Locale_NE    ~> ld "ne"    "Nepali"
            , Locale_NG    ~> ld "ng"    "Ndonga"
            , Locale_NL    ~> ld "nl"    "Dutch (Standard)"
            , Locale_NL_BE ~> ld "nl-BE" "Dutch (Belgian)"
            , Locale_NN    ~> ld "nn"    "Norwegian (Nynorsk)"
            , Locale_NO    ~> ld "no"    "Norwegian"
            , Locale_NV    ~> ld "nv"    "Navajo"
            , Locale_OC    ~> ld "oc"    "Occitan"
            , Locale_OM    ~> ld "om"    "Oromo"
            , Locale_OR    ~> ld "or"    "Oriya"
            , Locale_SQ    ~> ld "sq"    "Albanian"
            , Locale_TLH   ~> ld "tlh"   "Klingon"
            , Locale_ZH_TW ~> ld "zh-TW" "Chinese (Taiwan)"
            , Locale_ZH    ~> ld "zh"    "Chinese"
            , Locale_ZH_CN ~> ld "zh-CN" "Chinese (PRC)"
            , Locale_ZH_HK ~> ld "zh-HK" "Chinese (Hong Kong)"
            , Locale_ZH_SG ~> ld "zh-SG" "Chinese (Singapore)"
            ]
  where
    locale ~> localeData = (locale, localeData)
    ld = MkLocaleData
