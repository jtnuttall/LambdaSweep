module Import.Lens
  ( makeProjectClassy
  ) where

import           Control.Lens
import           Data.Char
import           Data.List                      ( isPrefixOf )
import           Language.Haskell.TH
import           Prelude

projectFieldNamer :: FieldNamer
projectFieldNamer (nameBase -> className) _ (nameBase -> fieldName)
  | "_" `isPrefixOf` fieldName            = []
  | lowerClassName `isPrefixOf` fieldName = [TopName (mkName lensNameSans)]
  | otherwise                             = [TopName (mkName lensName)]
 where
  lowerClassName = over _head toLower className
  lensName       = over _head toLower fieldName <> "L"
  lensNameSans   = over _head toLower . drop (length lowerClassName) $ lensName


projectClassyNamer :: ClassyNamer
projectClassyNamer (nameBase -> name) =
  Just (mkName $ "Has" <> name, mkName $ over _head toLower name <> "L")

makeProjectClassy :: Name -> DecsQ
makeProjectClassy = makeLensesWith
  (  classyRules
  &  lensClass
  .~ projectClassyNamer
  &  lensField
  .~ projectFieldNamer
  )
