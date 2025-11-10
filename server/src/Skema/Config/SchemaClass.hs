{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Typeclass-based configuration schema.
--
-- Define your config types once and derive everything:
-- - API request/response types
-- - YAML serialization
-- - Documentation
-- - Validation
module Skema.Config.SchemaClass
  ( -- * Core typeclass
    ConfigSchema(..)
  , FieldInfo(..)
  , FieldType(..)
    -- * Deriving utilities
  , deriveAPIResponse
  , deriveAPIUpdate
  , deriveYAMLExample
  , deriveMarkdownDocs
  ) where

import qualified Data.Text as T
import Data.Aeson (Value)
import qualified Data.Aeson.Types as Aeson (Parser)

-- | Type classification for config fields.
data FieldType
  = FText
  | FInt
  | FBool
  | FPath
  | FOptional FieldType
  deriving (Show, Eq)

-- | Metadata about a single config field.
data FieldInfo = FieldInfo
  { fieldName :: Text
    -- ^ Field name in snake_case (for YAML/JSON)
  , fieldDescription :: Text
    -- ^ Human-readable description
  , fieldType :: FieldType
    -- ^ Type of the field
  , fieldDefault :: Maybe Text
    -- ^ Default value as text
  , fieldExample :: Maybe Text
    -- ^ Example value for docs
  } deriving (Show, Eq)

-- | Typeclass for config types to implement.
--
-- Example:
-- @
-- instance ConfigSchema LibraryConfig where
--   configFields _ =
--     [ FieldInfo "path" "Library path" (FOptional FPath) Nothing (Just "/home/user/Music")
--     , FieldInfo "watch" "Enable watching" FBool (Just "true") Nothing
--     ]
--
--   configToJSON cfg = object
--     [ "path" .= libraryPath cfg
--     , "watch" .= libraryWatch cfg
--     ]
--
--   configFromJSON obj = do
--     path <- obj .:? "path"
--     watch <- obj .:? "watch" .!= True
--     pure $ LibraryConfig path watch ...
-- @
class ConfigSchema a where
  -- | Get field metadata for this config type
  configFields :: Proxy a -> [FieldInfo]

  -- | Get section name (e.g., "library", "system", "server")
  configSection :: Proxy a -> Text

  -- | Section description
  configDescription :: Proxy a -> Text

  -- | Convert config to JSON Value
  configToJSON :: a -> Value

  -- | Parse config from JSON Value (returns Aeson Parser for composability)
  configFromJSON :: Value -> Aeson.Parser a

  -- | Default instance of this config
  configDefault :: a

-- | Derive API response type structure from schema.
--
-- Returns field definitions as (snake_case_name, haskell_type)
deriveAPIResponse :: forall a. ConfigSchema a => Proxy a -> [(Text, Text)]
deriveAPIResponse proxy =
  let fields = configFields proxy
      section = configSection proxy
   in map (fieldToAPIField section) fields
  where
    fieldToAPIField :: Text -> FieldInfo -> (Text, Text)
    fieldToAPIField section field =
      let snakeName = section <> "_" <> fieldName field
          hsType = fieldTypeToHaskell (fieldType field)
       in (snakeName, hsType)

    fieldTypeToHaskell :: FieldType -> Text
    fieldTypeToHaskell FText = "Text"
    fieldTypeToHaskell FInt = "Int"
    fieldTypeToHaskell FBool = "Bool"
    fieldTypeToHaskell FPath = "Text"  -- Serialize paths as text for API
    fieldTypeToHaskell (FOptional t) = "Maybe " <> fieldTypeToHaskell t

-- | Derive API update type structure (all fields optional).
deriveAPIUpdate :: forall a. ConfigSchema a => Proxy a -> [(Text, Text)]
deriveAPIUpdate proxy =
  let fields = configFields proxy
      section = configSection proxy
   in map (fieldToUpdateField section) fields
  where
    fieldToUpdateField :: Text -> FieldInfo -> (Text, Text)
    fieldToUpdateField section field =
      let snakeName = section <> "_" <> fieldName field
          hsType = "Maybe (" <> fieldTypeToHaskell (fieldType field) <> ")"
       in (snakeName, hsType)

    fieldTypeToHaskell :: FieldType -> Text
    fieldTypeToHaskell FText = "Text"
    fieldTypeToHaskell FInt = "Int"
    fieldTypeToHaskell FBool = "Bool"
    fieldTypeToHaskell FPath = "Text"
    fieldTypeToHaskell (FOptional t) = "Maybe " <> fieldTypeToHaskell t

-- | Generate YAML example section from schema.
deriveYAMLExample :: forall a. ConfigSchema a => Proxy a -> Text
deriveYAMLExample proxy =
  let fields = configFields proxy
      section = configSection proxy
      desc = configDescription proxy
   in T.unlines $
        [ section <> ":"
        , "  # " <> desc
        ] ++ concatMap fieldToYAML fields
  where
    fieldToYAML :: FieldInfo -> [Text]
    fieldToYAML field =
      [ "  # " <> fieldDescription field
      ] ++ exampleComment field ++
      [ "  " <> fieldName field <> ": " <> getDefaultValue field
      ]

    exampleComment :: FieldInfo -> [Text]
    exampleComment field = case fieldExample field of
      Nothing -> []
      Just ex -> ["  # Example: " <> ex]

    getDefaultValue :: FieldInfo -> Text
    getDefaultValue field = case fieldDefault field of
      Just val -> val
      Nothing -> case fieldType field of
        FOptional _ -> "null"
        _ -> "# <required>"

-- | Generate markdown documentation from schema.
deriveMarkdownDocs :: forall a. ConfigSchema a => Proxy a -> Text
deriveMarkdownDocs proxy =
  let fields = configFields proxy
      section = configSection proxy
      desc = configDescription proxy
   in T.unlines $
        [ "## " <> T.toTitle section
        , ""
        , desc
        , ""
        , "| Field | Type | Default | Description |"
        , "|-------|------|---------|-------------|"
        ] ++ map fieldToMarkdown fields
  where
    fieldToMarkdown :: FieldInfo -> Text
    fieldToMarkdown field =
      "| `" <> fieldName field <> "` | "
        <> showFieldType (fieldType field) <> " | "
        <> showDefault field <> " | "
        <> fieldDescription field <> " |"

    showFieldType :: FieldType -> Text
    showFieldType FText = "text"
    showFieldType FInt = "integer"
    showFieldType FBool = "boolean"
    showFieldType FPath = "path"
    showFieldType (FOptional t) = showFieldType t <> "?"

    showDefault :: FieldInfo -> Text
    showDefault field = case fieldDefault field of
      Nothing -> "*required*"
      Just val -> "`" <> val <> "`"
