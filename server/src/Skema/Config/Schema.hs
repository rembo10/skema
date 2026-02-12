{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Simple, pragmatic config schema system.
--
-- Philosophy:
-- - Record types ARE the source of truth for structure
-- - Metadata is attached via a simple list (checked at startup)
-- - Generic provides defaults, merge, JSON
-- - Env vars are explicit per-field
--
-- Usage:
-- @
-- data LibraryConfig = LibraryConfig
--   { libraryPath :: Maybe OsPath
--   , libraryWatch :: Bool
--   , libraryPathFormat :: Text
--   } deriving (Generic, Show, Eq)
--
-- instance Default LibraryConfig  -- Uses Generic
-- instance Mergeable LibraryConfig  -- Uses Generic
--
-- librarySchema :: Schema
-- librarySchema = schema "library" "Library configuration"
--   [ "path"        .:: "Path to music library" & envVar "SKEMA_LIBRARY_PATH"
--   , "watch"       .:: "Watch for file changes" & envVar "SKEMA_LIBRARY_WATCH"
--   , "path_format" .:: "Directory structure format"
--   ]
-- @
module Skema.Config.Schema
  ( -- * Field types
    FieldType(..)
    -- * Field metadata
  , FieldMeta(..)
  , (.::)
  , envVar
  , cliFlag
  , cliShort
  , example
  , sensitive
  , required
  , advanced
  , dependsOn
  , dependsOnValue
  , fieldType
  , pathField
  , urlField
  , boolField
  , intField
  , enumField
    -- * Schema
  , Schema(..)
  , schema
  , schemaToJSON
    -- * Default values (Generic)
  , Default(..)
  , GDefault(..)
    -- * Merging (Generic)
  , Mergeable(..)
  , GMergeable(..)
    -- * Documentation
  , generateExampleYaml
  , generateFullYaml
    -- * TypeScript/React code generation
  , generateTypeScriptTypes
  , generateReactComponents
    -- * Schema definitions
  , librarySchema
  , systemSchema
  , serverSchema
  , downloadSchema
  , indexerSchema
  , musicbrainzSchema
  , mediaSchema
  , notificationSchema
  , allSchemas
  ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Char (toUpper)
import Data.Aeson (ToJSON(..), object, (.=), Value)
import qualified Data.Aeson.Key as Key

-- =============================================================================
-- Field Metadata
-- =============================================================================

-- | Field type for UI rendering
data FieldType
  = FTString        -- ^ Text input
  | FTPath          -- ^ Path picker
  | FTUrl           -- ^ URL input with protocol selector
  | FTInt           -- ^ Integer input
  | FTBool          -- ^ Toggle/checkbox
  | FTEnum [Text]   -- ^ Dropdown with options
  | FTList          -- ^ List of items
  | FTObject        -- ^ Complex nested object
  deriving (Show, Eq)

instance ToJSON FieldType where
  toJSON FTString = object ["type" .= ("string" :: Text)]
  toJSON FTPath = object ["type" .= ("path" :: Text)]
  toJSON FTUrl = object ["type" .= ("url" :: Text)]
  toJSON FTInt = object ["type" .= ("integer" :: Text)]
  toJSON FTBool = object ["type" .= ("boolean" :: Text)]
  toJSON (FTEnum opts) = object ["type" .= ("enum" :: Text), "options" .= opts]
  toJSON FTList = object ["type" .= ("list" :: Text)]
  toJSON FTObject = object ["type" .= ("object" :: Text)]

-- | Metadata for a config field
data FieldMeta = FieldMeta
  { fmName :: Text           -- ^ Field name (snake_case for TOML)
  , fmHelp :: Text           -- ^ Help text
  , fmType :: FieldType      -- ^ Field type for UI rendering
  , fmEnvVar :: Maybe Text   -- ^ Environment variable name
  , fmCliFlag :: Maybe Text  -- ^ CLI long flag
  , fmCliShort :: Maybe Char -- ^ CLI short flag
  , fmExample :: Maybe Text  -- ^ Default value (shown in YAML)
  , fmSensitive :: Bool      -- ^ Hide in logs/output
  , fmRequired :: Bool       -- ^ Is this field required?
  , fmAdvanced :: Bool       -- ^ Hidden unless "Show Advanced" is toggled
  , fmDependsOn :: Maybe Text -- ^ Only show if this field is truthy
  , fmDependsOnValue :: Maybe (Text, [Text]) -- ^ Only show if field has one of these values (field, [values])
  } deriving (Show, Eq)

instance ToJSON FieldMeta where
  toJSON fm = object $ catMaybes
    [ Just $ "name" .= fmName fm
    , Just $ "description" .= fmHelp fm
    , Just $ "type" .= fmType fm
    , ("env_var" .=) <$> fmEnvVar fm
    , ("example" .=) <$> fmExample fm
    , if fmSensitive fm then Just $ "sensitive" .= True else Nothing
    , if fmRequired fm then Just $ "required" .= True else Nothing
    , if fmAdvanced fm then Just $ "advanced" .= True else Nothing
    , ("depends_on" .=) <$> fmDependsOn fm
    , case fmDependsOnValue fm of
        Just (field, values) -> Just $ "depends_on_value" .= object ["field" .= field, "values" .= values]
        Nothing -> Nothing
    ]

-- | Create a field with help text (defaults to string type)
(.::) :: Text -> Text -> FieldMeta
name .:: helpText = FieldMeta
  { fmName = name
  , fmHelp = helpText
  , fmType = FTString
  , fmEnvVar = Nothing
  , fmCliFlag = Nothing
  , fmCliShort = Nothing
  , fmExample = Nothing
  , fmSensitive = False
  , fmRequired = False
  , fmAdvanced = False
  , fmDependsOn = Nothing
  , fmDependsOnValue = Nothing
  }

infixl 8 .::

-- | Add environment variable
envVar :: Text -> FieldMeta -> FieldMeta
envVar var fm = fm { fmEnvVar = Just var }

-- | Add CLI flag
cliFlag :: Text -> FieldMeta -> FieldMeta
cliFlag flag fm = fm { fmCliFlag = Just flag }

-- | Add CLI short flag
cliShort :: Char -> FieldMeta -> FieldMeta
cliShort c fm = fm { fmCliShort = Just c }

-- | Set default value (shown in YAML)
example :: Text -> FieldMeta -> FieldMeta
example ex fm = fm { fmExample = Just ex }

-- | Mark as sensitive
sensitive :: FieldMeta -> FieldMeta
sensitive fm = fm { fmSensitive = True }

-- | Mark as required
required :: FieldMeta -> FieldMeta
required fm = fm { fmRequired = True }

-- | Mark as advanced (hidden by default)
advanced :: FieldMeta -> FieldMeta
advanced fm = fm { fmAdvanced = True }

-- | Field depends on another field being truthy
dependsOn :: Text -> FieldMeta -> FieldMeta
dependsOn field fm = fm { fmDependsOn = Just field }

-- | Field depends on another field having one of the specified values
dependsOnValue :: Text -> [Text] -> FieldMeta -> FieldMeta
dependsOnValue field values fm = fm { fmDependsOnValue = Just (field, values) }

-- | Set field type
fieldType :: FieldType -> FieldMeta -> FieldMeta
fieldType ft fm = fm { fmType = ft }

-- | Convenience: mark as path type
pathField :: FieldMeta -> FieldMeta
pathField = fieldType FTPath

-- | Convenience: mark as URL type
urlField :: FieldMeta -> FieldMeta
urlField = fieldType FTUrl

-- | Convenience: mark as boolean type
boolField :: FieldMeta -> FieldMeta
boolField = fieldType FTBool

-- | Convenience: mark as integer type
intField :: FieldMeta -> FieldMeta
intField = fieldType FTInt

-- | Convenience: mark as enum type
enumField :: [Text] -> FieldMeta -> FieldMeta
enumField opts = fieldType (FTEnum opts)

-- =============================================================================
-- Schema
-- =============================================================================

-- | A config section schema
data Schema = Schema
  { schemaSection :: Text      -- ^ Section name (e.g., "library")
  , schemaHelp :: Text         -- ^ Section description
  , schemaFields :: [FieldMeta] -- ^ Field metadata
  } deriving (Show, Eq)

instance ToJSON Schema where
  toJSON s = object
    [ "section" .= schemaSection s
    , "description" .= schemaHelp s
    , "fields" .= schemaFields s
    ]

-- | Create a schema
schema :: Text -> Text -> [FieldMeta] -> Schema
schema = Schema

-- | Full schema as JSON (for /api/config/schema endpoint)
schemaToJSON :: [Schema] -> Value
schemaToJSON schemas = object
  [ Key.fromText (schemaSection s) .= object
      [ "description" .= schemaHelp s
      , "fields" .= schemaFields s
      ]
  | s <- schemas
  ]

-- =============================================================================
-- Default values via Generic
-- =============================================================================

class Default a where
  def :: a
  default def :: (Generic a, GDefault (Rep a)) => a
  def = to gDef

class GDefault f where
  gDef :: f p

instance GDefault U1 where
  gDef = U1

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gDef = gDef :*: gDef

instance GDefault a => GDefault (M1 i c a) where
  gDef = M1 gDef

instance Default a => GDefault (K1 i a) where
  gDef = K1 def

-- Base instances
instance Default Bool where def = False
instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Text where def = ""
instance Default String where def = ""
instance Default (Maybe a) where def = Nothing
instance Default [a] where def = []

-- =============================================================================
-- Merging via Generic (second wins)
-- =============================================================================

class Mergeable a where
  -- | Merge two configs. For Maybe fields, Just wins over Nothing.
  -- For other fields, second wins.
  merge :: a -> a -> a
  default merge :: (Generic a, GMergeable (Rep a)) => a -> a -> a
  merge a b = to $ gMerge (from a) (from b)

class GMergeable f where
  gMerge :: f p -> f p -> f p

instance GMergeable U1 where
  gMerge _ _ = U1

instance (GMergeable a, GMergeable b) => GMergeable (a :*: b) where
  gMerge (a1 :*: b1) (a2 :*: b2) = gMerge a1 a2 :*: gMerge b1 b2

instance GMergeable a => GMergeable (M1 i c a) where
  gMerge (M1 a) (M1 b) = M1 (gMerge a b)

-- For Maybe, prefer Just
instance GMergeable (K1 i (Maybe a)) where
  gMerge (K1 Nothing) (K1 b) = K1 b
  gMerge (K1 a) (K1 Nothing) = K1 a
  gMerge _ (K1 b) = K1 b  -- Both Just: take second

-- For other types, second wins
instance {-# OVERLAPPABLE #-} GMergeable (K1 i a) where
  gMerge _ (K1 b) = K1 b

-- =============================================================================
-- Environment variable loading
-- =============================================================================

-- | Type class for loading a field from a string
class FieldLoader a where
  loadField :: Text -> Maybe a

instance FieldLoader Text where
  loadField = Just

instance FieldLoader String where
  loadField = Just . toString

instance FieldLoader Int where
  loadField = readMaybe . toString

instance FieldLoader Bool where
  loadField t = case T.toLower t of
    "true" -> Just True
    "1" -> Just True
    "yes" -> Just True
    "false" -> Just False
    "0" -> Just False
    "no" -> Just False
    _ -> Nothing

instance FieldLoader a => FieldLoader (Maybe a) where
  loadField t
    | T.null t = Just Nothing
    | otherwise = Just <$> loadField t

-- =============================================================================
-- Documentation generation
-- =============================================================================

-- | Generate example YAML for a single schema section
generateExampleYaml :: Schema -> Text
generateExampleYaml s = T.unlines $
  [ schemaSection s <> ":"
  , "  # " <> schemaHelp s
  ] ++ concatMap fieldToYaml (schemaFields s)
  where
    fieldToYaml :: FieldMeta -> [Text]
    fieldToYaml fm =
      [ "  # " <> fmHelp fm
      ] ++ envComment fm ++
      [ "  " <> fmName fm <> ": " <> valueExample fm
      ]

    envComment :: FieldMeta -> [Text]
    envComment fm = case fmEnvVar fm of
      Nothing -> []
      Just var -> ["  # Env: " <> var]

    valueExample :: FieldMeta -> Text
    valueExample fm = fromMaybe "null" (fmExample fm)

-- | Generate complete example YAML for all schemas
generateFullYaml :: Text
generateFullYaml = T.unlines $
  [ "# Skema Configuration File"
  , "#"
  , "# Generated from Skema.Config.Schema"
  , ""
  ] ++ intersperse "" (map generateExampleYaml allSchemas)

-- =============================================================================
-- TypeScript/React Code Generation
-- =============================================================================

-- | Generate TypeScript types from schemas
generateTypeScriptTypes :: Text
generateTypeScriptTypes = T.unlines $
  [ "// AUTO-GENERATED FILE - DO NOT EDIT"
  , "// Generated by: cabal run skema-generate-config"
  , "// Source: Skema.Config.Schema"
  , ""
  , "// ==================================================================="
  , "// Config Types"
  , "// ==================================================================="
  , ""
  ] ++ concatMap genSectionInterface allSchemas ++
  [ ""
  , "// Full nested config structure (matches backend JSON)"
  , "export interface Config {"
  ] ++ map genConfigField allSchemas ++
  [ "}"
  , ""
  , "// ==================================================================="
  , "// Schema Metadata (for dynamic form generation)"
  , "// ==================================================================="
  , ""
  , "export type FieldType = 'string' | 'path' | 'integer' | 'boolean' | 'enum' | 'list' | 'object';"
  , ""
  , "export interface FieldMeta {"
  , "  name: string;"
  , "  description: string;"
  , "  type: FieldType;"
  , "  options?: string[];  // For enum types"
  , "  sensitive?: boolean;"
  , "  required?: boolean;"
  , "}"
  , ""
  , "export interface SectionMeta {"
  , "  section: string;"
  , "  description: string;"
  , "  fields: FieldMeta[];"
  , "}"
  , ""
  , "// Schema metadata for each section"
  ] ++ concatMap genSchemaConst allSchemas ++
  [ ""
  , "export const allSchemas: SectionMeta[] = ["
  , "  " <> T.intercalate ",\n  " (map (\s -> schemaSection s <> "Schema") allSchemas)
  , "];"
  ]
  where
    genSectionInterface :: Schema -> [Text]
    genSectionInterface s =
      [ "export interface " <> capitalize (schemaSection s) <> "Config {"
      ] ++ map (genFieldType s) (schemaFields s) ++
      [ "}"
      , ""
      ]

    genFieldType :: Schema -> FieldMeta -> Text
    genFieldType _ fm = "  " <> fmName fm <> tsOptional fm <> ": " <> fieldTypeToTS (fmType fm) <> ";"

    tsOptional :: FieldMeta -> Text
    tsOptional fm
      | fmRequired fm = ""
      | isOptionalType (fmType fm) = "?"
      | otherwise = ""

    isOptionalType :: FieldType -> Bool
    isOptionalType FTObject = True
    isOptionalType FTList = False
    isOptionalType _ = False

    fieldTypeToTS :: FieldType -> Text
    fieldTypeToTS FTString = "string"
    fieldTypeToTS FTPath = "string | null"
    fieldTypeToTS FTUrl = "string"
    fieldTypeToTS FTInt = "number"
    fieldTypeToTS FTBool = "boolean"
    fieldTypeToTS (FTEnum opts) = T.intercalate " | " (map (\o -> "'" <> o <> "'") opts)
    fieldTypeToTS FTList = "any[]"  -- Complex types stay as any[]
    fieldTypeToTS FTObject = "any"  -- Complex types stay as any

    genConfigField :: Schema -> Text
    genConfigField s = "  " <> schemaSection s <> ": " <> capitalize (schemaSection s) <> "Config;"

    genSchemaConst :: Schema -> [Text]
    genSchemaConst s =
      [ "export const " <> schemaSection s <> "Schema: SectionMeta = {"
      , "  section: '" <> schemaSection s <> "',"
      , "  description: '" <> escapeJS (schemaHelp s) <> "',"
      , "  fields: ["
      ] ++ intersperse "," (map genFieldMeta (schemaFields s)) ++
      [ "  ],"
      , "};"
      , ""
      ]

    genFieldMeta :: FieldMeta -> Text
    genFieldMeta fm = T.unlines
      [ "    {"
      , "      name: '" <> fmName fm <> "',"
      , "      description: '" <> escapeJS (fmHelp fm) <> "',"
      , "      type: '" <> fieldTypeToJSType (fmType fm) <> "'"
        <> genEnumOptions (fmType fm)
        <> genSensitive fm
        <> genRequired fm
      , "    }"
      ]

    fieldTypeToJSType :: FieldType -> Text
    fieldTypeToJSType FTString = "string"
    fieldTypeToJSType FTPath = "path"
    fieldTypeToJSType FTUrl = "url"
    fieldTypeToJSType FTInt = "integer"
    fieldTypeToJSType FTBool = "boolean"
    fieldTypeToJSType (FTEnum _) = "enum"
    fieldTypeToJSType FTList = "list"
    fieldTypeToJSType FTObject = "object"

    genEnumOptions :: FieldType -> Text
    genEnumOptions (FTEnum opts) = ",\n      options: [" <> T.intercalate ", " (map (\o -> "'" <> o <> "'") opts) <> "]"
    genEnumOptions _ = ""

    genSensitive :: FieldMeta -> Text
    genSensitive fm = if fmSensitive fm then ",\n      sensitive: true" else ""

    genRequired :: FieldMeta -> Text
    genRequired fm = if fmRequired fm then ",\n      required: true" else ""

    capitalize :: Text -> Text
    capitalize t = case T.uncons t of
      Nothing -> t
      Just (c, rest) -> T.cons (toUpper c) rest

    escapeJS :: Text -> Text
    escapeJS = T.replace "'" "\\'" . T.replace "\n" "\\n"

-- | Generate React form components from schemas
generateReactComponents :: Text
generateReactComponents = T.unlines $
  [ "// AUTO-GENERATED FILE - DO NOT EDIT"
  , "// Generated by: cabal run skema-generate-config"
  , "// Source: Skema.Config.Schema"
  , ""
  , "import { PathInput } from './PathInput';"
  , "import { UrlInput } from './UrlInput';"
  , "import type { Config } from '../types/api';"
  , ""
  , "// ==================================================================="
  , "// Field Rendering Component"
  , "// ==================================================================="
  , ""
  , "interface FieldProps {"
  , "  section: keyof Config;"
  , "  field: string;"
  , "  value: any;"
  , "  onChange: (section: keyof Config, field: string, value: any) => void;"
  , "  type: 'string' | 'path' | 'url' | 'integer' | 'boolean' | 'enum';"
  , "  description: string;"
  , "  options?: string[];"
  , "  sensitive?: boolean;"
  , "  advanced?: boolean;"
  , "  dependsOn?: string;"
  , "  dependsOnValue?: { field: string; values: string[] };"
  , "  showAdvanced?: boolean;"
  , "  sectionValues?: any;"
  , "}"
  , ""
  , "function ConfigField({ section, field, value, onChange, type, description, options, sensitive, advanced, dependsOn, dependsOnValue, showAdvanced, sectionValues }: FieldProps) {"
  , "  // Hide advanced fields unless showAdvanced is true"
  , "  if (advanced && !showAdvanced) return null;"
  , "  // Hide fields that depend on another field being truthy"
  , "  if (dependsOn && sectionValues && !sectionValues[dependsOn]) return null;"
  , "  // Hide fields that depend on another field having specific values"
  , "  if (dependsOnValue && sectionValues && !dependsOnValue.values.includes(sectionValues[dependsOnValue.field])) return null;"
  , ""
  , "  const id = `${section}_${field}`;"
  , "  const label = field.split('_').map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(' ');"
  , ""
  , "  switch (type) {"
  , "    case 'boolean':"
  , "      return ("
  , "        <div className=\"flex items-start gap-3\">"
  , "          <input"
  , "            id={id}"
  , "            type=\"checkbox\""
  , "            checked={value ?? false}"
  , "            onChange={(e) => onChange(section, field, e.target.checked)}"
  , "            className=\"mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated\""
  , "          />"
  , "          <div>"
  , "            <label htmlFor={id} className=\"text-sm font-medium text-dark-text\">{label}</label>"
  , "            <p className=\"text-sm text-dark-text-secondary mt-0.5\">{description}</p>"
  , "          </div>"
  , "        </div>"
  , "      );"
  , ""
  , "    case 'integer':"
  , "      return ("
  , "        <div>"
  , "          <label htmlFor={id} className=\"block text-sm font-medium text-dark-text mb-2\">{label}</label>"
  , "          <input"
  , "            type=\"number\""
  , "            id={id}"
  , "            value={value ?? ''}"
  , "            onChange={(e) => onChange(section, field, e.target.value ? parseInt(e.target.value) : null)}"
  , "            className=\"input w-48\""
  , "          />"
  , "          <p className=\"mt-2 text-sm text-dark-text-secondary\">{description}</p>"
  , "        </div>"
  , "      );"
  , ""
  , "    case 'path':"
  , "      return ("
  , "        <PathInput"
  , "          id={id}"
  , "          label={label}"
  , "          value={value || ''}"
  , "          onChange={(v) => onChange(section, field, v)}"
  , "          type=\"directory\""
  , "          description={description}"
  , "        />"
  , "      );"
  , ""
  , "    case 'url':"
  , "      return ("
  , "        <UrlInput"
  , "          id={id}"
  , "          label={label}"
  , "          value={value || ''}"
  , "          onChange={(v) => onChange(section, field, v)}"
  , "          description={description}"
  , "          placeholder=\"localhost:5000\""
  , "          defaultProtocol=\"http://\""
  , "          className=\"w-96\""
  , "        />"
  , "      );"
  , ""
  , "    case 'enum':"
  , "      return ("
  , "        <div>"
  , "          <label htmlFor={id} className=\"block text-sm font-medium text-dark-text mb-2\">{label}</label>"
  , "          <select"
  , "            id={id}"
  , "            value={value || ''}"
  , "            onChange={(e) => onChange(section, field, e.target.value)}"
  , "            className=\"input w-64\""
  , "          >"
  , "            {options?.map((opt) => ("
  , "              <option key={opt} value={opt}>{opt}</option>"
  , "            ))}"
  , "          </select>"
  , "          <p className=\"mt-2 text-sm text-dark-text-secondary\">{description}</p>"
  , "        </div>"
  , "      );"
  , ""
  , "    default: // string"
  , "      return ("
  , "        <div>"
  , "          <label htmlFor={id} className=\"block text-sm font-medium text-dark-text mb-2\">{label}</label>"
  , "          <input"
  , "            type={sensitive ? 'password' : 'text'}"
  , "            id={id}"
  , "            value={value || ''}"
  , "            onChange={(e) => onChange(section, field, e.target.value || null)}"
  , "            className=\"input w-64\""
  , "          />"
  , "          <p className=\"mt-2 text-sm text-dark-text-secondary\">{description}</p>"
  , "        </div>"
  , "      );"
  , "  }"
  , "}"
  , ""
  , "// ==================================================================="
  , "// Section Components"
  , "// ==================================================================="
  , ""
  ] ++ concatMap genSectionComponent (filter (not . isComplexSection) allSchemas) ++
  [ "// ConfigField is not exported with 'export function', so export it here"
  , "export { ConfigField };"
  ]
  where
    -- Sections with complex nested types that need manual handling
    isComplexSection :: Schema -> Bool
    isComplexSection s = schemaSection s `elem` ["download", "indexers", "notifications"]

    genSectionComponent :: Schema -> [Text]
    genSectionComponent s =
      [ "interface " <> capitalize (schemaSection s) <> "Props {"
      , "  config: Config;"
      , "  onChange: (section: keyof Config, field: string, value: any) => void;"
      , "  showAdvanced?: boolean;"
      , "}"
      , ""
      , "export function " <> capitalize (schemaSection s) <> "ConfigSection({ config, onChange, showAdvanced = false }: " <> capitalize (schemaSection s) <> "Props) {"
      , "  const section = config." <> schemaSection s <> ";"
      , "  return ("
      , "    <div className=\"space-y-6\">"
      ] ++ concatMap (genFieldCall s) (filter (shouldIncludeField s) (schemaFields s)) ++
      [ "    </div>"
      , "  );"
      , "}"
      , ""
      ]

    isSimpleField :: FieldMeta -> Bool
    isSimpleField fm = case fmType fm of
      FTList -> False
      FTObject -> False
      _ -> True

    -- Skip password field in server section (handled manually with special logic)
    shouldIncludeField :: Schema -> FieldMeta -> Bool
    shouldIncludeField s fm
      | schemaSection s == "server" && fmName fm == "password" = False
      | otherwise = isSimpleField fm

    genFieldCall :: Schema -> FieldMeta -> [Text]
    genFieldCall s fm =
      [ "      <ConfigField"
      , "        section=\"" <> schemaSection s <> "\""
      , "        field=\"" <> fmName fm <> "\""
      , "        value={section." <> fmName fm <> "}"
      , "        onChange={onChange}"
      , "        type=\"" <> fieldTypeToJSType (fmType fm) <> "\""
      , "        description=\"" <> escapeJS (fmHelp fm) <> "\""
        <> genOptionsAttr (fmType fm)
        <> genSensitiveAttr fm
        <> genAdvancedAttr fm
        <> genDependsOnAttr fm
        <> genDependsOnValueAttr fm
        <> "\n        showAdvanced={showAdvanced}"
        <> "\n        sectionValues={section}"
      , "      />"
      ]

    fieldTypeToJSType :: FieldType -> Text
    fieldTypeToJSType FTString = "string"
    fieldTypeToJSType FTPath = "path"
    fieldTypeToJSType FTUrl = "url"
    fieldTypeToJSType FTInt = "integer"
    fieldTypeToJSType FTBool = "boolean"
    fieldTypeToJSType (FTEnum _) = "enum"
    fieldTypeToJSType FTList = "list"
    fieldTypeToJSType FTObject = "object"

    genOptionsAttr :: FieldType -> Text
    genOptionsAttr (FTEnum opts) = "\n        options={[" <> T.intercalate ", " (map (\o -> "'" <> o <> "'") opts) <> "]}"
    genOptionsAttr _ = ""

    genSensitiveAttr :: FieldMeta -> Text
    genSensitiveAttr fm = if fmSensitive fm then "\n        sensitive" else ""

    genAdvancedAttr :: FieldMeta -> Text
    genAdvancedAttr fm = if fmAdvanced fm then "\n        advanced" else ""

    genDependsOnAttr :: FieldMeta -> Text
    genDependsOnAttr fm = case fmDependsOn fm of
      Just dep -> "\n        dependsOn=\"" <> dep <> "\""
      Nothing -> ""

    genDependsOnValueAttr :: FieldMeta -> Text
    genDependsOnValueAttr fm = case fmDependsOnValue fm of
      Just (field, values) ->
        "\n        dependsOnValue={{ field: \"" <> field <> "\", values: ["
        <> T.intercalate ", " (map (\v -> "\"" <> v <> "\"") values)
        <> "] }}"
      Nothing -> ""

    capitalize :: Text -> Text
    capitalize t = case T.uncons t of
      Nothing -> t
      Just (c, rest) -> T.cons (toUpper c) rest

    escapeJS :: Text -> Text
    escapeJS = T.replace "\"" "\\\"" . T.replace "\n" "\\n"

-- =============================================================================
-- Schema definitions (SINGLE SOURCE OF TRUTH for metadata)
-- =============================================================================

-- | Library configuration schema
librarySchema :: Schema
librarySchema = schema "library" "Music library configuration"
  [ "path" .:: "Path to music library directory"
      & pathField
      -- No example = null default
  , "watch" .:: "Watch library directory for changes"
      & boolField
      & example "true"
  , "auto_scan" .:: "Automatically scan library on interval"
      & boolField
      & example "false"
  , "auto_scan_interval_mins" .:: "Minutes between automatic scans"
      & intField
      & example "60"
  , "auto_scan_on_startup" .:: "Scan library when application starts"
      & boolField
      & example "true"
  , "normalize_featuring" .:: "Normalize featuring artist format in track titles"
      & boolField
      & example "false"
  , "normalize_featuring_to" .:: "Format to use for featuring artists"
      & example "feat."
      & dependsOn "normalize_featuring"
  , "path_format" .:: "Directory structure format for organizing files"
      & example "{album_artist}/{year} - {album}/"
      & advanced
  , "file_format" .:: "File naming format"
      & example "{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}"
      & advanced
  ]

-- | System configuration schema
systemSchema :: Schema
systemSchema = schema "system" "System and paths configuration"
  [ "watch_config_file" .:: "Watch config file for changes and reload automatically"
      & boolField
      & example "true"
  , "database_path" .:: "SQLite database file path (default: skema.db in data directory)"
      & pathField
      -- No example = null, uses default location
  , "data_dir" .:: "Data directory override (default: platform-specific)"
      & pathField
      -- No example = null, uses platform default
  , "cache_dir" .:: "Cache directory override (default: platform-specific)"
      & pathField
      -- No example = null, uses platform default
  ]

-- | Server configuration schema
serverSchema :: Schema
serverSchema = schema "server" "HTTP server configuration"
  [ "host" .:: "Server bind address"
      & example "0.0.0.0"
  , "port" .:: "Server port"
      & intField
      & example "8182"
  , "web_root" .:: "Web root path for hosting at subpaths (e.g., /skema or /)"
      & example "/"
      & advanced
  , "jwt_secret" .:: "JWT signing secret (auto-generated if not provided)"
      & sensitive
      & advanced
      -- No example = null, auto-generated
  , "jwt_expiration_hours" .:: "JWT token expiration time in hours"
      & intField
      & example "168"
      & advanced
  , "username" .:: "Username for API authentication (required)"
      -- No example = null, must be set by user
  , "password" .:: "Password for API authentication (required, will be bcrypt hashed)"
      & sensitive
      -- No example = null, must be set by user
  ]

-- | Download configuration schema
downloadSchema :: Schema
downloadSchema = schema "download" "Download client configuration"
  [ "nzb_client" .:: "NZB download client configuration"
      & fieldType FTObject
      -- No example = null, optional
  , "torrent_client" .:: "Torrent download client configuration"
      & fieldType FTObject
      -- No example = null, optional
  , "slskd_client" .:: "slskd (Soulseek) client configuration"
      & fieldType FTObject
      & example (T.unlines
          [ ""
          , "    url: http://localhost:5030"
          , "    api_key: your-api-key"
          , "    enabled: false"
          , "    download_directory: /downloads/slskd"
          ])
  , "directory" .:: "Directory for completed downloads (before import)"
      & pathField
      & example "./downloads"
  , "check_interval" .:: "How often to check for completed downloads (in seconds)"
      & intField
      & example "60"
  , "auto_import" .:: "Automatically import completed downloads"
      & boolField
      & example "true"
  , "min_seeders" .:: "Minimum seeders for torrents (null = no minimum)"
      & intField
      & example "5"
  , "max_size" .:: "Maximum download size in MB (null = no limit)"
      & intField
      & example "1000"
  ]

-- | Indexer configuration schema
indexerSchema :: Schema
indexerSchema = schema "indexers" "Usenet/torrent indexer configuration"
  [ "list" .:: "List of configured indexers"
      & fieldType FTList
      & example (T.unlines
          [ ""
          , "    - name: Bullet"
          , "      url: https://bullet.codeshy.com"
          , "      api_key: your-api-key"
          , "      enabled: false"
          , "      priority: 0"
          , "      categories: [3000, 3010]"
          ])
  , "search_timeout" .:: "Search timeout per indexer in seconds"
      & intField
      & example "30"
  ]

-- | MusicBrainz configuration schema
musicbrainzSchema :: Schema
musicbrainzSchema = schema "musicbrainz" "MusicBrainz metadata provider configuration"
  [ "server" .:: "Which MusicBrainz server to use"
      & enumField ["official", "headphones_vip", "custom"]
      & example "official"
  , "custom_url" .:: "Custom MusicBrainz server URL (required if server is 'custom')"
      & urlField
      & dependsOnValue "server" ["custom"]
      & example "https://musicbrainz.example.com"
  , "username" .:: "Username for authentication (required for Headphones VIP, optional for custom)"
      & dependsOnValue "server" ["headphones_vip", "custom"]
      -- No example = null
  , "password" .:: "Password for authentication (required for Headphones VIP, optional for custom)"
      & sensitive
      & dependsOnValue "server" ["headphones_vip", "custom"]
      -- No example = null
  , "album_types" .:: "Primary album types to fetch (e.g., Album, EP)"
      & fieldType FTList
      & example "[\"Album\"]"
  , "exclude_secondary_types" .:: "Secondary types to exclude (e.g., Live, Compilation)"
      & fieldType FTList
      & example "[\"Live\", \"Compilation\"]"
  ]

-- | Media (artwork/metadata) configuration schema
mediaSchema :: Schema
mediaSchema = schema "media" "Media providers configuration"
  [ "lastfm_api_key" .:: "Last.fm API key for artist images and scrobbling"
      & sensitive
      -- No example = null
  ]

-- | Notification configuration schema
notificationSchema :: Schema
notificationSchema = schema "notifications" "Notification configuration"
  [ "enabled" .:: "Enable/disable all notifications"
      & boolField
      & example "false"
  , "providers" .:: "List of notification providers"
      & fieldType FTList
      & example "[]"
  , "on_album_found" .:: "Send notification when new wanted albums are found"
      & boolField
      & example "true"
  , "on_album_downloaded" .:: "Send notification when albums are downloaded"
      & boolField
      & example "true"
  , "on_album_imported" .:: "Send notification when albums are imported to library"
      & boolField
      & example "true"
  ]

-- | All config schemas
allSchemas :: [Schema]
allSchemas =
  [ librarySchema
  , systemSchema
  , serverSchema
  , downloadSchema
  , indexerSchema
  , musicbrainzSchema
  , mediaSchema
  , notificationSchema
  ]
