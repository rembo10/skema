// Schema-driven config section renderer.
//
// Field metadata (types, descriptions, groups, visibility rules) comes from
// GET /api/config/schema, which the backend derives from the same schema
// definitions that generate config.example.yaml — so the settings UI can
// never drift from the backend's field list.

import { useEffect, useState } from 'react';
import { PathInput } from './PathInput';
import { UrlInput } from './UrlInput';
import { api } from '../lib/api';
import type { Config } from '../types/api';

// ===================================================================
// Schema types (mirrors Skema.Config.Schema's JSON encoding)
// ===================================================================

export interface SchemaFieldType {
  type: 'string' | 'path' | 'url' | 'integer' | 'boolean' | 'enum' | 'list' | 'object';
  options?: string[];
}

export interface SchemaField {
  name: string;
  description: string;
  type: SchemaFieldType;
  example?: string;
  sensitive?: boolean;
  required?: boolean;
  advanced?: boolean;
  group?: string;
  depends_on?: string;
  depends_on_value?: { field: string; values: string[] };
}

export interface SchemaSection {
  description: string;
  fields: SchemaField[];
}

export type ConfigSchema = Record<string, SchemaSection>;

// Fetch the schema once and share it across every mounted section.
let schemaPromise: Promise<ConfigSchema> | null = null;

function fetchSchema(): Promise<ConfigSchema> {
  const promise = schemaPromise ?? (api.getConfigSchema() as unknown as Promise<ConfigSchema>);
  schemaPromise = promise;
  return promise;
}

export function useConfigSchema(): ConfigSchema | null {
  const [schema, setSchema] = useState<ConfigSchema | null>(null);
  useEffect(() => {
    let cancelled = false;
    fetchSchema().then((s) => {
      if (!cancelled) setSchema(s);
    });
    return () => {
      cancelled = true;
    };
  }, []);
  return schema;
}

// ===================================================================
// Field rendering
// ===================================================================

function isFieldVisible(field: SchemaField, sectionValues: any, showAdvanced: boolean): boolean {
  if (field.advanced && !showAdvanced) return false;
  if (field.depends_on && sectionValues && !sectionValues[field.depends_on]) return false;
  if (
    field.depends_on_value &&
    sectionValues &&
    !field.depends_on_value.values.includes(sectionValues[field.depends_on_value.field])
  )
    return false;
  return true;
}

export function GroupHeader({ title }: { title: string }) {
  return (
    <div className="border-b border-dark-border pb-2 pt-2">
      <h3 className="text-xs font-semibold uppercase tracking-wide text-dark-text-tertiary">{title}</h3>
    </div>
  );
}

interface FieldProps {
  section: keyof Config;
  field: SchemaField;
  value: any;
  onChange: (section: keyof Config, field: string, value: any) => void;
}

function ConfigField({ section, field, value, onChange }: FieldProps) {
  const id = `${String(section)}_${field.name}`;
  const label = field.name
    .split('_')
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(' ');

  switch (field.type.type) {
    case 'boolean':
      return (
        <div className="flex items-start gap-3">
          <input
            id={id}
            type="checkbox"
            checked={value ?? false}
            onChange={(e) => onChange(section, field.name, e.target.checked)}
            className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
          />
          <div>
            <label htmlFor={id} className="text-sm font-medium text-dark-text">{label}</label>
            <p className="text-sm text-dark-text-secondary mt-0.5">{field.description}</p>
          </div>
        </div>
      );

    case 'integer':
      return (
        <div>
          <label htmlFor={id} className="block text-sm font-medium text-dark-text mb-2">{label}</label>
          <input
            type="number"
            id={id}
            value={value ?? ''}
            onChange={(e) => onChange(section, field.name, e.target.value ? parseInt(e.target.value) : null)}
            className="input w-48"
          />
          <p className="mt-2 text-sm text-dark-text-secondary">{field.description}</p>
        </div>
      );

    case 'path':
      return (
        <PathInput
          id={id}
          label={label}
          value={value || ''}
          onChange={(v) => onChange(section, field.name, v)}
          type="directory"
          description={field.description}
        />
      );

    case 'url':
      return (
        <UrlInput
          id={id}
          label={label}
          value={value || ''}
          onChange={(v) => onChange(section, field.name, v)}
          description={field.description}
          placeholder="localhost:5000"
          defaultProtocol="http://"
          className="w-96"
        />
      );

    case 'enum':
      return (
        <div>
          <label htmlFor={id} className="block text-sm font-medium text-dark-text mb-2">{label}</label>
          <select
            id={id}
            value={value || ''}
            onChange={(e) => onChange(section, field.name, e.target.value)}
            className="input w-64"
          >
            {field.type.options?.map((opt) => (
              <option key={opt} value={opt}>{opt}</option>
            ))}
          </select>
          <p className="mt-2 text-sm text-dark-text-secondary">{field.description}</p>
        </div>
      );

    case 'list':
    case 'object':
      // Complex fields (client lists, indexer lists) have dedicated UIs
      return null;

    default: // string
      return (
        <div>
          <label htmlFor={id} className="block text-sm font-medium text-dark-text mb-2">{label}</label>
          <input
            type={field.sensitive ? 'password' : 'text'}
            id={id}
            value={value || ''}
            onChange={(e) => onChange(section, field.name, e.target.value || null)}
            className="input w-64"
          />
          <p className="mt-2 text-sm text-dark-text-secondary">{field.description}</p>
        </div>
      );
  }
}

// ===================================================================
// Section component
// ===================================================================

interface ConfigSectionProps {
  section: keyof Config;
  config: Config;
  onChange: (section: keyof Config, field: string, value: any) => void;
  showAdvanced?: boolean;
  /** Field names rendered elsewhere with custom logic (e.g. server password) */
  exclude?: string[];
}

export function ConfigSection({ section, config, onChange, showAdvanced = false, exclude = [] }: ConfigSectionProps) {
  const schema = useConfigSchema();
  if (!schema) return null;

  const sectionSchema = schema[String(section)];
  if (!sectionSchema) return null;

  const sectionValues = config[section] as any;
  const fields = sectionSchema.fields.filter(
    (f) => !exclude.includes(f.name) && f.type.type !== 'list' && f.type.type !== 'object'
  );

  // A group's header renders before its first field, and only if at least one
  // of the group's fields is visible.
  const groupVisible = (group: string) =>
    fields.some((f) => f.group === group && isFieldVisible(f, sectionValues, showAdvanced));

  const rendered: React.ReactNode[] = [];
  let currentGroup: string | undefined;
  for (const field of fields) {
    if (field.group !== currentGroup) {
      currentGroup = field.group;
      if (field.group && groupVisible(field.group)) {
        rendered.push(<GroupHeader key={`group-${field.group}`} title={field.group} />);
      }
    }
    if (isFieldVisible(field, sectionValues, showAdvanced)) {
      rendered.push(
        <ConfigField
          key={field.name}
          section={section}
          field={field}
          value={sectionValues?.[field.name]}
          onChange={onChange}
        />
      );
    }
  }

  return <div className="space-y-6">{rendered}</div>;
}
