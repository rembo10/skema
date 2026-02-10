import { SystemConfigSection } from '../../../components/ConfigFields.generated';
import type { Config } from '../../../types/api';

interface SystemTabProps {
  formData: Config;
  showAdvanced: boolean;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
}

export function SystemTab({ formData, showAdvanced, onChange }: SystemTabProps) {
  return (
    <div className="card">
      <div className="px-6 py-5 border-b border-dark-border">
        <h2 className="text-lg font-medium text-dark-text">System Settings</h2>
        <p className="mt-1 text-sm text-dark-text-secondary">
          Configure system-level behavior and database settings
        </p>
      </div>
      <div className="px-6 py-5">
        <SystemConfigSection config={formData} onChange={onChange} showAdvanced={showAdvanced} />
      </div>
    </div>
  );
}
