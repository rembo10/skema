import { LibraryConfigSection } from '../../../components/ConfigFields.generated';
import type { Config } from '../../../types/api';

interface LibraryTabProps {
  formData: Config;
  showAdvanced: boolean;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
}

export function LibraryTab({ formData, showAdvanced, onChange }: LibraryTabProps) {
  return (
    <div className="card">
      <div className="px-6 py-5 border-b border-dark-border">
        <h2 className="text-lg font-medium text-dark-text">Library Settings</h2>
        <p className="mt-1 text-sm text-dark-text-secondary">
          Configure your music library location and scanning behavior
        </p>
      </div>
      <div className="px-6 py-5">
        <LibraryConfigSection config={formData} onChange={onChange} showAdvanced={showAdvanced} />
      </div>
    </div>
  );
}
