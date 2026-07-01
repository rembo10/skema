import { MediaConfigSection } from '../../../components/ConfigFields.generated';
import type { Config } from '../../../types/api';

interface MediaTabProps {
  formData: Config;
  showAdvanced: boolean;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
}

export function MediaTab({ formData, showAdvanced, onChange }: MediaTabProps) {
  return (
    <div className="card">
      <div className="px-6 py-5 border-b border-dark-border">
        <h2 className="text-lg font-medium text-dark-text">Media Settings</h2>
        <p className="mt-1 text-sm text-dark-text-secondary">
          Configure providers for artist images and album artwork
        </p>
      </div>
      <div className="px-6 py-5">
        <MediaConfigSection config={formData} onChange={onChange} showAdvanced={showAdvanced} />
      </div>
    </div>
  );
}
