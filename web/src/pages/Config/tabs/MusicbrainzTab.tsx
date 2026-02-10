import { MusicbrainzConfigSection } from '../../../components/ConfigFields.generated';
import type { Config } from '../../../types/api';

interface MusicbrainzTabProps {
  formData: Config;
  showAdvanced: boolean;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
}

export function MusicbrainzTab({ formData, showAdvanced, onChange }: MusicbrainzTabProps) {
  return (
    <div className="card">
      <div className="px-6 py-5 border-b border-dark-border">
        <h2 className="text-lg font-medium text-dark-text">MusicBrainz Settings</h2>
        <p className="mt-1 text-sm text-dark-text-secondary">
          Configure MusicBrainz server and authentication
        </p>
      </div>
      <div className="px-6 py-5">
        <MusicbrainzConfigSection config={formData} onChange={onChange} showAdvanced={showAdvanced} />
      </div>
    </div>
  );
}
