import { ServerConfigSection } from '../../../components/ConfigFields.generated';
import type { Config } from '../../../types/api';

interface ServerTabProps {
  formData: Config;
  config: Config;
  showAdvanced: boolean;
  serverPassword: string;
  passwordChanged: boolean;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
  onPasswordChange: (password: string) => void;
}

export function ServerTab({
  formData,
  config,
  showAdvanced,
  serverPassword,
  passwordChanged,
  onChange,
  onPasswordChange,
}: ServerTabProps) {
  return (
    <div className="card">
      <div className="px-6 py-5 border-b border-dark-border">
        <h2 className="text-lg font-medium text-dark-text">Server Settings</h2>
        <p className="mt-1 text-sm text-dark-text-secondary">
          Configure server network and authentication settings
        </p>
      </div>
      <div className="px-6 py-5 space-y-6">
        <ServerConfigSection config={formData} onChange={onChange} showAdvanced={showAdvanced} />

        {/* Custom password field with change tracking */}
        <div>
          <label htmlFor="server_password" className="block text-sm font-medium text-dark-text mb-2">
            Password (Optional)
          </label>
          <input
            type="password"
            id="server_password"
            value={serverPassword}
            onChange={(e) => onPasswordChange(e.target.value)}
            className="input w-64"
            placeholder={
              config.server.auth_enabled
                ? 'Password is set (enter new password to change)'
                : 'Enter password to enable authentication'
            }
          />
          <p className="mt-2 text-sm text-dark-text-secondary">
            Password for API authentication. Will be securely hashed when saved. {passwordChanged && '(Password changes require clicking Save Changes button)'}
          </p>
        </div>

        {/* Authentication Status */}
        <div className="rounded-lg bg-dark-info-muted border border-dark-info/30 p-4">
          <div className="flex gap-3">
            <svg className="h-5 w-5 text-dark-info flex-shrink-0 mt-0.5" viewBox="0 0 20 20" fill="currentColor">
              <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
            </svg>
            <div className="flex-1">
              <p className="text-sm text-dark-text">
                <strong>Current Status:</strong> {config.server.auth_enabled ? 'Authentication Enabled' : 'Authentication Disabled'}
                {config.server.username && ` (User: ${config.server.username})`}
              </p>
              <p className="mt-2 text-sm text-dark-text-secondary">
                <strong>Note:</strong> Environment variables SKEMA_USERNAME and SKEMA_PASSWORD will override config file values if set.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
