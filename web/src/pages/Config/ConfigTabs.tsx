import { Library, Settings, Server, Download, Search, Database, Bell } from 'lucide-react';
import type { TabId } from './useConfigState';

interface Tab {
  id: TabId;
  label: string;
  icon: React.ComponentType<{ className?: string }>;
}

const tabs: Tab[] = [
  { id: 'library', label: 'Library', icon: Library },
  { id: 'system', label: 'System', icon: Settings },
  { id: 'server', label: 'Server', icon: Server },
  { id: 'download', label: 'Downloads', icon: Download },
  { id: 'indexers', label: 'Indexers', icon: Search },
  { id: 'musicbrainz', label: 'MusicBrainz', icon: Database },
  { id: 'notifications', label: 'Notifications', icon: Bell },
];

interface ConfigTabsProps {
  activeTab: TabId;
  onTabChange: (tab: TabId) => void;
}

export function ConfigTabs({ activeTab, onTabChange }: ConfigTabsProps) {
  return (
    <div className="card p-2">
      <nav className="flex flex-wrap gap-2">
        {tabs.map((tab) => {
          const Icon = tab.icon;
          const isActive = activeTab === tab.id;
          return (
            <button
              key={tab.id}
              type="button"
              onClick={() => onTabChange(tab.id)}
              className={`flex items-center gap-2 px-4 py-2 rounded-lg transition-all duration-200 ${
                isActive
                  ? 'bg-dark-accent text-dark-bg font-medium'
                  : 'text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-text'
              }`}
            >
              <Icon className="h-4 w-4" />
              <span className="text-sm">{tab.label}</span>
            </button>
          );
        })}
      </nav>
    </div>
  );
}
