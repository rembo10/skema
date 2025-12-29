import { NavLink } from 'react-router-dom';
import { Layers, List } from 'lucide-react';

export function IdentificationNav() {
  return (
    <nav className="border-b border-dark-border mb-6">
      <div className="flex space-x-8">
        <NavLink
          to="/identification/clusters"
          className={({ isActive }) =>
            `flex items-center gap-2 px-1 py-4 border-b-2 text-sm font-medium transition-colors ${
              isActive
                ? 'border-dark-accent text-dark-accent'
                : 'border-transparent text-dark-text-secondary hover:text-dark-text hover:border-dark-border-bright'
            }`
          }
        >
          <Layers className="h-4 w-4" />
          <span>Clusters</span>
        </NavLink>
        <NavLink
          to="/identification/tracks"
          className={({ isActive }) =>
            `flex items-center gap-2 px-1 py-4 border-b-2 text-sm font-medium transition-colors ${
              isActive
                ? 'border-dark-accent text-dark-accent'
                : 'border-transparent text-dark-text-secondary hover:text-dark-text hover:border-dark-border-bright'
            }`
          }
        >
          <List className="h-4 w-4" />
          <span>Tracks</span>
        </NavLink>
      </div>
    </nav>
  );
}
