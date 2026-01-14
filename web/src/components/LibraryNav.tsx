import { NavLink } from 'react-router-dom';
import { GitCompare } from 'lucide-react';

export function LibraryNav() {
  return (
    <nav className="border-b border-dark-border mb-6">
      <div className="flex space-x-8">
        <NavLink
          to="/library/diffs"
          className={({ isActive }) =>
            `flex items-center gap-2 px-1 py-4 border-b-2 text-sm font-medium transition-colors ${
              isActive
                ? 'border-dark-accent text-dark-accent'
                : 'border-transparent text-dark-text-secondary hover:text-dark-text hover:border-dark-border-bright'
            }`
          }
        >
          <GitCompare className="h-4 w-4" />
          <span>Diffs</span>
        </NavLink>
      </div>
    </nav>
  );
}
