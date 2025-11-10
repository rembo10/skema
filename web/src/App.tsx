import { BrowserRouter, Routes, Route, NavLink, useNavigate, useLocation, Navigate } from 'react-router-dom';
import { Toaster } from 'react-hot-toast';
import { useEffect, useState } from 'react';
import { Settings, LogOut, LayoutDashboard, Database, Music, Disc, Sliders, Search, Menu, X, Download, Award, WifiOff } from 'lucide-react';
import Dashboard from './pages/Dashboard';
import MetadataDiffs from './pages/MetadataDiffs';
import Identification from './pages/Identification';
import FollowedArtists from './pages/FollowedArtists';
import ArtistDetail from './pages/ArtistDetail';
import WantedAlbums from './pages/WantedAlbums';
import AcquisitionSources from './pages/AcquisitionSources';
import Downloads from './pages/Downloads';
import QualityProfiles from './pages/QualityProfiles';
import Config from './pages/Config';
import { Login } from './pages/Login';
import { isAuthenticated, api, getBasePath } from './lib/api';
import { useSSE } from './hooks/useSSE';
import { useOnlineStatus } from './hooks/useOnlineStatus';
import { StatusLine } from './components/StatusLine';
import UniversalSearch from './components/UniversalSearch';
import { useAppStore } from './store';

// Protected route wrapper - redirects to login if auth is enabled and no token
function ProtectedRoute({ children }: { children: React.ReactNode }) {
  const authEnabled = useAppStore((state) => state.authEnabled);
  const authenticated = isAuthenticated();

  // If we don't know yet whether auth is required, show nothing (loading)
  if (authEnabled === null) {
    return null;
  }

  // If auth is required but user is not authenticated, redirect to login
  if (authEnabled && !authenticated) {
    return <Navigate to="/login" replace />;
  }

  return <>{children}</>;
}

function Sidebar({ isMobileMenuOpen, setIsMobileMenuOpen }: { isMobileMenuOpen: boolean; setIsMobileMenuOpen: (open: boolean) => void }) {
  const navigate = useNavigate();
  const location = useLocation();
  const authenticated = isAuthenticated();
  const authEnabled = useAppStore((state) => state.authEnabled);

  const handleLogout = () => {
    api.logout();
    navigate('/login');
  };

  // Don't show sidebar on login page
  if (location.pathname === '/login') {
    return null;
  }

  const navItems = [
    { to: '/', icon: LayoutDashboard, label: 'Overview' },
    { to: '/diffs', icon: Database, label: 'Library' },
    { to: '/artists', icon: Music, label: 'Artists' },
    { to: '/wanted', icon: Disc, label: 'Albums' },
    { to: '/downloads', icon: Download, label: 'Downloads' },
    { to: '/sources', icon: Sliders, label: 'Input Sources' },
    { to: '/quality', icon: Award, label: 'Quality Profiles' },
  ];

  const handleNavClick = () => {
    // Close mobile menu when navigating
    setIsMobileMenuOpen(false);
  };

  return (
    <>
      {/* Mobile backdrop */}
      {isMobileMenuOpen && (
        <div
          className="fixed inset-0 bg-black/50 z-40 lg:hidden"
          onClick={() => setIsMobileMenuOpen(false)}
        />
      )}

      {/* Sidebar */}
      <aside className={`
        fixed lg:static inset-y-0 left-0 z-50
        w-64 bg-dark-bg-elevated border-r border-dark-border flex flex-col
        transform transition-transform duration-300 ease-in-out
        ${isMobileMenuOpen ? 'translate-x-0' : '-translate-x-full lg:translate-x-0'}
      `}>
        {/* App branding */}
        <div className="p-6 border-b border-dark-border">
          <NavLink to="/" className="block group" onClick={handleNavClick}>
            <img src="/logo.png" alt="skema" className="w-full rounded-lg transition-opacity group-hover:opacity-80 mb-2" />
          </NavLink>
        </div>

        {/* Main navigation */}
        <nav className="flex-1 p-4 space-y-1">
          {navItems.map((item) => {
            const Icon = item.icon;
            const isActive = location.pathname === item.to ||
              (item.to === '/diffs' && location.pathname === '/identification');

            return (
              <NavLink
                key={item.to}
                to={item.to}
                onClick={handleNavClick}
                className={`flex items-center gap-3 px-4 py-3 rounded-lg transition-all duration-200 ${
                  isActive
                    ? 'bg-dark-accent text-dark-bg font-medium'
                    : 'text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-text'
                }`}
              >
                <Icon className="w-5 h-5" />
                <span>{item.label}</span>
              </NavLink>
            );
          })}
        </nav>

        {/* Bottom actions */}
        <div className="p-4 border-t border-dark-border space-y-2">
          <NavLink
            to="/config"
            onClick={handleNavClick}
            className={({ isActive }) =>
              `flex items-center gap-3 px-4 py-3 rounded-lg transition-all duration-200 ${
                isActive
                  ? 'bg-dark-bg-hover text-dark-accent'
                  : 'text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-text'
              }`
            }
          >
            <Settings className="w-5 h-5" />
            <span>Settings</span>
          </NavLink>

          {/* Only show logout if auth is enabled */}
          {authEnabled && authenticated && (
            <button
              onClick={handleLogout}
              className="w-full flex items-center gap-3 px-4 py-3 rounded-lg text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-error transition-all duration-200"
            >
              <LogOut className="w-5 h-5" />
              <span>Logout</span>
            </button>
          )}
        </div>
      </aside>
    </>
  );
}

// Inner app component that has access to useNavigate
function AppContent() {
  const navigate = useNavigate();
  const location = useLocation();
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);
  const authEnabled = useAppStore((state) => state.authEnabled);
  const setAuthEnabled = useAppStore((state) => state.setAuthEnabled);
  const isOnline = useOnlineStatus();

  // Don't connect to SSE on login page
  const isLoginPage = location.pathname === '/login';

  // Connect to SSE for real-time updates (except on login page, and only when online)
  useSSE(!isLoginPage && isOnline);

  // Check auth status once on initial mount
  useEffect(() => {
    const checkAuth = async () => {
      const { authEnabled: isAuthEnabled } = await api.checkAuthStatus();
      setAuthEnabled(isAuthEnabled);
    };
    checkAuth();
  }, [setAuthEnabled]);

  // Redirect away from login page if auth is disabled
  useEffect(() => {
    if (authEnabled === false && location.pathname === '/login') {
      navigate('/');
    }
  }, [authEnabled, location.pathname, navigate]);

  // Listen for 401 errors globally and redirect to login
  useEffect(() => {
    const handleUnauthorized = () => {
      navigate('/login');
    };

    window.addEventListener('unauthorized', handleUnauthorized);
    return () => window.removeEventListener('unauthorized', handleUnauthorized);
  }, [navigate]);

  // Watch for auth being enabled and redirect to login
  useEffect(() => {
    // If auth is enabled and user is not authenticated and not already on login page
    if (authEnabled === true && !isAuthenticated() && location.pathname !== '/login') {
      // Clear any existing JWT since credentials have changed
      api.logout();
      navigate('/login');
    }
  }, [authEnabled, navigate, location.pathname]);

  // Show loading state while checking auth
  if (authEnabled === null) {
    return (
      <div className="h-screen flex items-center justify-center bg-dark-bg">
        <div className="text-dark-text-secondary">Loading...</div>
      </div>
    );
  }

  return (
    <div className="h-screen flex bg-dark-bg">
      {/* Sidebar */}
      {!isLoginPage && <Sidebar isMobileMenuOpen={isMobileMenuOpen} setIsMobileMenuOpen={setIsMobileMenuOpen} />}

      {/* Main content area */}
      <div className="flex-1 flex flex-col overflow-hidden">
        {/* Offline banner */}
        {!isOnline && (
          <div className="bg-dark-error text-dark-text px-4 py-2 flex items-center justify-center gap-2">
            <WifiOff className="w-4 h-4" />
            <span className="text-sm font-medium">You are offline. Some features may not be available.</span>
          </div>
        )}

        {/* Top header bar - mobile hamburger menu and search */}
        {!isLoginPage && (
          <header className="bg-dark-bg-elevated border-b border-dark-border px-4 py-3 flex items-center gap-4">
            {/* Hamburger menu button - only visible on mobile */}
            <button
              onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
              className="lg:hidden p-2 rounded-lg hover:bg-dark-bg-hover text-dark-text-secondary transition-colors"
              aria-label="Toggle menu"
            >
              {isMobileMenuOpen ? <X className="w-6 h-6" /> : <Menu className="w-6 h-6" />}
            </button>

            {/* Search bar */}
            <div className="flex-1">
              <UniversalSearch />
            </div>
          </header>
        )}

        {/* Main content - scrollable */}
        <main className="flex-1 overflow-y-auto pb-20 sm:pb-0">
          <div className={isLoginPage ? '' : 'max-w-7xl mx-auto py-8 px-4 sm:px-6 lg:px-8'}>
            <Routes>
              <Route path="/login" element={<Login />} />
              <Route path="/" element={<ProtectedRoute><Dashboard /></ProtectedRoute>} />
              <Route path="/diffs" element={<ProtectedRoute><MetadataDiffs /></ProtectedRoute>} />
              <Route path="/identification" element={<ProtectedRoute><Identification /></ProtectedRoute>} />
              <Route path="/artists" element={<ProtectedRoute><FollowedArtists /></ProtectedRoute>} />
              <Route path="/artists/:id" element={<ProtectedRoute><ArtistDetail /></ProtectedRoute>} />
              <Route path="/wanted" element={<ProtectedRoute><WantedAlbums /></ProtectedRoute>} />
              <Route path="/sources" element={<ProtectedRoute><AcquisitionSources /></ProtectedRoute>} />
              <Route path="/downloads" element={<ProtectedRoute><Downloads /></ProtectedRoute>} />
              <Route path="/quality" element={<ProtectedRoute><QualityProfiles /></ProtectedRoute>} />
              <Route path="/config" element={<ProtectedRoute><Config /></ProtectedRoute>} />
            </Routes>
          </div>
        </main>

        {/* Status line - shows current operation */}
        {!isLoginPage && (
          <div className="flex-shrink-0 fixed sm:sticky bottom-0 left-0 right-0 sm:left-auto sm:right-auto z-30">
            <StatusLine />
          </div>
        )}
      </div>

      {/* Toast notifications */}
      <Toaster
        position="top-right"
        toastOptions={{
          duration: 4000,
          style: {
            background: '#15181d',
            color: '#e8e8e8',
            border: '1px solid #2a2f38',
          },
          success: {
            duration: 3000,
            iconTheme: {
              primary: '#10b981',
              secondary: '#15181d',
            },
          },
          error: {
            duration: 5000,
            iconTheme: {
              primary: '#ef4444',
              secondary: '#15181d',
            },
          },
        }}
      />
    </div>
  );
}

function App() {
  // Get the base path for proper routing when deployed at a subpath
  const basename = getBasePath();

  return (
    <BrowserRouter basename={basename}>
      <AppContent />
    </BrowserRouter>
  );
}

export default App;
