import { useMemo, memo } from 'react';
import { Link } from 'react-router-dom';
import { Music, ExternalLink, ArrowRight } from 'lucide-react';
import { useAppStore } from '../store';

function RecentlyFollowedArtistsComponent() {
  // Purely presentational - just display what's in the store
  // Artists are loaded via SSE events and by other pages
  const allArtists = useAppStore((state) => state.followedArtists);
  const recentArtists = useMemo(() => allArtists.slice(0, 5), [allArtists]);

  // Don't render if no artists
  if (recentArtists.length === 0) {
    return null;
  }

  const formatTimeAgo = (dateString: string) => {
    const date = new Date(dateString);
    const seconds = Math.floor((new Date().getTime() - date.getTime()) / 1000);

    if (seconds < 60) return 'just now';
    const minutes = Math.floor(seconds / 60);
    if (minutes < 60) return `${minutes}m ago`;
    const hours = Math.floor(minutes / 60);
    if (hours < 24) return `${hours}h ago`;
    const days = Math.floor(hours / 24);
    return `${days}d ago`;
  };

  return (
    <div className="card p-6">
      <div className="flex items-start justify-between mb-5">
        <div>
          <h2 className="text-lg font-semibold text-dark-text">Recently Followed</h2>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Artists you're following for new releases
          </p>
        </div>
        <Link to="/artists" className="link text-sm">View all →</Link>
      </div>

      <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-4">
        {recentArtists.map((artist) => (
            <Link
              key={artist.mbid}
              to={`/artists/${artist.id}`}
              className="card-hover p-4 flex flex-col items-center text-center group"
            >
              <div className="mb-3 relative">
                {artist.thumbnail_url ? (
                  <img
                    src={artist.thumbnail_url}
                    alt={artist.name}
                    className="w-16 h-16 rounded-full object-cover ring-2 ring-dark-border group-hover:ring-dark-accent transition-all duration-200"
                    onError={(e) => {
                      (e.target as HTMLImageElement).style.display = 'none';
                      (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
                    }}
                  />
                ) : null}
                <div className={`w-16 h-16 bg-dark-bg-subtle rounded-full flex items-center justify-center ring-2 ring-dark-border group-hover:ring-dark-accent transition-all duration-200 ${artist.thumbnail_url ? 'hidden' : ''}`}>
                  <Music className="w-8 h-8 text-dark-text-secondary" />
                </div>
              </div>
              <div className="min-w-0 w-full">
                <p className="text-sm font-medium text-dark-text truncate group-hover:text-dark-accent transition-colors" title={artist.name}>
                  {artist.name}
                </p>
                <p className="text-xs text-dark-text-tertiary mt-1">
                  {formatTimeAgo(artist.created_at)}
                </p>
              </div>
              <span
                onClick={(e) => {
                  e.preventDefault();
                  window.open(`https://musicbrainz.org/artist/${artist.mbid}`, '_blank');
                }}
                className="mt-3 text-xs link flex items-center gap-1"
                title="View on MusicBrainz"
              >
                <ExternalLink className="w-3 h-3" />
                <span>MB</span>
              </span>
            </Link>
        ))}

        {/* View All Card */}
        <Link
          to="/artists"
          className="card-hover p-4 flex flex-col items-center justify-center text-center border-2 border-dashed border-dark-border hover:border-dark-accent group"
        >
          <div className="w-16 h-16 bg-dark-bg-subtle rounded-full flex items-center justify-center mb-3 group-hover:bg-dark-accent/10 transition-all duration-200">
            <ArrowRight className="w-8 h-8 text-dark-text-secondary group-hover:text-dark-accent transition-colors" />
          </div>
          <p className="text-sm font-medium text-dark-text">
            View All
          </p>
          <p className="text-xs text-dark-text-tertiary mt-1">
            Followed Artists
          </p>
        </Link>
      </div>
    </div>
  );
}

export const RecentlyFollowedArtists = memo(RecentlyFollowedArtistsComponent);
