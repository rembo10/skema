import { useState, useEffect, memo } from 'react';
import { Link } from 'react-router-dom';
import { Calendar, Disc, ExternalLink } from 'lucide-react';
import { api } from '../lib/api';
import type { CatalogAlbumOverview } from '../types/api';

function UpcomingAlbumsComponent() {
  const [albums, setAlbums] = useState<CatalogAlbumOverview[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadUpcomingAlbums();
  }, []);

  const loadUpcomingAlbums = async () => {
    try {
      setLoading(true);
      const response = await api.getAlbumOverview({
        release_date_after: 'today',
        limit: 5,
        sort: 'date',
        order: 'asc',
      });

      setAlbums(response.albums);
    } catch (error) {
      console.error('Failed to load upcoming albums:', error);
    } finally {
      setLoading(false);
    }
  };

  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'Unknown';
    try {
      const date = new Date(dateString);
      return date.toLocaleDateString('en-US', {
        month: 'short',
        day: 'numeric',
        year: 'numeric',
      });
    } catch {
      return dateString;
    }
  };

  const getDaysUntil = (dateString: string) => {
    const releaseDate = new Date(dateString);
    const now = new Date();
    const diffTime = releaseDate.getTime() - now.getTime();
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

    if (diffDays === 0) return 'Today';
    if (diffDays === 1) return 'Tomorrow';
    if (diffDays < 7) return `${diffDays} days`;
    if (diffDays < 30) return `${Math.floor(diffDays / 7)} weeks`;
    return `${Math.floor(diffDays / 30)} months`;
  };

  if (loading) {
    return (
      <div className="card p-6">
        <div className="flex items-center gap-3 mb-5">
          <Calendar className="w-5 h-5 text-dark-accent" />
          <h2 className="text-lg font-semibold text-dark-text">Upcoming Albums</h2>
        </div>
        <div className="space-y-3">
          {[...Array(3)].map((_, i) => (
            <div key={i} className="animate-pulse flex gap-3">
              <div className="w-12 h-12 bg-dark-bg-subtle rounded" />
              <div className="flex-1 space-y-2">
                <div className="h-4 bg-dark-bg-subtle rounded w-3/4" />
                <div className="h-3 bg-dark-bg-subtle rounded w-1/2" />
              </div>
            </div>
          ))}
        </div>
      </div>
    );
  }

  if (albums.length === 0) {
    return null;
  }

  return (
    <div className="card p-6">
      <div className="flex items-start justify-between mb-5">
        <div>
          <div className="flex items-center gap-3">
            <Calendar className="w-5 h-5 text-dark-accent" />
            <h2 className="text-lg font-semibold text-dark-text">Upcoming Albums</h2>
          </div>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Wanted albums releasing soon
          </p>
        </div>
        <Link to="/albums?release_date_after=today" className="link text-sm">View all →</Link>
      </div>

      <div className="space-y-3">
        {albums.map((album) => (
          <div
            key={album.id}
            className="flex items-center gap-4 p-3 rounded-lg hover:bg-dark-bg-hover transition-colors"
          >
            {/* Album Cover */}
            <div className="w-12 h-12 flex-shrink-0 relative">
              {album.cover_thumbnail_url ? (
                <img
                  src={album.cover_thumbnail_url}
                  alt={album.title}
                  className="w-full h-full object-cover rounded border border-dark-border"
                  onError={(e) => {
                    (e.target as HTMLImageElement).style.display = 'none';
                    (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
                  }}
                />
              ) : null}
              <div className={`absolute inset-0 bg-dark-bg-subtle rounded border border-dark-border flex items-center justify-center ${album.cover_thumbnail_url ? 'hidden' : ''}`}>
                <Disc className="w-6 h-6 text-dark-text-tertiary" />
              </div>
            </div>

            {/* Album Info */}
            <div className="flex-1 min-w-0">
              <Link
                to={`/artists/${album.artist_id}`}
                className="font-medium text-dark-text hover:text-dark-accent transition-colors truncate block"
              >
                {album.title}
              </Link>
              <p className="text-sm text-dark-text-secondary truncate">
                {album.artist_name}
              </p>
            </div>

            {/* Release Date */}
            <div className="flex-shrink-0 text-right">
              <div className="text-sm font-medium text-dark-accent">
                {album.first_release_date ? getDaysUntil(album.first_release_date) : ''}
              </div>
              <div className="text-xs text-dark-text-tertiary">
                {formatDate(album.first_release_date)}
              </div>
            </div>

            {/* MusicBrainz Link */}
            <a
              href={`https://musicbrainz.org/release-group/${album.release_group_mbid}`}
              target="_blank"
              rel="noopener noreferrer"
              className="flex-shrink-0 text-dark-text-tertiary hover:text-dark-accent transition-colors"
              onClick={(e) => e.stopPropagation()}
              title="View on MusicBrainz"
            >
              <ExternalLink className="w-4 h-4" />
            </a>
          </div>
        ))}
      </div>
    </div>
  );
}

export const UpcomingAlbums = memo(UpcomingAlbumsComponent);
