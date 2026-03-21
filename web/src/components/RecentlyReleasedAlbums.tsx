import { useState, useEffect, memo } from 'react';
import { Link } from 'react-router-dom';
import { Disc, ExternalLink, ArrowRight } from 'lucide-react';
import { api } from '../lib/api';
import { formatRelativeDate } from '../lib/formatters';
import { ImageWithFallback } from './ImageWithFallback';
import { Skeleton } from './LoadingSkeleton';
import type { CatalogAlbumOverview } from '../types/api';

function RecentlyReleasedAlbumsComponent() {
  const [albums, setAlbums] = useState<CatalogAlbumOverview[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadRecentlyReleasedAlbums();
  }, []);

  const loadRecentlyReleasedAlbums = async () => {
    try {
      setLoading(true);
      // Get albums released on or before today, sorted by release date (most recent first)
      const today = new Date().toISOString().split('T')[0]; // Format: YYYY-MM-DD
      console.log('[RecentlyReleased] Fetching albums with release_date_before:', today);
      const response = await api.getAlbumOverview({
        release_date_before: today,
        limit: 5,
        sort: 'date',
        order: 'desc',
      });

      console.log('[RecentlyReleased] Received albums:', response.albums.map(a => ({
        title: a.title,
        artist: a.artist_name,
        date: a.first_release_date
      })));
      setAlbums(response.albums);
    } catch (error) {
      console.error('Failed to load recently released albums:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="card p-6">
        <div className="flex items-start justify-between mb-5">
          <div>
            <h2 className="text-lg font-semibold text-dark-text">Recently Released</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Latest albums from your catalog
            </p>
          </div>
        </div>
        <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-4">
          {[...Array(5)].map((_, i) => (
            <div key={i}>
              <Skeleton className="w-16 h-16 mb-3" />
              <Skeleton className="h-3 w-full mb-1" />
              <Skeleton className="h-3 w-3/4" />
            </div>
          ))}
        </div>
      </div>
    );
  }

  // Don't render if no albums
  if (albums.length === 0) {
    return null;
  }

  return (
    <div className="card p-6">
      <div className="flex items-start justify-between mb-5">
        <div>
          <h2 className="text-lg font-semibold text-dark-text">Recently Released</h2>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Latest albums from your catalog
          </p>
        </div>
        <Link to="/albums" className="link text-sm">View all →</Link>
      </div>

      <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-4">
        {albums.map((album) => (
          <Link
            key={album.id}
            to={`/artists/${album.artist_id}`}
            className="card-hover p-4 flex flex-col items-center text-center group"
          >
            <div className="mb-3 relative">
              <ImageWithFallback
                src={album.cover_thumbnail_url}
                alt={album.title}
                imgClassName="w-16 h-16 rounded object-cover ring-2 ring-dark-border group-hover:ring-dark-accent transition-all duration-200"
                fallbackClassName="w-16 h-16 bg-dark-bg-subtle rounded flex items-center justify-center ring-2 ring-dark-border group-hover:ring-dark-accent transition-all duration-200"
                fallbackIcon={<Disc className="w-8 h-8 text-dark-text-secondary" />}
              />
            </div>
            <div className="min-w-0 w-full">
              <p className="text-sm font-medium text-dark-text truncate group-hover:text-dark-accent transition-colors" title={album.title}>
                {album.title}
              </p>
              <p className="text-xs text-dark-text-tertiary mt-0.5 truncate" title={album.artist_name}>
                {album.artist_name}
              </p>
              <p className="text-xs text-dark-text-tertiary mt-1">
                {formatRelativeDate(album.first_release_date)}
              </p>
            </div>
            <span
              onClick={(e) => {
                e.preventDefault();
                window.open(`https://musicbrainz.org/release-group/${album.release_group_mbid}`, '_blank');
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
          to="/albums"
          className="card-hover p-4 flex flex-col items-center justify-center text-center border-2 border-dashed border-dark-border hover:border-dark-accent group"
        >
          <div className="w-16 h-16 bg-dark-bg-subtle rounded flex items-center justify-center mb-3 group-hover:bg-dark-accent/10 transition-all duration-200">
            <ArrowRight className="w-8 h-8 text-dark-text-secondary group-hover:text-dark-accent transition-colors" />
          </div>
          <p className="text-sm font-medium text-dark-text">
            View All
          </p>
          <p className="text-xs text-dark-text-tertiary mt-1">
            Released Albums
          </p>
        </Link>
      </div>
    </div>
  );
}

export const RecentlyReleasedAlbums = memo(RecentlyReleasedAlbumsComponent);
