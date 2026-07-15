import { CatalogAlbumOverview } from '../types/api';
import { X } from 'lucide-react';
import { AlbumReleasesList } from './AlbumReleasesList';

interface AlbumReleasesModalProps {
  album: CatalogAlbumOverview;
  onClose: () => void;
}

export function AlbumReleasesModal({ album, onClose }: AlbumReleasesModalProps) {
  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-4xl w-full max-h-[80vh] overflow-hidden flex flex-col">
        {/* Modal Header */}
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <div>
            <h2 className="text-xl font-bold text-dark-text">Available Releases</h2>
            <p className="text-sm text-dark-text-secondary mt-1">
              {album.title} - {album.artist_name}
            </p>
          </div>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="h-6 w-6" />
          </button>
        </div>

        {/* Modal Body */}
        <div className="flex-1 overflow-y-auto p-6">
          <AlbumReleasesList
            albumId={album.id}
            albumTitle={album.title}
            artistName={album.artist_name}
            onQueued={onClose}
          />
        </div>

        {/* Modal Footer */}
        <div className="px-6 py-4 border-t border-dark-border flex justify-end">
          <button
            onClick={onClose}
            className="btn-secondary"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
