import { useState } from 'react';
import { api } from '../../lib/api';
import { MBTrackInfo } from '../../types/api';
import toast from 'react-hot-toast';
import { X, Music, Search, Loader2, ExternalLink } from 'lucide-react';

interface TrackEditModalProps {
  trackId: number;
  trackTitle: string | null;
  currentClusterId: number;
  onClose: () => void;
  onUpdate: () => void;
}

export function TrackEditModal({
  trackId,
  trackTitle,
  currentClusterId,
  onClose,
  onUpdate,
}: TrackEditModalProps) {
  const [searchQuery, setSearchQuery] = useState(trackTitle || '');
  const [searching, setSearching] = useState(false);
  const [results, setResults] = useState<MBTrackInfo[]>([]);

  const handleSearch = async () => {
    if (!searchQuery.trim()) {
      toast.error('Please enter a search query');
      return;
    }

    setSearching(true);
    try {
      const recordings = await api.searchRecordings(searchQuery, 25);
      setResults(recordings);
      if (recordings.length === 0) {
        toast.info('No recordings found');
      }
    } catch (error) {
      console.error('Failed to search recordings:', error);
      toast.error('Failed to search recordings');
    } finally {
      setSearching(false);
    }
  };

  const handleSelectRecording = async (recording: MBTrackInfo) => {
    try {
      await api.updateTrackRecording(
        currentClusterId,
        trackId,
        recording.recording_id,
        recording.title
      );
      toast.success(`Matched to: ${recording.title}`);
      onUpdate();
      onClose();
    } catch (error) {
      console.error('Failed to update track recording:', error);
      toast.error('Failed to update recording');
    }
  };

  const formatDuration = (ms: number | null | undefined) => {
    if (!ms) return '—';
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(seconds / 60);
    const remainingSeconds = seconds % 60;
    return `${minutes}:${remainingSeconds.toString().padStart(2, '0')}`;
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg max-w-2xl w-full max-h-[80vh] overflow-hidden flex flex-col">
        {/* Header */}
        <div className="flex items-start justify-between p-6 border-b border-dark-border flex-shrink-0">
          <div className="flex items-start gap-3">
            <div className="p-2 bg-dark-bg rounded-lg">
              <Music className="h-5 w-5 text-dark-accent" />
            </div>
            <div>
              <h2 className="text-xl font-bold text-dark-text">Edit Recording Match</h2>
              <p className="text-sm text-dark-text-secondary mt-1">
                {trackTitle || 'Unknown Track'}
              </p>
            </div>
          </div>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="h-5 w-5" />
          </button>
        </div>

        {/* Search */}
        <div className="p-6 border-b border-dark-border flex-shrink-0">
          <label className="block text-sm font-medium text-dark-text mb-2">
            Search MusicBrainz Recordings
          </label>
          <div className="flex gap-2">
            <div className="flex-1 relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-dark-text-tertiary" />
              <input
                type="text"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && handleSearch()}
                placeholder="Search for recording..."
                className="w-full pl-9 pr-3 py-2 bg-dark-bg border border-dark-border rounded-lg text-sm text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:border-dark-accent"
              />
            </div>
            <button onClick={handleSearch} disabled={searching} className="btn-primary flex items-center gap-2">
              {searching ? (
                <>
                  <Loader2 className="h-4 w-4 animate-spin" />
                  Searching
                </>
              ) : (
                <>
                  <Search className="h-4 w-4" />
                  Search
                </>
              )}
            </button>
          </div>
        </div>

        {/* Results */}
        <div className="flex-1 overflow-auto p-6">
          {results.length === 0 && !searching ? (
            <div className="text-center text-dark-text-secondary py-12">
              <Music className="mx-auto h-12 w-12 text-dark-text-tertiary mb-3" />
              <p>Search for a MusicBrainz recording to match this track</p>
            </div>
          ) : (
            <div className="space-y-2">
              {results.map((recording) => (
                <button
                  key={recording.recording_id}
                  onClick={() => handleSelectRecording(recording)}
                  className="w-full text-left p-4 bg-dark-bg hover:bg-dark-bg-elevated border border-dark-border rounded-lg transition-colors"
                >
                  <div className="flex items-start justify-between gap-4">
                    <div className="flex-1 min-w-0">
                      <div className="font-medium text-dark-text">{recording.title}</div>
                      <div className="flex items-center gap-3 mt-1 text-xs text-dark-text-secondary">
                        <span>Duration: {formatDuration(recording.length)}</span>
                        <a
                          href={`https://musicbrainz.org/recording/${recording.recording_id}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          onClick={(e) => e.stopPropagation()}
                          className="flex items-center gap-1 text-dark-accent hover:underline"
                        >
                          <ExternalLink className="h-3 w-3" />
                          MusicBrainz
                        </a>
                      </div>
                    </div>
                  </div>
                </button>
              ))}
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="p-6 border-t border-dark-border flex justify-end flex-shrink-0">
          <button onClick={onClose} className="btn-secondary">
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
