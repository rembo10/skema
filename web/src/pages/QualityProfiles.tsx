import { useState, useEffect } from 'react';
import { Plus, Trash2, Edit2, GripVertical, X, Check, Star } from 'lucide-react';
import toast from 'react-hot-toast';
import { api } from '../lib/api';
import type { QualityProfile, QualityPreference, Quality } from '../types/api';
import {
  DndContext,
  closestCenter,
  KeyboardSensor,
  PointerSensor,
  useSensor,
  useSensors,
  DragEndEvent,
} from '@dnd-kit/core';
import {
  arrayMove,
  SortableContext,
  sortableKeyboardCoordinates,
  useSortable,
  verticalListSortingStrategy,
} from '@dnd-kit/sortable';
import { CSS } from '@dnd-kit/utilities';

// Quality labels for display
const QUALITY_LABELS: Record<Quality, string> = {
  unknown: 'Unknown',
  mp3_192: 'MP3 192kbps',
  vbr2: 'MP3 VBR V2',
  mp3_256: 'MP3 256kbps',
  vbr0: 'MP3 VBR V0',
  mp3_320: 'MP3 320kbps',
  lossless: 'FLAC/Lossless',
  hires_lossless: 'Hi-Res Lossless (24-bit)',
};

// All available qualities in default order
const ALL_QUALITIES: Quality[] = [
  'unknown',
  'mp3_192',
  'vbr2',
  'mp3_256',
  'vbr0',
  'mp3_320',
  'lossless',
  'hires_lossless',
];

interface SortableQualityItemProps {
  preference: QualityPreference;
  isCutoff: boolean;
  onToggleEnabled: () => void;
  onSetCutoff: () => void;
}

function SortableQualityItem({ preference, isCutoff, onToggleEnabled, onSetCutoff }: SortableQualityItemProps) {
  const {
    attributes,
    listeners,
    setNodeRef,
    transform,
    transition,
  } = useSortable({ id: preference.quality });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  };

  return (
    <div
      ref={setNodeRef}
      style={style}
      className={`flex items-center gap-3 p-3 rounded-lg border transition-colors ${
        preference.enabled
          ? 'bg-dark-bg-elevated border-dark-border'
          : 'bg-dark-bg border-dark-border opacity-50'
      }`}
    >
      {/* Drag handle */}
      <button
        {...attributes}
        {...listeners}
        className="cursor-grab active:cursor-grabbing text-dark-text-secondary hover:text-dark-text"
      >
        <GripVertical className="w-5 h-5" />
      </button>

      {/* Quality label */}
      <div className="flex-1">
        <div className="font-medium text-dark-text">{QUALITY_LABELS[preference.quality]}</div>
        <div className="text-xs text-dark-text-secondary">Rank: {preference.rank}</div>
      </div>

      {/* Cutoff indicator */}
      {isCutoff && (
        <div className="px-2 py-1 bg-dark-accent/20 text-dark-accent text-xs font-medium rounded">
          Cutoff
        </div>
      )}

      {/* Set as cutoff button */}
      {preference.enabled && !isCutoff && (
        <button
          onClick={onSetCutoff}
          className="px-2 py-1 text-xs text-dark-text-secondary hover:text-dark-accent border border-dark-border hover:border-dark-accent rounded transition-colors"
        >
          Set as Cutoff
        </button>
      )}

      {/* Enable/disable toggle */}
      <button
        onClick={onToggleEnabled}
        className={`w-10 h-6 rounded-full transition-colors relative ${
          preference.enabled ? 'bg-dark-accent' : 'bg-dark-bg-hover'
        }`}
      >
        <div
          className={`absolute top-1 left-1 w-4 h-4 bg-white rounded-full transition-transform ${
            preference.enabled ? 'translate-x-4' : 'translate-x-0'
          }`}
        />
      </button>
    </div>
  );
}

interface ProfileModalProps {
  profile: QualityProfile | null;
  onClose: () => void;
  onSave: () => void;
}

function ProfileModal({ profile, onClose, onSave }: ProfileModalProps) {
  const isEdit = !!profile;
  const [name, setName] = useState(profile?.name || '');
  const [upgradeAutomatically, setUpgradeAutomatically] = useState(profile?.upgrade_automatically ?? true);
  const [preferences, setPreferences] = useState<QualityPreference[]>(() => {
    if (profile) {
      return [...profile.quality_preferences].sort((a, b) => b.rank - a.rank);
    }
    // Default: all qualities enabled with descending ranks
    return ALL_QUALITIES.map((quality, index) => ({
      quality,
      rank: ALL_QUALITIES.length - index,
      enabled: true,
    }));
  });
  const [cutoffQuality, setCutoffQuality] = useState<Quality>(
    profile?.cutoff_quality || 'mp3_320'
  );
  const [saving, setSaving] = useState(false);

  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor, {
      coordinateGetter: sortableKeyboardCoordinates,
    })
  );

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;

    if (over && active.id !== over.id) {
      setPreferences((items) => {
        const oldIndex = items.findIndex((item) => item.quality === active.id);
        const newIndex = items.findIndex((item) => item.quality === over.id);
        const newItems = arrayMove(items, oldIndex, newIndex);

        // Recalculate ranks based on new order (higher index = higher rank)
        return newItems.map((item, index) => ({
          ...item,
          rank: newItems.length - index,
        }));
      });
    }
  };

  const handleToggleEnabled = (quality: Quality) => {
    setPreferences((items) =>
      items.map((item) =>
        item.quality === quality ? { ...item, enabled: !item.enabled } : item
      )
    );
  };

  const handleSetCutoff = (quality: Quality) => {
    setCutoffQuality(quality);
  };

  const handleSave = async () => {
    if (!name.trim()) {
      toast.error('Please enter a profile name');
      return;
    }

    // Check that at least one quality is enabled
    const enabledQualities = preferences.filter((p) => p.enabled);
    if (enabledQualities.length === 0) {
      toast.error('At least one quality must be enabled');
      return;
    }

    // Check that cutoff quality is enabled
    const cutoffPref = preferences.find((p) => p.quality === cutoffQuality);
    if (!cutoffPref?.enabled) {
      toast.error('Cutoff quality must be enabled');
      return;
    }

    setSaving(true);
    try {
      const profileData = {
        name: name.trim(),
        quality_preferences: preferences,
        cutoff_quality: cutoffQuality,
        upgrade_automatically: upgradeAutomatically,
      };

      if (isEdit && profile) {
        await api.updateQualityProfile(profile.id, profileData);
        toast.success('Quality profile updated');
      } else {
        await api.createQualityProfile(profileData);
        toast.success('Quality profile created');
      }

      onSave();
      onClose();
    } catch (error) {
      console.error('Error saving quality profile:', error);
      toast.error(error instanceof Error ? error.message : 'Failed to save quality profile');
    } finally {
      setSaving(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-xl border border-dark-border max-w-2xl w-full max-h-[90vh] overflow-hidden flex flex-col">
        {/* Header */}
        <div className="p-6 border-b border-dark-border flex items-center justify-between">
          <h2 className="text-xl font-semibold text-dark-text">
            {isEdit ? 'Edit Quality Profile' : 'New Quality Profile'}
          </h2>
          <button
            onClick={onClose}
            className="p-2 hover:bg-dark-bg-hover rounded-lg transition-colors text-dark-text-secondary"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-y-auto p-6 space-y-6">
          {/* Profile name */}
          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">
              Profile Name
            </label>
            <input
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
              placeholder="e.g., Lossless Preferred"
              className="w-full px-4 py-2 bg-dark-bg border border-dark-border rounded-lg text-dark-text placeholder-dark-text-secondary focus:outline-none focus:ring-2 focus:ring-dark-accent"
            />
          </div>

          {/* Auto-upgrade toggle */}
          <div className="flex items-center justify-between p-4 bg-dark-bg rounded-lg border border-dark-border">
            <div>
              <div className="font-medium text-dark-text">Upgrade Automatically</div>
              <div className="text-sm text-dark-text-secondary mt-1">
                Automatically search for quality upgrades
              </div>
            </div>
            <button
              onClick={() => setUpgradeAutomatically(!upgradeAutomatically)}
              className={`w-12 h-7 rounded-full transition-colors relative ${
                upgradeAutomatically ? 'bg-dark-accent' : 'bg-dark-bg-hover'
              }`}
            >
              <div
                className={`absolute top-1 left-1 w-5 h-5 bg-white rounded-full transition-transform ${
                  upgradeAutomatically ? 'translate-x-5' : 'translate-x-0'
                }`}
              />
            </button>
          </div>

          {/* Quality preferences */}
          <div>
            <div className="mb-3">
              <div className="text-sm font-medium text-dark-text mb-1">Quality Preferences</div>
              <div className="text-xs text-dark-text-secondary">
                Drag to reorder qualities by preference. Higher qualities will be preferred.
                Once you have the cutoff quality or better, upgrades will stop.
              </div>
            </div>

            <DndContext
              sensors={sensors}
              collisionDetection={closestCenter}
              onDragEnd={handleDragEnd}
            >
              <SortableContext
                items={preferences.map((p) => p.quality)}
                strategy={verticalListSortingStrategy}
              >
                <div className="space-y-2">
                  {preferences.map((preference) => (
                    <SortableQualityItem
                      key={preference.quality}
                      preference={preference}
                      isCutoff={preference.quality === cutoffQuality}
                      onToggleEnabled={() => handleToggleEnabled(preference.quality)}
                      onSetCutoff={() => handleSetCutoff(preference.quality)}
                    />
                  ))}
                </div>
              </SortableContext>
            </DndContext>
          </div>
        </div>

        {/* Footer */}
        <div className="p-6 border-t border-dark-border flex items-center justify-end gap-3">
          <button
            onClick={onClose}
            disabled={saving}
            className="px-4 py-2 text-dark-text-secondary hover:text-dark-text hover:bg-dark-bg-hover rounded-lg transition-colors disabled:opacity-50"
          >
            Cancel
          </button>
          <button
            onClick={handleSave}
            disabled={saving}
            className="px-4 py-2 bg-dark-accent text-dark-bg font-medium rounded-lg hover:bg-dark-accent/90 transition-colors disabled:opacity-50 flex items-center gap-2"
          >
            {saving ? (
              <>Saving...</>
            ) : (
              <>
                <Check className="w-4 h-4" />
                {isEdit ? 'Update Profile' : 'Create Profile'}
              </>
            )}
          </button>
        </div>
      </div>
    </div>
  );
}

interface DeleteConfirmModalProps {
  profile: QualityProfile;
  onClose: () => void;
  onConfirm: () => void;
}

function DeleteConfirmModal({ profile, onClose, onConfirm }: DeleteConfirmModalProps) {
  const [deleting, setDeleting] = useState(false);

  const handleDelete = async () => {
    setDeleting(true);
    try {
      await api.deleteQualityProfile(profile.id);
      toast.success('Quality profile deleted');
      onConfirm();
      onClose();
    } catch (error) {
      console.error('Error deleting quality profile:', error);
      toast.error(error instanceof Error ? error.message : 'Failed to delete quality profile');
    } finally {
      setDeleting(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-xl border border-dark-border max-w-md w-full p-6">
        <h2 className="text-xl font-semibold text-dark-text mb-3">Delete Quality Profile</h2>
        <p className="text-dark-text-secondary mb-6">
          Are you sure you want to delete the profile <span className="font-medium text-dark-text">"{profile.name}"</span>?
          This action cannot be undone.
        </p>
        <div className="flex items-center justify-end gap-3">
          <button
            onClick={onClose}
            disabled={deleting}
            className="px-4 py-2 text-dark-text-secondary hover:text-dark-text hover:bg-dark-bg-hover rounded-lg transition-colors disabled:opacity-50"
          >
            Cancel
          </button>
          <button
            onClick={handleDelete}
            disabled={deleting}
            className="px-4 py-2 bg-dark-error text-white font-medium rounded-lg hover:bg-dark-error/90 transition-colors disabled:opacity-50"
          >
            {deleting ? 'Deleting...' : 'Delete Profile'}
          </button>
        </div>
      </div>
    </div>
  );
}

export default function QualityProfiles() {
  const [profiles, setProfiles] = useState<QualityProfile[]>([]);
  const [loading, setLoading] = useState(true);
  const [showModal, setShowModal] = useState(false);
  const [editingProfile, setEditingProfile] = useState<QualityProfile | null>(null);
  const [deletingProfile, setDeletingProfile] = useState<QualityProfile | null>(null);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);

  const loadProfiles = async () => {
    try {
      const [data, defaultProf] = await Promise.all([
        api.getQualityProfiles(),
        api.getDefaultQualityProfile(),
      ]);
      setProfiles(data);
      setDefaultProfile(defaultProf);
    } catch (error) {
      console.error('Error loading quality profiles:', error);
      toast.error('Failed to load quality profiles');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadProfiles();
  }, []);

  const handleCreate = () => {
    setEditingProfile(null);
    setShowModal(true);
  };

  const handleEdit = (profile: QualityProfile) => {
    setEditingProfile(profile);
    setShowModal(true);
  };

  const handleDelete = (profile: QualityProfile) => {
    setDeletingProfile(profile);
  };

  const handleModalSave = () => {
    loadProfiles();
  };

  const handleSetDefault = async (profile: QualityProfile) => {
    try {
      await api.setDefaultQualityProfile(profile.id);
      setDefaultProfile(profile);
      toast.success(`${profile.name} set as default quality profile`);
    } catch (error) {
      console.error('Error setting default profile:', error);
      toast.error('Failed to set default quality profile');
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center py-12">
        <div className="text-dark-text-secondary">Loading quality profiles...</div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-dark-text">Quality Profiles</h1>
          <p className="text-dark-text-secondary mt-1">
            Manage quality preferences for downloads and upgrades
          </p>
        </div>
        <button
          onClick={handleCreate}
          className="flex items-center gap-2 px-4 py-2 bg-dark-accent text-dark-bg font-medium rounded-lg hover:bg-dark-accent/90 transition-colors"
        >
          <Plus className="w-4 h-4" />
          New Profile
        </button>
      </div>

      {/* Profiles list */}
      {profiles.length === 0 ? (
        <div className="text-center py-12 bg-dark-bg-elevated rounded-lg border border-dark-border">
          <div className="text-dark-text-secondary mb-4">No quality profiles yet</div>
          <button
            onClick={handleCreate}
            className="px-4 py-2 bg-dark-accent text-dark-bg font-medium rounded-lg hover:bg-dark-accent/90 transition-colors"
          >
            Create Your First Profile
          </button>
        </div>
      ) : (
        <div className="grid gap-4">
          {profiles
            .sort((a, b) => {
              // Keep default profile at the top
              if (defaultProfile?.id === a.id) return -1;
              if (defaultProfile?.id === b.id) return 1;
              return 0;
            })
            .map((profile) => {
            const sortedPrefs = [...profile.quality_preferences].sort((a, b) => b.rank - a.rank);
            const enabledPrefs = sortedPrefs.filter((p) => p.enabled);
            const cutoffPref = profile.quality_preferences.find((p) => p.quality === profile.cutoff_quality);

            return (
              <div
                key={profile.id}
                className={`bg-dark-bg-elevated rounded-lg border p-6 transition-colors ${
                  defaultProfile?.id === profile.id
                    ? 'border-dark-accent ring-2 ring-dark-accent/20'
                    : 'border-dark-border hover:border-dark-accent/50'
                }`}
              >
                <div className="flex items-start justify-between mb-4">
                  <div className="flex-1">
                    <div className="flex items-center gap-3">
                      <h3 className="text-lg font-semibold text-dark-text">{profile.name}</h3>
                      {defaultProfile?.id === profile.id && (
                        <div className="flex items-center gap-1.5 px-2 py-1 bg-dark-accent text-dark-bg text-xs font-medium rounded">
                          <Star className="w-3 h-3 fill-current" />
                          Default
                        </div>
                      )}
                    </div>
                    <div className="flex items-center gap-3 mt-2">
                      <div className="text-sm text-dark-text-secondary">
                        Cutoff: <span className="text-dark-accent font-medium">{QUALITY_LABELS[profile.cutoff_quality]}</span>
                      </div>
                      {profile.upgrade_automatically && (
                        <div className="px-2 py-1 bg-dark-accent/20 text-dark-accent text-xs font-medium rounded">
                          Auto-upgrade
                        </div>
                      )}
                    </div>
                  </div>
                  <div className="flex items-center gap-2">
                    {defaultProfile?.id !== profile.id && (
                      <button
                        onClick={() => handleSetDefault(profile)}
                        className="p-2 text-dark-text-secondary hover:text-dark-accent hover:bg-dark-bg-hover rounded-lg transition-colors"
                        title="Set as default profile"
                      >
                        <Star className="w-4 h-4" />
                      </button>
                    )}
                    <button
                      onClick={() => handleEdit(profile)}
                      className="p-2 text-dark-text-secondary hover:text-dark-accent hover:bg-dark-bg-hover rounded-lg transition-colors"
                      title="Edit profile"
                    >
                      <Edit2 className="w-4 h-4" />
                    </button>
                    <button
                      onClick={() => handleDelete(profile)}
                      className="p-2 text-dark-text-secondary hover:text-dark-error hover:bg-dark-bg-hover rounded-lg transition-colors"
                      title="Delete profile"
                    >
                      <Trash2 className="w-4 h-4" />
                    </button>
                  </div>
                </div>

                {/* Quality preference summary */}
                <div className="space-y-2">
                  <div className="text-xs font-medium text-dark-text-secondary uppercase tracking-wider">
                    Enabled Qualities ({enabledPrefs.length})
                  </div>
                  <div className="flex flex-wrap gap-2">
                    {enabledPrefs.map((pref) => (
                      <div
                        key={pref.quality}
                        className={`px-3 py-1 rounded text-sm ${
                          pref.quality === profile.cutoff_quality
                            ? 'bg-dark-accent text-dark-bg font-medium'
                            : 'bg-dark-bg border border-dark-border text-dark-text'
                        }`}
                      >
                        {QUALITY_LABELS[pref.quality]}
                      </div>
                    ))}
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}

      {/* Modals */}
      {showModal && (
        <ProfileModal
          profile={editingProfile}
          onClose={() => {
            setShowModal(false);
            setEditingProfile(null);
          }}
          onSave={handleModalSave}
        />
      )}

      {deletingProfile && (
        <DeleteConfirmModal
          profile={deletingProfile}
          onClose={() => setDeletingProfile(null)}
          onConfirm={loadProfiles}
        />
      )}
    </div>
  );
}
