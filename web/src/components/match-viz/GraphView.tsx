import { useCallback, useMemo, useState } from 'react';
import ReactFlow, {
  Node,
  Edge,
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
  Connection,
  NodeTypes,
  Panel,
} from 'reactflow';
import 'reactflow/dist/style.css';
import { Cluster } from '../../types/api';
import { ClusterNode } from './nodes/ClusterNode';
import { Info, Zap, Search } from 'lucide-react';

interface GraphViewProps {
  clusters: Cluster[];
  onClusterUpdate: () => void;
}

export function GraphView({ clusters, onClusterUpdate }: GraphViewProps) {
  const [selectedNode, setSelectedNode] = useState<string | null>(null);

  // Memoize nodeTypes to prevent re-creation on every render
  const nodeTypes = useMemo<NodeTypes>(() => ({
    cluster: ClusterNode,
  }), []);

  // Convert clusters to ReactFlow nodes
  const initialNodes: Node[] = useMemo(() => {
    return clusters.map((cluster, index) => {
      const angle = (index / clusters.length) * 2 * Math.PI;
      const radius = 300 + (clusters.length * 2);

      // Determine node color based on match status
      let nodeColor = '#ef4444'; // unmatched (red)
      if (cluster.mb_release_id) {
        if (cluster.match_locked) {
          nodeColor = '#a855f7'; // locked (purple)
        } else if (cluster.match_source === 'auto_fingerprint') {
          nodeColor = '#3b82f6'; // fingerprint (blue)
        } else {
          nodeColor = '#10b981'; // metadata (green)
        }
      }

      return {
        id: cluster.id.toString(),
        type: 'cluster',
        position: {
          x: Math.cos(angle) * radius,
          y: Math.sin(angle) * radius,
        },
        data: {
          cluster,
          color: nodeColor,
          onClusterUpdate,
        },
        style: {
          width: 200,
          height: 120,
        },
      };
    });
  }, [clusters, onClusterUpdate]);

  // Create edges between clusters with shared artists
  const initialEdges: Edge[] = useMemo(() => {
    const edges: Edge[] = [];
    const artistMap = new Map<string, string[]>();

    // Group clusters by artist
    clusters.forEach((cluster) => {
      const artist = cluster.album_artist || cluster.mb_release_artist;
      if (artist) {
        if (!artistMap.has(artist)) {
          artistMap.set(artist, []);
        }
        artistMap.get(artist)!.push(cluster.id.toString());
      }
    });

    // Create edges between clusters with the same artist
    artistMap.forEach((clusterIds) => {
      if (clusterIds.length > 1) {
        for (let i = 0; i < clusterIds.length - 1; i++) {
          edges.push({
            id: `${clusterIds[i]}-${clusterIds[i + 1]}`,
            source: clusterIds[i],
            target: clusterIds[i + 1],
            type: 'smoothstep',
            animated: false,
            style: {
              stroke: '#64748b',
              strokeWidth: 1,
              opacity: 0.3,
            },
          });
        }
      }
    });

    return edges;
  }, [clusters]);

  const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);

  const onConnect = useCallback(
    (params: Connection) => setEdges((eds) => [...eds, params as Edge]),
    [setEdges]
  );

  const onNodeClick = useCallback((_event: React.MouseEvent, node: Node) => {
    setSelectedNode(node.id);
  }, []);

  const stats = useMemo(() => {
    const matched = clusters.filter(c => c.mb_release_id).length;
    const fingerprint = clusters.filter(c => c.match_source === 'auto_fingerprint').length;
    const metadata = clusters.filter(c => c.match_source === 'auto_metadata').length;
    const locked = clusters.filter(c => c.match_locked).length;

    return { matched, fingerprint, metadata, locked, total: clusters.length };
  }, [clusters]);

  return (
    <div style={{ height: 800 }} className="w-full bg-dark-bg rounded-lg overflow-hidden border border-dark-border">
      <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onConnect={onConnect}
          onNodeClick={onNodeClick}
          nodeTypes={nodeTypes}
          fitView
          className="bg-dark-bg"
          minZoom={0.1}
          maxZoom={2}
        >
        <Background color="#1e293b" gap={16} />
        <Controls className="bg-dark-bg-elevated border border-dark-border rounded-lg" />
        <MiniMap
          className="bg-dark-bg-elevated border border-dark-border rounded-lg"
          nodeColor={(node) => {
            const cluster = clusters.find(c => c.id.toString() === node.id);
            if (!cluster?.mb_release_id) return '#ef4444';
            if (cluster.match_locked) return '#a855f7';
            if (cluster.match_source === 'auto_fingerprint') return '#3b82f6';
            return '#10b981';
          }}
        />

        <Panel position="top-right" className="bg-dark-bg-elevated border border-dark-border rounded-lg p-4 m-4">
          <div className="space-y-3">
            <div className="flex items-center gap-2 text-sm">
              <Info className="h-4 w-4 text-dark-accent" />
              <span className="font-semibold text-dark-text">Graph Legend</span>
            </div>

            <div className="space-y-2 text-xs">
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-green-500"></div>
                <span className="text-dark-text-secondary">Matched ({stats.matched})</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-blue-500"></div>
                <Zap className="h-3 w-3 text-blue-400" />
                <span className="text-dark-text-secondary">Fingerprint ({stats.fingerprint})</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-green-500"></div>
                <Search className="h-3 w-3 text-green-400" />
                <span className="text-dark-text-secondary">Metadata ({stats.metadata})</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-purple-500"></div>
                <span className="text-dark-text-secondary">Locked ({stats.locked})</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-red-500"></div>
                <span className="text-dark-text-secondary">Unmatched ({stats.total - stats.matched})</span>
              </div>
            </div>

            <div className="pt-3 border-t border-dark-border text-xs text-dark-text-tertiary">
              <p>• Scroll to zoom</p>
              <p>• Drag to pan</p>
              <p>• Click nodes for details</p>
              <p>• Lines connect same artist</p>
            </div>
          </div>
        </Panel>

        <Panel position="bottom-center" className="bg-dark-bg-elevated border border-dark-border rounded-full px-4 py-2 mb-4">
          <div className="flex items-center gap-4 text-sm text-dark-text">
            <span className="font-semibold">{stats.total} Albums</span>
            <span className="text-dark-text-tertiary">•</span>
            <span className="text-green-400">{stats.matched} Matched</span>
            <span className="text-dark-text-tertiary">•</span>
            <span className="text-red-400">{stats.total - stats.matched} Unmatched</span>
          </div>
        </Panel>
      </ReactFlow>
    </div>
  );
}
