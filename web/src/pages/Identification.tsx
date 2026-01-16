import { useLocation } from 'react-router-dom';
import { IdentificationNav } from '../components/IdentificationNav';
import Tracks from './Tracks';
import Clusters from './Clusters';


export default function Identification() {
  const location = useLocation();
  const isTracksView = location.pathname === '/identification/tracks';

  return (
    <div className="h-full flex flex-col">
      {isTracksView ? <Tracks /> : <Clusters />}
    </div>
  );
}
