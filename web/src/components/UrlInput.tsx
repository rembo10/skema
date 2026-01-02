type Protocol = 'http://' | 'https://';

interface UrlInputProps {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  className?: string;
  id?: string;
  label?: string;
  description?: string;
  defaultProtocol?: Protocol;
}

function parseUrl(url: string, defaultProtocol: Protocol): { protocol: Protocol; rest: string } {
  if (url.startsWith('https://')) {
    return { protocol: 'https://', rest: url.slice(8) };
  } else if (url.startsWith('http://')) {
    return { protocol: 'http://', rest: url.slice(7) };
  } else {
    return { protocol: defaultProtocol, rest: url };
  }
}

export function UrlInput({
  value,
  onChange,
  placeholder = 'localhost:8080',
  className = '',
  id,
  label,
  description,
  defaultProtocol = 'https://',
}: UrlInputProps) {
  const { protocol, rest } = parseUrl(value, defaultProtocol);

  const handleProtocolChange = (newProtocol: Protocol) => {
    onChange(newProtocol + rest);
  };

  const handleUrlChange = (newRest: string) => {
    // Strip any protocol the user might paste
    let cleanRest = newRest;
    if (cleanRest.startsWith('https://')) {
      cleanRest = cleanRest.slice(8);
      onChange('https://' + cleanRest);
      return;
    } else if (cleanRest.startsWith('http://')) {
      cleanRest = cleanRest.slice(7);
      onChange('http://' + cleanRest);
      return;
    }
    onChange(protocol + cleanRest);
  };

  return (
    <div className={className}>
      {label && (
        <label htmlFor={id} className="block text-sm font-medium text-dark-text mb-2">
          {label}
        </label>
      )}
      <div className="flex">
        <select
          value={protocol}
          onChange={(e) => handleProtocolChange(e.target.value as Protocol)}
          className="input rounded-r-none -mr-px w-28 pr-8 bg-dark-bg-subtle text-dark-text-secondary focus:z-10"
        >
          <option value="https://">https://</option>
          <option value="http://">http://</option>
        </select>
        <input
          type="text"
          id={id}
          value={rest}
          onChange={(e) => handleUrlChange(e.target.value)}
          className="input flex-1 rounded-l-none focus:z-10"
          placeholder={placeholder}
        />
      </div>
      {description && (
        <p className="mt-2 text-sm text-dark-text-secondary">{description}</p>
      )}
    </div>
  );
}
