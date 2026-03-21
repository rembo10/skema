import type { ReactElement } from 'react';

interface ImageWithFallbackProps {
  src: string | null | undefined;
  alt: string;
  imgClassName: string;
  fallbackClassName: string;
  fallbackIcon: ReactElement;
}

export function ImageWithFallback({ src, alt, imgClassName, fallbackClassName, fallbackIcon }: ImageWithFallbackProps) {
  return (
    <>
      {src ? (
        <img
          src={src}
          alt={alt}
          className={imgClassName}
          onError={(e) => {
            (e.target as HTMLImageElement).style.display = 'none';
            (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
          }}
        />
      ) : null}
      <div className={`${fallbackClassName} ${src ? 'hidden' : ''}`}>
        {fallbackIcon}
      </div>
    </>
  );
}
