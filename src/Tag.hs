module Tag
  ( read
  , write
  )
where


import           Protolude

import           Sound.HTagLib

data AudioTrack = AudioTrack
  { atTitle   :: Title
  , atArtist  :: Artist
  }
  deriving Show

audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter = AudioTrack <$> titleGetter <*> artistGetter

read :: FilePath -> IO ()
read path = do
  track <- getTags path audioTrackGetter
  putText
    $  (unArtist . atArtist $ track)
    <> " - "
    <> (unTitle . atTitle $ track)
    <> ".mp3"

write :: FilePath -> Text -> Maybe Text -> IO ()
write path artist title =
  setTags path Nothing
    $  artistSetter (mkArtist artist)
    <> maybe mempty (titleSetter . mkTitle) title

