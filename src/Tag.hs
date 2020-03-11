{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Tag
  ( Command(..)
  , processOptions
  , read
  , rename
  , write
  )
where

import           Protolude               hiding ( option )

import           Data.Aeson              hiding ( Options(..) )
import           System.Directory               ( renameFile )
import           Options.Applicative
import           Sound.HTagLib
import           Text.EDE

data AudioTrack = AudioTrack
  { audioTrackTitle   :: Title
  , audioTrackArtist  :: Artist
  , audioTrackAlbum   :: Album
  , audioTrackComment :: Comment
  , audioTrackGenre   :: Genre
  , audioTrackYear    :: Maybe Year
  , audioTrackTrack   :: Maybe TrackNumber }
  deriving (Eq, Show)

instance ToJSON AudioTrack where
  toJSON AudioTrack {..} = object
    [ "title" .= unTitle audioTrackTitle
    , "artist" .= unArtist audioTrackArtist
    , "album" .= unAlbum audioTrackAlbum
    , "comment" .= unComment audioTrackComment
    , "genre" .= unGenre audioTrackGenre
    , "year" .= (unYear <$> audioTrackYear)
    , "track" .= (unTrackNumber <$> audioTrackTrack)
    ]

audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter =
  AudioTrack
    <$> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> commentGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter

parseFormat :: AudioTrack -> Text -> Either Text Text
parseFormat track format =
  let template = eitherParse . toS $ format
      env      = maybe mempty identity (fromValue . toJSON $ track)
      rResult  = bimap toS toS . (flip eitherRender env =<<) $ template
  in  rResult

read :: Text -> FilePath -> IO ()
read format path = do
  track <- getTags path audioTrackGetter
  case parseFormat track format of
    Right formatted -> putText formatted
    Left  err       -> do
      putErrText $ "Parsing template failed" <> err
      exitFailure

rename :: Text -> FilePath -> IO ()
rename format path = do
  track <- getTags path audioTrackGetter
  case parseFormat track format of
    Right formatted -> renameFile path (toS formatted)
    Left  err       -> do
      putErrText $ "Parsing template failed" <> err
      exitFailure

write :: [Update] -> FilePath -> IO ()
write updates path = setTags path Nothing (composeUpdates updates)

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: Text) #-}
data Options = Options Command deriving (Eq, Show)

data Command =
  Read Text FilePath
  | Write [Update] FilePath
  | Rename Text FilePath
  deriving (Eq, Show)

data Update =
  Artist Artist
  | Title Title
  | Album Album
  | Comment Comment
  | Genre Genre
  | Year (Maybe Year)
  | Track (Maybe TrackNumber)
  deriving (Eq, Show)

-- Important note - this composes the updates however in reverse order thanks to Dual. Under the
-- default order should duplicate setters be combined the first one will "win". This however makes
-- less sense when the updates are fed from CLI where it makes more sense for the last entry to
-- win. For example --artist foo --artist bar should set the artist to bar.
composeUpdates :: [Update] -> TagSetter
composeUpdates = getDual . foldMap go
 where
  go = Dual . \case
    Artist  a -> artistSetter a
    Title   t -> titleSetter t
    Album   a -> albumSetter a
    Comment c -> commentSetter c
    Genre   g -> genreSetter g
    Year    y -> yearSetter y
    Track   t -> trackNumberSetter t

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser
    $  command
         "read"
         (parseReadCommand `withInfo` "Read media tag information from file")
    <> command
         "write"
         (parseWriteCommand `withInfo` "Write media tag information to file")
    <> command
         "rename"
         (          parseRenameCommand
         `withInfo` "Rename the file according to the specified format"
         )

parseReadCommand :: Parser Command
parseReadCommand =
  Read <$> argument str (metavar "format") <*> argument str (metavar "file")

parseWriteCommand :: Parser Command
parseWriteCommand =
  Write
    <$> some
          (   parseArtistUpdate
          <|> parseTitleUpdate
          <|> parseAlbumUpdate
          <|> parseCommentUpdate
          <|> parseGenreUpdate
          <|> parseYearUpdate
          <|> parseTrackUpdate
          )
    <*> argument str (metavar "file")
 where
  parseArtistUpdate = Artist <$> strOption
    (long "artist" <> short 'a' <> metavar "artist" <> help "Name of the artist"
    )
  parseTitleUpdate = Title <$> strOption
    (long "title" <> short 't' <> metavar "title" <> help "Title of the track")
  parseAlbumUpdate = Album <$> strOption
    (long "album" <> short 'l' <> metavar "album" <> help
      "Album on which track appears"
    )
  parseCommentUpdate = Comment <$> strOption
    (long "comment" <> short 'c' <> metavar "comment" <> help
      "Additional comment"
    )
  parseGenreUpdate = Genre <$> strOption
    (long "genre" <> short 'g' <> metavar "genre" <> help "Genre")
  parseYearUpdate = Year . mkYear <$> option
    auto
    (long "year" <> short 'y' <> metavar "year" <> help "Year")
  parseTrackUpdate = Track . mkTrackNumber <$> option
    auto
    (long "track" <> short 'n' <> metavar "track" <> help "Track number")

parseRenameCommand :: Parser Command
parseRenameCommand =
  Rename <$> argument str (metavar "format") <*> argument str (metavar "file")

processOptions :: IO Command
processOptions = do
  Options cmd <- execParser
    (parseOptions `withInfo` "Read and modify media tags")
  return cmd
