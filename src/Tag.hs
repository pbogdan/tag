{-# LANGUAGE FlexibleContexts #-}
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

import           Control.Lens                   ( view )
import           Data.Aeson              hiding ( Options(..) )
import           Data.Char                      ( toUpper )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.String                    ( String )
import qualified Data.Text                     as Text
import           Options.Applicative
import           Sound.HTagLib
import           System.Directory               ( renameFile )
import           System.FilePath.Lens           ( filename )
import           Text.EDE
import           Text.EDE.Filters

-- | Convert a word to title case by capitalising the first letter
-- capitalise :: IsString a => a -> a
capitalise :: (StringConv a String, StringConv String a) => a -> a
capitalise = toS . go . toS
 where
  go []       = []
  go (c : cs) = toUpper c : cs

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

displayAudioTrack :: AudioTrack -> Text
displayAudioTrack AudioTrack {..} =
  let keys = map
        (padString 20 ' ' . (<> ":"))
        ["Artist", "Title", "Album", "Comment", "Genre", "Year", "Track number"]
      vals =
          [ unArtist audioTrackArtist
          , unTitle audioTrackTitle
          , unAlbum audioTrackAlbum
          , unComment audioTrackComment
          , unGenre audioTrackGenre
          , maybe "" (show . unYear)        audioTrackYear
          , maybe "" (show . unTrackNumber) audioTrackTrack
          ]
  in  Text.intercalate "\n" . zipWith (<>) keys $ vals
 where
  padString
    :: (StringConv a String, StringConv String a) => Int -> Char -> a -> a
  padString n x xs
    | length (toS xs :: String) >= n = xs
    | otherwise = toS (toS xs ++ replicate (n - length (toS xs :: String)) x)

parseFormat :: AudioTrack -> Text -> FilePath -> Either Text Text
parseFormat track format path =
  let
    template = eitherParse . toS $ format
    filters :: HashMap Id Term
    filters = HashMap.fromList
      [("capitalise", quote "capitalise" 0 (capitalise :: Text -> Text))]
    vars = HashMap.fromList
      [ ("filename", String . toS . view filename $ path)
      , ("filepath", String . toS $ path)
      , ("all"     , String . displayAudioTrack $ track)
      ]
    env = maybe mempty (HashMap.union vars) (fromValue . toJSON $ track)
    rResult =
      bimap toS toS . (flip (eitherRenderWith filters) env =<<) $ template
  in
    rResult

read :: Maybe Text -> [FilePath] -> IO ()
read mFormat paths = do
  for_ paths $ \path -> do
    track <- getTags path audioTrackGetter
    case mFormat of
      Just format -> case parseFormat track format path of
        Right formatted -> putText formatted
        Left  err       -> do
          putErrText $ "Parsing template failed" <> err
          exitFailure
      Nothing -> putText . displayAudioTrack $ track

rename :: Text -> [FilePath] -> IO ()
rename format paths = do
  for_ paths $ \path -> do
    track <- getTags path audioTrackGetter
    case parseFormat track format path of
      Right formatted -> renameFile path (toS formatted)
      Left  err       -> do
        putErrText $ "Parsing template failed" <> err
        exitFailure

write :: [Update] -> [FilePath] -> IO ()
write updates paths =
  for_ paths $ \path -> setTags path Nothing (composeUpdates updates)

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: Text) #-}
data Options = Options Command deriving (Eq, Show)

data Command =
  Read (Maybe Text) [FilePath]
  | Write [Update] [FilePath]
  | Rename Text [FilePath]
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

-- Important note - this composes the updates however the updates are composed in reverse order
-- thanks to the Dual monoid. Under the default order, should duplicate setters be combined, the
-- first one will "win". This however makes less sense when the updates are fed from CLI where it
-- makes more sense for the last entry to win. For example --artist foo --artist bar should set the
-- artist to bar.
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
  Read
    <$> optional (strOption (long "format" <> short 'f' <> metavar "format"))
    <*> some (argument str (metavar "file"))

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
    <*> some (argument str (metavar "file"))
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
  Rename <$> strOption (long "format" <> short 'f' <> metavar "format") <*> some
    (argument str (metavar "file"))

processOptions :: IO Command
processOptions = do
  Options cmd <- execParser
    (parseOptions `withInfo` "Read and modify media tags")
  return cmd
