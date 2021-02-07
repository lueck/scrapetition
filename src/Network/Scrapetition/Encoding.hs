{-# LANGUAGE OverloadedStrings #-}
module Network.Scrapetition.Encoding where

-- | Parsers for the encoding of a received document.

import Text.HTML.Scalpel
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Control.Monad
import Control.Lens


-- | Parses the given document and tries to return a decoding function.
decoder :: BS.ByteString -> (T.Text, (T.OnDecodeError -> BS.ByteString -> T.Text))
decoder doc = do
  case (join $ scrapeStringLike doc getEncoding) of
    Just "ISO-8859-1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "iso-8859-1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "ISO_8859-1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "latin1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "csISOLatin1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "l1" -> ("ISO-8859-1", const T.decodeLatin1)
    Just "CP819" -> ("ISO-8859-1", const T.decodeLatin1)
    -- TODO: add more
    Just enc -> (enc, T.decodeUtf8With)
    _ -> ("UTF-8", T.decodeUtf8With) -- FIXME: return other enc?

-- | Try to scrape the encoding.
getEncoding :: Scraper BS.ByteString (Maybe T.Text)
getEncoding = htmlMeta -- TODO: add other parsers, use Parsec instead

-- | Scrape the encoding from html <meta> tag.
htmlMeta :: Scraper BS.ByteString (Maybe T.Text)
htmlMeta = (join . fmap (either (const Nothing) Just . T.decodeUtf8'))
  <$> (fmap (join .
             fmap ((^? ix 1) . BS.split 0x3d) . -- get string behind "="
             (^? ix 0) . filter (BS.isPrefixOf "charset") . -- get unit starting with charset
             map (BS.filter (/=0x20)) . BS.split 0x3b) $ -- split into units separated by ";"
       attr "content" $ "meta" @: ["http-equiv" @= "Content-Type"])
