{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Encoding where

-- | The correctness of SQL statements and the presence of tables and
-- columns can't be asserted by GHC. So unit tests are needed.

import Test.Framework

import Text.HTML.Scalpel
import qualified Data.ByteString as BS
-- import qualified Data.Text as T

import Network.Scrapetition.Encoding

latin1Meta :: BS.ByteString
latin1Meta = "<html>\n<head><title>Prefettura della Casa Ponitificia</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\"><style><!--"

test_htmlMetaLatin1Meta = do
  assertEqual (Just $ Just "iso-8859-1") (scrapeStringLike latin1Meta getEncoding)

test_htmlMetaNothing = do
  assertEqual Nothing (scrapeStringLike "asdf" getEncoding)
