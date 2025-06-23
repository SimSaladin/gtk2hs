{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- -*-haskell-*-

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Define a few useful bindings to harfbuzz symbols.
--
module Graphics.Rendering.Pango.HB (
  HBTag(..), HBTagPtr,
  HBFont(..),
  HBFeature(..),
  FontFeature(..),
  fromHBFeature,
  hbFeatureGetTags,
  hbFeatureGetTagsGSUB,
  hbFeatureGetTagsGPOS,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.Rendering.Pango.Structs

newtype HBTag = HBTag {#type hb_tag_t#} deriving (Eq, Ord, Storable)
{#pointer *hb_tag_t as HBTagPtr -> HBTag #}

instance Show HBTag where
  show (HBTag tag) = unsafePerformIO $ do
    allocaBytes 4 $ \buf -> do
      {#call unsafe hb_tag_to_string#} tag buf
      if buf == nullPtr then return "" else peekCStringLen (buf, 4)

{#pointer *hb_font_t as HBFont newtype #}

{#pointer *hb_face_t as HBFace newtype #}

{#pointer *hb_feature_t as HBFeature foreign newtype #} deriving (Eq)

fromHBFeature :: HBFeature -> IO FontFeature
fromHBFeature hbf = return $ FontFeature (show hbf) 0

instance Show HBFeature where
  show ft = unsafePerformIO $ do
    let bufLen = 128
    allocaArray bufLen $ \strPtr -> do
      {#call unsafe hb_feature_to_string#} ft strPtr (fromIntegral bufLen)
      if strPtr == nullPtr then return "" else peekUTFString strPtr

instance Storable HBFeature where
  sizeOf _ = {#sizeof hb_feature_t#}
  alignment _ = alignment (undefined :: CInt)
  peek p = liftM HBFeature $ newForeignPtr_ p
  poke p (HBFeature fptr) = withForeignPtr fptr $ \ptr -> poke (castPtr p) ptr

-- FontFeature

data FontFeature = FontFeature { fontFeatureTag :: String, fontFeatureValue :: Int } deriving (Eq, Ord)
instance Show FontFeature where
  show (FontFeature nm val) = nm ++ "=" ++ show val

-- | E.g. ss04=1 or ss04 (=1 is implied)
--fontFeatureFromString :: String -> FontFeature
--fontFeatureFromString str = let
--  (tag, str') = span (/= '=') str
--  val = case str' of { [] -> 1; _:s -> read s }
--  in FontFeature (map toLower tag) val

-- | font features separated by commas
--parseFontFeatures :: String -> [FontFeature]
--parseFontFeatures [] = []
--parseFontFeatures str = let
--  (str, rest) = span (/= ',') str
--  in fontFeatureFromString str : maybe [] (parseFontFeatures . snd) (L.uncons rest)

hbFeatureGetTagsGSUB, hbFeatureGetTagsGPOS :: HBFont -> IO [HBTag]
hbFeatureGetTagsGSUB fn = hbFeatureGetTags fn HBOTTagGSUB
hbFeatureGetTagsGPOS fn = hbFeatureGetTags fn HBOTTagGPOS

-- | Get font GSUB feature tags.
hbFeatureGetTags :: HBFont -> HBOTTag -> IO [HBTag]
hbFeatureGetTags hbFn tableTag = do
  --hbFn <- pangoFontGetHBFont fn
  hbFace <- liftM HBFace $ {#call unsafe hb_font_get_face#} hbFn
  count <- {#call unsafe hb_ot_layout_table_get_feature_tags#}
    hbFace tableTag' 0 nullPtr nullPtr
  countPtr <- new count
  allocaBytes (fromIntegral count * {#sizeof hb_tag_t#}) $ \arrPtr -> do -- hb_tag_t *
    count' <- {#call unsafe hb_ot_layout_table_get_feature_tags#}
      hbFace tableTag' 0 countPtr (arrPtr)
    peekArray (fromIntegral count') arrPtr :: IO [HBTag]
  where
    tableTag' = fromIntegral (fromEnum tableTag)
