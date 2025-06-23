{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions: Font
--
--  Author : Axel Simon
--
--  Created: 16 October 2005
--
--  Copyright (C) 1999-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Fonts. The selection of an appropriate font to render text becomes a
-- substantial task in the presence of Unicode where a single font does not
-- cover the whole range of possible characters. Pango provides several
-- concepts to find appropriate fonts and to query information about them:
--
-- * 'FontDescription': Font descriptions provide a way to query and state
--   requirements on
--   fonts. This data structure has several fields describing different
--   characteristics of a font. Each of these fields can be set of left
--   unspecified.
--
-- * 'FontMap' : A font map represents the set of fonts available for a
--   particular rendering system. In particular this map defines the
--   relation between font size and pixel size in terms of the output medium.
--
-- * 'FontFamily' : A font family represents a set of fonts that have
--   related faces, that is, their faces share a common design, but differ
--   in slant, weight, width and other aspects.
--
-- * 'FontFace': A face is a specific font where all characteristics are
--   fixed except for the size.
--
-- * 'Font': A font in the underlying rendering system.
--
-- * 'FontMetrics': Information about the font that will be used to render
--   a specific 'Context' or 'PangoItem'.
--
module Graphics.Rendering.Pango.Font (
  -- Functions to manage font descriptions.
  module Graphics.Rendering.Pango.Description,
  -- Font metrics.
  FontMap,
  FontMapClass,
  pangoFontMapListFamilies,
  pangoFontMapLoadFont,
#if PANGO_VERSION_CHECK(1,22,0)
  pangoFontMapCreateContext,
#endif
#if PANGO_VERSION_CHECK(1,46,0)
  pangoFontMapGetFamily,
#endif
  -- PangoFontFamily methods
  FontFamily,
  FontFamilyClass,
#if PANGO_VERSION_CHECK(1,4,0)
  pangoFontFamilyIsMonospace,
#endif
#if PANGO_VERSION_CHECK(1,44,0)
  pangoFontFamilyIsVariable,
#endif
#if PANGO_VERSION_CHECK(1,46,0)
  pangoFontFamilyGetFace,
#endif
  pangoFontFamilyListFaces,
  -- PangoFontFace methods
  FontFace,
  FontFaceClass,
#if PANGO_VERSION_CHECK(1,4,0)
  pangoFontFaceListSizes,
#endif
  pangoFontFaceDescribe,
#if PANGO_VERSION_CHECK(1,46,0)
  pangoFontFaceGetFamily,
#endif
  -- PangoFont methods
  Font,
  FontClass,
  pangoFontDescribe,
#if PANGO_VERSION_CHECK(1,10,0)
  pangoFontGetFontMap,
#endif
#if PANGO_VERSION_CHECK(1,14,0)
  pangoFontDescribeAbsolute,
#endif
  pangoFontGetMetrics,
#if PANGO_VERSION_CHECK(1,44,0)
  pangoFontGetHBFont,
  pangoFontGetFeatures,
  pangoFontHasChar,
#endif
#if PANGO_VERSION_CHECK(1,46,0)
  pangoFontGetFace,
#endif
#if PANGO_VERSION_CHECK(1,50,0)
  pangoFontGetLanguages,
#endif
  pangoGetFontMetrics,
  ) where

import Control.Monad    (liftM)
import qualified Data.Text as T (unpack)
import Data.Char (ord)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.Enums#} (FontMetrics(..))
import Graphics.Rendering.Pango.Description
import Graphics.Rendering.Pango.Structs
import Graphics.Rendering.Pango.HB

{# context lib="pango" prefix="pango" #}


-- | Ask for the different font families that a particular back-end supports.
--
-- * The 'FontMap' can be acquired by calling
--   'Graphics.Rendering.Pango.Cairo.cairoFontMapGetDefault'.
--
pangoFontMapListFamilies :: FontMap -> IO [FontFamily]
pangoFontMapListFamilies fm = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_map_list_families#} fm arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  ffsPtr <- peekArray (fromIntegral size)
            (castPtr arrPtr::Ptr (Ptr FontFamily)) -- c2hs is wrong here
  {#call unsafe g_free#} (castPtr arrPtr)
  mapM (makeNewGObject mkFontFamily . return . castPtr) ffsPtr

instance Show FontFamily where
  show ff = T.unpack . unsafePerformIO $ do
    strPtr <- {#call unsafe font_family_get_name#} ff
    peekUTFString strPtr

#if PANGO_VERSION_CHECK(1,4,0)
-- | Ask if the given family contains monospace fonts.
--
-- * A monospace font is a font designed for text display where the
--   characters form a regular grid. For Western languages this would
--   mean that the advance width of all characters are the same, but
--   this categorization also includes Asian fonts which include
--   double-width characters: characters that occupy two grid cells.
--
-- * The best way to find out the grid-cell size is to query the members
--   of the according 'FontMetrics' structure.
--
pangoFontFamilyIsMonospace :: FontFamily -> Bool
pangoFontFamilyIsMonospace ff = unsafePerformIO $
  liftM toBool $ {#call unsafe font_family_is_monospace#} ff
#endif

#if PANGO_VERSION_CHECK(1,44,0)
-- | Ask if the given family has variations.
--
-- * A variable font is a font which has axes that can be modified to produce
--   different faces.
--
-- Since: 1.44
pangoFontFamilyIsVariable :: FontFamily -> Bool
pangoFontFamilyIsVariable ff = unsafePerformIO $
  liftM toBool $ {#call unsafe font_family_is_variable#} ff
#endif

#if PANGO_VERSION_CHECK(1,46,0)
-- | Get font face by name in font family. Nothing will return the default
-- face e.g. "Regular" per fontconfig.
--
-- Since: 1.46
pangoFontFamilyGetFace :: FontFamily -> Maybe String -> IO (Maybe FontFace)
pangoFontFamilyGetFace ff mname = do
  let withName = case mname of
                     Nothing -> ($ nullPtr)
                     Just name -> withUTFString name
  ffPtr <- withName $ {#call unsafe font_family_get_face#} ff
  if ffPtr == nullPtr
    then return Nothing
    else liftM Just . makeNewGObject mkFontFace . return $ castPtr ffPtr
#endif

-- | Ask for the faces contained in a particular family.
--
-- * Asks for all font faces in the given family. The faces in a family
--   share a common design, but differ in slant, weight, width and other
--   aspects. For example, the font family "Sans" contains several fonts
--   such as Helvetica and Arial.
--
pangoFontFamilyListFaces :: FontFamily -> IO [FontFace]
pangoFontFamilyListFaces ff = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_family_list_faces#} ff arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  ffsPtr <- peekArray (fromIntegral size)
            (castPtr arrPtr::Ptr (Ptr FontFace)) -- c2hs is wrong here
  {#call unsafe g_free#} (castPtr arrPtr)
  mapM (makeNewGObject mkFontFace . return . castPtr) ffsPtr

instance Show FontFace where
  show ff = T.unpack . unsafePerformIO $ do
    strPtr <- {#call unsafe font_face_get_face_name#} ff
    peekUTFString strPtr

#if PANGO_VERSION_CHECK(1,4,0)
-- | Ask for available sizes of this font face.
--
-- * List the available sizes for a font. This is only applicable to bitmap
--   fonts since all other fonts can be scaled arbitrarily. For scalable
--   fonts, this function returns @Nothing@. The sizes returned are in
--   ascending order, their unit is points (1\/72 inch).
--
pangoFontFaceListSizes :: FontFace -> IO (Maybe [Double])
pangoFontFaceListSizes ff = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_face_list_sizes#} ff arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  if arrPtr==nullPtr then return Nothing else do
    sizes <- peekArray (fromIntegral size) arrPtr
    {#call unsafe g_free#} (castPtr arrPtr)
    return (Just (map intToPu sizes))
#endif

-- | Ask for a description of this face.
--
-- * Returns the family, style, variant, weight and stretch of a 'FontFace'.
--   The size field of the resulting font description will be unset.
--
pangoFontFaceDescribe :: FontFace -> IO FontDescription
pangoFontFaceDescribe ff = do
  fdPtr <- {#call unsafe font_face_describe#} ff
  makeNewFontDescription fdPtr

#if PANGO_VERSION_CHECK(1,46,0)
-- | Get the font family of font face.
--
-- Since: 1.46
pangoFontFaceGetFamily :: FontFace -> IO FontFamily
pangoFontFaceGetFamily ff = do
  fnmPtr <- {#call unsafe font_face_get_family#} ff
  makeNewGObject mkFontFamily . return $ castPtr fnmPtr
#endif

#if PANGO_VERSION_CHECK(1,22,0)
-- | Create a Pango context for font map.
--
-- Since: 1.22
pangoFontMapCreateContext :: FontMap -> IO PangoContext
pangoFontMapCreateContext fm = wrapNewGObject mkPangoContext $
  {#call unsafe font_map_create_context#} fm
#endif

#if PANGO_VERSION_CHECK(1,46,0)
-- | Get a font family by its name.
--
-- Since: 1.46
pangoFontMapGetFamily :: FontMap -> String -> IO FontFamily
pangoFontMapGetFamily fm name = withUTFString name $ \strPtr -> do
  fnmPtr <- {#call unsafe font_map_get_family#} fm strPtr
  makeNewGObject mkFontFamily . return $ castPtr fnmPtr
#endif

-- | Load a font.
--
-- Since: (n/a)
pangoFontMapLoadFont :: FontMap -> PangoContext -> FontDescription -> IO Font
pangoFontMapLoadFont fm ctx descr = do
  fPtr <- {#call unsafe font_map_load_font#} fm ctx descr
  makeNewGObject mkFont . return $ castPtr fPtr

-- | Describe font.
--
-- Since: (n/a)
pangoFontDescribe :: Font -> IO FontDescription
pangoFontDescribe fn = do
  fdPtr <- {#call unsafe font_describe#} fn
  makeNewFontDescription fdPtr

#if PANGO_VERSION_CHECK(1,14,0)
-- | Describe font using absolute (device) units.
--
-- Since: 1.14
pangoFontDescribeAbsolute :: Font -> IO FontDescription
pangoFontDescribeAbsolute fn = do
  fdPtr <- {#call unsafe font_describe_with_absolute_size#} fn
  makeNewFontDescription fdPtr
#endif

-- | Get font metrics.
--
-- Since: (n/a)
pangoFontGetMetrics :: Font -> Language -> IO FontMetrics
pangoFontGetMetrics fn lang = do
  mPtr <- {#call unsafe font_get_metrics#} fn lang
  pangoGetFontMetrics mPtr

pangoGetFontMetrics :: Ptr () -> IO FontMetrics
pangoGetFontMetrics mPtr = do
  ascent' <- {#call unsafe font_metrics_get_ascent#} mPtr
  descent' <- {#call unsafe font_metrics_get_descent#} mPtr
#if PANGO_VERSION_CHECK(1,44,0)
  height' <- {#call unsafe font_metrics_get_height#} mPtr
#endif
  approximate_char_width <-
      {#call unsafe font_metrics_get_approximate_char_width#} mPtr
  approximate_digit_width <-
      {#call unsafe font_metrics_get_approximate_digit_width#} mPtr
#if PANGO_VERSION_CHECK(1,6,0)
  underline_position <-
      {#call unsafe font_metrics_get_underline_position#} mPtr
  underline_thickness <-
      {#call unsafe font_metrics_get_underline_thickness#} mPtr
  strikethrough_position <-
      {#call unsafe font_metrics_get_strikethrough_position#} mPtr
  strikethrough_thickness <-
      {#call unsafe font_metrics_get_strikethrough_thickness#} mPtr
#endif
  return (FontMetrics
          (intToPu ascent')
          (intToPu descent')
#if PANGO_VERSION_CHECK(1,44,0)
          (intToPu height')
#endif
          (intToPu approximate_char_width)
          (intToPu approximate_digit_width)
#if PANGO_VERSION_CHECK(1,6,0)
          (intToPu underline_thickness)
          (intToPu underline_position)
          (intToPu strikethrough_thickness)
          (intToPu strikethrough_position)
#endif
         )

#if PANGO_VERSION_CHECK(1,44,0)
-- | Get Harfbuzz reference to the font.
--
-- Since: 1.44
pangoFontGetHBFont :: Font -> IO HBFont
pangoFontGetHBFont fn = do
  hbfPtr <- {#call unsafe pango_font_get_hb_font#} fn
  return $ HBFont $ castPtr hbfPtr

-- | Get available font features. (This usually does not work.)
--
-- Since: 1.44
pangoFontGetFeatures :: Font -> IO [FontFeature]
pangoFontGetFeatures fn = allocaArray len $ \featArrPtr -> alloca $ \sizePtr -> do
  poke sizePtr 0
  featArr <- peek featArrPtr
  {# call unsafe font_get_features#} fn featArr (fromIntegral len) sizePtr
  size <- peek sizePtr
  feats <- peekArray (fromIntegral size) (castPtr featArrPtr)
  mapM fromHBFeature feats
  where len = 64

-- | Ask if the given font contains a glyph for the character.
--
-- Since: 1.44
pangoFontHasChar :: Font -> Char -> IO Bool
pangoFontHasChar fn ch = liftM toBool $
  {#call unsafe font_has_char#} fn (fromIntegral (ord ch))
#endif

#if PANGO_VERSION_CHECK(1,46,0)
-- | Get the face of the font.
--
-- * Only nullable if the font as outlived its fontmap.
--
-- Since: 1.46
pangoFontGetFace :: Font -> IO (Maybe FontFace)
pangoFontGetFace fn = do
  ffsPtr <- {#call unsafe font_get_face#} fn
  if ffsPtr == nullPtr
    then return Nothing
    else liftM Just . makeNewGObject mkFontFace . return $ castPtr ffsPtr
#endif

#if PANGO_VERSION_CHECK(1,10,0)
-- | Get the FontMap of the font.
--
-- * Only nullable if the fontmap was finalized.
--
-- Since: 1.10
pangoFontGetFontMap :: Font -> IO (Maybe FontMap)
pangoFontGetFontMap fn = do
  fmPtr <- {#call unsafe font_get_font_map#} fn
  if fmPtr == nullPtr
    then return Nothing
    else liftM Just . makeNewGObject mkFontMap . return $ castPtr fmPtr
#endif

#if PANGO_VERSION_CHECK(1,50,0)
-- | Get languages of the font.
--
-- Since: 1.50
pangoFontGetLanguages :: Font -> IO [Language]
pangoFontGetLanguages fn = do
  langPtrPtr <- {# call unsafe font_get_languages#} fn
  langPtrs <- peekArray0 nullPtr (castPtr langPtrPtr)
  return $ map Language langPtrs
#endif
