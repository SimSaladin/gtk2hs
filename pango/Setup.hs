{-# LANGUAGE CPP, ViewPatterns #-}
-- Adjustments specific to this package,
-- all Gtk2Hs-specific boilerplate is kept in
-- gtk2hs-buildtools:Gtk2HsSetup
--
import Distribution.Simple ( defaultMainWithHooks, UserHooks(postConf),
                             PackageIdentifier(..), PackageName(..) )
import Gtk2HsSetup ( gtk2hsUserHooks, getPkgConfigPackages)
import Distribution.Simple.Setup ( ConfigFlags(configVerbosity), fromFlag)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Text ( display )
import Distribution.Version ( Version(..) )
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (die)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path ( getSymbolicPath )
#endif
import System.FilePath
import System.Exit (die)

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version ( versionNumbers )
import Distribution.Types.PackageName ( unPackageName )
#endif

main =
  defaultMainWithHooks gtk2hsUserHooks {

    postConf = \args cf pd lbi -> do
      let verb = (fromFlag (configVerbosity cf))
      cPkgs <- getPkgConfigPackages verb lbi pd
      let [pangoVersion] = [ v | PackageIdentifier (unPackageName -> "pango") v <- cPkgs ]
      writePangoVersionHeaderFile verb lbi pangoVersion
      postConf gtk2hsUserHooks args cf pd lbi
  }

------------------------------------------------------------------------------
-- Generate CPP defines for version of C libs.
------------------------------------------------------------------------------

writePangoVersionHeaderFile :: Verbosity -> LocalBuildInfo -> Version -> IO ()
#if MIN_VERSION_Cabal(2,0,0)
writePangoVersionHeaderFile verbosity lbi (versionNumbers -> (major:minor:micro:_)) = do
#else
writePangoVersionHeaderFile verbosity lbi (Version (major:minor:micro:_) []) = do
#endif
  createDirectoryIfMissingVerbose verbosity True targetDir
  rewriteFileEx verbosity targetFile $ unlines
    [ "#define PANGO_VERSION_MAJOR " ++ show major
    , "#define PANGO_VERSION_MINOR " ++ show minor
    , "#define PANGO_VERSION_MICRO " ++ show micro
    ]
  where
#if MIN_VERSION_Cabal(3,14,0)
    targetDir  = getSymbolicPath . autogenPackageModulesDir $ lbi
#else
    targetDir  = autogenPackageModulesDir lbi
#endif
    targetFile = targetDir </> "hspangoversion.h"

writeVersionHeaderFile _ _ version =
  die $ "unexpected pango version number: " ++ display version
