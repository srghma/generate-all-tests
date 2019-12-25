{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Main where

-- TODO: use http://hackage.haskell.org/package/managed instead of turtle
-- TODO: dont use system-filepath (depreced, though should not), dont use filepath, use https://hackage.haskell.org/package/path-io-1.6.0/docs/Path-IO.html waliDirAccumRel

import "protolude" Protolude hiding (find)
import qualified "turtle" Turtle
import "turtle" Turtle ((</>))
import qualified "directory" System.Directory
import qualified "filepath" System.FilePath
-- import qualified Filesystem.Path.CurrentOS
import "base" Data.String
import "base" Data.List
import "text" Data.Text
import qualified "foldl" Control.Foldl
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases

data SpecTree
  = Describe Text [SpecTree]
  | It Text
  deriving (Show)

anyCaseToCamelCase :: Text -> Text
anyCaseToCamelCase = Cases.process Cases.title Cases.camel -- first letter is always upper

dirTreeToSpecTree :: DirTree a -> IO SpecTree
dirTreeToSpecTree (Failed name err) = Turtle.die $ "Dir tree error: filename " <> show name <> ", error " <> show err
dirTreeToSpecTree (Dir name contents) = do
  output :: [SpecTree] <- traverse dirTreeToSpecTree contents
  let name' = anyCaseToCamelCase . toS $ name
  pure $ Describe name' output
dirTreeToSpecTree (File name _file) =
  let name' = anyCaseToCamelCase . toS . System.FilePath.takeBaseName $ name
  in pure $ It name'

filterDirTreeByFilename :: (String -> Bool) -> DirTree a -> Bool
filterDirTreeByFilename _ (Dir ('.':_) _) = False
filterDirTreeByFilename pred (File n _) = pred n
filterDirTreeByFilename _ _ = True

type SpecName = [Text] -- e.g. [ FeatureTests, Register, SuccessSpec ]

{-
  Example

  > Describe "Registration" [It "Test1", It "Test2"]
  [ ["registration", "test1"]
  , ["registration", "test2"]
  ]
-}
specTreeToList :: SpecTree -> [SpecName]
specTreeToList (It name) = [[name]]
specTreeToList (Describe name tree) =
  let output :: [SpecName] = join $ fmap specTreeToList tree
   in fmap (\(specName :: SpecName) -> name:specName ) output

{-
  Example

  > Describe "Registration" [It "Test1", It "Test2"]

  """
  describe "registration" do
    it "test1" FeatureTests.Test1.spec
    it "test2" FeatureTests.Test2.spec
  """

  > Describe "registration" []

  """
  describe "registration" do
    pure unit
  """
-}

specTreeToSpecsWrappedInDecribesAndIt :: SpecTree -> Text
specTreeToSpecsWrappedInDecribesAndIt specTree = Data.Text.unlines $ go [] specTree
  where
    appendTab :: Text -> Text
    appendTab = Data.Text.append "  "

    go :: [Text] -> SpecTree -> [Text]
    go pathAccum (It name) =
      let pathAccum' :: [Text]  = pathAccum ++ [name]
          moduleNames :: [Text] = fmap (Cases.process Cases.title Cases.camel) pathAccum'
          name' :: Text         = Cases.process Cases.lower Cases.whitespace $ fromMaybe name $ Data.Text.stripSuffix "Spec" name
       in pure $ appendTab $ "it \"" <> name' <> "\" " <> Data.Text.intercalate "." moduleNames <> ".spec"
    go pathAccum (Describe name tree) =
      let pathAccum' :: [Text] = pathAccum ++ [name]
          output :: [Text]     = fmap appendTab $ go pathAccum' =<< tree
          name' :: Text        = Cases.process Cases.lower Cases.whitespace name
          describe :: Text     = appendTab $ "describe \"" <> name' <> "\" do"
       in describe:output

main :: IO ()
main = Turtle.sh $ do
  projectRoot :: Turtle.FilePath <- Turtle.pwd

  let testsDir :: Turtle.FilePath = projectRoot </> "src/FeatureTests/"

  _base :/ (dirTree :: DirTree ()) <- liftIO $ System.Directory.Tree.readDirectoryWith (const $ pure ()) (Turtle.encodeString testsDir)

  let (dirTreeWithOnlyPurescriptFiles :: DirTree ()) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename
            (\n ->
              System.FilePath.takeExtension n == ".purs" &&
              "-spec" `Data.List.isSuffixOf` System.FilePath.takeBaseName n
            )
          )
          dirTree

  specTree :: SpecTree <- liftIO $ dirTreeToSpecTree dirTreeWithOnlyPurescriptFiles
  let specNameList :: [SpecName] = specTreeToList specTree

  liftIO $ print specTree
  liftIO $ print specNameList

  let imports = Data.Text.unlines $ specNameList <&> (
        \(specName :: SpecName) ->
          let specPath = Data.Text.intercalate "." specName
          in "import " <> specPath <> " as " <> specPath
        )

  let specsWrappedInDecribesAndIt :: Text = specTreeToSpecsWrappedInDecribesAndIt specTree

  let fileContent :: Text = Data.Text.unlines
        [ "module Test.AllTests where"
        , ""
        , "import Prelude"
        , ""
        , "import Test.Spec (describe)"
        , ""
        , "import Lib.FeatureTest (FeatureTestSpecInternal, it)"
        , ""
        , imports
        , ""
        , "allTests :: FeatureTestSpecInternal Unit"
        , "allTests = do"
        , specsWrappedInDecribesAndIt
        ]

  -- liftIO $ putStrLn fileContent
  liftIO $ Turtle.writeTextFile (projectRoot </> "test/" </> "AllTests.purs") fileContent

  return ()
