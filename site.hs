{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (fromJust)
import Data.Monoid ((<>),mconcat)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime,formatTime)
import Hakyll
import Network.URI (parseAbsoluteURI,URI(URI,uriScheme,uriPath))
import System.Locale (iso8601DateFormat,defaultTimeLocale)
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Text.Printf (printf)
import Text.Blaze.Html (toValue,toHtml,(!))

addDate :: Metadata -> Routes
addDate metadata = customRoute addDateToIdentifier
  where
    format = iso8601DateFormat Nothing
    date :: UTCTime
    date = fromJust $ do
      value <- Map.lookup "published" metadata
      parseTime defaultTimeLocale format value
    prefix = formatTime defaultTimeLocale "%Y/%m/%d/" date
    addDateToIdentifier identifier = prefix ++ toFilePath identifier

postRoute :: Routes
postRoute = gsubRoute "posts/" (const "") `composeRoutes`
            metadataRoute addDate `composeRoutes`
            setExtension "html"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

tagsCtx :: Tags -> Context String
tagsCtx = tagsFieldWith getTags renderLink (mconcat.concatLinks) "tags"
  where
    concatLinks [] = []
    concatLinks (x:xs) = x : concatTail xs
    concatTail [] = []
    concatTail (x:[]) = [" and ", x]
    concatTail (x:xs) = ", " : x : concatTail xs
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) =
      Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

archivePageCtx :: String -> [Item String] -> Context String
archivePageCtx title posts =
  constField "title" title <>
  listField "posts" postCtx (return posts) <>
  defaultContext

referenceEmacsLispSymbol :: String -> String -> String
referenceEmacsLispSymbol =
  printf "http://bruce-connor.github.io/emacs-online-documentation/%s%%2F%s"

transformSpecialSchemes :: URI -> Compiler (Maybe String)
transformSpecialSchemes URI{uriScheme=scheme,uriPath=path} =
  case scheme of
    "internal:" -> do
      target <- getRoute (fromFilePath path)
      return (fmap ("/"++) target)
    "el-variable:" -> return (Just (referenceEmacsLispSymbol "Var" path))
    "el-function:" -> return (Just (referenceEmacsLispSymbol "Fun" path))
    "el-face:" -> return (Just (referenceEmacsLispSymbol "Face" path))
    _ -> return Nothing

transformLinks :: Inline -> Compiler Inline
transformLinks link@(Link ref (url,title)) =
  case parseAbsoluteURI url of
    Nothing -> return link
    Just uri -> do
      transformed <- transformSpecialSchemes uri
      case transformed of
        Nothing -> return link
        Just newUrl -> return (Link ref (newUrl,title))
transformLinks x = return x

transformDocument :: Pandoc -> Compiler Pandoc
transformDocument = walkM transformLinks

transformingPandocCompiler :: Compiler (Item String)
transformingPandocCompiler =
  pandocCompilerWithTransformM defaultHakyllReaderOptions
  defaultHakyllWriterOptions transformDocument

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let context = archivePageCtx (printf "Posts tagged “%s”" tag) posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      cloud <- renderTagCloud 100 130 tags
      let context = constField "title" "Tags" <> defaultContext
      makeItem cloud
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "pages/*" $ do
    route   $ setExtension "html"
    compile $ transformingPandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route postRoute
    compile $ transformingPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags <> postCtx)
      >>= loadAndApplyTemplate "templates/default.html" postCtx

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let context = archivePageCtx "Archives" posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context

  match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Home"                <>
                  defaultContext
          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match "templates/*" $ compile templateCompiler
