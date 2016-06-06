--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

host = "https://blog.majecty.com"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let ctx = postCtx tags

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagsCtx = addTagContext tag `mappend`
                          constField "title" (tagTitle tag) `mappend`
                          listField "posts" ctx (return posts) `mappend`
                          defaultContext
            makeItem ""
                >>= loadAndApplyTemplate (tagTemplate tag) tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" ctx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" ctx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         pages <- loadAll "pages/*"
         let allPosts = (return (posts ++ pages))
         let sitemapCtx = mconcat
                          [ listField "entries" ctx allPosts
                          , defaultContext
                          ]
         makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "customTags/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    constField "host" host `mappend`
    tagsField "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tagTemplate :: String -> Identifier
tagTemplate "2016-06-07-foldr-presentation" = "customTags/2016-06-07-foldr-presentation.html"
tagTemplate _ = "templates/tag.html"

tagTitle :: String -> String
tagTitle "haskell" = "하스켈 글들"
tagTitle "2016-06-07-foldr-presentation" = "foldr 발표를 위해 준비한 글"
tagTitle tag = tag ++ " 태그가 붙은 글들"

addTagContext :: String -> Context String
addTagContext "haskell" = constField "home" "haskell"
addTagContext _ = mempty
