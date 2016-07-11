--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
import           Data.Monoid (mappend)
import           Hakyll

host = "https://blog.majecty.com"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  {
    feedTitle = "하스켈과 게임개발에 관한 블로그 글들",
    feedDescription = "하스켈과 게임개발에 대해서 공부한 것, 경험한 것들을 정리했습니다.",
    feedAuthorName = "주형",
    feedAuthorEmail = "majecty+feed@gmail.com",
    feedRoot = "https://blog.majecty.com"
  }

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
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let customizedTag = customTag tag
        route idRoute
        compile $ do
            let order = case sortOrder customizedTag of
                            RecentFirst -> recentFirst
                            RecentLast -> chronological
            posts <- order =<< loadAll pattern
            let tagsCtx = context customizedTag `mappend`
                          constField "title" (title customizedTag) `mappend`
                          listField "posts" ctx (return posts) `mappend`
                          defaultContext
            makeItem ""
                >>= loadAndApplyTemplate (template customizedTag) tagsCtx
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

    create ["search.html"] $ do
        route idRoute
        compile $ do
            let searchCtx = constField "title" "Search" `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/search.html" searchCtx
                >>= loadAndApplyTemplate "templates/default.html" searchCtx
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

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
          let feedCtx = ctx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
          renderAtom feedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    constField "host" host `mappend`
    tagsField "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

data SortOrder = RecentFirst | RecentLast

data CustomTag = CustomTag {
      template :: Identifier,
      title :: String,
      context :: Context String,
      sortOrder :: SortOrder
  }

defaultTag tag = CustomTag "templates/tag.html" (tag ++ " 태그가 붙은 글들") mempty RecentFirst

customTag :: String -> CustomTag
customTag tag@"2016-06-07-foldr-presentation" = (defaultTag tag) {
        template = "customTags/2016-06-07-foldr-presentation.html",
        title = "foldr 발표를 위한 글들",
        context = constField "home" "haskell",
        sortOrder = RecentLast
    }
customTag "haskell" = (defaultTag "haskell") { title = "하스켈 글들", context = constField "home" "haskell" }
customTag tag = defaultTag tag
