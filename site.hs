--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
import           Data.Monoid (mappend)
import           Hakyll
import          Data.Time.Format (TimeLocale(..))

hostName = "https://blog.majecty.com"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  {
    feedTitle = "하스켈과 게임개발, 블록체인에 관한 블로그 글들",
    feedDescription = "하스켈과 게임개발, 블록체인에 대해서 공부한 것, 경험한 것들을 정리했습니다.",
    feedAuthorName = "주형",
    feedAuthorEmail = "majecty+feed@gmail.com",
    feedRoot = "https://blog.majecty.com"
  }

allPostsPat = "posts/*" .&&. hasNoVersion

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let ctx = postCtx tags

    wikiTags <- buildTags "wikis/*" (fromCapture "wikiTags/*.html")
    let wikiCtx = postCtx wikiTags

    match "images/**" $ do
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

    match allPostsPat $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "wikis/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" wikiCtx
            >>= loadAndApplyTemplate "templates/default.html" wikiCtx
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
                >>= loadAndApplyTemplate (customTagTemplate customizedTag) tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls

    tagsRules wikiTags $ \tag pattern -> do
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
                >>= loadAndApplyTemplate (customTagTemplate customizedTag) tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll allPostsPat
            let archiveCtx =
                    listField "posts" ctx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["wikis.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "wikis/*"
            let archiveCtx =
                    listField "posts" wikiCtx (return posts) `mappend`
                    constField "title" "주제별 문서들"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/wikis.html" archiveCtx
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
            posts <- recentFirst =<< loadAll allPostsPat
            let indexCtx =
                    listField "posts" ctx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    constField "isHome" "true"            `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll allPostsPat
         pages <- loadAll "pages/*"
         wikis <- loadAll "wikis/*"
         let allPosts = (return (posts ++ pages ++ wikis))
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
            loadAllSnapshots allPostsPat "content"
          renderAtom feedConfiguration feedCtx posts

    version "redirects" $ createRedirects [
        ("posts/2021-07-17-b-undrstanding-gradle-2.html", "2021-07-17-b-understanding-gradle-2.html")
      ]

koreanTimeLocale :: TimeLocale
koreanTimeLocale = TimeLocale {
    wDays = [ ("일", "일요일")
            , ("월", "월요일")
            , ("화", "화요일")
            , ("수", "수요일")
            , ("목", "목요일")
            , ("금", "금요일")
            , ("토", "토요일")
            ]
  , months = [ ("1월", "1"), ("2월", "2"), ("3월", "3"),
                ("4월", "4"), ("5월", "5"), ("6월", "6"),
                ("7월", "7"), ("8월", "8"), ("9월", "9"),
                ("10월", "10"), ("11월", "11"), ("12월", "12")
             ]
  , amPm = ("오전", "오후")
  , dateTimeFmt = "%a %b %e %H:%M:%S %Y"
  , dateFmt = "%Y-%m-%d"
  , timeFmt = "%H:%M:%S"
  , time12Fmt = "%I:%M:%S %p"
  , knownTimeZones = []
  }

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    constField "host" hostName `mappend`
    tagsField "tags" tags `mappend`
    dateFieldWith koreanTimeLocale "date" "%Y년 %B %e일" `mappend`
    defaultContext

data SortOrder = RecentFirst | RecentLast

data CustomTag = CustomTag {
      customTagTemplate :: Identifier,
      title :: String,
      context :: Context String,
      sortOrder :: SortOrder
  }

customTag :: String -> CustomTag
customTag tag@"2016-06-07-foldr-presentation" = (defaultTag tag) {
        customTagTemplate = "customTags/2016-06-07-foldr-presentation.html",
        title = "foldr 발표를 위한 글들",
        context = constField "home" "haskell",
        sortOrder = RecentLast
    }
customTag "haskell" = (defaultTag "haskell") { title = "하스켈 글들", context = constField "home" "haskell" }
customTag tag = defaultTag tag

defaultTag tag = CustomTag "templates/tag.html" (tag ++ "에 대해 작성한 글들") mempty RecentFirst

