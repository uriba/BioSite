{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Yaml (decodeFile, Value, (.:), parseMaybe)
import qualified Data.HashMap.Strict as HM

siteName :: IO Text
siteName = do
    x <- decodeFile "config/biosite-settings.yml"
    let sitename = case x of
            Just (Object o) -> 
                case "sitename" `HM.lookup` o of
                    Just (String sn) -> 
                            sn
                    _ -> ""
            _ -> ""
    return sitename

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    handlerName <- liftIO $ siteName
    let submission = Nothing :: Maybe (FileInfo, Text)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
