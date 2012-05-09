{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.ExperimentUpload where

-- Pages used for uploading initial plate-reader data to the server.
-- There are two parts for such an upload:
-- 1. Create a new experiment in the experiment table.
-- 2. Upload the read data, either as the xml output file of the reader (one for each measurement) or as a csv file containing all the measurements combined.

import Import
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

getExpUploadR :: Handler RepHtml
getExpUploadR = do
    (formWidget, formEnctype) <- generateFormPost expUploadForm
    let submission = Nothing :: Maybe ExpXmlFile
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Experiment XML upload"
        $(widgetFile "experimentUpload")

data ExpXmlFile = ExpXmlFile { exfId :: Text, exfPlate :: Int, exfFile :: FileInfo } deriving Show

postExpUploadR :: Handler RepHtml
postExpUploadR = do
    ((result, formWidget), formEnctype) <- runFormPost expUploadForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing :: Maybe ExpXmlFile
    liftIO $ T.putStrLn $ fromMaybe "no id" . fmap exfId $ submission
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Experiment XML upload processed"
        $(widgetFile "experimentUpload")

expUploadForm :: Form ExpXmlFile
expUploadForm = renderDivs $ ExpXmlFile
    <$> areq textField "Experiment id" Nothing
    <*> areq intField "Plate" Nothing
    <*> fileAFormReq "Xml file"

getCreateExpR :: Handler RepHtml
getCreateExpR = do
    (formWidget, formEnctype) <- generateFormPost createExpForm
    let submission = Nothing :: Maybe Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Experiment XML upload"
        $(widgetFile "createExperiment")

postCreateExpR :: Handler RepHtml
postCreateExpR = do
    ((result, formWidget), formEnctype) <- runFormPost createExpForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing :: Maybe Text
    liftIO $ T.putStrLn $ fromMaybe "no id" submission
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Experiment XML upload processed"
        $(widgetFile "createExperiment")

createExpForm :: Form Text
createExpForm = renderDivs $  id <$> areq textField "New experiment id" Nothing
