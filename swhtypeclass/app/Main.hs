{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

data Action = Flying Animal | Landing Animal | Standing Animal  

instance Show Animal where
    show Falcon = "Falcon"
    show Eagle = "Eagle"

instance Show Action where
    show (Flying s) = "Flying " <> show s
    show (Landing s) = "Landing " <> show s
    show (Standing s) = "Standing " <> show s

class Bird a where
    fly::a->Action
    land::a->Action
    stand::a->Action

data Animal =  Falcon | Eagle 

instance Bird Animal where
    fly = Flying
    land = Landing
    stand = Standing
 
--import Lib

main::IO()
main = do 
        print (fly Falcon)
        do
            print (land Falcon)
        do 
            print (stand Falcon )
