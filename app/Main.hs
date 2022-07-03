{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
--import System.Console.ANSI
import System.IO

import Lib
import AppData
import Init
import Display

-- clear screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- get char from screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- keyboard characters for user interaction
buttons :: String
buttons = actions ++ menu 
            where 
                actions ="123456"
                menu = "q"

--  Utility Function - needs t0 be moved to Lib when working, used in runSim to lift the character entred in getChar in IO Char context so can be used
io :: MonadIO m => IO a -> m a   
io = liftIO

-- renders top of screen currently passes config data type, but I think this needs to be changed so it gets Config from Reader instead of passed directly - updated when
renderHeader ::  (MonadIO m, MonadReader Config m, MonadState Simulation m) =>  m  ()
renderHeader  = do
    config <- ask
    io $ cls
    io $ putStrLn $ show $ (titleStr config)
    io $ putStrLn $ show $ (headerLine config)
    io $ putStrLn $ show $ (menuStr1 config)  
    io $ putStrLn $ show $ (menuStr2 config)  
    io $ putStrLn $ show $ (footerStr config)

-- renders the containers and the supply chain locations , passes the simulation data type, but this should be part of State?
renderSupplyChain :: (MonadIO m, MonadReader Config m, MonadState Simulation m) =>  m  ()
renderSupplyChain  = do
    simulation <- get
    io $  putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns $ (prepContainerListforDisplay  (conList simulation)) )



getNextContainer :: (MonadIO m, MonadReader Config m, MonadState Simulation m) => LocationType -> m ()
getNextContainer loctype = do
          simulation <- get
          if length ( getContainersByLocation loctype (conList simulation) )  >= 1
                             then updateContainerList $ head $ (getContainersByLocation loctype (conList simulation))
                             else return ()

-- not sure if need to create this full stack.
runSim :: ReaderT Config (StateT Simulation IO) ()
--runSim :: (StateT Simulation IO) ()
runSim =  do 
    config <- ask
    simulation <- get
    renderHeader
    renderSupplyChain
    k <- io $ getCh
    if elem k buttons 
        then 
          if k =='q' 
            then return () 
            else do 
               case k of
                 '1' -> getNextContainer PICKUP
                 '2' -> getNextContainer PICKUP_TRANSPORT
                 '3' -> getNextContainer DISCHARGE_PORT
                 '4' -> getNextContainer VESSEL
                 '5' -> getNextContainer ARRIVAL_PORT
                 '6' -> getNextContainer DELIVERY_TRANSPORT
               runSim
        else runSim


updateContainerList ::  (MonadIO m, MonadReader Config m, MonadState Simulation m)  => Container -> m ()
updateContainerList c = do 
                         simulation <- get 
                         put ( simulation { conList = actionUpdateContainerList c (conList simulation) (simRoute simulation)})

main ::  IO ()
main = do 
    config <- initConfig
    simulation <- initSimulation
    runStateT (runReaderT (runSim) config ) simulation
    return()
    
  
    





    
    {-  --- ########  old code and repl test statemnents -- To be removed

{-}
renderTest1 :: (MonadIO m, MonadReader Config m, MonadState Simulation m) => m ()
renderTest1 = do 
    config <- ask
    simulation <- get 
    renderHeader
    renderSupplyChain
-}


{-  
process :: (MonadIO m, MonadState Simulation m) => Char -> m ()
process ch | ch == '1' = putStrLn "pressed 1"
    -- putStrLn "pressed 1"
    -- (actionUpdateContainerList  (head $ (getContainersByLocation PICKUP (conList simState) ) )  (conList simState) (simRoute simState) )
               | ch == '2' = putStrLn "pressed 2"
               | ch == '3' = putStrLn "pressed 3"
               | ch == '4' = putStrLn "pressed 4"
               | ch == '5' = putStrLn "pressed 5"
               | ch == '6' = putStrLn "pressed 6"
         --     | otherwise return
-}


        --putStrLn $ (show (conList simulation) ) 
    --putStrLn $ show $ head $ (getContainersByLocation PICKUP (conList simulation) )
   -- putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns $ (prepContainerListforDisplay  (conList simulation)) )
    c <- getCh
    if c == '1' then do 
                          --actionMoveContainerNext ( head $ (getContainersByLocation PICKUP (conList simulation)) ) (simRoute simulation) 
                          -- need to update state here then it is reference in next few lines
                          renderHeader config
                          putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList2 )
         else    do
                          renderHeader config
                          renderSupplyChain simulation
                          --putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns $ (prepContainerListforDisplay  (conList simulation)) )
-}


{-  to be removed
    --putStrLn $ show $ (titlePoint config)

   --writeat (titlePoint config) (titleStr config)
   -- writeat (headerPoint config) (headerStr config)
    --writeat (menuPoint config) (menuStr config)
   --writeat (footerPoint config) (footerStr config)



--type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++"H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStrLn xs

-}
    
