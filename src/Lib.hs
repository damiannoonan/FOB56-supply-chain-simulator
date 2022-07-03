{-# LANGUAGE InstanceSigs #-}

module Lib
    ( getContainerNo, 
      getCurrentLocation, 
      getLocationName, 
      getLocationType, 
      getNextRouteLocation,
      getContainersByLocation,
      actionMoveContainerNext,
      actionUpdateContainerList,
      prepContainerListforDisplay
    ) where



import AppData
import Data.List

--titleFunc :: IO ()
--titleFunc = putStrLn "Damian's Projectv0.3"

-- #  Functions #
getContainerNo :: Container -> ContainerNumber
getContainerNo ( Container { cnumber = cn }) = cn

getCurrentLocation :: Container -> Location
getCurrentLocation ( Container { loc_current = x }) = x

getLocationName :: Location -> String
getLocationName ( Location {locname = lname }) = lname

getLocationType :: Location -> LocationType
getLocationType ( Location {loctype = ltype }) = ltype

getContainerNumbersbyLocationType :: LocationType -> [Container] -> [ContainerNumber]
getContainerNumbersbyLocationType _ []       = []
getContainerNumbersbyLocationType ltp (c:cs) = if ltp == (getLocationType $ getCurrentLocation c) 
                                                then (getContainerNo c) : getContainerNumbersbyLocationType ltp cs 
                                                else getContainerNumbersbyLocationType ltp cs 

getContainersByLocation :: LocationType -> [Container] -> [Container]
getContainersByLocation _ []       = []
getContainersByLocation ltp (c:cs) = if ltp == (getLocationType $ getCurrentLocation c) 
                                                then  c : getContainersByLocation ltp cs 
                                                else getContainersByLocation ltp cs 

getNextRouteLocation :: Container -> ContainerRoute ->  Location
getNextRouteLocation c [] = getCurrentLocation c
getNextRouteLocation  c (r:r1:rs) = if getCurrentLocation c == r then r1 else getNextRouteLocation c (r1:rs) 

actionMoveContainerNext :: Container -> [Location] -> Container
actionMoveContainerNext c croutes = c { loc_current = (getNextRouteLocation c croutes) } 

actionUpdateContainerList :: Container -> [Container] -> [Location] -> [Container]
actionUpdateContainerList _ [] _        = []
actionUpdateContainerList _ ccs []      = ccs
actionUpdateContainerList c (c1:cs) rts  = if c == c1 
                                             then (actionMoveContainerNext c1 rts) : ( actionUpdateContainerList c cs rts) 
                                             else c1 : actionUpdateContainerList c cs rts


prepContainerListforDisplay :: [Container] -> [(String,[ContainerNumber])]
prepContainerListforDisplay []    = []
prepContainerListforDisplay cs =  [ ("Shipper    ", if getContainerNumbersbyLocationType PICKUP             cs == [] then ["           "] else getContainerNumbersbyLocationType PICKUP             cs  ) ,
                                    ("Transport  ", if getContainerNumbersbyLocationType PICKUP_TRANSPORT   cs == [] then ["           "] else getContainerNumbersbyLocationType PICKUP_TRANSPORT   cs  ) ,
                                    ("Port       ", if getContainerNumbersbyLocationType DISCHARGE_PORT     cs == [] then ["           "] else getContainerNumbersbyLocationType DISCHARGE_PORT     cs  ) ,
                                    (""           , ["           "]) ,
                                    ("Vessel     ", if getContainerNumbersbyLocationType VESSEL             cs == [] then ["           "] else getContainerNumbersbyLocationType VESSEL             cs  ) ,
                                    (""           , ["           "]) ,
                                    ("Port       ", if getContainerNumbersbyLocationType ARRIVAL_PORT       cs == [] then ["           "] else getContainerNumbersbyLocationType ARRIVAL_PORT       cs  ) ,
                                    ("Transport  ", if getContainerNumbersbyLocationType DELIVERY_TRANSPORT cs == [] then ["           "] else getContainerNumbersbyLocationType DELIVERY_TRANSPORT cs  ) ,
                                    ("Consignee  ", if getContainerNumbersbyLocationType DELIVERY           cs == [] then ["           "] else getContainerNumbersbyLocationType DELIVERY           cs  )  
                                  ]


{- 
prepContainerListforDisplay cs =  [ ("Shipper    ",  ["BNMV1234567","KJHG1234567"] ) , 
                                    ("Transport  ",  getContainerNumbersbyLocationType PICKUP_TRANSPORT cs ) , 
                                    ("Port       ",  getContainerNumbersbyLocationType DISCHARGE_PORT cs ) , 
                                    ("", ["           "]) ,
                                    ("Vessel     ",  getContainerNumbersbyLocationType VESSEL cs ) , 
                                    ("", ["           "]) ,
                                    ("Port       ",  getContainerNumbersbyLocationType ARRIVAL_PORT cs ) , 
                                    ("Transport  ",  getContainerNumbersbyLocationType DELIVERY_TRANSPORT cs ) , 
                                    ("Consignee  ",  getContainerNumbersbyLocationType DELIVERY cs ) 
                                  ]

                                  -}