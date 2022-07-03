{-# LANGUAGE InstanceSigs #-}

module Init (initConfig,
            initSimulation) where

import Control.Monad.IO.Class
import Data.Maybe

import AppData

-- ## Data setup to populate Container records 

-- Intialise Vessel record
v = Vessel "Hamburg" "345765"

-- Intialise Organisation Records
shipperALDI = Organisation "ALDI" Customer [NoLocation] 
shipperKmart = Organisation "Kmart" Customer [] 
shipperColes = Organisation "Coles" Customer [] 
conALDI = Organisation "ALDI" Customer [] 
conKmart = Organisation "Kmart" Customer [] 
conColes  = Organisation "Coles" Customer [] 
geodis = Organisation "GEODIS" FreightForwarder [] 
chr = Organisation "CH Robinson" FreightForwarder [] 
cosco = Organisation "COSCO" ShippingLine [] 
maersk = Organisation "Maersk" ShippingLine [] 
hapag = Organisation "Hapag-Lloyd" ShippingLine [] 

-- Initialise Address records
a = Address 15 "Wharf Rd" "Sydney" "NSW" "Australia"

-- Initialise Geo Location records
x = (3,4)

-- Data Setup for Locations - create set of locations to be track containers and to setup delivery route
-- these are used to set the simulated route - simRoute in the Simulation state object.
shipperloc = Location "Shipper Pickup Address" PICKUP x a
pickupttransportloc = Location "Transport from Pickup" PICKUP_TRANSPORT x a
dischargeportloc = Location "Discharge Port" DISCHARGE_PORT x a
vesselloc = Location "Vessel" VESSEL x a
destportloc = Location "Destiantion Port" ARRIVAL_PORT x a
delivertransportloc  = Location "Transport to Delivery" DELIVERY_TRANSPORT x a
consigneeloc  = Location "Consignee Delviery Address" DELIVERY x a

-- Initialising of Containers for the demo.
c1 =  Container shipperALDI conALDI cosco geodis shipperloc dischargeportloc destportloc Full Import "TLG6774UU45" GEN Twenty "OBL094747" "Seal4747" 
c2 =  Container shipperKmart conKmart cosco geodis pickupttransportloc dischargeportloc destportloc Full Import "MOU12345645" REF Forty "OBL081391" "Seal1234" 
c3 =  Container shipperColes conColes cosco geodis shipperloc dischargeportloc destportloc Full Import "TLKG6718145" HAZ Forty "OBL023533" "Seal8976" 
c4 =  Container shipperALDI conALDI cosco geodis shipperloc dischargeportloc destportloc Full Import "COSU6756745" REFHAZ Twenty "OBL84997" "774747" 

-- Initialise the Config state. 
initConfig :: MonadIO m => m Config 
initConfig = do return $ Config { titlePoint = (1,1)

                                , headerPoint = (2,1)
                                , menuPoint = (3,1)
                                , footerPoint = (12,1)
                                , titleStr = "                                           CONTAINER SUPPLY CHAIN SIMULATION"
                                , headerLine = concat (replicate 133 "-"  )
                                , menuStr1 = concat (replicate 61 " "  ) ++ "ACTION KEYS:  (q) to quit" ++ concat (replicate 47 " "  )
                                , menuStr2 = "   (1) Shipper->Transport   (2) Transport->Port   (3) Load on Vessel   (4) Unload off Vessel   (5) Port->Transport   (6) Deliver     "
                                , footerStr =  concat (replicate 133 "-"  )
                              }

-- Initialise the Simulation state. 'test' property used for troubleshooting during build, to be removed
initSimulation :: MonadIO m => m Simulation
initSimulation = do return $ Simulation { conList  = c1:c2:c3:c4:[]
                                        , simRoute =   shipperloc 
                                                    : pickupttransportloc 
                                                    : dischargeportloc
                                                    : vesselloc
                                                    : destportloc 
                                                    : delivertransportloc
                                                    : consigneeloc : []
                                        }







-- ## Repl statement references  used to validate and test functions using 

{- 
-- setup container list2 for testing and was used in repl to unit test functions
locList2 =  [ ("Shipper    ", ["BNMV1234567"]) , 
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567","KJHG1234567"]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("", ["           "]) ,
             ("Vessel     ", ["BNMV1234567","KJHG1234567"]),
             ("", ["           "]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567"]) ,
             ("Consignee  ", ["BNMV1234567","KJHG1234567"])  
           ]

-}

{-
-- various expressions and statements used in reply while building to unit test functions, here as easy reference only 

type ContainerNo = String
c :: String
c = "ABCD1234567"

ctest :: String
ctest = "TGHU9876543"


clist :: [ContainerNo]
clist = c:c:c:c:[]
clist2 = "           ":[]

b1 = ["               "]
blankclist = concat (replicate 8 b1)


a = Address 15 "Wharf Rd" "Sydney" "NSW" "Australia"
x = (3,4)
loc1 = Location "AUSYD" DISCHARGE_PORT x a
loc2 = Location "Factory" PICKUP x a

containerlist = c1:c2:c3:c4:[]

vs = VesselSchedule v "096065" containerlist
first = cnumber $ head $ containers vs    



-- setup container list for testing and was used in repl to unit test functions
locList =  [ ("Shipper    ", ["BNMV1234567","KJHG1234567"]) , 
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567"]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("", ["           "]) ,
             ("Vessel     ", ["BNMV1234567","KJHG1234567"]),
             ("", ["           "]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567"]) ,
             ("Consignee  ", ["BNMV1234567","KJHG1234567"])  
           ]

-- setup container list2 for testing and was used in repl to unit test functions
locList2 =  [ ("Shipper    ", ["BNMV1234567"]) , 
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567","KJHG1234567"]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("", ["           "]) ,
             ("Vessel     ", ["BNMV1234567","KJHG1234567"]),
             ("", ["           "]) ,
             ("Port       ", ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"]) ,
             ("Transport  ", ["BNMV1234567","KJHG1234567","KJHG1234567"]) ,
             ("Consignee  ", ["BNMV1234567","KJHG1234567"])  
           ]


-}