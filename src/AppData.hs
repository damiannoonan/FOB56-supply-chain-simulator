{-# LANGUAGE InstanceSigs #-}

module AppData (
            Container(..),
            Address (..),
            LocationType(..),
            Organisation(..),
            OrganisationType(..),
            Location(..),
            Vessel(..),
            VesselSchedule(..),
            CargoType (..), 
            ContainerSize (..),
            JobType(..),
            ContainerStatus(..),
            ContainerRoute,
            ContainerNumber,
            ContainerList(..),
            Config(..),
            Simulation(..),
            Pos(..)
        

               )where

import Data.List 


-- Data type for Simulation state
-- ConList is set of containers being tracked through supply chain. These are identified to be on the same route via simRoute which is collection of locations
data Simulation  = Simulation { conList  :: ContainerList
                              , simRoute :: ContainerRoute
                              } deriving Show

-- Data type for Config state
-- Used store the header, menu at top of screen
-- the points were to be used to define on screen where to start writing each line but run out of time to implement
data Config = Config { titlePoint  :: Pos
                     , headerPoint :: Pos
                     , footerPoint :: Pos
                     , menuPoint   :: Pos
                     , titleStr    :: String
                     , headerLine  :: String
                     , footerStr   :: String
                     , menuStr1    :: String
                     , menuStr2    :: String } deriving Show

-- to have bee used for rendering positions on screen
type Pos = (Int, Int) 

-- Vessel data types
type VesselName = String
type VesselID   = String
type TokenID    = String
type VoyageNo   = String
data Vessel     = Vessel  { name   ::  VesselName
                      , vesselid :: VesselID
                      }  deriving Show

-- Vessel Schedule data types
-- The simulation could be extended to add containers to the Vessel Schedule when they are loaded onto vessel and unloaded form Vessel in destination port.
data VesselSchedule = VesselSchedule { vessel :: Vessel
                                     , voyageNo :: VoyageNo
                                     , containers  :: [Container]} deriving Show              

-- Location data types
type LocationName = String
type StreetNumber = Int
type Street       = String
type City         = String
type State        = String
type Country      = String
type Region       = String
type Lat          = Float
type Long         = Float
data LocationType = DISCHARGE_PORT | ARRIVAL_PORT | WHARF | VESSEL | TRANSPORT | PICKUP | DELIVERY | DELIVERY_TRANSPORT | PICKUP_TRANSPORT | WAREHOUSE | YARD | MTPARK | BUSINESS  deriving (Eq, Show)

-- Location and Address data types
type GeoLocation = (Lat, Long)

-- Address data types, used different constructors as you need to specify an argument for address when creating a location however it may come later,
-- so have the NoAddress constructor to handle that situation
data Address = NoAddress | Address { number  :: StreetNumber
                                   , street  :: Street
                                   , city    :: City
                                   , state   :: State 
                                   , country :: Country
                                   --   , region  :: Region
                                   } deriving (Eq,Show)

-- Lcoation data types, used different constructors as you need to specify an argument for Container, Organisation however it may come later,
-- so have the NoLocation constructor to handle that sitiation
data Location = NoLocation | Location { locname         :: LocationName
                                      , loctype         :: LocationType
                                      , geolocation     :: GeoLocation
                                      , locaddress      :: Address
                                      } deriving (Eq,Show)

-- Persona / Organisation Types
-- Caters for different organisation types in the supply chain process
type OrganisationName = String
data OrganisationType = ShippingLine | Customer | FreightForwarder  | Business | Govt | Logistics deriving (Eq, Show)

-- Have not accounted for parent or hierarchical organisation relationships
data Organisation = Organisation { org_name :: OrganisationName
                                 , org_type :: OrganisationType
                                 , org_loc  :: [Location]
                                 } deriving (Eq,Show)

-- Container Types
-- Data points for a Container
-- Extending - adding unique minted NFT for ContainerNo ++ OceanBL number to track uniquenes of a container journey ( vs just a container) 
--- adding relatled list of cargo items.  
-- Some notes and learnings - could not use '20' and '40' for ContainerSize product types, Haskell appears to not allow numbers, so had to go with words
type ContainerNumber = String  -- 4 Alpha characters + 7 numbers
type OceanBL         = String  -- Should be unique for this container for this particular journey
type SealNo          = String  
type IMOCode         = String  
type UNCode          = String  
type GrossWeight     = Float
type TareWeigth      = Float
data CargoType       = GEN | REF | HAZ | REFHAZ deriving (Eq, Show)
data ContainerSize   = Twenty | Forty deriving (Eq, Show)
data JobType         = Import | Export deriving (Eq, Show)
data ContainerStatus = Full | Empty deriving (Eq, Show)
type ContainerList   = [Container]
type ContainerRoute =  [Location]


-- Object Data Types - Container.  Cargo, Product not yet implemented
-- commented out fields only because did not have time to add data 
-- KEY Learnings - I orginally wrote a lot of recursive functions to determine if a 
-- location matched another by trying to check the record fields on object etc, then 
-- realised I could do away with that by adding 'deriving Eq' . Something so simple was so powerful
data Container = Container  { shipper      :: Organisation
                            , consignee    :: Organisation
                            , shippingline :: Organisation
                            , fforwarder   :: Organisation
                            , loc_current  :: Location
                            , loc_pickup   :: Location
                            , loc_delivery :: Location
                            , con_status   :: ContainerStatus
                            , jobtype      :: JobType
                            , cnumber      :: ContainerNumber
                            , cargotype    :: CargoType
                            , size         :: ContainerSize
                            , oceanBL      :: OceanBL
                            , sealno       :: SealNo
                          --  , imo_code     :: IMOCode
                          --  , un_code      :: UNCode
                          --  , gweight      :: GrossWeight
                          --  , tweight      :: TareWeigth
                           } deriving  (Eq,Show)



