{-# LANGUAGE InstanceSigs #-}

module Display (
               putTextCol,
               convertTextColsToRows,
               buildColumns,
               prepareTextColumns
               )
               where

import Data.Char
import Data.List
import Data.Maybe
import System.IO
import AppData
import Lib

-- Boy! this was fun getting my head around the display and how I wanted it to work. originally went down a rabbit hole
-- thinking about writing a ASCII character world map with each char a geo location..... too hard and not visually impactful
-- so landed on some way to show a container 'move' through the transition of different locaitons in a supply chain
-- The final solution was inspired by looking at the Calculator example (pg 192, Section 13.9) and section 11.4 Displaying a Grid 
-- with a simple tic tac toe screen.  
-- I separated the Header to the Columns. Kept the Header simple for now.  The way I approached thhe columns was to think about
-- it as a process of a data pipeline with each stage doing 1 thing simply. This allowed me to build and test iteratively.

-- The pipeline is summarised below:
-- putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns $ (prepContainerListforDisplay  (conList simulation)) )
--   1.  prepContainerListforDisplay (Lib.hs)  - takes a list of Containers, 
--                                             - categorise them by LocationType
--                                             - return a list of tuples, each tuple represents the col header text and list of Container Numbers
--                                             - blank columns are represented by tuple ("",["      "]). 
--                                             - hard coded structure for now due to time
--   2.  prepareTextColumns  - take tuple list and padd strings with border characters and spacing
--   3.  buildColumns        - takes the tuples and now creates a list of strings which represent the rows for that column
--   4.  convertTextColsToRows - takes the list of all the converts the list of row strings ( cols) and zips each row with its corresponding row in the other columns
--   5.  concat              - flattens the structure into an array of strings which represent each row
--   6.  putTextCol          - uses putStrLn and  'unlines' to render each list element as a line on the screen.         
-- 
--  If you want see how each stage works then run repl and load Display and run each of these commads
{-- ## Tests for REPL  locList is added to Display.hs at bottom so this should run and find locList
locList
prepareTextColumns locList
buildColumns $ prepareTextColumns locList
convertTextColsToRows $ buildColumns $ prepareTextColumns locList
concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList )
putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList )
-}


-- # characters to define column and row borders
-- character for vertical borders
rowBorder :: String
rowBorder = "|"

-- padding characters between between border
rowTextPadding :: String
rowTextPadding = " "

-- character for horizontal borders
colBorder :: String
colBorder = "-"

-- Setup column configurations
-- spacing between vertical border
rowTextWidth :: Int
rowTextWidth = 11

-- # row and column paramaters
-- border character width
rowBorderWidth = length(rowBorder)

-- padding between border and text
rowTextPaddingWidth = length(rowTextPadding )

rowTotalWidth :: Int
rowTotalWidth = (2 * rowBorderWidth)  + (2 * rowTextPaddingWidth) + rowTextWidth

rowTextBlank = concat ( replicate rowTextWidth rowTextPadding )
rowBlank     = concat ( replicate rowTotalWidth rowTextPadding )

-- text f
type ColHeaderText  =  String
type TextColumnRowList = [String]

-- height header of columns i.e the number  of rows
colHeaderHeight :: Int
colHeaderHeight = 2

-- height header of text area of columns i.e the number  of rows
colTextHeight :: Int
colTextHeight = 8

-- height header of columns i.e the number  of rows
colFooterHeight :: Int
colFooterHeight = 2

colHorizontalBorder :: String
colHorizontalBorder = concat(replicate rowTotalWidth  colBorder)

showTextRow :: String -> String
showTextRow cont = rowBorder ++ rowTextPadding ++ cont ++ rowTextPadding ++ rowBorder

showRowTextList :: [ContainerNumber] -> [String]
showRowTextList [] = []
showRowTextList (x:xs) = (showTextRow x) : showRowTextList xs

-- pads height of column with Blank text row if # of containers in location is less than the colTextHeight
padTextColumn ::  [ContainerNumber] -> Int -> [ContainerNumber] 
padTextColumn [] _ = []
padTextColumn xs height = if  ( length xs < height ) && ( length xs > 0 )  
                          then (replicate ( height - length xs)  rowTextBlank ) ++ xs
                          else take height xs

prepareTextColumns ::  [ (ColHeaderText,  [ContainerNumber])  ] -> [ (ColHeaderText,  [ContainerNumber])  ]
prepareTextColumns [] = []
prepareTextColumns ( (chtext, cs) : cols ) = (chtext, padTextColumn cs colTextHeight ) : (prepareTextColumns cols)

-- function takes text string to dislay as column header and list of text to display the 
-- text area of the column. In this case Container numbers
buildTextColumn :: (ColHeaderText, [ContainerNumber]) -> TextColumnRowList
buildTextColumn (headername, xs ) = if headername == "" then 
                                      rowBlank :  rowBlank : []  ++ (rowBlank:[]) ++ (replicate colTextHeight rowBlank)  ++ (rowBlank:[])
                                     else  colHorizontalBorder : ((showTextRow headername ) : [])  ++ (colHorizontalBorder:[]) ++ (showRowTextList xs )  ++ (colHorizontalBorder:[])


buildColumns :: [ (ColHeaderText,  [ContainerNumber])  ] -> [TextColumnRowList]
buildColumns [] = []  
buildColumns ((chtext, cs) : cols) =  (buildTextColumn (chtext, cs)) : buildColumns cols

convertTextColsToRows :: [TextColumnRowList] -> [[String]]
convertTextColsToRows [] = []
convertTextColsToRows [xs] = [xs]
convertTextColsToRows (x:y:xs) =    convertTextColsToRows ( (zipWith (++) x y ) : xs) 


putTextCol :: [String] -> IO ()
putTextCol c   = putStrLn  (unlines c)


-- setup container list for testing and was used in repl to unit test functions
locList =  [ ("Shipper    ", ["BNMV1234567","KJHG1234567"]) , 
             ("Transport  ", ["BNMV1234567"]) ,
             ("Port       ", ["BNMV1234567"]) ,
             ("", ["           "]) ,
             ("Vessel     ", ["BNMV1234567"]),
             ("", ["           "]) ,
             ("Port       ", ["BNMV1234567"]) ,
             ("Transport  ", ["BNMV1234567"]) ,
             ("Consignee  ", ["BNMV1234567"])  
           ]


-- # DATA for testing




{-
-- ## Tests for REPL
locList
prepareTextColumns locList
buildColumns $ prepareTextColumns locList
convertTextColsToRows $ buildColumns $ prepareTextColumns locList
concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList )
putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList )

-- putTextCol $ concat ( convertTextColsToRows $ buildColumns $ prepareTextColumns locList )
-- buildTextColumn ("Location", ["BNMV1234567","KJHG1234567"])  
-- buildColumns  [ ("Location2", ["BNMV1234567","KJHG1234567"]) , ("Location", ["BNMV1234567","KJHG1234567"])   ]
--padTextColumn ["BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567","BNMV1234567","KJHG1234567"] 5

--zipWith (++) (zipWith (++) c c2)

twocol = zipWith (++) collist collist2
--threecol = zipWith (++) twocol collist3
threecol = zipWith (++) twocol blankclist
fourcol  = zipWith (++) threecol collist4



-- createBlankColumn pass column width and character and create list of spaces
-- insertBlankColumn (list of )

collist  = colHorizontalBorder : ((showTextRow "  Shipper  "): []) ++ (colHorizontalBorder:[]) ++ (showRowTextList clist )  ++ (colHorizontalBorder:[])
collist2 = colHorizontalBorder : ((showTextRow "  Truck    "): []) ++ (colHorizontalBorder:[])++ (showRowTextList clist2 )  ++ (colHorizontalBorder:[])
collist3 = colHorizontalBorder : ((showTextRow "  Port     "): []) ++ (colHorizontalBorder:[])++ (showRowTextList clist2 )  ++ (colHorizontalBorder:[])
collist4 = colHorizontalBorder : ((showTextRow "  Vessel   "): []) ++ (colHorizontalBorder:[])++ (showRowTextList clist2 )  ++ (colHorizontalBorder:[])

-}