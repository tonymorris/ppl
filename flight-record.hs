{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Data.String

data VideoType =
  YouTube
  | Vimeo
  deriving (Eq, Ord, Show)

data Video =
  Video
    String -- uri
    (Maybe String) -- name
    VideoType
  deriving (Eq, Ord, Show)

newtype Videos =
  Videos
    [Video]
  deriving (Eq, Ord, Show)
    
data ImageType =
  Png
  | Jpg
  deriving (Eq, Ord, Show)

data Image =
  Image
    String -- uri
    (Maybe String) -- name
    ImageType
  deriving (Eq, Ord, Show)

newtype Images =
  Images
    [Image]
  deriving (Eq, Ord, Show)
    
data VisualisationType =
  Doarama
  deriving (Eq, Ord, Show)

data Visualisation =
  Visualisation
    String -- uri
    (Maybe String) -- name
    VisualisationType
  deriving (Eq, Ord, Show)

newtype Visualisations =
  Visualisations
    [Visualisation]
  deriving (Eq, Ord, Show)
    
data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageLog ImageType
  deriving (Eq, Ord, Show)

data TrackLog =
  TrackLog
    String -- uri
    (Maybe String) -- name
    TrackLogType
  deriving (Eq, Ord, Show)

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

data DayNight =
  Day
  | Night
  | DayNight
  deriving (Eq, Ord, Show)

data Engine =
  Single
  | Multi
  deriving (Eq, Ord, Show)

data Aircraft =
  Aircraft
    String -- type
    String -- registration
    Engine
  deriving (Eq, Ord, Show)

data Hours =
  Hours
    Int -- full
    Int  -- partial
  deriving (Eq, Ord, Show)

data PoB =
  PoB Int
  deriving (Eq, Ord, Show)

single ::
  PoB
single =
  PoB 1

dual ::
  PoB
dual =
  PoB 2

data FlightPath =
  FlightPath
    String -- start
    [String] -- touch-downs
    String -- end
  deriving (Eq, Ord, Show)

newtype Name =
  Name
    String
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString =
    Name

newtype Sequence =
  Sequence
    String
  deriving (Eq, Ord, Show)

instance IsString Sequence where
  fromString =
    Sequence
newtype Date =
  Date
    String
  deriving (Eq, Ord, Show)

instance IsString Date where
  fromString =
    Date

newtype PiC =
  PiC
    String
  deriving (Eq, Ord, Show)

instance IsString PiC where
  fromString =
    PiC

{-

This data structure currently only handles Day VFR. More modifications are
likely to result in a complete flight log entry structure.

-}
data FlightLogEntry =
  FlightLogEntry
    Name
    Sequence
    Date 
    Aircraft
    Hours
    PoB
    FlightPath
    DayNight
    PiC
    TrackLogs
    Visualisations
    Images
    Videos
  deriving (Eq, Ord, Show)

newtype FlightLog =
  FlightLog
    [FlightLogEntry]
  deriving (Eq, Ord, Show)

class Markdown s where
  markdown ::
    s
    -> String

instance Markdown VideoType where
  markdown YouTube =
    "youtube"
  markdown Vimeo =
    "vimeo"

instance Markdown Video where
  markdown (Video uri name vtype) =
    let t = markdown vtype
        n = fromMaybe ("Video (" ++ t ++ ")") name
    in  concat
          [
            "**"
          , n
          , ":** ["
          , t
          , "]("
          , uri
          , ")"
          ]

instance Markdown Videos where
  markdown (Videos v) =
    case v of
      [] ->
        ""
      _ ->
        "* **Videos**\n" ++ (v >>= \w -> "  * " ++ markdown w ++ "\n")

instance Markdown ImageType where
  markdown Png =
    "png"
  markdown Jpg =
    "jpg"

instance Markdown Image where
  markdown (Image uri name itype) =
    let t = markdown itype
        n = fromMaybe ("Image (" ++ t ++ ")") name
    in  concat
          [
            "<a href=\""
          , uri
          , "\"><img src=\""
          , uri
          , "\" width=\"120\" alt=\""
          , n
          , "\"/></a>"
          ]

instance Markdown Images where
  markdown (Images i) =
    case i of
      [] ->
        []        
      _ ->
        "\n<div style=\"text-align: justify\">\n" ++ (i >>= \j -> markdown j ++ "\n") ++ "</div>"

instance Markdown VisualisationType where
  markdown Doarama =
    "doarama"

instance Markdown Visualisation where
  markdown (Visualisation uri name vtype) =
    let t = markdown vtype
        n = case name of
              Nothing -> ""
              Just n' -> "**" ++ n' ++ "**: "
    in  concat
          [
            n
          , "["
          , t
          , "]("
          , uri
          , ")"
          ]
       
instance Markdown Visualisations where
  markdown (Visualisations v) =
    case v of
      [] ->
        ""
      _ ->
        "* **Visualisations**\n" ++ (v >>= \w -> "  * " ++ markdown w ++ "\n")
    
instance Markdown TrackLogType where
  markdown Gpx =
    "gpx"
  markdown Kml =
    "kml"
  markdown Kmz =
    "kmz"
  markdown (ImageLog i) =
    markdown i

instance Markdown TrackLog where
  markdown (TrackLog uri name ttype) =
    let t = markdown ttype        
        n = fmap (\z -> "**" ++ z ++ "**") name
    in  case ttype of 
          ImageLog i ->
            concat
              [
                "  * "
              , case n of
                  Nothing ->
                    "*" ++ t ++ "*"
                  Just n' ->
                    n'
              , "\n\n    "
              , "<a href=\""
              , uri
              , "\"><img src=\""
              , uri
              , "\" width=\"360\" alt=\""
              , fromMaybe t name
              , "\"/></a>"
              ]
          _ ->
            concat
              [
                "  * "
              , case n of
                  Nothing ->
                    ""
                  Just n' ->
                    n' ++ ": "
              , "["
              , t
              , "]("
              , uri
              , ")"
              ]

instance Markdown TrackLogs where
  markdown (TrackLogs t) =
    case t of
      [] ->
        []
      _ ->
        "* **Track**\n" ++ (t >>= \u -> markdown u ++ "\n")
    
instance Markdown DayNight where
  markdown x =
    concat
      [
        "* Day/Night: **"
      , case x of
          Day -> "Day"
          Night -> "Night"
          DayNight -> "Day & Night"
      , "**\n"
      ]
    
instance Markdown Engine where
  markdown Single =
    "single"
  markdown Multi =
    "multi"

instance Markdown Aircraft where
  markdown (Aircraft t r e) =
    concat
      [
        "* Aircraft"
      , "\n  * Type: **"
      , t
      , "**\n  * Registration: **`"
      , r
      , "`**\n  * Engine: **`"
      , markdown e
      , "`**\n"
      ]

instance Markdown Hours where
  markdown (Hours t p) =
    "* Hours: **`" ++ show t ++ "." ++ show p ++ "`**\n"

instance Markdown PoB where  
  markdown (PoB n) =
    concat
      [
        "* PoB: **`"
      , case n of
          0 -> "unmanned"
          1 -> "single"
          2 -> "dual"
          _ -> show n
      , "`**\n"
      ]

instance Markdown FlightPath where
  markdown (FlightPath s x e) =
    concat
      [
        "* Path: **"
      , s
      , x >>= (" - " ++)
      , " - "
      , e
      , "**\n"
      ]

instance Markdown Name where
  markdown (Name s) =
    "### " ++ s ++ "\n"

instance Markdown Sequence where
  markdown (Sequence s) =
    "* Sequence: **" ++ s ++ "**\n"
    
instance Markdown Date where
  markdown (Date s) =
    "* Date: **`" ++ s ++ "`**\n"
    
instance Markdown PiC where
  markdown (PiC s) =
    "* Pilot in Command: **" ++ s ++ "**\n"
    
instance Markdown FlightLogEntry where
  markdown (FlightLogEntry name sequence date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos) =
    concat
      [
        markdown name
      , markdown date
      , markdown sequence
      , markdown aircraft
      , markdown hours
      , markdown pob
      , markdown flightpath
      , markdown daynight
      , markdown pic
      , markdown tracklogs
      , markdown videos
      , markdown visualisations
      , markdown images
      ]

instance Markdown FlightLog where
  markdown (FlightLog g) =
    intercalate "\n\n----\n\n" (fmap markdown g)

----

vhldo ::
  Aircraft
vhldo =
  Aircraft
    "2011 Cessna 162 Skycatcher"
    "VH-LDO"
    Single

vhafr ::
  Aircraft
vhafr =
  Aircraft
    "2000 Cessna 172S Skyhawk"
    "VH-AFR"
    Single

ybaf2ybaf ::
  FlightPath
ybaf2ybaf =
  FlightPath
    "YBAF"
    []
    "YBAF"

flightlog ::
  FlightLog
flightlog =
  FlightLog
    [
      FlightLogEntry
        "P1.1 Effects of Controls"
        "P1.1 Effects of Controls"
        "20151214"
        vhldo
        (
          Hours
            1
            2
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        (
          TrackLogs
            []
        )
        (
          Visualisations
            []
        )
        (
          Images
          []
        )
        (
          Videos
          []
        )
    , FlightLogEntry
        "P1.2 Straight & Level"
        "P1.2 Straight & Level"
        "20151218"
        vhldo
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151218-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151218-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/595690"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=13BVior4VmY"
              (Just "Head camera")
              YouTube
          ]
        )
    , FlightLogEntry
        "P1.3 Climbing & Descending"
        "P1.3 Climbing & Descending"
        "20151220"
        vhldo
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151220-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151220-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/596790"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=8tZ8kxsVz6E"
              (Just "Head camera")
              YouTube
          ]      
        )
    , FlightLogEntry
        "P1.4 Turning"
        "P1.4 Turning"
        "20160104"
        vhldo
        (
          Hours
            1
            3
        )
        dual
        ybaf2ybaf
        Day
        "Ryan Low"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160104-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160104-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/607004"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          []  
        )
    , FlightLogEntry
        "P1.5 Stalling"
        "P1.5 Stalling"
        "20160108"
        vhldo
        (
          Hours
            1
            3
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160108-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160108-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/611980"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          []                          
        )
    , FlightLogEntry
        "P1.6 Consolidation"
        "P1.6 Consolidation"
        "20160115"
        vhafr
        (
          Hours
            1
            4
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160115-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160115-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/617610"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          []                
        )
    , FlightLogEntry
        "P2.1 Circuits"
        "P2.1 Circuits"
        "20160122"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160122-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160122-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/622258"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
            Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
              Nothing
              Png
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
              Nothing
              Png 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
              Nothing
              Png 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
              Nothing
              Png                                     
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=gz8Ivcjas9o"
              (Just "Head camera")
              YouTube
          ]
        )     
    , FlightLogEntry
        "P2.2 Circuits"
        "P2.2 Circuits"
        "20160129"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160129-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160129-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/626694"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=k9c1GdxkdQY"
              (Just "Head camera")
              YouTube
          ]       
        )
    , FlightLogEntry
        "P2.3 Circuits"
        "P2.3 Circuits"
        "20160205"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160205-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160205-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/635027"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          []  
        )
    , FlightLogEntry
        "P2.4 Circuits"
        "P2.4 Circuits"
        "20160212"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160212-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160212-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/640001"
              Nothing
              Doarama
          ]
        )
        (
          Images
          []
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=e7UcjgxOmDw"
              (Just "Head camera")
              YouTube
          ]
        )                                                            
    , FlightLogEntry
        "P2.5 Circuits"
        "P2.5 Circuits"
        "20160218"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160218-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160218-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/644663"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
            Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090347.jpg"
              Nothing
              Png
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090348.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090351.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090409.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090411.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090421.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090423.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090432.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090436.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090441.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090446.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090452.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090454.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090459.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090501.jpg"
              Nothing
              Png            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090502.jpg"
              Nothing
              Png            
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=EO4D4zFnpIU"
              (Just "Head camera")
              YouTube
          ]  
        )            
    ]
