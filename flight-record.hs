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

data VisualisationType =
  Doarama
  deriving (Eq, Ord, Show)

data Visualisation =
  Visualisation
    String -- uri
    (Maybe String) -- name
    VisualisationType
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

data FlightLogEntry =
  FlightLogEntry
    String -- name
    String -- sequence
    String -- date
    Aircraft
    Hours
    PoB
    FlightPath
    DayNight
    String -- Pilot in Command
    [TrackLog]
    [Visualisation]
    [Image]
    [Video]
  deriving (Eq, Ord, Show)

newtype FlightLog =
  FlightLog
    [FlightLogEntry]
  deriving (Eq, Ord, Show)

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
    "2000 Cessna 172 Skyhawk"
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
        []
        []
        []
        []
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
        [
          Visualisation
            "http://doarama.com/view/595690"
            Nothing
            Doarama
        ]
        []
        [
          Video
            "https://www.youtube.com/watch?v=13BVior4VmY"
            (Just "Head camera")
            YouTube
        ]
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
        [
          Visualisation
            "http://doarama.com/view/596790"
            Nothing
            Doarama
        ]
        []
        [
          Video
            "https://www.youtube.com/watch?v=8tZ8kxsVz6E"
            (Just "Head camera")
            YouTube
        ]        
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
        [
          Visualisation
            "http://doarama.com/view/607004"
            Nothing
            Doarama
        ]
        []
        []      
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
        [
          Visualisation
            "http://doarama.com/view/611980"
            Nothing
            Doarama
        ]
        []
        []                          
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160115-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160115-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/617610"
            Nothing
            Doarama
        ]
        []
        []                
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160122-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160122-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/622258"
            Nothing
            Doarama
        ]
        [
          Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_082411.jpg"
            Nothing
            Png
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_082856.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_082902.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_082905.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_083348.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_083352.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_083353.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_084902.jpg"
            Nothing
            Png 
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_084906.jpg"
            Nothing
            Png 
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160122_084908.jpg"
            Nothing
            Png                                     
        ]
        [
          Video
            "https://www.youtube.com/watch?v=gz8Ivcjas9o"
            (Just "Head camera")
            YouTube
        ]     
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160129-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160129-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/626694"
            Nothing
            Doarama
        ]
        []
        [
          Video
            "https://www.youtube.com/watch?v=k9c1GdxkdQY"
            (Just "Head camera")
            YouTube
        ]       
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160205-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160205-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/635027"
            Nothing
            Doarama
        ]
        []
        []  
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160212-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160212-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/640001"
            Nothing
            Doarama
        ]
        []
        [
          Video
            "https://www.youtube.com/watch?v=e7UcjgxOmDw"
            (Just "Head camera")
            YouTube
        ]                                                            
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
        [
          TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160218-vh-ldo.gpx"
            Nothing
            Gpx
        , TrackLog
            "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160218-vh-ldo.png"
            Nothing
            (ImageLog Png)
        ]
        [
          Visualisation
            "http://doarama.com/view/644663"
            Nothing
            Doarama
        ]
        [
          Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090347.jpg"
            Nothing
            Png
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090348.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090351.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090409.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090411.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090421.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090423.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090432.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090436.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090441.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090446.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090452.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090454.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090459.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090501.jpg"
            Nothing
            Png            
        , Image
            "https://github.com/tonymorris/ppl/raw/master/images/20160218-vh-afr/20160218_090502.jpg"
            Nothing
            Png            
        ]
        [
          Video
            "https://www.youtube.com/watch?v=EO4D4zFnpIU"
            (Just "Head camera")
            YouTube
        ]              
    ]
