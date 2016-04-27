{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aviation.Casr.Logbook

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

vhvvo ::
  Aircraft
vhvvo =
  Aircraft
    "2000 Cessna 172R Skyhawk"
    "VH-VVO"
    Single

vhzwy ::
  Aircraft
vhzwy =
  Aircraft
    "2000 Cessna 172S Skyhawk"
    "VH-ZWY"
    Single

ybaf2ybaf ::
  FlightPath
ybaf2ybaf =
  directPath
    "YBAF"
    "YBAF"

flightlogentries ::
  FlightLogEntries
flightlogentries =
  FlightLogEntries
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
        mempty
        mempty
        mempty
        mempty
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
        mempty
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
        mempty
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
        mempty
        mempty
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
        mempty
        mempty
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
        mempty
        mempty
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
              Jpg
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
              Nothing
              Jpg 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
              Nothing
              Jpg 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
              Nothing
              Jpg                                     
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
        mempty
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
        mempty
        mempty
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
        mempty
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
        "P3.1 Circuit Emergencies"
        "P3.1 Circuit Emergencies"
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
              Jpg
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090348.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090351.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090409.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090411.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090421.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090423.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090432.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090436.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090441.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090446.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090452.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090454.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090459.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090501.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090502.jpg"
              Nothing
              Jpg            
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
    , FlightLogEntry
        "P3.2 Circuit Emergencies"
        "P3.2 Circuit Emergencies"
        "20160304"
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
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160304-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160304-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/674600"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=jWUijREg4aE"
              (Just "Head camera")
              YouTube
          ]
        )                        
    , FlightLogEntry
        "P3.3 Circuit Emergencies"
        "P3.3 Circuit Emergencies"
        "20160324"
        vhzwy
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160324-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160324-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/691106"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=RNiYu3ZsfZw"
              (Just "Forward camera")
              YouTube
          ]
        )                        
    , FlightLogEntry
        "P3.4 Circuits"
        "P3.4 Circuits"
        "20160330"
        vhzwy
        (
          Hours
            0
            9
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/696748"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )
    , FlightLogEntry
        "P3.4 Circuits"
        "P3.4 Circuits"
        "20160330"
        vhzwy
        (
          Hours
            0
            4
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/696748"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )           
    , FlightLogEntry
        "P3.5 Circuits (Crosswind)"
        "P3.5 Circuits (Crosswind)"
        "20160415"
        vhafr
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160415-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160415-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/711553"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )
    , FlightLogEntry
        "P3.6 Circuits (Solo check)"
        "P3.6 Circuits (Solo check)"
        "20160422"
        vhafr
        (
          Hours
            0
            5
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/719248"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=UR1IkF5RSh4"
              (Just "Forward camera")
              YouTube
          ]
        )                                     
    , FlightLogEntry
        "P3.6 Circuits (Solo)"
        "P3.6 Circuits (Solo)"
        "20160422"
        vhafr
        (
          Hours
            1
            0
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/719248"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=UR1IkF5RSh4"
              (Just "Forward camera")
              YouTube
          ]
        )                                     
    , FlightLogEntry
        "P3.7 Circuits (Dual)"
        "P3.7 Circuits (Dual)"
        "20160427"
        vhvvo
        (
          Hours
            0
            6
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/724934"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )                                     
    , FlightLogEntry
        "P3.7 Circuits (Solo)"
        "P3.7 Circuits (Solo)"
        "20160427"
        vhvvo
        (
          Hours
            0
            8
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/724934"
              Nothing
              Doarama
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "https://www.youtube.com/watch?v=2QNp74n2Lx8"
              (Just "Forward camera")
              YouTube
          ]
        )                                     
    ]

flightlog ::
  FlightLog
flightlog =
  FlightLog
    "Tony John Morris"
    "19771102"
    "1007036"
    flightlogentries

main ::
  IO ()
main =
  printMarkdown flightlog
