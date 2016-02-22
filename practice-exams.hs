module Main where

import Control.Monad.State
import Data.List
import Data.Monoid
import Data.String
import System.Environment

data Multichoice a =
  Multichoice [a] a [a]
  deriving (Eq, Ord, Show)

instance Functor Multichoice where
  fmap f (Multichoice l x r) =
    Multichoice (fmap f l) (f x) (fmap f r)

instance Foldable Multichoice where
  foldr f z (Multichoice l x r) =
    foldr f (f x (foldr f z r)) l

instance Traversable Multichoice where
  traverse f (Multichoice l x r) =
    Multichoice <$> traverse f l <*> f x <*> traverse f r

choiceList ::
  Multichoice a
  -> [a]
choiceList (Multichoice l x r) =
  l <> (x : r)

onFocus ::
  (a -> a)
  -> Multichoice a
  -> Multichoice a
onFocus f (Multichoice l x r) =
  Multichoice l (f x) r

number ::
  Traversable t =>
  t a ->
  t (Char, a)
number q =
  evalState (traverse (\x -> state (\c -> ((c, x), succ c))) q) 'a'

focus ::
  Multichoice a
  -> a
focus (Multichoice _ x _) =
  x

data Question a =
  Question
    String -- question
    (Multichoice String)
    a
  deriving (Eq, Ord, Show)

data Manual =
  Part61
  | PrePart61
  deriving (Eq, Ord, Show)

(~>) ::
  String
  -> Multichoice String
  -> Question Manual
(~>) s q =
  Question s q Part61

infixr 5 ~>

(!>) ::
  String
  -> Multichoice String
  -> Question Manual
(!>) s q =
  Question s q PrePart61

infixr 5 !>

data Exam =
  Exam
    String -- title
    (Maybe String) -- subtitle
    (Maybe String) -- subsubtitle
    [Question Manual]
  deriving (Eq, Ord, Show)

markdownExam ::
  Exam
  -> String
markdownExam (Exam t1 t2 t3 qs) =
  let markdownQuestion (n, Question q m x) = 
        let q' = case x of
                   Part61 -> q
                   PrePart61 -> "~~" <> q <> "~~"
        in  show n <> ". " <> q' <> "\n" <>
            ((=<<) (\b -> b <> "\n") . zipWith (\n c -> "  " <> fromString (show n) <> ". " <> c) [1..] . choiceList . onFocus (\a -> "**" <> a <> "**") $ m)

  in  intercalate "\n"
        [
          "# " <> t1 <> "\n" <> 
          maybe "" (\t -> "## " <> t <> "\n\n") t2 <> 
          maybe "" (\t -> "#### " <> t <> "\n\n") t3
        , intercalate "\n" (fmap markdownQuestion (zip [1..] qs))
        ]

markdownExams ::
  [Exam]
  -> String
markdownExams =
  intercalate "\n\n----\n\n" . fmap markdownExam

flashcardExam ::
  Exam
  -> String
flashcardExam (Exam t1 t2 t3 qs) =
  let flashcardQuestion (n, Question q m x) =
        let q' = case x of
                   Part61 -> q
                   PrePart61 -> "~~" <> q <> "~~"
        in  replicate 40 '-' <>
            "\n\n" <> 
            q' <>
            "\n" <>
            let z = number m
                display (c, s) = c : (". " <> s)
            in  (intercalate "\n" . choiceList . fmap display $ z) <>
                "\n\n" <>
                display (focus z) <>
                "\n\n"
  in  intercalate "\n"
        [
          "# " <> t1 <> "\n" <> 
          maybe "" (\t -> "## " <> t <> "\n\n") t2 <> 
          maybe "" (\t -> "#### " <> t <> "\n\n") t3
        , intercalate "\n" (fmap flashcardQuestion (zip [1..] qs))
        ]

flashcardExams ::
  [Exam]
  -> String
flashcardExams =
  intercalate ("\n\n" <> replicate 40 '=' <> "\n\n") . fmap flashcardExam

atcPreCircuit ::
  Exam
atcPreCircuit =
  Exam
    "Aviation Theory Centre"
    (Just "Basic Aeronautical Knowledge")
    (Just "Pre-Circuit Solo Practice Exam")
    [
      "Seat belts/safety harnesses must be worn, at all stages of flight, by:" ~>
        Multichoice
          [              
          ]

          "the pilot when he/she is the sole pilot."

          [
            "the occupants of both control seats."
          , "all on board during climb and descent."
          , "two children when they occupy a single seat."
          ]
    , "Civil Aviation Regulations require that turns onto final approach must:" ~>
        Multichoice
          [
            "commence not below 500ft AGL."
          , "be completed not closer than 500 metres from the threshold."
          ]

          "be completed not closer than 500 metres from aerodrome perimeter."

          [
            "be completed not closer than 500 metres away from any displaced threshold."
          ]
    , "A pilot in command shall not consume any alcoholic liquor:" ~>
        Multichoice
          [
            "12 hours before the departure of the flight."
          , "24 hours before reporting for duty."
          , "8 hours before reporting for duty."
          ]

          "8 hours before the departure of the flight."

          [
          ]          
    , "The minimum horizontal distance from cloud for a fixed-wing aircraft, operating in Class G airspace, is:" ~>
        Multichoice
          [
          ]

          "1,500 metres."

          [
            "1,500ft."
          , "1,000ft."
          , "clear of cloud."
          ]          
    , "The fuel system in an aircraft must be checked for water contamination:" ~>
        Multichoice
          [
            "before each flight."            
          ]

          "before the first flight of the day and after each refuelling."

          [
            "before the first flight of the day and after the last flight of the day."
          , "at least once a day."
          ]          
    , "One restriction placed on VFR aircraft, operating at or below 2,000ft, is:" ~>
        Multichoice
          [
            "minimum horizontal distance from cloud is 2,000 metres."
          , "minimum flight visibility is 6 km."
          ]

          "navigation must be by visual reference to the ground or water."

          [
            "minimum vertical distance from cloud is 1,000ft."
          ] 
    , "A student pilot is not permitted to carry passengers unless:" !>
        Multichoice
          [
            "he/she has completed 2 hours as pilot in command."
          ]

          "he/she has passed the General Flying Progress Test and flies within the student pilot area limits."

          [
            "he/she has completed 5 hours of cross-country flying."
          , "he/she has had a dual flight within the last 30 days."
          ]                         
    , "With local QNH set on a subscale, an altimeter will always read:" ~>
        Multichoice
          [
            "pressure height."
          , "density height."
          ]

          "height above mean sea level."

          [
            "height above ground level."
          ] 
    , "At a non-towered aerodrome, you should not continue an approach beyond the threshold until a preceding light aeroplane, using the same runway has:" ~>
        Multichoice
          [
          ]

          "landed, and has vacated the runway, irrespective of the runway length, and is taxiing away."

          [
            "landed, and is at least 1,800 metres from the landing threshold."
          , "taken off and is at least 200 AGL."
          , "taken off and is at least 600 metres ahead of the landing threshold."
          ] 
    , "You are to operate from a non-towered aerodrome where there is no ATIS and no other way of knowing the latest QNH. Before take-off, you should:" ~>
        Multichoice
          [
          ]

          "set the altimeter to read the aerodrome elevation."

          [
            "set the altimeter to read zero feet."
          , "set an approximate aerodrome pressure in the subscale."
          , "set 1013hPa on the subscale."
          ] 
    , "One item which must be included in a passenger briefing prior to take-off is:" ~>
        Multichoice
          [
            "a demonstration of the correct crash landing position."
          , "the use and location of fire extuingishers."
          ]

          "the use and adjustment of seat belts."

          [
            "the demonstration of evacuation procedures."
          ] 
    , "With regard to the rules of the air in CAR, which statement is correct?" ~>
        Multichoice
          [
            "An aircraft that is overtaking another aeroplane, has right of way."
          , "If an aeroplane and a glider are approaching head-on at approximately the same height the aeroplane must give way to the glider."
          ]

          "An aircraft that is within 70 degrees of the astern position of an aircraft ahead is considered an overtaking aircraft, if its speed is greater."

          [
            "An aircraft that is overtaking another aircraft must do so by altering its heading to the left."
          ] 
    , "On which of the following types of flying is a passenger not permitted to be carried?" ~>
        Multichoice
          [
            "An aircraft engaged in aerobatic flying."
          ]

          "Flying training given to a person who has not passed the General Flying Progress Test (GFPT)."

          [
            "An aircraft carrying out formation flying."
          , "An aircraft engaged in search and rescue (SAR) operations."
          ] 
    , "You are to carry out a flight from Parafield (GAAP) to the training area in a Piper Arrow -- XYG. You are also in receipt of ATIS information \"Charlie\", indicating Runway 21 Right is being used. Your pre-taxi radiocall will be:"  ~>
        Multichoice
          [
          ]

          "Parafield Ground, X-ray Yankee Golf, Piper Arrow, received Charlie, VFR for the Training Area, Runway 21 Right."

          [
            "Parafield Ground, Piper Arrow X-ray Yankee Golf, for the Training Area, Runway 21 Right, taxi clearance, received Charlie."
          , "Parafield Ground, X-ray Yankee Golf, for the Training Area, received Charlie."
          , "Parafield Ground, Piper Arrow X-ray Yankee Golf, taxi clearance for the Training Area, Runway 21 Right, received Charlie."
          ] 
    , "Area QNH is:" ~>
        Multichoice
          [
            "the aerodrome pressure set in the altimeter before taxiing."
          , "the mean sea level pressure set in the altimeter before taxiing."
          , "the local aerodrome pressure set in the altimeter while on a navigation or cross-country flight."
          ]

          "the mean sea level pressure set in the altimeter of the area over which the aircraft is flying while on a navigation or cross-country exercise."

          [
          ] 
    , "When the aerodrome QNH is set, during taxi, the altimeter should read:" ~>
        Multichoice
          [
            "zero."
          ]

          "airfield elevation."

          [
            "vertical distance AGL."
          , "vertical distance AAL."
          ] 
    , "One of the basic rules following an engine failure immediately after take-off is:" ~>
        Multichoice
          [
            "always try to restart the engine."
          , "immediately make a distress call."
          ]

          "never turn back to the runway."

          [
          ] 
    , "A hard pull up on the control column can cause an aeroplane to stall at almost any speed because:" ~>
        Multichoice
          [
            "of reduced control effectiveness."
          ]

          "the critical angle of attack can be exceeded."

          [
            "of decreased angle of attack."
          , "of reduced airspeed."
          ] 
    , "A white dumb-bell symbol has been placed adjacent the primary wind indicator on your airfield. This means:" ~>
        Multichoice
          [
            "the airfield is completely unserviceable."
          , "poor surface this area, do not taxi here."
          , "parachute operations in progress."
          ]

          "use hard surfaces only."

          [
          ]           
    , "An aircraft stall-warning device will:" ~>
        Multichoice
          [
            "indicate that a stall has occurred."
          , "cause a light to come on and a horn to sound after the aircraft stalls."
          ]

          "indicate that the aircraft is approaching the stall at any attitude."

          [
          ] 
    ]

atcPreArea ::
  Exam
atcPreArea =
  Exam
    "Aviation Theory Centre"
    (Just "Basic Aeronautical Knowledge")
    (Just "Pre-Area Solo Practice Exam")
    [
      "An aircraft fitted with a piston engine must be fuelled with:" ~>
      Multichoice
        [              
          "AVTUR fuel."
        , "Unleaded fuel."
        , "Leaded Fuel."
        , "MOGAS."
        ]

        "AVGAS."

        [
        ]
    , "What colour is AVGAS 100LL?" ~>
      Multichoice
        [              
          "Clear or straw colour."
        ]

        "Blue."

        [
          "Red or purple."
        , "Green."
        ]
    , "The carburettor heat can be used as:" ~>
      Multichoice
        [              
          "an anti-icing action (ice prevention)."
        , "a de-icing action (ice removal)."
        , "an alternate supply of air to the engine."        
        ]

        "all the above."

        [
        ]
    , "Should ice form in your carburettor and you have a small reduction in MP, rpm, or airspeed, you should:" ~>
      Multichoice
        [         
          "keep adjusting power by opening the throttle."     
        ]

        "adjust carburettor heat, full on."

        [
          "use alternate static."
        ]
    , "Ice that forms on the engine air filter is known as:" ~>
      Multichoice
        [
          "throttle ice."
        , "fuel evaporation ice."
        ]

        "impact ice."

        [
        ]
    , "What common action should be taken if impact or throttle icing occurs?" ~>
      Multichoice
        [
          "Reduce power."
        , "Increase power."
        ]

        "Apply carburettor heat."

        [
          "Warm the engine."
        ]
    , "After flying for sometime, the centre-zero ammeter shows almost zero (just a small positive indication). This would mean:" ~>
      Multichoice
        [
          "that the aircraft alternator has failed and the battery is providing all the power."
        , "insufficient electrical output is being produced by the alternator."
        , "that the battery is being recharged."
        ]

        "that the battery is fully charged and the system is working normally."

        [
        ]                
    , "If the alternator fails, the electrical system will receive its power from" ~>
      Multichoice
        [
          "an emergency generator."
        ]

        "the battery."

        [
          "a wind driven electrical generating device."
        ]
    , "Should the alternator fail and the battery power be consumed:" ~>
      Multichoice
        [
          "the engine-driven fuel pump would stop working."
        , "the fuel boost pump would still be available."
        ]

        "most of the engine instruments would stop working."

        [
          "the engine would malfunction."
        ]
    , "Excessive priming of an engine for start can:" ~>
      Multichoice
        [
          "cause the spark to occur at the incorrect time."
        ]

        "increase the risk of a manifold fire during start."

        [
          "reduce the amount of fuel available in the manifold."
        , "adversely affect the oil pressure."
        ]    
    , "Aircraft fuel tanks require a vent, the purpose of which is to:" ~>
      Multichoice
        [
          "avoid a build-up of dangerous fumes."
        , "allow condensation inside the tank to escape."
        ]

        "allow the pressure inside the tank to equalise with the outside pressure."

        [
        ]
    , "Aircraft spark plugs may become fouled with a build-up of lead from the fuel, if the engine is operated for any length of time at a:" ~>
      Multichoice
        [
          "high power setting with a lean mixture."
        , "low power setting with a lean mixture."
        , "high power setting with a rich mixture."
        ]

        "low power setting with a rich mixture."

        [
        ]
    , "If you set the altimeter to QNH, the instrument should read:" ~>
      Multichoice
        [
          "zero feet."
        , "density altitude."
        , "pressure altitude."
        ]

        "vertical distance AMSL."

        [
        ]
    , "The red radial line on the airspeed indicator represents the:" ~>
      Multichoice
        [
          "structural cruising speed."
        , "landing gear lowering speed."
        , "normal operating speed range."
        ]

        "never exceed speed."

        [
        ]
    , "Rotation about the longitudinal axis is referred to as:" ~>
      Multichoice
        [
          "yawing, and is effected by the rudder."
        , "yawing, and is effected by the ailerons."
        ]

        "rolling, and is effected by the ailerons."

        [
          "pitching, and is effected by the elevator."
        ]
    , "The tendency of an aircraft to return to its original condition when disturbed from straight and level flight is known as:" ~>
      Multichoice
        [
          "controllability"
        , "manoeuvreability."
        ]

        "stability."

        [
          "balance."
        ]
    , "During a climbing turn you must be careful of:" ~>
      Multichoice
        [
        ]

        "the tendency to over-bank the aircraft as the horizon is not always obvious."

        [
          "decreasing your rate of climb."
        , "the tendency to under-bank the aircraft as the horizon is not always obvious."
        ]
    , "How is the aircraft's performance affected by mud or minor damage on the surface of the wings?" ~>
      Multichoice
        [
          "Lift is decreased, drag is decreased."
        , "Lift is increased, drag is decreased."
        , "Lift is increased, drag is increased."
        ]

        "Lift is decreased, drag is increased."

        [
        ]
    , "A blockage of the static system would likely be corrected by applying:" ~>
      Multichoice
        [
          "alternate air on."
        , "pitot heat on."
        ]

        "alternate static on."

        [
          "carburettor heat on."
        ]
    , "During your take-off roll, the airspeed indicator remains at zero. You should:" ~>
      Multichoice
        [
        ]

        "abort the take-off."

        [
          "try to work out why the ASI is not working."
        , "estimate your rotate speed."
        , "select alternate static and pitot heat on."
        ]
    , "If after landing, you find one brake is not working, you should:" ~>
      Multichoice
        [
          "stop on the runway."
        , "taxi back to the parking tarmac as the nosewheel steering will still be functional."
        ]

        "allow your aircraft to slow using most of the runway, taxi clear of the landing area and radio for assistance on the ground frequency."

        [
        ]
    , "For proper internal cooling the engine must have:" ~>
      Multichoice
        [
          "a functioning oil cooler thermostat."
        , "a correctly leaned mixture setting."
        ]

        "circulating oil at a pressure within the permissible range."

        [
          "proper airflow over the exhaust manifold."
        ]
    , "If you encounter gusting wind in the circuit, what speed would you try to maintain on the final approach?" ~>
      Multichoice
        [
          "About 5kt above the stall to minimise the impact of the gusts."
        , "Normal approach speed because it is chosen to account for such conditions."
        ]

        "About 5 to 10kt above the normal approach speed depending upon the strength of the gusts."

        [
          "Manoeuvre speed."
        ]
    , "After start, engine oil pressure must reach normal operating pressure (the green sector) within:" ~>
      Multichoice
        [
          "30 minutes."
        , "10 minutes."
        , "15 seconds."
        ]

        "30 seconds."

        [
        ]
    , "While taxiing, you notice another aircraft is approaching you on the same taxiway from the opposite direction. Should you:" ~>
      Multichoice
        [
          "continue straight ahead and be prepared to stop."
        , "move to the left and stop if necessary."
        ]

        "move to the right and stop if necessary."

        [
          "stop immediately."
        ]
    , "An aircraft will stall:" ~>
      Multichoice
        [
          "at a higher speed if power is increased."
        ]

        "at the same indicated airspeed as altitude increases."

        [
          "when the aircraft's weight exceeds lift."
        , "at a lower indicated airspeed as altitude decreases."
        ]
    , "When considering the hazards of wake turbulence, the wingtip vortices trailing behind large aeroplanes in flight:" ~>
      Multichoice
        [
          "will present no hazard when the vortices are encountered in level cruising flight."
        , "will increase in intensity and violence as the speed of the large aeroplane increases."
        ]

        "are most severe when the large aeroplane is at low speed during climbs or approaches for landings."

        [
        ]
    , "For proper cooling the engine must have:" ~>
      Multichoice
        [
          "the correct mixture setting for that operation."
        , "the correct quantity of oil in the engine."
        , "the correct amount of cooling airflow around the engine."
        ]

        "all of the above."

        [
        ]
    , "Detonation occurs in a reciprocating engine when:" ~>
      Multichoice
        [
          "a spark plug is fouled and shorts out."
        , "the mixture is too rich."
        ]

        "the charge explodes instead of just burning."

        [
          "hot spots ignite the mixture too soon."
        ]
    , "Although the master switch is off, before leaving an aircraft a pilot mustalso ensure that the magneto switch is off. The reason for this is:" ~>
      Multichoice
        [
        ]

        "the magneto system is independent of aircraft power and the engine could start if the propeller was turned."

        [
          "so the key can be removed."
        , "the magneto system will have no earth and cannot be energised."
        , "the battery will not discharge over night."
        ]
    ]

atcBAK ::
  Exam
atcBAK =
  let q56to60PerformanceData = "*Using the following performance data:*\n  * Elevation: `1,890ft`\n  * Runways:\n    * `17/35 sealed TODA 3,000 metres (slope: level)`\n    * `12/30 short grass TODA 1,000 metres (slope: 1% down to SE)`\n  * ATIS\n    * `Terminal Information Delta`\n    * `Runway 12`\n    * `Wind: 150/15`\n    * `QNH 1,010\n    * Temperature: 15 degrees\n    * Cloud `FEW 3000`\n    * `Runway wet`\n    * `Works in progress Runway 17/35`\n\n"
  in  Exam
        "Aviation Theory Centre"
        (Just "Basic Aeronautical Knowledge")
        (Just "Basic Aeronautical Knowledge Practice Exam")
        [
          "Where the refuelling equipment is not mobile, the Civil Aviation Orders require that an aeroplane being refuelled shall:" ~>
          Multichoice
            [
              "have no persons on board."
            , "have a fire extuingisher on board."
            , "have all electrical systems switched off."
            ]

            "be so placed that it can rapidly be moved to a place of safety if needed."

            [
            ]
        , "An aeroplane's fuel must be checked for the presence of water:" ~>
          Multichoice
            [
              "prior to every flight."
            , "prior to the first flight of the day and at each change of pilots."
            ]

            "prior to the first flight of the day and following each refuelling."

            [
              "after each refuelling and/or change of pilots."
            ]
        , "Follow an incident, an Air Safety Incident Report shall be submitted:" ~>
          Multichoice
            [
              "within 48 hours of the completion of the flight."
            , "within 72 hours of the completion of the incident occurring."
            , "within 24 hours of the completion of the incident occurring."
            ]

            "immediately."

            [
            ]
        , "Which of the following would justify the use of a Mayday call?" ~>
          Multichoice
            [
              "You need navigational assistance."
            , "You sight a capsized yacht off the coast."
            , "A passenger becomes ill and you need to land for medical assistance."
            ]

            "You have an engine failure on a training flight."

            [
            ]
        , "An aeroplane must not fly over a populated area at a height lower than:" ~>
          Multichoice
            [
              "1,500ft AMSL."
            , "1,000ft on the area QNH."
            ]

            "1,000ft AGL."

            [
              "500ft AGL."
            ]
        , "Increasing power when flying straight and level at normal cruise speed will cause:" ~>
          Multichoice
            [
              "an increase in airspeed and a decrease in drag."
            ]

            "an increase in airspeed and an increase in drag."

            [
              "a decrease in airspeed and a decrease in drag."
            , "a decrease in airspeed and an increase in drag."
            ]
        , "What is the effect of lowering full flap?" ~>
          Multichoice
            [
              "An increase in lift and a decrease in drag."
            , "A decrease in lift and a decrease in drag."
            , "A increase in lift and an increase in drag."
            ]

            "An increase in lift and an increase in drag."

            [
            ]
        , "Stalling speed is increased if:" ~>
          Multichoice
            [
            ]

            "weight is increased."

            [
              "weight is decreased."
            , "load factor is decreased."
            , "angle of attack is increased."
            ]
        , "What do we call the angle between the chord line of an aerofoil and the relative airflow?" ~>
          Multichoice
            [
              "Angle of incidence."
            , "Stalling angle."
            , "Sweepback."
            ]

            "Angle of attack."

            [
            ]
        , "Increasing the load factor will:" ~>
          Multichoice
            [
              "decrease the stalling speed."
            ]

            "increase the stalling speed."

            [
              "increase the stalling angle."
            , "decrease the stalling angle."
            ]        
        , "What happens to the lift and drag as the angle of attack approaches the stalling angle?" ~>
          Multichoice
            [
            ]

            "Lift increases and drag increases."

            [
              "Lift increases and drag decreases."
            , "Lift decreases and drag decreases."
            , "Lift decreases and drag increases."
            ] 
        , "An aeroplane is descending at the best gliding speed. Raising the nose will:" ~>
          Multichoice
            [
              "decrease the gliding range."
            ]

            "increase the gliding range."
  
            [
              "decrease the glide angle."
            , "decrease the rate of descent."
            ]          
        , "If a tailwind is encountered during a glide descent, when compared with a glide in still-air conditions, the:" ~>
          Multichoice
            [
              "aeroplane pitch angle will be shallower."
            , "flightpath over the ground will be steeper."
            ]

            "distance over the ground will be greater."

            [
              "rate of descent will be less."
            ]          
        , "When compared to the stall IAS at 1,000ft, the stall IAS at 10,000ft will:" ~>
          Multichoice
            [
              "increase by approximately 40%."
            , "decrease by approximately 40%."
            ]

            "be the same."

            [
              "vary according to the actual density at 10,000ft."
            ]    
        , "When in a 30 degree banked level turn, the load factor:" ~>
          Multichoice
            [
              "is the same as in straight and level flight."
            , "decreases by a factor of 15%."
            ]

            "will increase to 1.15g."

            [
              "will be limited by the airspeed."
            ] 
        , "What are the VMC requirements for a VFR flight conducted in Class G airspace between 3,000ft AMSL (or 1,000ft AGL if higher), and 10,000ft AMSL?" ~>
          Multichoice
            [
              "Visibility 5km, clear of cloud."
            , "Visibility 8km, 1,000 metres horizontally from cloud, 1,000ft above or below cloud."
            ]

            "Visibility 5,000 metres, 1,500 metres horizontally from cloud, and 1,000ft vertically from cloud."

            [
              "Visibility 8,000 metres, clear of cloud."
            ]                           
        , "A cold front is likely to produce:" ~>
          Multichoice
            [
              "cumiliform clouds with smooth flying conditions."
            , "stratiform clouds with smooth flying conditions."
            ]

            "cumiliform clouds with turbulent conditions."

            [
              "stratiform clouds with turbulent flying conditions."
            ]  
        , "The term INTER on a forecast means that the weather will be:" ~>
          Multichoice
            [
              "interminable in nature."
            , "expected to last for at least 60 minutes."
            , "interrupted ever 30 minutes."
            ]

            "expected to last less than 30 minutes."

            [
            ]   
        , "The cloud base in a TAF is given as a height above:" ~>
          Multichoice
            [
              "the 1013.2 hPa level."
            , "the highest ground within 10km of the aerodrome."
            , "mean sea level."
            ]

            "aerodrome level."

            [
            ]     
        , "The wind direction in the ATIS is given in:" ~>
          Multichoice
            [
              "degrees true from the wind direction."
            , "degrees magnetic to the wind direction."
            ]

            "degrees magnetic from the wind direction."

            [
              "degrees true to the wind direction."
            ]
        , "Using your navigation computer, determine the true airspeed (TAS) if you are flying at 120kt IAS, with a temperature of +15 degrees celsius at a pressure altitude of 8,000ft." ~>
          Multichoice
            [
            ]

            "139kt."

            [
              "132kt."
            , "127kt."
            , "120kt."
            ]
        , "If the time in Sydney on 23 March is 0628 EST, what is the UTC time?" ~>
          Multichoice
            [
              "231628 UTC."
            ]

            "222028 UTC."

            [

              "232028 UTC."
            , "221628 UTC."
            ]
        , "The track to your training area is 270 degrees Magnetic. If the wind is from the south and is causing 10 degrees of drift, your heading would be closest to:" ~>
          Multichoice
            [
              "270 degrees True."
            , "280 degrees Magnetic."
            , "290 degrees True."
            ]

            "260 degrees Magnetic."

            [
            ]    
        , "Magnetic variation is:" ~>
          Multichoice
            [
              "the angular difference between magnetic north and compass north."
            , "the direction in which all compass errors occur."
            ]

            "always expressed in degrees, either east or west."

            [
              "the angular difference between true north and compass north."
            ]
        , "A line of longitude on a WAC is:" ~>
          Multichoice
            [
            ]

            "drawn north-south and represents true north and true south."

            [
              "drawn east-west and represents magnetic north and magnetic south."
            , "sometimes referred to as in isogonal."
            , "drawn parallel to the next meridian."
            ]
        , "On a day when the temperature is 30 degrees and the relative humidity is 30%, carburettor ice:" ~>
          Multichoice
            [
            ]

            "is unlikely to form."

            [
              "will form with maximum power set."
            , "is likely to form with normal cruise power set."
            , "is highly likely to form with low power set."
            ]
        , "Why do most aeroplane piston engines have dual ignition systems?" ~>
          Multichoice
            [
              "For the sole reason of safety in the event of the failure of one magneto."
            ]

            "For safety and for improved combustion."

            [
              "For improved combustion even though safety is not increased."
            , "To spread the load between each of the spark plugs in a cylinder."
            ]
        , "If, following combustion, there is unburned fuel remaining in the cylinders, then the mixture is described as:" ~>
          Multichoice
            [
            ]

            "rich."

            [
              "chemically correct for the power setting being used."
            , "lean."
            , "having a fuel/air ratio of 100%."
            ]        
        , "What do we call explosive, spontaneous combustion in the cylinders?" ~>
          Multichoice
            [
              "Run-on."
            ]

            "Detonation."

            [
              "Pre-ignition."
            , "Normal combustion."
            ]
        , "What is one effect of applying carburettor heat?" ~>
          Multichoice
            [
              "It will result in more air immediately going through the carburettor."
            , "It will not affect the fuel-air mixture."
            ]

            "It will enrich the mixture."

            [
              "It will lean the mixture."
            ]
        , "Which flight instrument gives a pilot a direct reading of the bank angle?" ~>
          Multichoice
            [
              "Direction indicator."
            ]

            "Attitude indicator."

            [
              "Turn coordinator."
            , "Magnetic compass."
            ]
        , "You are cruising in an aeroplane with a fixed-pitch propeller and notice a slight drop in engine rpm. You suspect carburettor icing and apply full carburettor heat. Which one of the following statements most correctly indicates that ice was present?" ~>
          Multichoice
            [
              "There will be an immediate increase in engine rpm as soon as carburettor heat is applied."
            , "The engine will start to run roughly with a further decrease in engine rpm."
            , "The engine will remain constant."
            ]

            "The engine rpm will decrease and then increase."

            [
            ] 
        , "When compared with an engine fitted with a carburettor, which of the following applies to a fuel-injected engine?" ~>
          Multichoice
            [
              "It is more prone to starting problems and is less susceptible to fuel contamination."
            , "It is easier to start when hot and is more responsive to throttle movement."
            ]

            "It is less likely to have icing problems and is more efficient."

            [
              "It requires greater care in throttle handling and is more susceptible to fuel contamination."
            ]               
        , "Fuel that appears to be uncoloured or a very pale yellow is:" ~>
          Multichoice
            [
              "100LL low-lead AVGAS for piston engines."
            , "100/130 AVGAS for piston engines."
            , "MOGAS."
            ]

            "AVTUR (kerosene) for turbine engines (i.e. jet or prop-jet engines)."

            [
            ]        
        , "To improve engine cooling during a maximum power climb, you should:" ~>
          Multichoice
            [
            ]

            "fly at a higher IAS."

            [
              "fly at a lower IAS."
            , "lean the fuel-air mixture."
            , "apply carburettor heat."
            ]
        , "The correct sequence of the various strokes in a four-stroke engine is:" ~>
          Multichoice
            [
            ]

            "intake, compression, power, exhaust."

            [
              "intake, exhaust, power, compression."
            , "intake, power, compression, exhaust."
            , "intake, power, exhaust, compression."
            ]        
        , "Which statement concerning carburettor ice do you consider most accurate?" ~>
          Multichoice
            [
            ]

            "Carburettor ice is most likely to form when the air temperature is in the range -10 degrees celsius to +20 degrees celsius with visible moisture or high humidity."

            [
              "The carburettor heater is a de-icing device that heats the air after it leaves the carburettor."
            , "Carburettor ice will always form when the temperature is below freezing."
            , "The first indication of carburettor icing in an aeroplane with a fixed-pitch propeller is an increase in rpm."
            ]        
        , "If the static vent ices over during a descent, the airspeed indicator will then read:" ~>
          Multichoice
            [
              "zero."
            ]

            "higher than the actual IAS."

            [
              "lower than the actual IAS."
            , "correctly."
            ]
        , "With battery and generator switches ON, what is the probable reason for a zero reading on a left-zero ammeter?" ~>
          Multichoice
            [
              "The battery is fully charged."
            , "The alternator is still charging the battery."
            , "The battery is completely flat."
            ]

            "The alternator has failed."

            [
            ]        
        , "The pitot-static system supplies pressure for:" ~>
          Multichoice
            [
              "the ASI only."
            , "the ASI, artificial horizon and VSI."
            , "the ASI, turn and slip indicator and altimeter."
            ]

            "the ASI, VSI and altimeter."

            [
            ] 
        , "If you decide to fly when you have a cold or flu, you put yourself at risk of:" ~>
          Multichoice
            [
              "being too tired to concentrate."        
            ]

            "having balance difficulties and sinus pain."

            [
              "becoming dehydrated."
            , "developing hypoxia."
            ]   
        , "Atmospheric pressure variations affect the human body. These pressure changes are:" ~>
          Multichoice
            [
              "minimum at low level so we generally don't need to worry about them."
            , "minimum at high altitude."
            , "maximum at high altitude so we need pressurisation and oxygen supplied."
            ]

            "maximum at low level so we need to understand their adverse effects."

            [
            ]  
        , "Which of the following drugs are considered acceptable for flying?" ~>
          Multichoice
            [
              "Antibiotics."
            , "Analgesics."
            , "Antihistamines."
            , "Amphetamines."
            ]

            "None of the above."

            [
            ]  
        , "If an aeroplane is loaded in such a way that the centre of gravity is outside the forward limit, it will:" ~>
          Multichoice
            [
              "be very unstable longitudinally."
            , "have a very short moment arm."
            , "be very unstable about the normal axis."
            ]

            "be very nose heavy and difficult to rotate on take-off."

            [
            ] 
        , "Along which axis of the aeroplane is the centre of gravity computed?" ~>
          Multichoice
            [
              "Lateral."
            , "Normal."
            ]

            "Longitudinal."

            [
              "All of the above."
            ] 
        , "An aeroplane must not be operated at a weight in excess of the maximum certificated gross weight because:" ~>
          Multichoice
            [
            ]

            "structural limitations will be exceeded."

            [
              "an overloaded aeroplane is excessively stable in flight."
            , "flight in excess of certificated weights is not possible."
            , "fuel consumption will be greater."
            ]                                              
        , "Which of the following factors improves take-off performance?" ~>
          Multichoice
            [
              "The runway sloping upwards."
            , "A tailwind."
            , "A crosswind."
            , "Low atmospheric pressure."
            ]

            "Low atmospheric temperature."

            [
            ]  
        , "Vno is defined as the:" ~>
          Multichoice
            [
              "maximum indicated airspeed for extending flaps."
            , "true airspeed beyond which flight is not permitted."
            ]

            "maximum indicated airspeed for normal operations in smooth conditions."

            [
              "indicated airspeed beyond which flight is not permitted."
            ]  
        , "Strong wake turbulence produced by wingtip vortices is most likely to form:" ~>
          Multichoice
            [
            ]

            "behind a heavy aeroplane which is flying slowly just after take-off."

            [
              "behind any aeroplane which is flying slowly with full flap extended."
            , "behind any aeroplane which is flying at high speed."
            , "only behind aeroplanes powered by turbo-jet engines."
            ]
        , "As you approach to land the wind is gusting. You increase your approach speed by 5kt to ensure a margin of safety. This means:" ~>
          Multichoice
            [
              "the approach path flown will be steeper."
            , "climbing for a go-around would not be possible."
            ]

            "the LDR will be greater."

            [
              "descent will be much more rapid."
            ]        
        , "To load an aeroplane, where would you find the weight and balance details for that aircraft?" ~>
          Multichoice
            [
              "In the maintenance release."
            , "On the passenger manifest."
            , "In your flight instructors handbook."
            ]

            "In the aircraft flight manual."

            [
            ]
        , "The weight of 230 litres of AVGAS is closest to:" ~>
          Multichoice
            [
              "128kg."
            , "145kg."
            ]

            "163kg."

            [
              "172kg."
            , "180kg."
            ]  
        , "Using Load System Charlie ![Load System Charlie](http://i.imgur.com/e4AEVb7.jpg), Given:\n\n  > EW 695kg\n  > IU 19.788\n  > The student pilot weighs 60kg and the instructor weighs 75kg\n\n  The maximum amount of fuel on board that will allow \"utility category\" operations immediately after take-off is closest to:" ~>
          Multichoice
            [   
              "62 litres."       
            , "88 litres."
            , "100 litres."
            ]

            "123 litres."

            [
              "135 litres."
            ]        
        , "Using Load System Charlie ![Load System Charlie](http://i.imgur.com/e4AEVb7.jpg), Given:\n\n  > EW 695kg\n  > IU 19.788\n  > The student pilot weighs 60kg and the instructor weighs 75kg\n  > Load the aircraft with 95kg weight of baggage and load 170 litres of fuel\n\n  The zero fuel weight conditions is closest to:" ~>
          Multichoice
            [   
              "weight 824kg, arm 2,821mm."
            , "weight 890kg, arm 2,933mm."
            ]

            "weight 932kg, arm 2,960mm."

            [
              "weight 990kg, arm 3,004mm."
            ]        
        , "Using Load System Charlie ![Load System Charlie](http://i.imgur.com/e4AEVb7.jpg), Given:\n\n  > EW 695kg\n  > IU 19.788\n  > The student pilot weighs 60kg and the instructor weighs 75kg\n  > Load the aircraft with 95kg weight of baggage and load 170 litres of fuel\n\n  The take-off condition is closest to:" ~>
          Multichoice
            [   
              "weight 932kg, arm 2,960mm."
            , "weight 1,013kg, arm 2,999mm."
            , "weight 1,035kg, arm 3,004mm."
            ]

            "weight 1,053kg, arm 2,960mm."

            [
              "weight 1,012kg, arm 2,999mm."
            ]        
        , (q56to60PerformanceData <> "  The pressure altitude of this airfield is:\n") ~>
          Multichoice
            [
              "1,800ft."              
            ]

            "1,980ft."

            [
              "2,260ft."
            , "2,240ft."
            ]
        , (q56to60PerformanceData <> "  The density altitude of this airfield is:\n") ~>
          Multichoice
            [
              "1,800ft."
            , "1,980ft."             
            , "2,260ft."
            ]

            "2,460ft."

            [
            ]            
        , (q56to60PerformanceData <> "  The headwind component on the duty runway is closest to:\n") ~>
          Multichoice
            [
              "5kt."
            , "8kt."             
            , "10kt."
            ]

            "12kt."

            [
            ]                          
        , (q56to60PerformanceData <> "  Use the Cessna Landing chart ![Cessna Landing Chart](http://i.imgur.com/axGWoHJ.jpg) At the MTOW, the take-off distance required on Runway 12 is closest to:\n") ~>
          Multichoice
            [
              "550m."
            , "730m."             
            ]

            "890m."

            [
              "980m."
            ]                          
        , (q56to60PerformanceData <> "  Use the Piper Landing chart ![Piper Landing Chart](http://i.imgur.com/64t1Sju.jpg) Under the conditions given the landing distance required for this aircraft is closest to:\n") ~>
          Multichoice
            [
              "310m."
            , "420m."             
            ]

            "530m."

            [
              "640m."
            ]                          
    ]

taitPreSolo ::
  Exam
taitPreSolo =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Pre-Circuit Solo Practice Exam")
    [
      "Which of the following is true of VHF radio transmissions?" ~>
      Multichoice
        [              
          "more than one transmission can be received at the same time without interference"
        , "distant stations can be received even when the aircraft is on the ground"
        ]

        "the signals travel by 'line of sight' and are shielded by the curvature of the earth"

        [
          "the signals travel further over land than it does over water"
        ]
    , "Which of the following is a VHF frequency?" ~>
      Multichoice
        [              
        ]

        "124.7 MHz"

        [
          "6166 MHz"
        , "12.47 MHz"
        , "616.6 MHz"
        ]
    , "When the VHF radio is turned on, a loud hissing noise is heard. Which control should be adjusted to eliminate the noise?" ~>
      Multichoice
        [              
          "the volume control"
        , "the frequency selector"
        , "the speaker phones selector switch"
        ]

        "the squelch control"

        [
        ]
    , "You hear the first part of a radio message clearly then a loud squeal drowns out the remainder. The most likely cause would be" ~>
      Multichoice
        [              
          "the station transmitting has a faulty microphone"
        , "your squelch control is not properly set"
        ]

        "another station has transmitted on your frequency"

        [
          "your receiver is set to the incorrect frequency"
        ]
    , "The number 7500 would be transmitted as" ~>
      Multichoice
        [              
        ]

        "seven thousand five hundred"

        [
          "seven and a half thousand"
        , "seven five zero zero"
        , "seventy-five hundred"
        ] 
    , "Which of the following situations would warrant the transmission of a MAYDAY call in a single engine aeroplane?" ~>
      Multichoice
        [              
        ]

        "the engine fails during a flight in your training area"

        [
          "a passenger becomes very ill and obviously requires medical attention"
        , "a passenger becomes air sick and asks you to land as soon as possible"
        , "you encounter severe turbulence in the circuit area"
        ]               
    , "Which of the choices would warrant the transmission of a PAN call?" ~>
      Multichoice
        [      
          "the engine fails during a flight in your training area"        
        ]

        "a passenger becomes very ill and obviously requires medical attention"

        [
          "a passenger becomes air sick and asks you to land as soon as possible"
        , "you encounter severe turbulence in the circuit area"
        ] 
    , "Another aircraft calls to advise that he estimates your aerodrome at three five. This means that" ~>
      Multichoice
        [    
          "he will be overflying your aerodrome at three thousand five hundred feet"
        , "he is presently thirty-five miles from your aerodrome"
        , "he expects to arrive in your circuit area at five minutes past three"          
        ]

        "he expects to arrive in your circuit area at thirty-five minutes past the current hour"

        [
        ]               
    , "What procedures apply at your local aerodrome if the radio fails while you are doing circuits?" ~>
      Multichoice
        [              
        ]

        "*Check with your instructor for the appropriate procedures for your aerodrome.*"

        [
        ]        
    , "List the radio calls that apply at your aerodrome for the situations below\n  * At the commencement of taxiing\n  * Before crossing the holding point\n  * Before take-off\n  * Downwind\n  * Base" ~>
      Multichoice
        [              
        ]

        "*Check with your instructor for the calls that apply at your aerodrome.*"

        [
        ]  
    , "Which of the following types of aircraft motion combine to produce a spiral dive?" ~>
      Multichoice
        [ 
          "pitch and roll"
        , "pitch and yaw"             
        ]

        "roll and yaw"

        [
          "roll and further roll"
        ]
    , "The best way for a pilot to assess the aircraft's attitude in space is to" ~>
      Multichoice
        [              
          "look at the wing tips in relation to the natural horizon"
        ]

        "look straight ahead to relate the nose of the aircraft to the natural horizon"

        [
          "look at the artificial horizon"
        , "check the power setting and the resulting airspeed"
        ]        
    , "A pilot knows that the aircraft is correctly trimmed when" ~>
      Multichoice
        [              
          "it is not losing or gaining any height"
        , "the wings are level and the 'skid ball' is centred"
        , "the trim position indicator indicates zero"
        ]

        "the required nose attitude is maintained even after the controls are released"

        [
        ]
    , "As airspeed decreases [eg during the float before touch-down]," ~>
      Multichoice
        [              
          "forward visibility improves"
        ]

        "a larger degree of control deflection is required to maintain a given attitude"

        [
          "a given attitude can be maintained with less control deflection"
        , "full back elevator should be applied to prevent the aircraft sinking"
        ]
    , "The most important recovery action required if an aircraft has entered a spiral dive is" ~>
      Multichoice
        [              
        ]

        "level the wings"

        [
          "lower the nose to increase the airspeed"
        , "apply full power to assist the recovery"
        , "pull firmly back on the elevator controls to raise the nose above the horizon"
        ]
    , "If airspeed has been allowed to decay to the point where the pilot suspects that a stall is imminent, the most important actions required are" ~>
      Multichoice
        [              
          "apply full power and lift the nose"
        , "lower the nose and reduce the power"
        , "reduce the power and lift the nose"
        ]

        "add power and lower the nose"

        [
        ]
    , "In the event of an engine failure during the climb-out after take-off in a single engine aircraft, the most important immediate action required is" ~>
      Multichoice
        [              
        ]

        "lower the nose to maintain a safe airspeed"

        [
          "raise the nose to maintain a safe airspeed"
        , "commence a turn back towards the field"
        , "maintain the nose attitude to prevent an excessive build-up of airspeed"
        ]
    , "A change in which of the following factors has no effect on the stalling IAS of an aeroplane?" ~>
      Multichoice
        [              
          "weight"
        ]

        "wind"

        [
          "power"
        , "flap"
        ]
    , "If it becomes necessary to commence a go-around during a landing approach, which of the following is the correct sequence of actions to ensure a safe transition to the climb?" ~>
      Multichoice
        [              
          "raise the flap, raise the nose to the climbing attitude and apply full power"
        , "raise the nose to the climbing attitude, raise the flap and apply full power"
        ]

        "apply full power while raising the nose to the climbing attitude and raise the flap"

        [
          "raise the flap while applying full power then raise the nose to the climbing attitude"
        ]        
    , "Which of the following combination of symptoms would indicate an approaching stall during final approach to land?" ~>
      Multichoice
        [              
        ]

        "low airspeed, sluggish control response and high nose attitude"

        [
          "low power setting, high approach path and low nose attitude"
        , "low airspeed and low nose attitude"
        , "flatter than normal approach and high power setting"
        ]        
    , "If an aircraft which is about to stall suffers a wing drop, the pilot should" ~>
      Multichoice
        [              
        ]

        "lower the nose, keep straight with rudder and use no aileron until the speed increases"

        [
          "lower the nose, keep straight with aileron and use no rudder until the speed increases"
        , "raise the nose and apply opposite aileron"
        , "raise the nose, keep straight with rudder and use no aileron until the speed increases"
        ]
    , "During take-off in conditions of no wind, it is normally necessary to apply right rudder to maintain a straight take-off path. This is necessary because of" ~>
      Multichoice
        [              
          "the propeller slipstream pushing on the left side of the rudder"
        , "engine torque pushing the left wheel harder onto the runway surface"
        , "the lower airspeed over the ailerons during the take-off run"
        ]

        "Both the propeller slipstream pushing on the left side of the rudder and engine torque pushing the left wheel harder onto the runway surface"

        [
        ]
    , "Compared to a normal approach, during a flapless approach at a given IAS the pilot will notice" ~>
      Multichoice
        [              
        ]

        "a higher nose attitude and a flatter approach path"

        [
          "a lower nose attitude and a flatter approach path"
        , "a higher nose attitude and a steeper approach path"
        , "a lower attitude and a steeper approach path"
        ]
    , "During an approach on a windy day the pilot notices that the windsock is indicating frequent changes in surface wind speed and direction. In these conditions the pilot should" ~>
      Multichoice
        [              
          "approach at a lower IAS and higher nose attitude than normal"
        , "be prepared to add power to restore IAS if wind shear is encountered"
        , "be prepared to lower the nose to restore IAS if wind shear is encountered"
        ]

        "be prepared to add power and lower the nose to restore IAS if wind shear is encountered"

        [
        ]        
    , "During a glide approach in no wind at the recommended gliding IAS, raising the nose slightly will result in a slower approach speed but a flatter approach angle" ~>
      Multichoice
        [
          "the statement is true"              
        ]

        "the statement is false"

        [
        ]
    , "The indication on the balance indicator *with ball to the left of centre* would require the pilot to" ~>
      Multichoice
        [              
          "apply more right aileron"
        , "apply more right rudder"
        , "apply more left aileron"
        ]

        "apply more left rudder"

        [
        ]
    , "If the aerodrome ground marker *with two adjacent unfilled plus signs* is displayed near the wind sock of an uncontrolled aerodrome it indicates that" ~>
      Multichoice
        [              
        ]

        "gliding operations are in progress"

        [
          "the aerodrome is closed to all operations"
        , "all unsealed areas are unserviceable"
        , "the aerodrome is suitable for light aircraft only"
        ]        
    , "By convention the downwind leg of the circuit for piston engine aeroplanes is flown at a height of" ~>
      Multichoice
        [              
          "1000ft AMSL"
        , "1500ft AMSL"
        ]

        "1000ft AGL"

        [
          "1500ft AGL"
        ]
    , "What is the minimum height at which an aeroplane may commence a turn in the direction of the circuit after take-off" ~>
      Multichoice
        [
          "1000ft AGL"
        , "1500ft AGL"              
        ]

        "500ft AGL"

        [
          "200ft AGL"
        ]
    , "If the pilot sets the QNH while on the ground at an aerodrome, the altimeter will read" ~>
      Multichoice
        [              
          "height above the aerodrome"
        ]

        "height above sea-level"

        [
          "height in the standard atmosphere"
        , "height above terrain"
        ]        
    , "You are lining up to take off behind another aircraft which has just landed on the same runway. You may not commence your take off run until that aircraft has" ~>
      Multichoice
        [              
          "finished its landing run and stopped"
        , "commenced a turn towards a taxiway exit"
        ]

        "vacated the runway and is taxiing away"

        [
          "passed a point 600m ahead of your present position"
        ]        
    , "A landing aeroplane must be established on a straight final approach path by" ~>
      Multichoice
        [              
          "500 metres from the aerodrome boundary"
        ]

        "500 feet AGL"
        
        [
          "800 feet AGL"
        , "1500 metres from the aerodrome boundary"
        ]
    , "A taxiway leading into a runway *has a transverse solid white line and immediately subsequent transverse dashed white line*. What is the significance of the solid and dashed white lines across the taxiway?" ~>
      Multichoice
        [              
        ]

        "Aircraft taxiing in *that direction* must hold short and check before entering the runway"

        [
          "No significance providing you are taxiing in the direction of the arrow"
        , "Aircraft vacating the runway must hold at this point and check that the taxiway is clear"
        , "Runway edge marker -- no significance to taxiing aircraft"
        ]
    , "You observe another aircraft approaching head on at your level. The correct action to take is" ~>
      Multichoice
        [              
          "dive to pass beneath the approaching aircraft"
        , "climb to pass above the approaching aircraft"
        , "turn to your left toavoid the approaching aircraft"
        ]

        "turn to your right to avoid the approaching aircraft"

        [
        ]
    , "After you turn onto final approach at an uncontrolled aerodrome you notice another aircraft below you and slightly ahead of you on approach to the same runway. You should" ~>
      Multichoice
        [              
          "continue the approach because the lower aircraft must give way"
        , "descend beneath the other aircraft and land to one side of it"
        , "remain above the other aircraft and land to one side of it"
        ]

        "commence a go-around and give way to the lower aircraft"

        [
        ]
    , "You intend to fly in the morning at 6.00 am. What is the latest time at which you may have an alcoholic drink prior to this flight?" ~>
      Multichoice
        [              
          "midnight before the flight"
        ]

        "10pm on the evening before the flight"

        [
          "5.00 am on the morning of the flight"
        , "24 hours before the proposed flight"
        ]
    , "The top of the white arc on an Airspeed Indicator marks" ~>
      Multichoice
        [              
          "the lowest IAS at which an approach to land should be made"
        , "the highest IAS at which the aircraft should be permitted to remain on the ground during the take-off run"
        , "the IAS which will produce the best rate of climb"
        ]

        "the maximum IAS at which flaps should be lowered"

        [
        ]
    , "An aircraft is taxiing toward you on a taxiway. What is the correct procedure to adopt to pass the other aircraft?" ~>
      Multichoice
        [              
          "move to the left side of the taxiway"
        ]

        "move to the right side of the taxiway"

        [
          "the smaller aircraft should pull off the taxiway and allow the bigger one to pass"
        , "move to whichever side of the taxiway is closest to you at the time"
        ]        
    , "When must the flying controls be checked for full, free and correct movement?" ~>
      Multichoice
        [              
          "immediately before start-up"
        , "immediately after start-up"
        , "before taxiing commences"
        ]

        "immediately before take-off"

        [
        ]
    , "Find the following about your aircraft engine:\n  * What make or model is it?\n  * How many cylinders does it have?\n  * What is its capacity and rated power output?\n  * Does it have a carburettor or is it fuel injected?\n  * What is the recommended oil level before start-up?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the significance of the red line on the tachometer?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the recommended procedure to adopt in the event of an engine fire?\n  * On the ground\n  * In flight" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the recommended IAS for:\n  * Lift off\n  * Normal Climb\n  * Best Rate of Climb\n  * Normal Approach\n  * Flapless Approach\n  * Glide Approach\n  * Stall with flap\n  * Stall without flap" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What should you look for after start-up on the:\n  * Tachometer\n  * Oil Pressure Gauge\n  * Oil Temperature Gauge\n  * Vacuum Gauge\n  * Ammeter" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What RPM should be used to check the magnetos?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the maximum allowable drop in RPM when one magneto is selected?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the maximum difference between the RPM indicated on each magneto?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What might be wrong if there *no* RPM drop?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What should you see when you select carburettor heat?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What would it mean if the selection of carburettor heat had no effect?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "When should carburettor heat be used in flight?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What RPM would you normally expect to see during the take-off run?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What would you do if you noticed the RPM significantly below *the normally expected during the take-off run*?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What type of oil does your aeroplane take?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What type of fuel does your aeroplane take? How much does it hold?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the normal fuel consumption in Litres per Hour?" ~>
      Multichoice
        [              
        ]

        "**RESEARCH THIS QUESTION FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    ]

taitPreAreaSolo ::
  Exam
taitPreAreaSolo =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Pre-Area Solo Practice Exam")
    [
      "The point about which pitch, roll and yaw occur during flight is the" ~>
      Multichoice
        [
          "centre of pressure"
        , "wing roots"
        , "the mid point between the nose and the tail"
        ]

        "the centre of gravity"

        [
        ]
    , "The location of the centre of gravity of an aeroplane" ~>
      Multichoice
        [
          "is a fixed point determined by the manufacturer"
        ]

        "depends upon the load distribution within the aircraft"

        [
          "remains constant during any one flight"
        , "moves forward if weight is added to the rear"
        ]
    , "The induced drag acting on aircraft in straight and level flight is" ~>
      Multichoice
        [
        ]

        "greatest when speed is low and the aircraft is heavy"

        [
          "greatest when speed is high and the aircraft is heavy"
        , "greatest when speed is low and the aircraft is light"
        , "greatest when speed is high and the aircraft is light"
        ]
    , "Which of the following would increase the parasite drag on an aircraft?" ~>
      Multichoice
        [
          "raising the flaps"
        , "retracting the undercarriage"
        ]

        "increasing the IAS"

        [
          "decreasing the IAS"
        ]
    , "The least amount of total drag in level flight occurs" ~>
      Multichoice
        [
          "at the high end of the speed range at about cruising speed"
        , "when the aircraft is flying as slowly as possible"
        ]

        "at a speed about equal to the best gliding speed, with the flaps retracted"

        [
          "at a speed about equal to the best gliding speed, with the flaps extended"
        ]
    , "The IAS which procudes the best gliding performance in still air is" ~>
      Multichoice
        [
          "the IAS which produces maximum lift"
        ]

        "the IAS which produces the best lift/drag ratio"

        [
          "the IAS which requires least control deflection"
        , "the IAS which results from a level nose attitude"
        ]
    , "If flap is lowered while maintaining level flight at constant power" ~>
      Multichoice
        [
        ]

        "the nose attitude will be lower and the resulting IAS will be lower"

        [
          "the nose attitude will be higher and the resulting IAS will be lower"          
        , "the nose attitude will be lower and the resulting IAS will be higher"
        , "the nose attitude will be higher and the resulting IAS will be higher"
        ]
    , "If two aircraft identical in every respect except for weight are flying at the same height and IAS," ~>
      Multichoice
        [
        ]

        "the heavier aircraft will require a higher nose attitude and more power"

        [
          "the heavier aircraft will require a lower nose attitude and more power"
        , "the heavier aircraft will require a lower nose attitude and less power"
        , "both aircraft will require the same nose attitude and power"
        ]
    , "Which of the following factors will increase the angle of climb of an aircraft?" ~>
      Multichoice
        [
          "an increase in weight"        
        ]

        "an increase in headwind"

        [
          "the use of full flap"
        , "climbing at the slowest possible speed will full power"
        ]
    , "To achieve the best rate of climb in any given wind the climb should be made" ~>
      Multichoice
        [
          "at an IAS lower than that required for best angle with full power applied"
        , "at the same IAS as is required for best angle, but with less than full power"
        , "at an IAS higher than that required for best angle, but with less than full power"
        ]

        "at an IAS higher than that required for best angle with full power applied"

        [
        ]
    , "What effect will turning have on climb performance?" ~>
      Multichoice
        [
          "the angle of climb will decrease but the rate of climb will increase"
        ]

        "both angle and rate of climb with decrease"

        [
          "both angle and rate of climb with increase"
        , "the angle of climb will increase but the rate of climb will decrease"
        ]
    , "An aircraft is flying level at 5000 feet at constant power setting in a tail wind. Which of the following is true?" ~>
      Multichoice
        [
        ]

        "the IAS is less than the TAS"

        [
          "the IAS is higher than the TAS"
        , "the IAS is greater than the GS"
        , "the TAS is higher than the GS"
        ]
    , "An aircraft is carrying out a level balanced turn at 60 degrees angle of bank. Which of the following would be correct?" ~>
      Multichoice
        [
          "lift is equal to weight"
        ]

        "lift is twice the weight"

        [
          "lift is three times weight"
        , "lift is 1.4 times weight"
        ]
    , "An aircraft stalls at 50 KIAS in level flight at a particular weight and power setting. At the same weight and power setting, in a 60 degree bank level turn the stalling speed would be closest to" ~>
      Multichoice
        [
        ]

        "70 KIAS"

        [
          "50 KIAS"
        , "60 KIAS"
        , "100 KIAS"
        ]        
    , "The elevator control is abruptly pulled to te fully aft position to induce a stall" ~>
      Multichoice
        [
        ]

        "the stall will occur at a higher speed than normal"

        [
          "the stall will occur at a lower speed than normal"
        , "the stall will occur at the same speed but a higher angle of attack than normal"
        , "the stall will occur at the same speed and same angle of attack as normal"
        ]
    , "Which of the following factors has no effect on the stalling IAS of an aircraft?" ~>
      Multichoice
        [
          "weight"
        , "load factor"
        , "power"
        ]

        "height"

        [
        ]                
    , "Where in relation to a heavy aircraft will wake turbulence be worst?" ~>
      Multichoice
        [
          "behind and above the heavy aircraft"
        ]

        "behind and below the heavy aircraft"

        [
          "in front of and below the heavy aircraft"
        , "in front of anf above the heavy aircraft"
        ]                
    , "Which of the following control difficulties is most likely to be associated with an encounter with wake turbulence?" ~>
      Multichoice
        [
          "a violent pitch"
        , "a violent pitch and yaw"
        , "a violent yaw"
        ]

        "a violent roll"

        [
        ]                
    , "The best way to avoid the wake turbulence behind a heavy aircraft which has just landed is to" ~>
      Multichoice
        [
          "aim to touch down at point before the touch-down point of the heavy aircraft"
        , "aim to touch down at the same point as the heavy aircraft"
        ]

        "aim to touch down at a point ahead of the touch-down point of the heavy aircraft"

        [
          "aim to touch down behind the heavy aircraft but on one side of the runway"
        ]                
    , "Which of the following would result in a decrease in the rate of climb?" ~>
      Multichoice
        [
        ]

        "a reduction in engine power output"

        [
          "an increase in headwind component"
        , "a decrease in headwind component"
        , "a reduction in gross weight"
        ]                
    , "The purpose of a carburettor is" ~>
      Multichoice
        [          
        ]

        "to mix the correct amount of air with fuel to ensure efficient combustion"

        [
          "to remove any dust particles from the air on its way to the engine"
        , "to remove any water from the fuel before it can get to the engine"
        , "to cool the air on its way to the engine to prevent detonation"
        ]                
    , "Under what circumstances can carburettor ice form?" ~>
      Multichoice
        [
          "only when the outside air temperature is below zero"
        , "only if the aircraft is flying in rain or drizzle"
        ]

        "only at low power settings in cold moist air"

        [
          "over a very wide range of temperatures, both inside and outside of cloud"
        ]                
    , "Under which of the following circumstances would carburettor ice be difficult to detect?" ~>
      Multichoice
        [
          "at full power during take-off"
        , "during a long climb"
        ]

        "during a long descent at low power"

        [
          "during cruise just below a cloud base"
        ]                
    , "What is the correct method of using carburettor heat during a long power-off glide?" ~>
      Multichoice
        [
          "carburettor heat should be applied only if carburettor ice is suspected"
        ]

        "carburettor heat should be applied before the throttle is closed and it should be left on during the glide"

        [
          "carburettor heat should be applied just before power is re-applied at the end of the glide"
        , "carburettor heat should be applied regularly for brief intervals during the glide"
        ]                
    , "Carburettor icing during cruise will be indicated to the pilot by" ~>
      Multichoice
        [
        ]

        "a drop in RPM, a reduction in cruise speed and rough running"

        [
          "no change in RPM, but a reduction in cruise speed and a height loss"
        , "the engine back firing when power is increased"
        , "increasing RPM even though there has been no change in the throttle position"
        ]
    , "If an aircraft fitted with a carburettor and a gravity feed fuel system has a blockage in the fuel tank vent" ~>
      Multichoice
        [
          "there will be less power available during take-off"
        , "the engine will be very difficult to start"
        ]

        "the engine may fail during flight due to fuel starvation"

        [
          "the engine may run roughly due to an excessively rich mixture"
        ]
    , "Detonation in an aircraft engine is most likely when" ~>
      Multichoice
        [
        ]

        "both the engine and fuel are hot and high power is applied"

        [
          "full power is applied during take-off with the mixture fully rich"
        , "the throttle is opened after a long descent"
        , "the aircraft is cruising at high power for a long period of time"
        ]
    , "If detonation is suspected during a long climb you should" ~>
      Multichoice
        [
          "reduce the power and raise the nose to climb at a lower speed"
        , "increase the power and raise the nose to climb at the same airspeed"
        , "reduce the power and raise the nose to increase rate of climb"
        ]

        "reduce the power, check that the mixture is fully rich and lower the nose to climb at a higher airspeed"

        [
        ]
    , "Which of the following instruments would be affected by a failure of the vaccuum pump?" ~>
      Multichoice
        [
          "the altimeter"
        , "the vertical speed indicator"
        , "the airspeed indicator"
        ]

        "the artificial horizon"

        [
        ]
    , "If the altimeter fails to indicate any height change after take-off the most likely cause is" ~>
      Multichoice
        [
          "the vaccuum pump has failed"
        , "the pitot tube is blocked"
        ]

        "the static vent is blocked"

        [
          "the incorrect QNH was set before take-off"
        ]
    , "If your aircraft was accidentally refuelled with jet fuel [AVTUR] instead of the correct type of fuel" ~>
      Multichoice
        [
          "you would notice a big increase in power during take-off"
        ]

        "you would experience a significant power loss accompanied by rough running, detonation and high engine temperatures"

        [
          "there would be no particular effect as long as the tank contains less than half AVTUR"
        , "there would be a slight reduction in power but you could continue with the proposed flight"
        ]
    , "If you take off from an uncontrolled airstrip which has left hand circuits, what is the earliest point after take-off at which you may turn right?" ~>
      Multichoice
        [
          "500 feet AGL"
        , "500 metres from the aerodrome boundary"
        , "1000 feet AGL"
        ]

        "1500 feet AGL or 3 nautical miles from the aerodrome boundary"

        [
        ]
    , "The lowest height at which you may fly in the training area is" ~>
      Multichoice
        [
        ]

        "1000 ft over a town or populous area otherwise 500ft"

        [
          "1500 ft over a town or populous area otherwise 500ft"
        , "500 ft over a town or populous area otherwise 1000ft"
        , "200 ft provided you are practising forced landings"
        ]
    , "With regard to the end of daylight, you must leave the training area in time to land" ~>
      Multichoice
        [
          "30 minutes before the end of daylight"
        ]

        "10 minutes before the end of daylight"

        [
          "by the end of daylight"
        , "5 minutes after the end of daylight"
        ]
    , "When turning from upwind to downwind at low level on a very windy day, you may experience the illusion of" ~>
      Multichoice
        [
          "skidding out"
        ]

        "slipping in"

        [
          "overbanking"
        , "underbanking"
        ]
    , "Where may a student pilot **not** fly solo?" ~>
      Multichoice
        [
          "beyond the circuit area of the aerodrome"
        ]

        "beyond circuit area of the aerodrome or beyond the associated training area"

        [
          "within the training area"
        , "flying the most direct route between the training area and the aerodrome"
        ]
    , "What is the maximum number of consecutive solo hours that a student pilot may fly?" ~>
      Multichoice
        [
          "2"
        , "10"
        ]

        "3"

        [
          "5"
        ]
    , "When must a fuel inspection be carried out?" ~>
      Multichoice
        [
          "before every flight"
        , "after each refuelling"
        , "before the first flight of each day"

        ]

        "after each refuelling and before the first flight of the day"

        [
        ]
    , "When operating at 2000 ft AMSL in your local training area, which of the following would be the minimum requirement for maintaining VMC?" ~>
      Multichoice
        [
          "500 ft below the cloud base and visibility of 8 km"
        , "500 ft below the cloud base and visibility of 5 km"
        ]

        "clear of cloud and visibility of 5 km"

        [
          "clear of cloud with no special requirement for visibility"
        ]
    , "What is the longest time since your last flight which would allow you to fly solo today without a dual check?" ~>
      Multichoice
        [
          "7 days"
        , "30 days"
        , "90 days"
        ]

        "14 days"

        [
        ]        
    , "State the IAS you would use for climb-out and approach at a short field\n  * Short field take-off climb-out speed\n  * Short field landing approach speed" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "Answer the following questions with regard to the fuel system in your aeroplane.\n  * Does it have a carburettor or is it fuel-injected?\n  * Is the system gravity fed or is it pump fed?\n  * Would your fuel system continue to operate if the aeroplane were upside-down?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "[Electronics]\n  * What voltage is your electrical system?\n  * What indication would you have if the alternator failed in flight?\n  * What would you do if the alternator failed in flight?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "List the services you would lose if you suffered a loss of all electrical power." ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "[Brakes]\n  * What would be the symptoms of air in the brake lines?\n  * A leak in the brake lines?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "what is the minimum amount of fuel that must be on board your aircraft at start-up if you intend to carry out a one hour flight in the training area?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What altimeter setting should be used for a flight in the training area?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the IAS and flap setting that would provide best gliding range in your aircraft?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "While practising steep turns in your aircraft you notice that the airspeed is rapidly increasing past the green arc and the nose is dropping below the horizon in spite of your attempts to raise it by applying back elevator. List the actions, in order that you would take to recover from this situation." ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    ]

exams ::
  [Exam]
exams =
  [
    atcPreCircuit
  , atcPreArea
  , atcBAK
  , taitPreSolo
  , taitPreAreaSolo
  ]

main ::
  IO ()
main =
  do a <- getArgs
     let action = case a of
                    [] -> markdownExams
                    (_:_) -> flashcardExams
     putStrLn (action exams)   
