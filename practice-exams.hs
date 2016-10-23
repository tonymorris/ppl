module Main where

import Control.Monad.State
import Data.List
import Data.Monoid
import Data.String
import System.Environment

data Answer a =
  DirectAnswer a
  | DirectListAnswer [a]
  | Multichoice [a] a [a]
  deriving (Eq, Ord, Show)

instance Functor Answer where
  fmap f (DirectAnswer a) =
    DirectAnswer (f a)
  fmap f (DirectListAnswer a) =
    DirectListAnswer (fmap f a)
  fmap f (Multichoice l x r) =
    Multichoice (fmap f l) (f x) (fmap f r)

instance Foldable Answer where
  foldr f z (DirectAnswer a) =
    f a z
  foldr f z (DirectListAnswer a) =
    foldr f z a
  foldr f z (Multichoice l x r) =
    foldr f (f x (foldr f z r)) l

instance Traversable Answer where
  traverse f (DirectAnswer a) =
    DirectAnswer <$> f a
  traverse f (DirectListAnswer a) =
    DirectListAnswer <$> traverse f a
  traverse f (Multichoice l x r) =
    Multichoice <$> traverse f l <*> f x <*> traverse f r

choiceList ::
  Answer a
  -> [a]
choiceList (DirectAnswer a) =
  [a]
choiceList (DirectListAnswer a) =
  a
choiceList (Multichoice l x r) =
  l <> (x : r)

onFocus ::
  (a -> a)
  -> Answer a
  -> Answer a
onFocus f (DirectAnswer a) =
  DirectAnswer (f a)
onFocus f (DirectListAnswer a) =
  DirectListAnswer (fmap f a)
onFocus f (Multichoice l x r) =
  Multichoice l (f x) r

number ::
  Traversable t =>
  t a ->
  t (Char, a)
number q =
  evalState (traverse (\x -> state (\c -> ((c, x), succ c))) q) 'a'

focus ::
  Answer a
  -> [a]
focus (DirectAnswer a) =
  [a]
focus (DirectListAnswer a) =
  a
focus (Multichoice _ x _) =
  [x]

data Question a =
  Question
    String -- question
    (Answer String)
    a
  deriving (Eq, Ord, Show)

data Manual =
  Part61
  | PrePart61
  | NoManual
  deriving (Eq, Ord, Show)

(~>) ::
  String
  -> Answer String
  -> Question Manual
(~>) s q =
  Question s q Part61

infixr 5 ~>

(!>) ::
  String
  -> Answer String
  -> Question Manual
(!>) s q =
  Question s q PrePart61

infixr 5 !>

(!-) ::
  String
  -> Answer String
  -> Question Manual
(!-) s q =
  Question s q NoManual

infixr 5 !-

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
        let markdownSource =  case x of
                                Part61 -> q
                                PrePart61 -> "~~" <> q <> "~~"
                                NoManual -> q
        in  show n <> ". " <> markdownSource <> "\n" <> 
              case m of                
                Multichoice l z r ->
                  zipWith (\n c -> "  " <> show n <> ". " <> c) [1..] (l ++ ("**" ++ z ++ "**") : r) >>= (<> "\n")
                DirectAnswer a ->
                  "\n  **" ++ a ++ "**\n"
                DirectListAnswer a ->
                  "\n" ++ ((\q -> "* **" ++ q ++ "**\n") =<< a) ++ "\n"
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
        let flashcardSource = case x of
                                Part61 -> q
                                PrePart61 -> "~~" <> q <> "~~"
                                NoManual -> q
        in  replicate 40 '-' <> "\n\n" <> flashcardSource <> "\n" <>
              case m of
                Multichoice _ _ _ ->
                  let z = number m
                      display :: (Char, String) -> String
                      display (c, s) = c : (". " <> s)
                      displayN :: [(Char, String)] -> String
                      displayN x = x >>= display
                      rr = focus z
                  in  (intercalate "\n" . choiceList . fmap display $ z) <>
                      "\n\n" <>
                      displayN (focus z) <>
                      "\n\n"
                DirectAnswer a ->
                  "\n" ++ a ++ "\n"
                DirectListAnswer a ->
                  "\n" ++ intercalate "\n" a ++ "\n"
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

          "be completed not closer than 500 metres from the aerodrome perimeter."

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
    , "The IAS which produces the best gliding performance in still air is" ~>
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

        "both angle and rate of climb will decrease"

        [
          "both angle and rate of climb will increase"
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
    , "The elevator control is abruptly pulled to the fully aft position to induce a stall" ~>
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
    , "Which of the following instruments would be affected by a failure of the vacuum pump?" ~>
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
          "the vacuum pump has failed"
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

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "Answer the following questions with regard to the fuel system in your aeroplane.\n  * Does it have a carburettor or is it fuel-injected?\n  * Is the system gravity fed or is it pump fed?\n  * Would your fuel system continue to operate if the aeroplane were upside-down?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "[Electronics]\n  * What voltage is your electrical system?\n  * What indication would you have if the alternator failed in flight?\n  * What would you do if the alternator failed in flight?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "List the services you would lose if you suffered a loss of all electrical power." ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "[Brakes]\n  * What would be the symptoms of air in the brake lines?\n  * A leak in the brake lines?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the minimum amount of fuel that must be on board your aircraft at start-up if you intend to carry out a one hour flight in the training area?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What altimeter setting should be used for a flight in the training area?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the IAS and flap setting that would provide best gliding range in your aircraft?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "While practising steep turns in your aircraft you notice that the airspeed is rapidly increasing past the green arc and the nose is dropping below the horizon in spite of your attempts to raise it by applying back elevator. List the actions, in order that you would take to recover from this situation." ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "List all the radio calls you would make from start-up to shut-down for a flight to and from your local training area." ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    ]

taitPreRPL ::
  Exam
taitPreRPL =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Pre-RPL Practice Exam")
    [
      "The purpose of streamlining and fairings in aircraft design is to" ~>
      Multichoice
        [
          "reduce induced drag"
        ]

        "reduce parasite drag"

        [
          "increase lift"
        , "increase weight"
        ]
    , "Mass imbalances should be inspected before flight. The loss of a mass balance could result in " ~>
      Multichoice
        [
          "the controls becoming too heavy to move"
        ]

        "flutter of the control surface and vibration in the control column"

        [
          "unpredictable response to control inputs"
        , "the controls jamming in one position"
        ]
    , "If a trim cable failed during flight, making movement of the elevator trim tab impossible" ~>
      Multichoice
        [
          "the aircraft would become uncontrollable"
        , "the elevator control surface would become over sensitive"
        ]

        "a strong forward or back pressure may be required to maintain a given nose attitude"

        [
          "the aircraft nose would pitch nose-up"
        ]
    , "Which of the following factors would decrease the length of the take-off run?" ~>
      Multichoice
        [
          "a decrease in headwind component"
        , "applying full flap for take-off"
        , "an increase in air temperature"
        ]

        "a down-hill slope for take-off"

        [
        ]
    , "Which of the following conditions would cause the greatest increase in landing distance?" ~>
      Multichoice
        [
        ]

        "approaching at a higher IAS than normal"

        [
          "landing at maximum weight"
        , "landing on an up-hill slope"
        , "raising the flaps immediately after touch-down"
        ]
    , "An aircraft cannot enter a spin unless" ~>
      Multichoice
        [
          "it is over its maximum take-off weight"
        ]

        "the pilot allows it to stall"

        [
          "the centre of gravity is beyond the aft limit"
        , "the pilot has allowed the angle of bank to become too steep"
        ]
    , "Which of the following is the most vital action required following an engine failure just after take-off?" ~>
      Multichoice
        [
          "raise the nose to maintain height"
        ]

        "lower the nose to maintain airspeed"

        [
          "immediately commence a turn back towards the field"
        , "apply full flap to reduce the rate of descent"
        ]
    , "In a four-stroke internal combustion engine the valves open when the push rod pushes against the" ~>
      Multichoice
        [
          "valve spring"
        , "connecting rod"
        ]

        "rocker arm"

        [
          "crankshaft"
        ]
    , "An exhaust gas temperature gauge is designed to respond to changes in" ~>
      Multichoice
        [
          "throttle position"
        , "air density"
        , "RPM"
        ]

        "mixture strength"

        [
        ]
    , "Black smoke coming from the exhaust pipe of a taxiing aircraft would be an indication of" ~>
      Multichoice
        [
        ]

        "mixture too rich"

        [
          "mixture too lean"
        , "carburettor heat is left on"
        , "an oil leak"
        ]
    , "Which of the following is true of a fuel-injected engine?" ~>
      Multichoice
        [
          "it cannot become flooded during start-up"
        , "it does not need a mixture control"
        ]

        "it does not need a carburettor"

        [
          "it can only be used in a gravity fed system"
        ]
    , "A thorough warm up period before take-off is essential because" ~>
      Multichoice
        [
        ]

        "the engine oil must be brought to its correct viscosity to ensure proper lubrication"

        [
          "it takes time for the oil pressure to reach its correct value"
        , "impurities must be removed by the oil filter before full power is applied"
        , "oil is too thin when it is cold and it must be thickened by heating"
        ]
    , "Which of the following would indicate the most serious engine problem, requiring an immediate landing" ~>
      Multichoice
        [
          "oil temperature gauge indicating zero"
        , "oil temperature gauge indicating in the top end of the green arc"
        , "oil pressure gauge on the red line on the high end of the scale"
        ]

        "oil pressure gauge indicating zero"

        [
        ]
    , "If an engine is allowed to overspeed [RPM past the red line], engine damage could result because of" ~>
      Multichoice
        [
          "excessive cooling by the high speed airflow"
        , "excessive heating due to the increased power"
        ]

        "internal stresses on the bearings and valves"

        [
          "excessively lean mixture due to the increased air flow"
        ]
    , "The electrical current which is delivered to the spark plugs in an aircraft engine comes from" ~>
      Multichoice
        [
          "the battery"
        ]

        "magnetos"

        [
          "the alternator"
        , "the voltage regulator"
        ]
    , "When the magnetos are checked prior to take-off, there is a slight RPM drop when the switches are selected from 'BOTH' to 'RIGHT'. This indicates that" ~>
      Multichoice
        [
        ]

        "the left magneto is off and the engine is running on the right magneto only"

        [
          "the right magneto is off and the engine is running on the left magneto only"
        , "both magnetos are operating, but the right one is weak"
        , "there is a fault in the magneto switch and it should be reported"
        ]
    , "During a magneto check prior to take-off, there is *no* RPM drop when the switches are selected from 'BOTH' to 'LEFT' or from 'BOTH' to 'RIGHT'. This indicates that" ~>
      Multichoice
        [
          "both magnetos are in perfect condition"
        ]

        "there is a dangerous fault in the switch and it should be reported"

        [
          "all cylinders are completely free of oil fouling"
        , "all spark plugs are functioning perfectly"
        ]
    , "The propeller of an inoperative engine should never be handled by an inexperienced person because" ~>
      Multichoice
        [
          "the engine should never be moved when there is no oil pressure"
        , "the ignition timing could be spoilt"
        , "fuel vapour could escape from the cylinders"
        ]

        "there is always a possibility that a magneto could be 'live'"

        [
        ]
    , "The normal reading for a centre-zero ammeter during flight is" ~>
      Multichoice
        [
          "zero"
        ]

        "a slight charge"

        [
          "a slight discharge"
        , "variable depending on the number of electrical loads turned on"
        ]
    , "The purpose of a fuse or circuit breaker is to" ~>
      Multichoice
        [
        ]

        "protect the equipment from too much current"

        [
          "stop current flowing in the circuit when the equipment is turned off"
        , "ensure the correct voltage exists in the circuit"
        , "to convert direct current to alternating current"
        ]
    , "Which of the following is true of a magnetic compass?" ~>
      Multichoice
        [
          "the compass always reads the true direction in which the aircraft is pointing"
        , "the compass always reads correctly, even when the aircraft is turning"
        ]

        "errors can be induced in the compass if magnetic materials are placed close by"

        [
          "the reading of a compass should be checked for accuracy against the directional gyro before take-off"
        ]
    , "The sea-level pressure in the international standard atmosphere is normally expressed as" ~>
      Multichoice
        [
        ]

        "1013hPa"

        [
          "29.9 Hg\""
        , "14.7 psi"
        , "QNH"
        ]
    , "You are flying at 3000 ft AMSL. The pressure of the air around you is" ~>
      Multichoice
        [
          "1000 hPa"
        ]

        "100 hPa less than that at sea-level"

        [
          "100hPa"
        , "10 hPa less than that at sea-level"
        ]
    , "The term 'QNH' is used to indicate" ~>
      Multichoice
        [
          "the atmospheric pressure of the ambient air"
        , "the atmospheric pressure of the air at sea-level in the standard atmosphere"
        ]

        "the atmospheric pressure of the air at sea-level at any given time or place"

        [
          "the atmospheric pressure at the airfield"
        ]
    , "A field has an elevation of 1200ft. If the QNH is 1022 hPa, what is the pressure height?" ~>
      Multichoice
        [
          "1200 ft"
        , "1470 ft"
        ]

        "930 ft"

        [
          "1500 ft"
        ]
    , "Which of the following would increase the take-off distance required?" ~>
      Multichoice
        [
        ]

        "an increase in air temperature"

        [
          "a decrease in air temperature"
        , "a decrease in pressure height"
        , "an increase in headwind component"
        ]
    , "The take-off distance required is defined as the distance" ~>
      Multichoice
        [
          "from the beginning of the take-off run to lift-off"
        , "from lift-off to reaching a height of 50 ft"
        , "from the beginning of the take-off run to passing through the stalling IAS"
        ]

        "from the beginning of the take-off run to reaching a height of 50 ft"

        [
        ]
    , "The zero fuel weight [ZFW] of an aircraft is" ~>
      Multichoice
        [
          "the maximum weight at which it can safely take-off"
        ]

        "the weight of the fully-loaded aircraft without fuel"

        [
          "the weight of the aircraft without passengers or fuel"
        , "the weight of the aircraft without passengers, crew, cargo or fuel"
        ]
    , "Taking off up-hill may" ~>
      Multichoice
        [
          "have no significant effect providing there is no wind"
        ]

        "significantly increase the take-off distance required"

        [
          "assist take-off because some height is being gained during the take-off run"
        , "improve directional control during take-off"
        ]
    , "Taking off down-wind greatly increases the take-off distance required. It also" ~>
      Multichoice
        [
        ]

        "greatly decreases the ability to clear obstacles during the climb after take-off"

        [
          "reduces the effectiveness of the aircraft's controls after take-off"
        , "requires the use of a higher IAS during the climb after take-off"
        , "reduces the engine power available during the take-off run"
        ]
    , "Refer to the take-off chart in your supplement. [Bob Tait RPL Study Guide supplement](https://github.com/tonymorris/ppl/raw/master/handouts/RPL_Supplement.pdf). Given that Airfield pressure height = 2000ft, Outside air temperature = 20 degrees Celsius, TODA = 600 metres, Surface = short wet grass, Slope = level, Wind = 10 kt headwind, The maximum take-off weight permitted by the chart is closest to" ~>
      Multichoice
        [
        ]

        "925 kg"

        [
          "1060 kg"
        , "1090 kg"
        , "850 kg"
        ]
    , "Refer to the take-off chart in your supplement. [Bob Tait RPL Study Guide supplement](https://github.com/tonymorris/ppl/raw/master/handouts/RPL_Supplement.pdf). Given that Airfield pressure height = 1500ft, Outside air temperature = 30 degrees Celsius, Surface = short dry grass, Wind = calm, Slope = 1% down, Take off weight = 1000kg, The take-off distance required by the chart is closest to" ~>
      Multichoice
        [
          "600 m"
        , "800 m"
        ]

        "680 m"

        [
          "710 m"
        ]
    , "Refer to the landing chart in your supplement. [Bob Tait RPL Study Guide supplement](https://github.com/tonymorris/ppl/raw/master/handouts/RPL_Supplement.pdf). Given that Pressure height = 3500ft, Outside air temperature = 15 degrees Celsius, Wind = 10 knot headwind, Slope = 1% down, The landing distance required by the chart is" ~>
      Multichoice
        [
          "600 m"
        , "450 m"
        , "550 m"
        ]

        "525 m"

        [
        ]
    , "Refer to the loading system in your supplement. [Bob Tait RPL Study Guide supplement](https://github.com/tonymorris/ppl/raw/master/handouts/RPL_Supplement.pdf). Given that the empty aircraft weight and moment index is 1250 lbs and 109.64 respectively, determine if it is safe to fly with the following load distribution. Engine oil = 15lb, Fuel = 25 US Gallons, Front seats = Pilot and passenger each weighing 150lbs, Rear seats = Two passengers weighing 155lbs and 130lbs, Baggage = 20lbs. With this load distribution the aircraft is safe or unsafe to fly?" ~>
      Multichoice
        [
          "safe to fly"
        ]

        "unsafe to fly"

        [
        ]
    , "Which of the following overnight conditions is most likely to be followed by early morning fog?" ~>
      Multichoice
        [
          "heavy rain and low cloud"
        , "clear skies, strong winds and high humidity"
        ]

        "clear skies, light winds and high humidity"

        [
          "cloudy skies, light winds and high humidity"
        ]
    , "What conditions apply to the carriage of passengers in an aircraft fitted with dual controls?" ~>
      Multichoice
        [
          "passengers must not occupy a control seat"
        , "passengers may occupy a control seat only if they are a student pilot"
        ]

        "a passenger in a control seat must be instructed not to interfere with the controls and there must be a means of communication available between the pilot and that passenger"

        [
          "passengers must not occupy a control seat if they do not understand English"
        ]
    , "What age defines an infant?" ~>
      Multichoice
        [
        ]

        "under the age of 3 years"

        [
          "under the age of 2 years"
        , "under the age of 1 year"
        , "any child who cannot walk unassisted"
        ]
    , "Two children may occupy the same seat on an aircraft providing" ~>
      Multichoice
        [
        ]

        "their combined weights do not exceed 77kg"

        [
          "one sits on the other's lap"
        , "they are both under the age of 12 years"
        , "only one wears the seat belt"
        ]
    , "What is the maximum number of persons, including children and infants, that may be carried on a four-seat aircraft?" ~>
      Multichoice
        [
          "4"
        , "7"
        , "6"
        ]

        "5"

        [
        ]
    , "When *must* the pilot of a single pilot aeroplane wear a seat belt?" ~>
      Multichoice
        [
          "during take-off and landing only"
        ]

        "at all times during flight"

        [
          "during take-off, landing and in turbulence"
        , "whenever the aircraft is less than 1000ft AGL"
        ]
    , "What is the minimum distance between an aircraft being fuelled and another stationary aircraft?" ~>
      Multichoice
        [
          "10 m"
        , "15 m"
        ]

        "6 m"

        [
          "5 m"
        ]
    , "Flight through a restricted area is permitted provided that" ~>
      Multichoice
        [
        ]

        "a clearance is obtained from the operating authority"

        [
          "the pilot keeps a good lookout for other aircraft"
        , "the flight remains below 500ft AGL"
        , "a constant heading is maintained while passing through the area"
        ]
    , "What is the empty weight of the aeroplane you fly most often?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is its maximum take-off weight? The difference between the maximum take-off weight and the empty weight is the weight available for fuel, passengers, pilot and baggage, what is it for this aircraft?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "If you carried fuel, how much weight would be left for passengers, pilot and baggage?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "If you had 10kg of baggage and all passenger seats were occupied by a person weighing 77kg, how much fuel could you carry, in litres?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "What is the maximum weight allowed in your baggage compartment in kg?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "How long could you fly this aircraft on a full tank of fuel, in minutes?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "Could you have each seat occupied by a 77kg passenger and still carry full fuel?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    , "Under the same conditions, which is greater, the landing distance or the take-off distance?" ~>
      Multichoice
        [
        ]

        "**RESEARCH THESE QUESTIONS FOR YOUR TRAINING AEROPLANE. CHECK YOUR ANSWERS WITH YOUR FLYING INSTRUCTOR.**"

        [
        ]
    ]

airborneAviationPreSolo ::
  Exam
airborneAviationPreSolo =
  Exam
    "airborne-aviation.com.au Pre-Solo"
    Nothing
    (Just "PRESOLO01/PRESOLO02/PRESOLO03/PRESOLO04")
    [
      "What is the minimum height that an aircraft may commence a turn in the circuit direction after take-off?" ~>
      Multichoice
        [
          "1000 ft AGL"
        , "800 ft AGL"
        ]

        "500 ft AGL"

        [
          "No minimum unless specified by local regulations"
        ]
    , "If a balance ball is indicating right of centre you should apply:" ~>
      Multichoice
        [
        ]

        "More right rudder to centre the ball"

        [
          "More right aileron to centre the ball"
        , "More left rudder to centre the ball"
        , "More left aileron to centre the ball"
        ]
    , "If your aircraft was to enter a spiral dive to effect recovery you should:" ~>
      Multichoice
        [
          "Pull back firmly to stop the descent"
        ]

        "Level the wings, reduce power, pull out of dive"

        [
          "Reduce power to idle, pull nose up, roll wings level"
        , "Apply spin recovery technique"
        ]
    , "What effect will turning have on climb performance?" ~>
      Multichoice
        [
        ]

        "Both angle and rate of climb will decrease"

        [
          "Angle of climb will increase, but rate will decrease"
        , "Both angle and rate of climb will increase"
        , "Rate of climb will increase, but angle will decrease"
        ]
    , "If an aircraft has a stall speed (Vs) of 40 KIAS in level flight the stall speed during a level 60 degree angle of bank turn with all other variables remaining unchanged would be closest to:" ~>
      Multichoice
        [
          "45 KIAS"
        , "80 KIAS"
        , "70 KIAS"
        ]

        "55 KIAS"

        [
        ]
    , "What effect will turning have on climb performance?" ~>
      Multichoice
        [
        ]

        "Both angle and rate of climb will decrease"

        [
          "Angle of climb will increase, but rate will decrease"
        , "Both angle and rate of climb will increase"
        , "Rate of climb will increase, but angle will decrease"
        ]
    , "If an aircraft has a stall speed (Vs) of 40 KIAS in level flight the stall speed during a level 60 degree angle of bank turn with all other variables remaining unchanged would be closest to:" ~>
      Multichoice
        [
          "45 KIAS"
        , "80 KIAS"
        , "70 KIAS"
        ]

        "55 KIAS"

        [
        ]
    , "Under what circumstances is carburettor ice likely to form?" ~>
      Multichoice
        [
        ]

        "Temperatures below 25 degrees C at low power"

        [
          "In cold and moist air at low power"
        , "Only below a temperature of 0 degrees C"
        , "Only if the aircraft is flying in rain"
        ]
    , "Which of the following instruments might be affected by a vacuum system failure?" ~>
      Multichoice
        [
          "Altimeter"
        ]

        "Attitude Indicator"

        [
          "Vertical Speed Indicator"
        , "Airspeed Indicator"
        ]
    , "Which of the following would result in a decreased rate of climb?" ~>
      Multichoice
        [
          "An increase in headwind component"
        ]

        "A reduction in engine power"

        [
          "A reduction in weight"
        , "A decrease in headwind component"
        ]
    , "If a fuel gauge indicates 12 US gallons in a fuel tank and it is properly calibrated you would expect to see how many litres on your dipstick?" ~>
      Multichoice
        [
          "54 litres"
        , "48 litres"
        ]

        "45 litres"

        [
          "36 litres"
        ]
    , "If the elevator control is abruptly pulled fully aft at cruise speed - how does the aeroplane respond?" ~>
      Multichoice
        [
        ]

        "A stall will occur at a higher speed than normal"

        [
          "A stall will occur at a lower speed than normal"
        , "A stall will occur at the same speed but at a higher angle of attack than normal"
        , "The aircraft will not stall as it's flying faster than Vs, the aircraft climbs rapidly"
        ]
    , "In what circumstance would it be most appropriate to transmit a PAN call?" ~>
      Multichoice
        [
          "One of your passengers just threw up on the back seat!"
        , "Your engine has failed and you are planning on ditching in to water"
        ]

        "You are experiencing a rough running engine but have enough power to maintain level flight"

        [
          "Your vertical speed indicator has failed in flight"
        ]
    , "Unless assigned a discrete code by ATC for your transponder you should squawk which code when flying VFR?" ~>
      Multichoice
        [
        ]

        "1200"

        [
          "7700"
        , "2100"
        , "7600"
        ]
    , "Which of the following is a VHF frequency?" ~>
      Multichoice
        [
          "281 KHz"
        , "12.010 MHz"
        , "13075 MHz"
        ]

        "124.55 MHz"

        [
        ]
    , "Which factor listed below has no impact on the stalling IAS of an aeroplane?" ~>
      Multichoice
        [
          "Weight"
        ]

        "Wind"

        [
          "Power setting"
        , "Loading (CG)"
        ]
    , "During a glide approach at the recommended IAS for your aeroplane type if you were to raise the nose and slow the aeroplane in nil wind your gliding range would:" ~>
      Multichoice
        [
        ]

        "decrease and a steeper approach path would be flown"

        [
          "remain the same and approach path would be relatively unchanged"
        , "increase and a flatter approach path would be flown"
        , "remain the same however the slower speed would ensure the aeroplane remained airborne longer"
        ]
    , "When climbing, if one magneto was unserviceable, the effect on climb performance would be:" ~>
      Multichoice
        [
          "Reduced angle of climb, rate unaffected"
        , "Reduced rate of climb, angle unaffected"
        ]

        "Reduced rate and angle of climb"

        [
          "No effect"
        ]
    , "VNE (Velocity Never Exceed) is indicated on an airspeed indicator by:" ~>
      Multichoice
        [
          "the end of the green arc"
        , "not indicated - only found in flight manual or on cockpit placard"
        , "the end of the yellow arc"
        ]

        "a red line"

        [
        ]
    , "When slowing down on downwind in the circuit to follow a slower aircraft flap may be applied. This is primarily to:" ~>
      Multichoice
        [
          "create more drag to oppose the thrust"
        ]

        "improve forward visibility"

        [
          "increase the lift/drag ratio"
        , "raise the stall speed"
        ]
    , "You are in the circuit flying the base leg. You find yourself too close to an aircraft also on the base leg. The best course of action is to:" ~>
      Multichoice
        [
        ]

        "go-around from the base leg"

        [
          "commence an orbit and rejoin base"
        , "overshoot final approach before rejoining final to provide extra space"
        , "reduce your speed below the normal approach speed"
        ]
    , "If an aircraft had a fuel consumption for planning purposes of 35 litres per hour what is the minimum flight fuel required for a flight of 1 hour and 15 minutes including the mandatory fuel reserve?" ~>
      Multichoice
        [
          "44 litres"
        , "62 litres"
        ]

        "70 litres"

        [
          "79 litres"
        ]
    , "An aircraft must be established on a straight final approach path by:" ~>
      Multichoice
        [
          "at a distance determined to be sensible by the pilot"
        ]

        "500 feet AGL"

        [
          "500 metres from the aerodrome boundary"
        , "there is no restriction"
        ]
    , "What document contains specific aerodrome procedures and information?" ~>
      Multichoice
        [
        ]

        "ERSA"

        [
          "AIP-MAP"
        , "CAO"
        , "VFG"
        ]
    , "When must the flying controls be checked for full, free and correct movement on an aircraft?" ~>
      Multichoice
        [
          "Before engine start"
        , "During the take-off roll"
        , "After engine start"
        ]

        "Immediately before take-off"

        [
        ]
    , "You are listening to a radio transmission and hear the first part clearly, while the second part is a loud squealing noise. The most likely cause is:" ~>
      Multichoice
        [
        ]

        "two stations transmitting at the same time"

        [
          "a problem with your squelch adjustment"
        , "normal interference as a result of solar radiation flares"
        , "the electrical system in the aircraft is failing"
        ]
    , "If your engine fails shortly after take-off while on the upwind leg you should:" ~>
      Multichoice
        [
          "execute an immediate turn back towards the runway you departed from"
        ]

        "lower the nose to maintain a safe airspeed and choose a landing site that requires minimal turning"

        [
          "raise the nose to extend your glide performance and select a field within range"
        , "transmit a MAYDAY call immediately with details of your situation then decide upon a plan of action"
        ]
    , "You are on a taxiway and another aircraft approaches you head on. You must:" ~>
      Multichoice
        [
        ]

        "move to the right and pass with care"

        [
          "move only if you are the smaller aircraft"
        , "move to the left and pass with care"
        , "hold your position and flash your taxi light"
        ]
    , "The runway is occupied and you are required to go-around. The most correct sequence of actions is:" ~>
      Multichoice
        [
          "Raise the nose to a climbing attitude, apply full power, retract the flaps"
        , "Retract the flaps, raising the nose with power application"
        , "Raise the nose to a climbing attitude, retract the flaps to reduce drag, apply full power"
        ]

        "Apply full power while raising the nose to a climbing attitude, raising the flap slowly when climbing"

        [
        ]
    , "Compared to an approach at full flap, an approach with no flaps at the same IAS will require:" ~>
      Multichoice
        [
          "a lower nose attitude and steeper approach path"
        , "a lower nose attitude and flatter approach path"
        , "a higher nose attitude and steeper approach path"
        ]

        "a higher nose attitude and flatter approach path"

        [
        ]
    , "You are lining up behind another aircraft that has just landed on the runway you are planning to take-off from. You must not commence your take-off roll until the landing aircraft has:" ~>
      Multichoice
        [
          "started to turn towards the runway exit"
        , "stopped on the runway"
        ]

        "vacated the runway and is taxiing away"

        [
          "passed beyond where you feel your aircraft will be airborne"
        ]
    , "During an approach to land what would the symptoms of an impending stall be?" ~>
      Multichoice
        [
          "Low power setting and falling nose attitude"        
        ]

        "Low airspeed, sluggish controls, high nose attitude"

        [
          "Low nose attitude at higher than normal power setting"
        , "Aircraft tending to want to drop a wing at high power"
        ]
    , "In which of the following situations would you transmit a MAYDAY call?" ~>
      Multichoice
        [
          "There is a sick passenger on board who asks you to land as soon as possible"
        , "A bird strikes your aircraft causing minor damage"
        ]

        "The engine has failed in flight"

        [
          "You are unsure of your position with 2 hours fuel remaining"
        ]
    , "A pilot can establish that the elevator is correctly trimmed when:" ~>
      Multichoice
        [
        ]

        "the required aircraft attitude is maintained when the controls are released"

        [
          "the wings are level"
        , "the trim position indicator is in the neutral position"
        , "the aircraft is maintaining attitude"
        ]
    , "Airspeed has been allowed to drop significantly such that the pilot suspects a stall may be imminent. The immediate actions to rectify this situation are:" ~>
      Multichoice
        [
          "power off, wings level, lower the nose"
        , "add full power rapidly"
        , "reduce power and raise the nose"
        ]

        "add power and lower the nose"

        [
        ]
    , "At the windsock of an aerodrome a double cross marker is displayed, this means:" ~>
      Multichoice
        [
          "the aerodrome is completely unserviceable"
        ]

        "gliding operations are in progress"

        [
          "unsealed surfaces should not be used"
        , "CTAF procedures are in force"
        ]
    , "The circuit height at an aerodrome for a light piston engine aircraft by convention is:" ~>
      Multichoice
        [
        ]

        "1000 ft AGL"

        [
          "1300 ft AMSL"
        , "1000 ft AMSL"
        , "1500 ft AGL"
        ]
    , "During take-off in no wind it is necessary to apply right rudder in most aircraft to maintain a straight path on the runway - why is this?" ~>
      Multichoice
        [
          "The propeller slipstream pushing on the left side of the rudder and vertical fin"
        , "Engine torque"
        , "The coriolis effect"
        ]

        "Both slipstream and torque as listed above"

        [
        ]
    , "When setting QNH on the altimeter while parked on an aerodrome the altimeter will read:" ~>
      Multichoice
        [
          "zero feet"
        ]

        "height above sea level"

        [
          "height in a standard atmosphere"
        , "250 feet"
        ]
    , "You are heading for a night out at the pub on Friday. You have a flying lesson booked for 7am Saturday morning, when must you stop drinking alcohol?" ~>
      Multichoice
        [
          "It is dependant on the amount you drink, you must not return a blood alcohol reading of more than 0.02"
        , "1:00am Saturday and ensure you are not affected by alcohol at the time of your flight"
        , "You must not drink the day before your flight"
        ]

        "11:00pm Friday and ensure you are not affected by alcohol at the time of your flight"

        [
        ]
    , "When taxiing you must always give way to aircraft" ~>
      Multichoice
        [
          "that are larger than your aircraft"
        ]

        "on your right and those exiting a runway"

        [
          "on your right or any aircraft displaying strobes"
        , "on your left and those exiting a runway"
        ]
    , "The high speed end of the white arc on an airspeed indicator (ASI) shows:" ~>
      Multichoice
        [
        ]

        "the maximum IAS at which flaps may be lowered"

        [
          "the preferred speed for approach and landing"
        , "VNE - velocity never exceed"
        , "the minimum IAS at which flaps may be retracted"
        ]
    ]

airborneAviationAreaSolo ::
  Exam
airborneAviationAreaSolo =
  Exam
    "airborne-aviation.com.au Area Solo"
    Nothing
    (Just "AREASOLO01/AREASOLO02/AREASOLO03/AREASOLO04")
    [
      "A number of factors affect the stalling IAS of an aircraft. Which is not a factor?" ~>
      Multichoice
        [
          "Power"
        , "Load factor (G)"
        ]

        "Altitude"

        [
          "Aircraft weight"
        ]
    , "You are flying in the training area at 1,800ft. What is the minimum requirements to ensure you remain in Visual Meteorological Conditions (VMC)?" ~>
      Multichoice
        [
        ]

        "Clear of cloud with 5km visibility"

        [
          "1,000ft clear of cloud vertically with 5km visibility"
        , "1,000ft clear of cloud vertically with 8km visibility"
        , "Whatever seems appropriate to the pilot"
        ]
    , "Induced drag acting on an aircraft during straight and level flight is:" ~>
      Multichoice
        [
          "greatest when airspeed is low and the aircraft is light"
        ]

        "greatest when airspeed is low and the aircraft is heavy"

        [
          "greatest when airspeed is high and the aircraft is light"
        , "greatest when airspeed is high and the aircraft is heavy"
        ] 
    , "Detonation of an aircraft engine is most likely to occur when?" ~>
      Multichoice
        [
        ]

        "During a full power climb with the mixture set lean"

        [
          "During descent at idle power"
        , "During the cruise with a mixture too rich"
        , "During take-off with the mixture at full rich"
        ]
    , "If you are flying late in the afternoon. You must ensure you plan to land how long before the end of daylight?" ~>
      Multichoice
        [
          "0 minutes - simply ensure you last before dark"
        , "30 minutes"
        , "5 minutes"
        ]

        "10 minutes"

        [
        ]
    , "After take-off the altimeter fails to climb, what is the most likely cause?" ~>
      Multichoice
        [
        ]

        "A blocked static port"

        [
          "The QNH was set incorrectly"
        , "A blocked pitot tube"
        , "An electrical failure in the instrument panel"
        ] 
    , "When would carburettor ice be most difficult to detect?" ~>
      Multichoice
        [
          "During the cruise in freezing rain"
        ]

        "During a descent at low power"

        [
          "During take-off at full power"
        , "During a cruise climb at an intermediate power setting"
        ] 
    , "What is the legal minimum requirement with respect to performing a fuel system inspection?" ~>
      Multichoice
        [
          "Before each and every flight"
        ]

        "Before the first flight of the day and after refueling"

        [
          "Only after refueling"
        , "Before the first flight of the day"
        ] 
    , "You are carrying out a level turn of 60 degrees angle of bank. Which of the following is correct?" ~>
      Multichoice
        [
          "Lift is equal to weight"
        , "Lift is 3.0 times weight"
        ]

        "Lift is 2.0 times weight"

        [
          "Lift is 1.4 times weight"
        ] 
    , "Your aircraft has been filled with JET A1 (AVTUR) instead of AVGAS as specified in the flight manual. Which of the following statements is correct?" ~>
      Multichoice
        [
        ]

        "There would be power loss and rough running or engine failure - you should not depart"

        [
          "The performance of your aircraft would increase substantially when running on jet fuel"
        , "There would be no performance impact and you would be safe to proceed with your planned flight"
        , "There would be a slight reduction in power however you could proceed with your planned flight"
        ] 
    , "You are climbing with one magneto that is unserviceable. What is the effect on climb performance?" ~>
      Multichoice
        [
          "Reduced angle of climb"
        , "Reduced rate of climb"
        ]

        "Reduced rate and angle of climb"

        [
          "No effect as the other magneto is still operating"
        ] 
    , "The maximum speed an aircraft is permitted to fly is abbreviated as:" ~>
      Multichoice
        [
        ]

        "VNE"

        [
          "VNO"
        , "VA"
        , "VMAX"
        ] 
    , "During a level steep turn, the load factor:" ~>
      Multichoice
        [
          "Decreases due to increased power being used"
        ]

        "Increases with increasing angle of bank"

        [
          "Remains unchanged"
        , "Increases due to increasing angle of attack"
        ] 
    , "An increase in parasite drag would result from which of the following actions?" ~>
      Multichoice
        [
        ]

        "Increasing the IAS"

        [
          "Decreasing the IAS"
        , "Reducing or raising the flaps"
        , "Retracting the undercarriage"
        ] 
    , "In your before takeoff checks you notice that there is no RPM drop when carburettor heat is moved to \"ON\". What is the most likely cause?" ~>
      Multichoice
        [
          "The engine is not yet to normal operating temperature"
        , "Carburettor ice is present"
        , "The outside air temperature (OAT) is high and we do not expect a drop"
        ]

        "The carburettor heat mechanism is defective"

        [
        ] 
    , "The load factor (G force) that an average person might start to \"grey out\" is:" ~>
      Multichoice
        [
        ]

        "3.5 G"

        [
          "5.0 G"
        , "2.0 G"
        , "7.0 G"
        ] 
    , "One of the early symptoms of carbon monoxide poisoning is a:" ~>
      Multichoice
        [
          "Loss of vision"
        ]

        "Slight headache"

        [
          "Severe headache"
        , "Loss of muscular power"
        ] 
    , "What colour is AVGAS 100LL when sampled?" ~>
      Multichoice
        [
          "Green"
        ]

        "Blue"

        [
          "Clear"
        , "Yellow"
        ] 
    , "As a student pilot (pre-GFPT) how many hours are you permitted to fly solo before a dual check must be carried out?" ~>
      Multichoice
        [
          "1.0 hour"
        , "5.0 hours"
        ]

        "3.0 hours"

        [
          "2.0 hours"
        ] 
    , "You're in flight and there is a glider approaching on your right at the same level, you should:" ~>
      Multichoice
        [
        ]

        "Turn right to pass behind the glider"

        [
          "Climb over and above the glider"
        , "Expect the glider to give way"
        , "Descend beneath the glider"
        ] 
    , "You have mishandled the aircraft and are experiencing a load factor (G force) of over +5 G's. After a short period of time you black-out, this means that:" ~>
      Multichoice
        [
          "You have tunnel vision and are seeing in shades of grey only"
        , "You have passed out"
        ]

        "You have lost all vision however you are still conscious and able to move the controls"

        [
          "You have lost all vision however you are still conscious and able to move the controls"
        ] 
    , "If there was a small quantity of water present in your fuel tank how would it be presented when sampling your avgas?" ~>
      Multichoice
        [
        ]

        "Clear blobs of water at the base of your fuel sample"

        [
          "The blue tint of the 100LL avgas would be lighter than usual"
        , "The water mixes with the avgas and it's not possible to detect"
        , "Clear blobs of water at the top of your fuel sample"
        ] 
    , "While taxiing for take-off another aeroplane approaches head-on, you would:" ~>
      Multichoice
        [
          "Continue to taxi, expecting the other aircraft to move"
        ]

        "Continue to taxi, moving to pass on the right"

        [
          "Continue to taxi, moving to pass on the left"
        , "Pull over to the left and stop."
        ] 
    , "On a visual terminal chart (VTC), major built up areas, such as a city, are shown in which colour?" ~>
      Multichoice
        [
        ]

        "Yellow"

        [
          "Brown"
        , "Green"
        , "Purple"
        ] 
    , "When gliding for maximum distance it would be best to glide:" ~>
      Multichoice
        [
          "In to wind"
        , "Across the wind"
        , "At the best glide speed - wind has no bearing on glide distance"
        ]

        "With the wind behind you"

        [
        ] 
    , "How often should a direction indicator (DI) be aligned with the magnetic compass?" ~>
      Multichoice
        [
        ]

        "Every 10-15 minutes"

        [
          "Before take-off only"
        , "At least every 5 minutes"
        , "Approximately every 30 minutes"
        ] 
    , "When flying in the training area you must not fly lower than:" ~>
      Multichoice
        [
          "500ft AGL"
        ]

        "1,000ft AGL over populated areas and 500ft AGL elsewhere"

        [
          "1,500ft AGL over populated areas and 1,000ft AGL elsewhere"
        , "1,000ft unless on approach to land"
        ] 
    , "Which of the following would result in a decreased rate of climb?" ~>
      Multichoice
        [
          "An increase in headwind component"
        ]

        "A reduction in engine power"

        [
          "A reduction in weight"
        , "A decrease in headwind component"
        ] 
    , "If a fuel gauge indicates 24 US gallons in a fuel tank and it is properly calibrated you would expect to see how many litres on your dipstick?" ~>
      Multichoice
        [
          "108 litres"
        , "96 litres"
        ]

        "90 litres"

        [
          "72 litres"
        ] 
    , "The ATIS reports that the QNH is 1015 and temperature is 25 degrees C at an airport where the elevation is 250ft. When QNH is set on the subscale of your altimeter it should:" ~>
      Multichoice
        [
        ]

        "Indicate airport elevation"

        [
          "Indicate zero feet"
        , "Indicate higher than the airport elevation due to the temperature"
        , "Indicate lower than the airport elevation due to the temperature"
        ] 
    , "If an aeroplane is stalling and a wing drops at the moment of stall, then the pilot should do what to prevent the wing drop dropping further?" ~>
      Multichoice
        [
          "Use ailerons to stop the wing from falling further"
        , "Use ailerons to level the wings"
        ]

        "Use rudder in the opposite direction to the wing drop to prevent further yaw"

        [
          "Use rudder in the same direction to the wing drop to prevent further yaw"
        ] 
    , "You are climbing at your best rate of climb airspeed (Vy), if you start a turn your rate of climb will:" ~>
      Multichoice
        [
        ]

        "Decrease with increasing angle of bank"

        [
          "Decrease due to asymmetric propeller effect"
        , "Remain the same regardless of angle of bank"
        , "Increase if turning in to a headwind"
        ] 
    , "If during engine start you suspect a fire in the engine compartment, the best course of action initially is to:" ~>
      Multichoice
        [
          "Turn off the ignition switch"
        ]

        "Continue trying to start the engine with the starter"

        [
          "Evacuate the aeroplane without delay"
        , "Put out the fire with the onboard fire extinguisher"
        ] 
    , "An area shown on a visual terminal chart (VTC) is marked \"R580 - SFC-1500\". This indicates that you may not over fly this area without a clearance:" ~>
      Multichoice
        [
        ]

        "Below 1,500ft AMSL"

        [
          "Below 1,500ft AGL"
        , "Above 1,500ft AMSL"
        , "Above 1,500ft AGL"
        ] 
    , "Indications of carburettor icing in an aeroplane with a fixed pitch propeller could be:" ~>
      Multichoice
        [
          "Decreased fuel consumption"
        , "A rise in RPM due to resulting richer mixture."
        , "Decreased exhaust gas temperature"
        ]

        "Rough running and a drop in RPM"

        [
        ] 
    , "Spot heights on a visual terminal chart (VTC) are:" ~>
      Multichoice
        [
          "Never higher than the surrounding hyposometric tint"
        , "Measured in metres above mean sea level"
        , "Measured in feet above the surrounding terrain"
        ]

        "Measured in feet above mean sea level"

        [
        ] 
    , "A centre-zero ammeter indicates zero in flight, this most likely indicates:" ~>
      Multichoice
        [
          "The battery is completely flat"
        ]

        "The battery is fully charged"

        [
          "The alternator has failed"
        , "The magnetos are running off the battery"
        ] 
    , "You have experienced a radio failure while operating in the circuit. ATC have realised this and display light signals to communicate with you. A steady red light while you are airborne indicates:" ~>
      Multichoice
        [
          "Aerodrome unsafe, do not land"
        ]

        "Give way to other aircraft and continue circling"

        [
          "Radio failure acknowledged, cleared to land"
        , "No significance"
        ] 
    , "You should plan to land with how many minutes of fixed reserve fuel in your tank(s)?" ~>
      Multichoice
        [
          "20 minutes"
        , "30 minutes"
        ]

        "45 minutes"

        [
          "60 minutes"
        ] 
    , "If you had a wake turbulence encounter having flown near the path of a heavy aircraft you would most likely experience:" ~>
      Multichoice
        [
        ]

        "A violent roll"

        [
          "A violent yaw"
        , "A violent pitch"
        , "A violent pitch and roll"
        ] 
    ]


airborneAviationMeteorology ::
  Exam
airborneAviationMeteorology =
  Exam
    "airborne-aviation.com.au Meteorology"
    Nothing
    (Just "MET01")
    [
      "A wind in a terminal area forecast expressed as VRB30G45 would:" ~>
      Multichoice
        [
          "Always be associated with closely spaced isobars around a trough"
        , "Be improbable, since variable wind directions are only associated with light winds and high pressure systems"
        ]

        "Probably be associated with a thunderstorm"

        [
          "Always be associated with closely spaced isobars around a ridge"
        ]
    , "The validity of a METAR is:" ~>
      Multichoice
        [
          "3 hours"
        , "6 hours"
        , "30 hours"
        ]

        "Not applicable"

        [
        ]
    , "The wind direction is given in degrees:" ~>
      Multichoice
        [
          "Magnetic in both area and aerodrome forecasts"
        , "Magnetic in an area forecast, and true in an aerodrome forecast"
        ]

        "True in both area and aerodrome forecasts"

        [
          "True in an area forecast, and magnetic in an aerodrome forecast"
        ]
    , "In a METAR for YAAA, the figures quoted immediately before the QNH read 9/8, and at YBBB they read 10/8. This means that, at place YAAA, the dry bulb is 9C and the:" ~>
      Multichoice
        [
        ]

        "Dew point is 8C and fog is closer to forming at YAAA than YBBB"

        [
          "Wet bulb is 8C and fog is closer to forming at YAAA than YBBB"
        , "Dew point is 8C and fog is closer to forming at YBBB than YAAA"
        , "Wet bulb is 8C and fog is closer to forming at YBBB than YAAA"
        ]
    , "A TAF includes the following:\n\n> INTER 0710/0714 2000 -SHSN\n\nThis forecasts, between the times indicated:" ~>
      Multichoice
        [
          "Showers and snow of expected duration 30 to 60 minutes"
        , "Showers of snow of expected duration 30 to 60 minutes"
        , "Showers and snow of expected duration less than 30 minutes"
        ]

        "Showers of snow of expected duration less than 30 minutes"

        [
        ]
    , "The validity period of an aerodrome forecast is from 0000Z on the 1st to 0600Z on the 2nd. In the forecast this is written:" ~>
      Multichoice
        [
          "0100/0206"
        , "010006"
        , "0100/06"
        ]

        "01000206"

        [
        ]
    , "In a TAF, the term FU VV030 means:" ~>
      Multichoice
        [
          "Smoke and a visibility of 3,000 metres"
        ]

        "Smoke and a vertical visibility of 3,000 feet"

        [
          "Fumes and a visibility of 3.000 metres"
        , "Fumes and a vertical visibility of 3,000 feet"
        ]
    , "Observed intermediate QNH values are rounded:" ~>
      Multichoice
        [
          "Down so that the altimeter reading is from a pressure level slightly lower than MSL"
        ]

        "Down so that the altimeter reading is from a pressure level slightly higher than MSL"

        [
          "Up so that the altimeter reading is from a pressure level slightly lower than MSL"
        , "Up so that the altimeter reading is from a pressure level slightly higher than MSL"
        ]
    , "When cruising below the transition layer (10,000') you can set a local QNH of a station along the route provided it is within:" ~>
      Multichoice
        [
          "10NM of the aircraft"
        , "50NM of the aircraft"
        ]

        "100NM of the aircraft"

        [
          "200NM of the aircraft"
        ]
    , "A TAF commences and finishes as follows:\n\n  > TAF YCOM 070635Z 0708/0720 ... Q 1020 1021 1019 1018\n\n  The forecast QNH for time 071320 is:" ~>
      Multichoice
        [
          "1020"
        , "1021"
        ]

        "1019"

        [
          "1018"
        ]
    ]

taitPart13Meteorology ::
  Exam
taitPart13Meteorology =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Part 13 Meteorology")
    [
      "Which conditions are most likely to produce radiation fog?" ~>
      Multichoice
        [              
          "thin overcast with no wind"
        , "broken cloud with no wind"
        ]

        "nil cloud and light wind"

        [
          "scattered cumulus with light wind"
        ]
    , "During what stage of a thunderstorm would lightning be most frequent?" ~>
      Multichoice
        [
          "before any precipitation"
        , "during the most rapid growth"            
        ]

        "at the mature stage"

        [
          "during the dissipating stage"
        ]
    , "The lifting of fog may be caused by" ~>
      Multichoice
        [              
          "a fall of temperature"
        , "the formation of upper cloud"
        ]

        "an increase in wind strength"

        [
          "an increase in humidity"
        ]
    , "Which phenomena in a thunderstorm causes low level wind shear at some distance away from the cell?" ~>
      Multichoice
        [              
          "updraft"
        ]

        "downdraft"

        [
          "hail"
        , "turbulence"
        ]
    , "Where are tornadoes most likely to be encountered?" ~>
      Multichoice
        [              
          "over hot desert country"
        , "with mountain waves"
        , "strong low pressure systems"
        ]

        "wide spread severe thunderstorm activity"

        [
        ]
    , "Which process is involved in the formation of hoar frost?" ~>
      Multichoice
        [              
          "accumulation of ice crystals suspended in the atmosphere"
        , "liquid water freezing on impact with the aircraft surface"
        ]

        "deposition of ice directly from the water vapour mixed with the air"

        [
          "freezing of large supercooled water droplets"
        ]
    , "The condition most likely to produce advection fog is" ~>
      Multichoice
        [              
          "cold air passing over a cold surface"
        , "warm air passing over a warm surface"
        , "cold air passing over a warm surface"
        ]

        "warm air passing over a cold surface"

        [
        ]
    , "One of the prerequisites for the formation of a thunderstorm is" ~>
      Multichoice
        [              
        ]

        "the presence of moist air through a considerable depth of the atmosphere"

        [
          "a rapid drop in surface temperature with an increase in wind strength"
        , "a gradual increase in cloud cover above 20000ft"
        , "a cold isothermal layer through a considerable depth of the atmosphere"
        ]
    , "Advection fog forms when" ~>
      Multichoice
        [              
          "cold air passing over a cold surface"
        , "warm air passing over a warm surface"
        , "cold air passing over a warm surface"
        ]

        "warm air passing over a cold surface"

        [
        ]
    , "Dust storms are most likely to produce which of the following hazards?" ~>
      Multichoice
        [              
          "severe visibility restriction in a localised area below 10000ft"
        , "severe visibility restriction over a widespread area below 10000ft"
        , "severe visibility restriction over a localised area to heights above 10000ft"
        ]

        "severe visibility restriction over a widespread area to heights above 10000ft"

        [
        ]
    , "The most dangerous effect of a mature thunderstorm is" ~>
      Multichoice
        [              
        ]

        "turbulence"

        [
          "lightning"
        , "hail"
        , "icing"
        ]
    , "Slant visibility in fog causes" ~>
      Multichoice
        [              
          "reduced visibility towards the sun"
        ]

        "the runway is visible from 'over the top' but not in the circuit area or on final"

        [
          "better visibility looking up than down"
        , "runway visual range is less than meteorological visibility"
        ]
    , "The runway at a coastal aerodrome runs parallel to the coast. When would the crosswind component be likely to be strongest?" ~>
      Multichoice
        [              
          "early in the morning"
        , "late at night"
        ]

        "in the early afternoon"

        [
          "at sunset"
        ]
    ]

atcFROL1 ::
  Exam
atcFROL1 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 1")
    [
      "What is the unit of frequency called, and what is its abbreviation?" !-
      DirectAnswer
        "Hertz, Hz"
    , "A frequency of 1,000 hertz can also be described as one (kilohertz/megahertz/microhertz)." !-
      DirectAnswer
        "Kilohertz."
    , "Sketch a typical wave and label it with wavelength and amplitude." !-
      DirectAnswer
        "Refer to figure A2-1."
    , "In your own words, define wavelength and amplitude." !-
      DirectAnswer
        "Wavelength is the length of one cycle of the wave; amplitude is the maximum extent of an oscillation as measured from the position of equilibrium."
    , "Radio waves travel at the speed of (light/sound)." !-
      DirectAnswer
        "Light."
    , "Light waves travel (slower than/the same speed as/faster than) sound waves." !-
      DirectAnswer
        "Faster than."
    , "What is the preferred frequency band for aeronautical radio communication?" !-
      DirectAnswer
        "VHF."
    , "The range of VHF transmissions (increases/decreases) with gain of height." !-
      DirectAnswer
        "Increases."
    , "What control is used on VHF-COM radio sets to minimise unwanted background noise?" !-
      DirectAnswer
        "Squelch."
    , "When transmitting on a particular frequency, can other people in the vicinity be transmitting on that frequency simultaneously?" !-
      DirectAnswer
        "No."
    , "If the transmit button becomes stuck in the transmit position or is not released after a transmission, will other transmissions be affected? Will it block that frequency?" !-
      DirectAnswer
        "Yes. Yes."
    , "The VOR radio navaid operates in a range of frequencies within the (VHF/MF/UHF) band." !-
      DirectAnswer
        "VHF."
    , "In which frequency bands do the NDB radio beacons operate?" !-
      DirectAnswer
        "LF and MF."
    , "What equipment enables an air traffic radar controller to identify your aircraft by receiving coded signals?" !-
      DirectAnswer
        "Transponder."
    , "For long-range voice communications, some aircraft are fitted with (UHF/MF/HF) radio equipment." !-
      DirectAnswer
        "HF."        
    ]

atcFROL2 ::
  Exam
atcFROL2 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 2")
    [
      "Write out and say the phonetic alphabet that is used to transmit letters." !-
      DirectAnswer
        "Refer to table 2-1, page 23."
    , "How would transmit the number 59?" !-
      DirectAnswer
        "Five nine (pronounced FIFE NINer)."   
    , "How would you transmit the number 8,500?" !-
      DirectAnswer
        "Eight thousand five hundred (pronounced AIT TOU-SAND FIFE HUN-dred)."
    , "What word or phrase would you use to express \"yes\" or \"that is correct\"?" !-
      DirectAnswer
        "Affirm."
    , "What word or phrase means \"no\"?" !-
      DirectAnswer
        "Negative."
    , "How would you transmit the callsign of an aircraft registered VH-RQT?" !-
      DirectAnswer
        "Romeo Quebec Tango (pronounced ROW-me-oh Keh-BECK TANG-go)."
    , "How would you transmit the callsign of VH-UVP?" !-
      DirectAnswer
        "Uniform Victor Papa (pronounced YOU-nee-form VIK-tah Pah-PAH)."
    , "How would you transmit the hours and minutes of 0835 UTC?" !-
      DirectAnswer
        "Zero eight three five (pronounced ZE-RO AIT TREE FIFE)."
    , "The time for aviation purposes is specific in (UTC/EST/LST)." !-
      DirectAnswer
        "UTC."
    , "UTC is 10 hours (ahead of/behind) Eastern Standard Time." !-
      DirectAnswer
        "Behind."
    , "UTC is 9 hours 30 minutes (ahead of/behind) Central Standard Time." !-
      DirectAnswer
        "Behind."
    , "1810 CST is ... UTC" !-
      DirectAnswer
        "0840 UTC."
    , "UTC is ... hours (ahead of/behind) Western Standard Time." !-
      DirectAnswer
        "8, behind"
    , "2000 WST is ... UTC." !-
      DirectAnswer
        "1200 UTC."
    , "0030 UTC is ... EST." !-
      DirectAnswer
        "1030 EST."
    , "0800 EST ... UTC." !-
      DirectAnswer
        "2200 UTC the previous day."
    , "0700 WST is ... UTC." !-
      DirectAnswer
        "2300 UTC the previous day."
    ]

atcFROL3 ::
  Exam
atcFROL3 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 3")
    [
      "Radio transmissions fall into three categories: reports, broadcasts and calls. Explain these." !-
      DirectAnswer
        "Reports are made to a specific air traffic services unit i.e. air traffic control or FLIGHTWATCH, as appropriate. Broadcasts are mandatory or voluntary traffic advisory transmissions prefixed with \"all stations\". Calls are made to specfic stations, e.g. when requesting information from air traffic services or FLIGHTWATCH or when contacting another aircraft direct to arrange mutual separation."
    , "In general terms, what should you transmit when establishing radio contact?" !-
      DirectAnswer
        "The name of the station you are calling plus your callsign."
    , "If you were in VH-POW, how would you establish contact with Cairns Tower?" !-
      DirectAnswer
        "Cairns Tower, Papa Oscar Whisky."
    , "Having established radio contact on a particular frequency and there is no danger of confusion with another aircraft or another ground station, what procedure is adopted for subsequent transmissions?" !-
      DirectAnswer
        "The callsign of the addressed station is dropped."
    , "How should you acknowledge calls directed at your aircraft?" !-
      DirectAnswer
        "With your callsign."
    , "Which of the following is/are required to be read back? (a) Clear to taxi. (b) Clearance to track via the Westgate Bridge, one thousand five hundred. (c) Line up. (d) Cleared for takeoff. (e) Amended route track via the city, eight thousand. (f) Cleared to land." !-
      DirectAnswer
        "All clearances from (a) to (f)."
    , "Does the pilot need to read back any item notified in an airways clearance as *amended*?" !-
      DirectAnswer
        "Yes."
    , "Should cruise levels specified in an airways clearance granted to you by air traffic control for flight in controlled airspace be read back?" !-
      DirectAnswer
        "Yes, they should."
    , "What standard phrase would you include when making a radio check call on the ground prior to starting up for a flight?" !-
      DirectAnswer
        "Radio check (and if you want, add \"how do you read?\")"
    , "How would you reply to a perfectly readable check call?" !-
      DirectAnswer
        "[Callsign], reading you five."
    , "How would you respond to a check call that was readable, but with difficulty?" !-
      DirectAnswer
        "[Callsign], reading you three."
    , "If a response to your signal check inflight is *reading you four*, how would you interpret this?" !-
      DirectAnswer
        "Readable, but not perfect."
    , "You are the pilot of VH-DRY. How would air traffic control tell you to turn the transponder on?" !-
      DirectAnswer
        "Delta Romeo Yankee, squawk normal."
    , "What is meant by *squawk ident*?" !-
      DirectAnswer
        "Press the transponder ident button."
    , "What is meant by *squawk standby*?" !-
      DirectAnswer
        "Select standby on the transponder."
    , "How would you acknowledge the following instruction from air control: *Delta Romeo Yankee, squawk 4162*?" !-
      DirectAnswer
        "Four one six two, Delta Romeo Yankee."
    , "How would you acknowledge the following: *Delta Romeo Yankee, squawk normal*?" !-
      DirectAnswer
        "Delta Romeo Yankee."
    , "If you squawk on code 7600, what does it mean?" !-
      DirectAnswer
        "A radio failure."
    , "In an emergency, what code would you select?" !-
      DirectAnswer
        "Code 7700."
    ]

atcFROL4 ::
  Exam
atcFROL4 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 4")
    [
      "What are the two fundamental types of airspace?" !-
      DirectAnswer
        "Controlled and non-controlled airspace."
    , "Information to flights in Class G airspace is provided by (air traffic control/FLIGHTWATCH)." !-
      DirectAnswer
        "FLIGHTWATCH"
    , "What are the two divisions of controlled airspace?" !-
      DirectAnswer
        "Control areas and control zones."
    , "What are the types of control zones?" !-
      DirectAnswer
        "Civil (Class C and D), and military."
    , "The abbreviation CTR stands for (control zone/control area/controlled terminal radar)." !-
      DirectAnswer
        "Control zone."
    , "At aerodromes in Class G airspace, traffic advisory frequencies have been established. What are these?" !-
      DirectAnswer
        "Common traffic advisory frequencies or CTAF."
    , "The abbreviation FIA stands for (flight information aerodrome/flight information area)." !-
      DirectAnswer
        "Flight information area (FIA)."
    , "When inbound to a non-towered aerodrome, pilots should make a broadcast on the CTAF by what distance?" !-
      DirectAnswer
        "10 nm or broadcast area boundary whichever is greater."
    , "Outside tower hours of operation, may controlled aerodromes revert to non-towered aerodromes with mandatory carriage and use of radio?" !-
      DirectAnswer
        "Yes, CERT, REG or MIL aerodromes on CTAF."
    , "Where a discrete CTAF frequency is not listed for a non-towered aerodrome, broadcasts should be made on what frequency?" !-
      DirectAnswer
        "126.7MHz"
    , "Carriage of radio (is/is not) mandatory at a CERT aerodrome." !-
      DirectAnswer
        "Is."
    , "What are the broadcasts you must make at a CERT or REG non-towered aerodrome?" !-
      DirectAnswer
        "Taxiing, entering runway, clear of runway, when entering the vicinity of the aerodrome, intending to make a straight-in approach, joining Base."
    , "At a CTAF (UNCR), the carriage of radio (is/is not) mandatory." !-
      DirectAnswer
        "Not mandatory for UNCR aerodromes, but radio-equipped aircraft should still make broadcasts (taxiing, entering the runway, inbound and joining the circuit)."
    , "When a Class D control zone is deactivated after tower hours of operation, the zone can/cannot become a CTAF with mandatory radio." !-
      DirectAnswer
        "Can."
    , "At a REG aerodrome, the clearance to taxi (is/is not) required." !-
      DirectAnswer
        "Is not."
    , "Before taking off from a registered aerodrome with a CTAF, is an airways clearance required?" !-
      DirectAnswer
        "Not if going into Class G."
    , "When operating in Class G airspace, not near a CTAF, you should listen out on appropriate (ATC centre frequency/FIA frequency/UNICOM frequency)." !-
      DirectAnswer
        "FIA frequency."
    , "What are the two types of air traffic clearances which can be issued for operating in controlled airspace?" !-
      DirectAnswer
        "Clearances (to taxi, cross a runway, takeoff and land) and airways clearances (to operate in controlled airspace)."
    , "List the classes of airspace and explain what each means." !-
      DirectAnswer
        "Class A - high altitude. Class C - controlled airspace, below FL600, including Class C CTRs. Class D - non-radar aerodrome CTRs (below 4,500 feet). Class E - controlled airspace with radar coverage between 8,500ft and FL125. Class G - all non-controlled airspace."
    , "What facilities are provided to you in a flight information area (FIA)?" !-
      DirectAnswer
        "Flight information and SAR alerting services."
    ]

atcFROL5 ::
  Exam
atcFROL5 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 5")
    [
      "What is meant by the abbreviation ATIS?" !-
      DirectAnswer
        "Automatic terminal information service."
    , "List four items of meteorological data which are included in an ATIS broadcast." !-
      DirectAnswer
        "ATIS includes: wind velocity; aerodrome QNH; cloud (amount and base) plus visibility and other conditions (or CAVOK if visibility is 10km or more, there is no cloud below 5,000ft AAL and no precipitation, thunderstorm, shallow fog, low drifting snow or dust devils); temperature."
    , "The ATIS is usually broadcast (on a navaid or a VHF-COM frequency/by the ATC tower controller/direct from the weather office)." !-
      DirectAnswer
        "Navaid or VHF-COM frequency."
    , "Before taking off at a controlled aerodrome where ATIS is provided, you should notify receipt of the ATIS broadcast in your (taxiing call/request for airways clearance/request for takeoff)." !-
      DirectAnswer
        "Taxiing call."
    , "Is the duty runway nominated in the ATIS at controlled aerodrome?" !-
      DirectAnswer
        "Yes."
    , "Is the duty runway nominated in the ATIS at a CTAF aerodrome?" !-
      DirectAnswer
        "No."
    ]

atcFROL6 ::
  Exam
atcFROL6 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 6")
    [
      "Is an ATC clearance required to taxi at an aerodrome in a Class D control zone?" !-
      DirectAnswer
        "Yes."
    , "You are departing from an aerodrome in a Class C CTR for the training area, which is in Class G Airspace. Is an airways clearance required?" !-
      DirectAnswer
        "Yes."
    , "You are departing from an aerodrome in a Class D CTR for a session of circuit work. Is an airways clearance required?" !-
      DirectAnswer
        "Yes."
    , "Prior to departing a Class C controlled aerodrome, you would request your airways clearance (prior to/after) reporting \"ready\" for takeoff." !-
      DirectAnswer
        "Prior to."
    , "Over Kywong at 5,500 ft AMSL in Cessna 172 VH-IES en route to Wagga (a Class C CTR) on a cross-country flight from Hay, you call Wagga Tower for an airways clearance. What will you say?" !-
      DirectAnswer
        "Wagga Tower India Echo Sierra Cessna 172 Kywong five thousand five hundred inbound received... [ATIS code letter] request clearance."
    , "If operating from a Class D capital city aerodrome, is an airways clearance required for either circuit work or flights in the local training area?" !-
      DirectAnswer
        "No, unless the training area is in other controlled airspace (unlikely)."
    , "You have received ATIS information Foxtrot at Parafield, a Class D aerodrome, and are taxiing for Runway 21 right in Cessna 182 VH-RON prior to departing for the training area. What will be your taxiing report?" !-
      DirectAnswer
        "Parafield Ground Romeo Oscar November received Foxtrot Cessna 182 for the training area Runway 21 right, request taxi."
    , "Continuing from the previous question, when you reach the runway holding point at Parafield, how would you obtain your take-off clearance? (a) Change to the TWR frequency and report: *'Romeo Oscar November, ready Runway right, for the training area'*. (b) Wait for the ground controller to advise you to call the TWR for takeoff clearance. (c) Call the TWR: *'Request takeoff clearance for the training area, Romeo Oscar November'*." !-
      DirectAnswer
        "Answer (a)."        
    , "On your final approach to land at a controlled aerodrome in VH-WOZ, you noticed an aeroplane still on the runway. How would you advise the TWR that you are discontinuing your approach?" !-
      DirectAnswer
        "Whisky Oscar Zulu, going around"
    , "When taxiing at a CERT aerodrome within a CTAF, you (must/need not) report taxiing." !-
      DirectAnswer
        "Must."
    , "You are taxiing for Runway 20 at Robinvale, a CTAF near Mildura, for circuits in your Warrior VH-BDR. Write down your taxiing broadcast." !-
      DirectAnswer
        "Robinvale traffic Bravo Delta Romeo Warrior taxiing at Robinvale"
    , "You have just departed from Bairnsdale (a CTAF) for Merimbula in VH-JIM with a planned VFR cruise level of 7,500ft. You are unable to obtain the local DNH from a ground station, so you call FLIGHTWATCH for the area QNH. What would you say?" !-
      DirectAnswer
        "Melbourne Centre FLIGHTWATCH Juliet India Mike departed Bairnsdale for Merimbula on climb to seven thousand five hundred request area QNH."        
    , "When should you make an inbound broadcast if you are returning from the training area to a non-towered aerodrome?" !-
      DirectAnswer
        "Inbound broadcast prior to entering the vicinity of the aerodrome (10nm), a circuit joining broadcast is also required as you approach the circuit area."        
    , "Make up a sample inbound broadcast for arriving at Esperance from the north in VH-ASB, a Piper Lance." !-
      DirectAnswer
        "For example: Esperance traffic Alfa Sierra Bravo Piper Lance one eight miles north of Esperance on descent from three thousand inbound estimate Esperance at three three Esperance."
    , "You wish to cancel SARTIME after landing at Esperance. The Perth FIA frequency is 119.8MHz and the Esperance CTAF is 126.7MHz. What would be your radio call?" !-
      DirectAnswer
        "Melbourne Centre FLIGHTWATCH Alfa Sierra Bravo landed Esperance cancel SARTIME (on area frequency 119.8)."        
    , "You are taxiing at Parafield in Warrior VH-DIM for a period of airwork in the local training area. You have received ATIS information 'Golf' and decide the appropriate runway for you to operate on is 21 right. Give your taxi report." !-
      DirectAnswer
        "Parafield Ground Delta India Mike Warrior received Golf for the training ara Runway 21 right, request taxi."
    , "If Parafield Tower instructs you to join downwind and you intend to do a full-stop landing in Trinidad VH-PES, what is your next call?" !-
      DirectAnswer
        "Papa Echo Sierra downwind full stop."
    ]

atcFROL7 ::
  Exam
atcFROL7 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 7")
    [
      "What transponder code should you select if you suffer a VHF-COM radio failure?" !-
      DirectAnswer
        "7600."
    , "Where can you find the format of the distress (mayday) radio call?" !-
      DirectAnswer
        "In the EMERG section of ERSA."
    , "Write down the pattern of a Mayday call. (You must get this right!)" !-
      DirectAnswer
        "**Mayday mayday mayday [aircraft callsign] [position and time] [heading] [airspeed] [altitude] [aircraft type] [nature of distress] [captain's intentions] [any other information that may facilitate the rescue].**"
    , "As further support for a Mayday call, when operating in a radar environment in an aircraft that is fitted with a transponder, what code should you squawk?" !-
      DirectAnswer
        "7700."
    , "Write down the pattern of a Pan-pan call." !-
      DirectAnswer
        "**Pan-pan pan-pan pan-pan [callsign of a specific station or 'all stations'] [aircraft callsign] [request for bearing, course or position, if required] [position and time] [heading] [airspeed] [altitude] [aircraft type] [available flight time] [nature of emergency] [captain's intentions].**"
    , "Where would you find the procedures to follow in the event of a radio communications failure?" !-
      DirectAnswer
        "ERSA EMERG."
    , "When are you required to carry an emergency locator transmitter?" !-
      DirectAnswer
        "On all flights for which carriage of life rafts is required, and on such over water flights as may be specified, and through designated remote areas if HF radio is not carried."
    ]

atcFROL8 ::
  Exam
atcFROL8 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 8")
    [
      "What are the appropriate frequencies to listen out when operating in Class G airspace?" !-
      DirectAnswer
        "The CTAF/MULTICOM, FIA or SIS frequency appropriate to that part of Class G airspace in which you are operating."
    , "List five items which are strictly prohibited for use in radio transmissions." !-
      DirectAnswer
        "Calls containing: profane or obscene language; deceptive or false information; improper use of callsigns of other aircraft; non-operational requirements; and calls of a personal nature."
    ]

atcFROL9 ::
  Exam
atcFROL9 =
  Exam
    "Aviation Theory Centre"
    (Just "Flight Radio for Pilots VFR Operations")
    (Just "Review Questions 9")
    [
      "List the frequency bands used in aviation and state what each band is used for." !-
      DirectAnswer
        "Low frequency (LF) - non-directional beacons (NDB). Medium frequency (MF) - non-directional beacons (NDB). High frequency (HF) - long range voice communications. Very high frequency (VHF) - normal, everyday voice communications, VOR and ILS localiser directional signal (radio navigation aids). Ultra high frequency (UHF) - military voice communications, ILS glideslope, international DME, secondary surveillance radar (SSR). Very low frequency (VLF) - global navigation systems such as Omega."
    , "The range over which VHF transmissions can be received is known as (visible/radio) horizon." !-
      DirectAnswer
        "Radio horizon."    
    , "What is the expected range of VHF reception for an aeroplane flying at 7,000ft?" !-
      DirectAnswer
        "VHF range in nm = `sqrt (1.5 * 7,000) = sqrt 10,500 = 102nm`."
    , "What are the approximate ranges of VHF transmissions for an aeroplane when it is at 1,000ft, 5,000ft and 10,000ft?" !-
      DirectAnswer
        "`1,000ft: 40nm`. `5,000ft: 90nm`. `10,000ft: 120nm`."    
    , "In which CASA publication would you find the expected VHF communication coverage at 5,000ft and 10,000ft?" !-
      DirectAnswer
        "Planning Chart Australia (PCA)."    
    , "Draw a diagram showing reflecting and refraction of radio waves." !-
      DirectAnswer
        "Refer to Figure A2-2."    
    , "When a radio wave is attenuated, the signal strength is progressively (increased/decreased)." !-
      DirectAnswer
        "Decreased."    
    , "Draw a diagram to show the difference between amplitude and frequency modulation." !-
      DirectAnswer
        "Refer to Figure A2-3."
    , "List five fundamental components of a radio transmitter, and briefly explain their function." !-
      DirectAnswer
        "Each radio transmitter has: a power supply to provide direct current; an oscillator to generate a radio frequency (carrier wave) and a device for controlling the frequency of the generated signal; an amplifier to increase the output of the oscillator; a modulator to add the intelligence signals (e.g. from the microphone) to the carrier wave; and an antenna (or aerial) to transmit the signals out into space."
    , "If two radio waves arrive simultaneously at a receiver, and are in phase, the resultant signal will be (stronger/weaker) than when they are received out of phase." !-
      DirectAnswer
        "Stronger."    
    , "The layer in the atmoshpere which refracts radio waves back to the surface of the earth is called the (troposphere/ionosphere/mesosphere)." !-
      DirectAnswer
        "Ionosphere."
    , "The radio waves that are refracted back to the earth's surface by a layer in the atmosphere are in the (VHF/HF/UHF) band." !-
      DirectAnswer
        "HF."
    , "When using HF for voice communications at night, the (lower/higher) the frequency, the greater the transmission range." !-
      DirectAnswer
        "Lower."
    , "As a general rule, when using HF communications, the closer you are to a station, the (lower/higher) frequency you should use." !-
      DirectAnswer
        "Lower."
    , "Draw a diagram and show skip distance varying with radio transmission frequency." !-
      DirectAnswer
        "Refer to Figure A2-4."
    ]

atcFROL ::
  [Exam]
atcFROL =
  [
    atcFROL1
  , atcFROL2
  , atcFROL3
  , atcFROL4
  , atcFROL5
  , atcFROL6
  , atcFROL7
  , atcFROL8
  , atcFROL9
  ]

frol ::
  Exam
frol =
  Exam
    "Flight Radio Operator Licence"
    (Just "Aircraft Radio Operator Certificate of Proficiency")
    (Just "Self-made practice exam")
    [
      "*Initial contact* ARCHER TOWER. Callsign AFR" !-
      DirectAnswer
        "Archer Tower Alpha Foxtrot Romeo"
    , "*Readback* Delta Alpha November cleared to land runway two eight right" !-
      DirectAnswer
        "cleared to land runway two eight right Delta Alpha November"
    , "*Readback* India Echo Sierra descend to and maintain three thousand five hundred" !-
      DirectAnswer
        "three thousand five hundred India Echo Sierra"
    , "*Readback* Oscar Romeo Echo squawk two thousand" !-
      DirectAnswer
        "two thousand Oscar Romeo Echo [transponder to 2000]"
    ]

flightOneFROL ::
  Exam
flightOneFROL =
  Exam
    "Flight One paper FROL exam"
    (Just "Flight One paper FROL exam")
    Nothing
    [
      "As a VFR aircraft, airborne and tracking for a Class C Control Zone to transit, you should:" ~>
      Multichoice
        [
        ]

        "Call the local Centre/Radar Frequency for a Squawk Code and Clearance"

        [
          "Call the associated Military Approach (APP) Frequency for a Squawk Code and Clearance"
        , "Call the associated Military Airways Clearance Delivery (ACD) frequency for a Squawk Code and Clearance"
        , "Call the associated Aerodrome Tower (TWR) frequency for a Squawk Code and Clearance"
        ]
    , "Attenuation can be best described as:" ~>
      Multichoice
        [
          "A button pressed on the audio panel which attenuates the volume so you can listen to your ipod and monitor the COMS at the same time"
        ]

        "The absorption and scattering of some radio waves as a result of water vapour, cloud, dust, etc."

        [
          "A decrease in the power of radio waves returning to a transceiver as a result of great distance travelled"
        , "A decrease in the power of radio waves arriving at a transceiver as a result of great distance travelled"
        ]
    , "If you switch to another ATC unit, wait to find no other transmissions and make your call to find the response: TWO STATIONS IN TOGETHER ON BRISBANE CENTRE. This means:" ~>
      Multichoice
        [
          "Your transmission was over transmitted by someone else and you should await their call then make yours"
        , "Your transmission was over transmitted by someone else and you should make your call again quickly before the jump on your transmission a second time"
        ]

        "Your transmission was made simultaneously with another station and you should wait for the other station to make a call unless yours is of an urgent or distress manner"

        [
          "Your transmission was made simultaneously with another station and you should make your call again quickly, regardless of the nature"
        ]
    , "A cable antenna extending from one extremity of an aircraft to another under a degree of tension is most likely to be for which of the following?" ~>
      Multichoice
        [
          "The ADF"
        ]

        "The HF Radio"

        [
          "The VHF COM"
        , "The VHF NAV"
        ]
    , "The NDB/ADF system relies primarily on which of the following types of wave propagation?" ~>
      Multichoice
        [
          "Sky waves"
        , "Direct waves"
        ]

        "Ground waves"

        [
          "None of the above"
        ]
    , "Once you have established two-way COMS with a ATC Unit and you have been **IDENTIFIED**, this means:" ~>
      Multichoice
        [
          "You are under radar control"
        , "Your transponder is on the correct code and is periodically interrogated by Secondary Surveillance Radar"
        , "You are given ATC guided separation from IFR Traffic"
        ]

        "All of the above"

        [
        ]
    , "Which of the following is/are some or all of the minimum requirements to hold a Radiotelephone Operator Licence?" ~>
      Multichoice
        [
          "Pass a written test/examination"
        , "Pass a PPL *(should be RPL)* flight test or other suitable practical test"
        , "Be at least 16 years of age or older"
        ]

        "All of the above"

        [
        ]
    , "After receiving a report of: READING YOU FIFES. From ATC, you understand that:" ~>
      Multichoice
        [
        ]

        "On a scale of one to five for clarity and strength, you have five for each"

        [
          "Your radio is only working at half strength and clarity"
        , "Your radio is clear but signal is faint"
        , "Carrier wave only"
        ]
    , "VHF radio waves propagate as:" ~>
      Multichoice
        [
          "Sky waves"
        ]

        "Direct waves"

        [
          "Ground waves"
        , "None of the above"
        ]
    , "The Automatic Terminal Information Service (ATIS) is usually broadcast on an aerodrome's:" ~>
      Multichoice
        [
          "Discrete ATIS only frequency where available"
        , "VHF NAV frequency where available"
        , "NDB frequency where available"
        ]

        "All of the above"

        [
        ]
    , "Which of the following styles of headphones and microphone combination Jack might you possibly find in a general aviation aircraft?" ~>
      Multichoice
        [
        ]

        "Two Jacks: one headphone one microphone with straight cables"

        [
          "Two Jacks: one headphone one microphone with coiled cables"
        , "1 Jack: All through one straight cable"
        , "None of the above"
        ]
    , "Approaching a Class C control boundary you plan to request clearance having not made two-way COMS with the ATC unit during this sector. The most correct example of an approaching radio call is:" ~>
      Multichoice
        [
          "Brisbane Centre, Alpha Bravo Charlie. (to gain two way COMS first before your request)"
        ]

        "Brisbane Centre, Alpha Bravo Charlie Cessna 172 is over head Maleny, 4500 feet, tracking for Archerfield Request clearance"

        [
          "Brisbane Centre, Alpha Bravo Charlie Cessna 172 is over head Maleny, 4500 feet, tracking for Archerfield Request Code"
        , "Brisbane Centre, Alpha Bravo Charlie, request when ready. (to gain two way COMS first before your request)"
        ]
    , "Which of the following is not an error associated with VHF NAV Ground stations?" ~>
      Multichoice
        [
          "Site Error"
        ]

        "Terrain effect"

        [
          "Aggregate Error"
        , "Scalloping"
        ]
    , "The Automatic Terminal Information Service (ATIS) is usually broadcast on an aerodrome's:" ~>
      Multichoice
        [
          "Discrete ATIS only frequency where available"
        , "HF frequency where available"
        , "VHF NAV frequency where available"
        ]

        "All of the above"

        [
        ]
    , "On the radio control panel, is it possible to set up the avionics so that multiple radios are receiving at once?" ~>
      Multichoice
        [
          "Yes - but only if it is a Garmin Stack"
        ]

        "Yes - select the COM and NAV units desired to be receiving and tune correct frequency"

        [
          "No - no brand is capable of this at this stage"
        , "No - you can only select one COM and one NAV at a time"
        ]
    , "A departure report for a VFR aircraft is required when operating at a:" ~>
      Multichoice
        [
          "Class C Aerodrome"
        , "Class D Aerodrome"
        , "Class G Aerodrome"
        ]

        "All of the above"

        [
        ]
    , "When requested by an ATC unit ALPHA BRAVO CHARLIE, BRISBANE CENTRE, RADIO CHECK HOW DO YOU READ? To give a report of very good clarity and strength, you would reply:" ~>
      Multichoice
        [
        ]

        "Reading you fifes"

        [
          "Reading you fine"
        , "Reading you Nines"
        , "None of the above"
        ]
    , "The NDB transmits typically within a range of:" ~>
      Multichoice
        [
          "200 to 400"
        ]

        "200 to 430"

        [
          "100 to 400"
        , "195 to 419"
        ]
    , "Where would you find the frequency for the Automatic Enroute Information Service (AERIS) that related to your current position?" ~>
      Multichoice
        [
          "A World Aeronautical Chart (WAC)"
        , "A Planning Chart Australia (PCA)"
        ]

        "A Visual Terminal Chart (VTC)"

        [
          "All of the above"
        ]
    , "The HF Radio relies primarily on which of the following types of wave propagation?" ~>
      Multichoice
        [
        ]

        "Sky waves"

        [
          "Direct waves"
        , "Ground waves"
        , "None of the above"
        ]
    , "What is the purpose of an antenna?" ~>
      Multichoice
        [
        ]

        "To receive and transmit radio signals"

        [
          "To receive radio signals only"
        , "To transmit radio signals only"
        , "None of the above"
        ]
    , "An ATC unit replies to your request as: *Alpha Bravo Charlie, squawk code 3447, QNH 1016*. Your reply should be:" ~>
      Multichoice
        [
          "Squawk code 3447, 1016 ALPHA BRAVO CHARLIE"
        , "3447, QNH 1016 ALPHA BRAVO CHARLIE"
        , "Squawk code 3447, QNH 1016 ALPHA BRAVO CHARLIE"
        ]

        "3447, 1016 ALPHA BRAVO CHARLIE"

        [
        ]
    , "The use of flight radios is limited to" ~>
      Multichoice
        [
          "Emergencies only"
        , "Aviation related clearances, requests, cancellations"
        , "Company communications"
        ]

        "All of the above"

        [
        ]
    , "The squelch control on your radio can be best described as:" ~>
      Multichoice
        [
          "A control to tune slim dusty and other country music during flight"
        , "A control for individual headphone volume"
        , "A control for overall system volume"
        ]

        "A control for pickup sensitivity at a microphone"

        [
        ]
    , "When inside controlled airspace and transiting from one frequency zone to another, you are handed off to make two-way COMS with Brisbane Centre 121.2. Your first call on this frequency should be:" ~>
      Multichoice
        [
          "Brisbane Centre 121.2 Alpha Bravo Charlie maintaining 4500"
        , "Brisbane Centre 121.2 Alpha Bravo Charlie"
        ]

        "Brisbane Centre Alpha Bravo Charlie maintaining 4500"

        [
          "Brisbane Centre Alpha Bravo Charlie"
        ]
    , "As the holder of a FROL, you are authorised to:" ~>
      Multichoice
        [
          "Operate a radiotelephone system that is installed in the aircraft only"
        , "Operate a radiotelephone system that is carried but not hard wired in the aircraft only"
        ]

        "Operate a radiotelephone system that is installed or carried in the aircraft"

        [
          "Not allowed to use the system at all"
        ]
    , "A VHF NAV radio receives in a frequency band of:" ~>
      Multichoice
        [
          "118.00 to 134.95"
        , "117.00 to 133.95"
        , "112.00 to 117.95"
        ]

        "108.00 to 116.95"

        [
        ]
    , "Which of the following factors may reduce coverage or limit the useful range of a VHF transmitter?" ~>
      Multichoice
        [
          "Thunderstorms"
        , "Coastal Refraction"
        ]

        "Terrain separating transmitter and receiver"

        [
          "Scalloping"
        ]
    , "Which of the following is an accurate example of a distress call?" ~>
      Multichoice
        [
        ]

        "MAYDAY MAYDAY MAYDAY, Brisbane Radar, Alpha Bravo Charlie, Alpha Bravo Charlie, Alpha Bravo Charlie, Cessna 172, Engine Failure, Attempting Forced Landing near Logan Village, 2 POB."

        [
          "MAYDAY MAYDAY MAYDAY, Brisbane Radar, Alpha Bravo Charlie, Alpha Bravo Charlie, Alpha Bravo Charlie, Cessna 172, Engine Failure, Logan Village, 2 POB."
        , "PAN PAN, PAN PAN, PAN PAN, Brisbane Radar, Alpha Bravo Charlie, Alpha Bravo Charlie, Alpha Bravo Charlie, Cessna 172, Engine Failure, Attempting Forced Landing near Logan Village, 2 POB."
        , "PAN PAN, PAN PAN, PAN PAN, Brisbane Radar, Alpha Bravo Charlie, Alpha Bravo Charlie, Alpha Bravo Charlie, Cessna 172, Engine Failure, Attempting Forced Landing near Logan Village, 2 POB."
        ]
    , "The memory verse of TUNE IDENTIFY TEST is to be carried out while using:" ~>
      Multichoice
        [
          "VHF NAV"
        , "VHF COM"
        , "ADF"
        ]

        "VHF NAV and ADF"

        [
        ]
    , "Approaching the runway on final approach at a controlled aerodrome having conducted the appropriate COM Failure procedure, you look to the tower for confirmation the landing area is safe to land. You see a green light - to acknowledge receipt to the tower you should:" ~>
      Multichoice
        [
        ]

        "Flash the landing lights"

        [
          "Turn all lights off"
        , "Turn all lights on"
        , "Manouvre the aircraft in roll to 15deg AOB each way"
        ]
    , "Company communications are limited to:" ~>
      Multichoice
        [
          "Company discrete frequencies"
        , "Pertinent operational information"
        , "Discussions relating to company employees"
        ]

        "Company discrete frequencies and pertinent operational information"

        [
        ]
    , "When asked by an ATC unit to advise an estimate for a place of destination, you should express the following time as: June 24th 10:52am EST" ~>
      Multichoice
        [
          "Estimate XYZ at 06241052"
        , "Estimate XYZ at 06241052 ZULU"
        , "Estimate XYZ at 1052 ZULU"
        ]

        "Estimate XYZ at 0052 ZULU"

        [
        ]
    , "Which of the following is a limitation of the NDB/ADF system?" ~>
      Multichoice
        [
          "Scalloping"
        , "Site Error"
        ]

        "Coastal Refraction"

        [
          "Line of sight"
        ]
    , "What would be the transponder code to display in the event of a suspected COMS failure?" ~>
      Multichoice
        [
          "7700"
        ]

        "7600"

        [
          "7500"
        , "3000"
        ]
    , "Some aircraft systems are installed with a coinciding Avionics Master Switch. What is the function of this switch?" ~>
      Multichoice
        [
          "It is a way to cut power only from all radios"
        , "It is a way to control which power source the radios operate from"
        ]

        "Controls power source of all radio equipment"

        [
          "Controls power to communications VHF radios only"
        ]
    , "As a VFR aircraft, airborne and tracking for a Military Restricted Control Zone to transit, you should:" ~>
      Multichoice
        [
          "Call the local Civilian Centre Frequency for a Squawk Code and Clearance"
        , "Call the associated Military Approach (APP) Frequency for a Squawk Code and Clearance"
        ]

        "Call the associated Military Airways Clearance Delivery (ACD) frequency for a Squawk Code and Clearance"

        [
          "Call the associated Military Tower (TWR) frequency for a Squawk Code and Clearance"
        ]
    , "WILCO means:" ~>
      Multichoice
        [
          "Understood"
        ]

        "Understood and will comply with requirement"

        [
          "Looking for the traffic"
        , "None of the above"
        ]
    , "A VHF COM radio receives in a frequency band of:" ~>
      Multichoice
        [
          "118.00 to 134.95"
        ]

        "117.00 to 133.95"

        [
          "109.00 to 117.95"
        , "108.00 to 116.95"
        ]
    , "123.6 as a frequency should be read as:" ~>
      Multichoice
        [
          "ONE TWO THREE DAYCIMAL SIX"
        ]

        "WUN TOO TREE DAYCIMAL SIX"

        [
          "ONE TWO TREE DECIMAL SIX"
        , "WUN TOO TREE DAYCIMAL SIIIX"
        ]
    ]

instruments ::
  Exam
instruments =
  Exam
    "Instrument Flight"
    (Just "Self-made questions")
    Nothing
    [
      "Which instruments are vacuum powered?" !-
      DirectAnswer
       "Attitude Indicator, Heading Indicator"
    , "Which instruments are pitot-static powered?" !-
      DirectAnswer
        "Air Speed Indicator, Altimeter, Vertical Speed Indicator"
    , "Which pitot-static powered instruments use only the static port?" !-
      DirectAnswer
        "Altimeter, Vertical Speed Indicator"     
    , "Which instruments are electric gyrometer powered?" !-
      DirectAnswer
        "Turn Indicator"                
    , "How is the Air Speed Indicator powered?" !-
      DirectAnswer
        "Pitot-static"  
    , "How is the Altimeter powered?" !-
      DirectAnswer
        "Pitot-static (static port only)"
    , "How is the Vertical Speed Indicator powered?" !-
      DirectAnswer
        "Pitot-static (static port only)"
    , "How is the Attitude Indicator powered?" !-
      DirectAnswer
        "Vacuum"
    , "How is the Heading Indicator powered?" !-
      DirectAnswer
        "Vacuum"
    , "How is the Turn Indicator powered?" !-
      DirectAnswer
        "Electric gyrometer"
    ]

bobTaitChapterRevisionA1 ::
  Exam
bobTaitChapterRevisionA1 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A1 Aerodynamics, Motion and Control")
    [
      "Deflection of the elevators in flight cause the aeroplane to" ~>
      Multichoice
        [
          "pitch about its normal axis"
        ]

        "pitch about its lateral axis"

        [
          "yaw about its normal axis"
        , "pitch about its longitudinal axis"
        ]
    , "Which of the following correctly associates the control surface, type of motion involved and the axis about which that motion occurs?" ~>
      Multichoice
        [
          "rudder, yaw, longitudinal"
        , "elevator, pitch, normal"
        ]

        "aileron, roll, longitudinal"

        [
          "rudder, pitch, normal"
        ]
    , "The centre of gravity of an aircraft is a point" ~>
      Multichoice
        [
          "halfway along the longitudinal axis"
        , "in the centre of the wing"
        ]

        "at which all three axes intersect"

        [
          "directly above the wheels"
        ]
    , "The best indication of nose attitude is obtained when the pilot" ~>
      Multichoice
        [
          "looks to the side and notes the position of the wing tips in relation to the horizon"
        ]

        "looks to the front and notes the position of the horizon in the aircraft's windscreen"

        [
          "looks at the instrument panel and notes the aircraft's present height and speed"
        , "looks to the right and asks the instructor to point it out"
        ]
    , "When the control column is turned to the left during flight" ~>
      Multichoice
        [
          "the right aileron is raised and the aircraft rolls to the right"
        ]

        "the left aileron is raised and the aircraft rolls to the left"

        [
          "the right aileron is raised and the aircraft rolls to the left"
        , "the left aileron is raised and the aircraft rolls to the right"
        ]
    , "When the right rudder pedal is depressed during flight" ~>
      Multichoice
        [
          "the rudder moves to the left and the aircraft yaws to the left"
        , "the rudder moves to the left and the aircraft yaws to the right"
        , "the rudder moves to the right and the aircraft yaws to the left"
        ]

        "the rudder moves to the right and the aircraft yaws to the right"

        [
        ]
    , "When a back pressure is applied to the control column during straight and level flight" ~>
      Multichoice
        [
        ]

        "the elevator is deflected up and the aircraft adopts a higher nose attitude"

        [
          "the elevator is deflected up and the aircraft adopts a lower nose attitude"
        , "the elevator is deflected down and the aircraft adopts a lower nose attitude"
        , "the elevator is deflected down and the aircraft adopts a higher nose attitude"
        ]
    , "If no control input is made by the pilot, which of the following types of aircraft motion will always occur simultaneously?" ~>
      Multichoice
        [
          "pitch and yaw"
        , "pitch and roll"
        , "pitch, yaw and roll"
        ]

        "yaw and roll"

        [
        ]
    , "Which is the most appropriate recovery action if an aircraft is allowed to enter a spiral dive?" ~>
      Multichoice
        [
          "pull back firmly on the control column"
        , "apply full rudder against the direction of turn"
        ]

        "apply opposite aileron to level the wings"

        [
          "apply aileron in the direction of turn until the wings are level"
        ]
    , "A single-engine aircraft is climbing with high power applied and low airspeed. Which of the following will be true concerning the effectiveness of the flying controls?" ~>
      Multichoice
        [
          "the ailerons and rudder will be more effective than the elevator"
        ]

        "the ailerons will be less effective than the elevator or rudder"

        [
          "the ailerons will be more effective than the elevator or rudder"
        , "the ailerons and elevator will be more effective than the rudder"
        ]
    ]

bobTaitChapterRevisionA2 ::
  Exam
bobTaitChapterRevisionA2 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A2 Aerodynamics - Vectors, Forces and Stalling - LIFT")
    [
      "The relative airflow is always" ~>
      Multichoice
        [
          "opposite the direction of motion and faster than the true airspeed"
        , "in the same direction as motion and equal to the true airspeed"
        ]

        "opposite the direction of motion and equal to the true airspeed"

        [
          "in the same direction as motion and faster to the true airspeed"
        ]
    , "All of the aerodynamic forces acting on an aerofoil can be resolved into a single force acting on a single point. This single force is called" ~>
      Multichoice
        [
          "lift"
        , "drag"
        ]

        "the total reaction"

        [
          "total lift"
        ]
    , "The force of lift is" ~>
      Multichoice
        [
        ]

        "that component of the total reaction acting at right angles to the relative airflow"

        [
          "that component of the total reaction acting parallel to the relative airflow"
        , "the combined effect of all forces acting on an aerofoil"
        , "that component of the total reaction acting at right angles to the chord line"
        ]
    , "The angle of attack of an aerofoil is the angle between" ~>
      Multichoice
        [
          "the relative airflow and the bottom surface of the wing"
        , "the nose of the aircraft and the horizon"
        , "the chord line and the horizon"
        ]

        "the chord line and the direction of the relative airflow"

        [
        ]
    , "The magnitude of the total aerodynamic reaction on an aerofoil depends upon" ~>
      Multichoice
        [
          "the speed of the relative airflow only"
        , "the angle of attack only"
        ]

        "both the angle of attack of the aerofoil and the speed of the relative airflow"

        [
          "the forward speed of the aerofoil only"
        ]
    , "To maintain level flight at a lower airspeed" ~>
      Multichoice
        [
        ]

        "the aircraft must fly at a higher nose attitude and an increased angle of attack"

        [
          "the aircraft must fly at a lower nose attitude and an increased angle of attack"
        , "the aircraft must fly at a higher nose attitude and an decreased angle of attack"
        , "the aircraft must fly at a lower nose attitude and an decreased angle of attack"
        ]
    , "Once the stalling angle has been reached, any further increase in angle of attack will result in" ~>
      Multichoice
        [
          "more lift and less drag"
        , "more lift and more drag"
        ]

        "less lift and less drag"

        [
          "less lift and more drag"
        ]
    , "Level flight is not possible at speeds below the stalling speed because" ~>
      Multichoice
        [
        ]

        "the maximum lift available is less than weight"

        [
          "there is no lift available"
        , "drag becomes greater than lift"
        , "the air flowing over the control surfaces produces no reaction"
        ]
    , "For any given airspeed, the stalling angle of attack at which" ~>
      Multichoice
        [
          "drag reaches its maximum value"
        , "lift reaches its minimum value"
        , "drag reaches its minimum value"
        ]

        "lift reaches its maximum value"

        [
        ]
    , "As the angle of attack is increased from zero to beyond the stalling angle at a constant airspeed, the magnitude of the force of lift" ~>
      Multichoice
        [
          "increases continuously"
        ]

        "increases then decreases"

        [
          "decreases continuously"
        , "decreases then increases"
        ]
    ]

bobTaitChapterRevisionA3 ::
  Exam
bobTaitChapterRevisionA3 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A3 Aerodynamics - Vectors, Forces and Stalling - DRAG")
    [
      "Drag is that component of the total reaction which acts" ~>
      Multichoice
        [
          "at right angles to the relative airflow"
        ]

        "opposite to the direction of motion and parallel to the relative airflow"

        [
          "at right angles to the chord line"
        , "in the same direction as motion and parallel to the relative airflow"
        ]
    , "The chief source of drag on an aircraft flying level at low airspeed is" ~>
      Multichoice
        [
          "lift is no longer acting at right angles to the relative airflow"
        ]

        "air spilling over the wingtips producing vortices and eddies"

        [
          "the angle of attack being lower than at normal cruising speed"
        , "the elevator requiring a large degree of deflection"
        ]
    , "The form of drag which predominates at low airspeed is called" ~>
      Multichoice
        [
          "parasite drag"
        , "total drag"
        , "airflow drag"
        ]

        "induced drag"

        [
        ]
    , "The form of drag which predominates at high airspeed is called" ~>
      Multichoice
        [
        ]

        "parasite drag"

        [
          "total drag"
        , "airflow drag"
        , "induced drag"
        ]
    , "As airspeed is increased in level flight from just above stalling speed to maximum speed, the induced drag acting" ~>
      Multichoice
        [
          "increases continuously"
        ]

        "decreases continuously"

        [
          "increases then decreases"
        , "decreases then increases"
        ]
    , "As airspeed is increased in level flight from just above stalling speed to maximum speed, the parasite drag acting" ~>
      Multichoice
        [
        ]

        "increases continuously"

        [
          "decreases continuously"
        , "increases then decreases"
        , "decreases then increases"
        ]
    , "The combined effect of induced drag and parasite drag gives rise to total drag. The *least* total drag occurs" ~>
      Multichoice
        [
          "at low airspeed when parasite drag is lowest"
        , "at high airspeed when parasite drag is lowest"
        ]

        "at an intermediate airspeed when both induced and parasite drag are fairly low"

        [
          "at the stalling speed when the speed of the relative airflow is lowest"
        ]
    , "As speed is reduced in level flight from maximum speed to the stalling speed, the total drag acting" ~>
      Multichoice
        [
          "increases continuously"
        , "decreases continuously"
        , "increases then decreases"
        ]

        "decreases then increases"

        [
        ]
    , "A wing enjoys its maximum efficiency in level flight when" ~>
      Multichoice
        [
          "airspeed is highest"
        , "airspeed is lowest"
        ]

        "total drag is least"

        [
          "lift is greatest"
        ]
    , "The best lift/drag ratio occurs" ~>
      Multichoice
        [
          "at the stalling angle where maximum lift is being produced"
        , "at the smallest possible angle of attack where minimum drag is being produced"
        ]

        "at a medium angle of attack where the required lift is accompanied by least drag"

        [
          "at the lowest airspeed where the least disturbance to the airflow occurs"
        ]
    ]

bobTaitChapterRevisionA4 ::
  Exam
bobTaitChapterRevisionA4 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A4 Aerodynamics - Design and Performance")
    [
      "The application of high power with low airspeed is accompanied by" ~>
      Multichoice
        [
        ]

        "engine torque producing a roll"

        [
          "a reduction in elevator effectiveness"
        , "engine torque producing a yaw"
        , "slipstream effect producing a roll"
        ]
    , "The effect of propellor slipstream on a single-engine aeroplane is to" ~>
      Multichoice
        [
        ]

        "produce a yaw at low IAS and high power"

        [
          "produce a roll at low IAS and high power"
        , "produce a yaw at high IAS and low power"
        , "product a nose-down pitch at low IAS and high power"
        ]
    , "The effect of a strong crosswind from the left while taxiing on level ground is" ~>
      Multichoice
        [
        ]

        "the aircraft will tend to yaw to the left"

        [
          "the aircraft will tend to yaw to the right"
        , "the aircraft will tend to roll to the left"
        , "the aircraft will tend to roll and yaw to the right"
        ]
    , "Consider an aircraft flying at 5,500ft in a headwind. Under these conditions" ~>
      Multichoice
        [
          "IAS would be higher than TAS which would be higher than GS"
        , "GS would be higher than TAS which would be higher than IAS"
        , "IAS would be higher than TAS which would be lower than GS"
        ]

        "IAS would be lower than TAS which would be higher than GS"

        [
        ]
    , "Which of the following would be most effective in reducing induced drag?" ~>
      Multichoice
        [
          "fitting wheel fairings"
        , "streamlining all surfaces exposed to the airflow"
        , "fitting wing root fairings"
        ]

        "increasing the aspect ratio of the wing"

        [
        ]
    , "If airspeed is kept constant, which of the following is an effect of extending flap during level flight?" ~>
      Multichoice
        [
          "lift increases and drag decreases"
        ]

        "lift increases and drag increases"

        [
          "lift decreases and drag decreases"
        , "lift decreases and drag increases"
        ]
    , "Which of the following is an effect of lowering full flap during a glide?" ~>
      Multichoice
        [
        ]

        "a steeper descent path may be flown at the same airspeed"

        [
          "the extra lift will produce a flatter descent path"
        , "the aircraft will be able to fly down the same descent path at a lower airspeed"
        , "forward visibility will be reduced"
        ]
    , "Under what circumstances should flap be used for take-off?" ~>
      Multichoice
        [
          "flap should never be used for take-off under any circumstances"
        ]

        "flap should be used only as specified by the manufacturer"

        [
          "full flap should be used if the field is of marginal length"
        , "flap should be used during the take-off roll if the pilot feels it is necessary"
        ]
    , "If airspeed is kept constant, which of the following is an effect of extending flap during level flight?" ~>
      Multichoice
        [
          "a higher nose attitude is required to maintain lift"
        ]

        "it is possible to fly level with a lower nose attitude"

        [
          "less power is required to maintain a given airspeed"
        , "the elevator and rudder controls will become more effective"
        ]
    , "If all other factors remain the same, what effect will a decrease in air density have on the take off distance required?" ~>
      Multichoice
        [
        ]

        "the distance required increases"

        [
          "the distance required remains the same unless weight is increased"
        , "the distance required decreases because of reduced drag"
        , "the distance required remains the same providing full power is used"
        ]
    , "Which of the figures below correctly shows the relative positions of the tailplane, elevator and trim tab on an aircraft which has been trimmed for level flight at low airspeed?" ~>
      Multichoice
        [
        ]

        "elevator deflecting upward from aerofoil, trim tab deflecting downward from elevator"

        [
          "elevator deflecting downward from aerofoil, trim tab deflecting downward from elevator"
        , "elevator deflecting downward from aerofoil, trim tab deflecting upward from elevator"
        , "elevator deflecting upward from aerofoil, trim tab deflecting upward from elevator"
        ]
    , "Compared to nil wind conditions, a tailwind component during take-off will result in" ~>
      Multichoice
        [
          "a decrease in the distance required for take-off"
        ]

        "an increase in the distance required for take-off"

        [
          "a decrease in the IAS required for take-off"
        , "an increase in the IAS required for take-off"
        ]
    , "Which of the following combination of factors would have the most detrimental effect on the take-off performance of an aeroplane?" ~>
      Multichoice
        [
          "high temperature, high airfield elevation and a high pressure system"
        ]

        "high temperature, high airfield elevation and a low pressure system"

        [
          "low temperature, low airfield elevation and a low pressure system"
        , "high temperature, low airfield elevation and a low pressure system"
        ]
    , "If frost forms on the wings of an aircraft overnight and is not removed before flight" ~>
      Multichoice
        [
          "the acceleration during the take-off run will be less than normal"
        , "the stalling angle will be higher than normal"
        ]

        "the take-off distance could be greatly increased"

        [
          "there will be an increase in stalling speed and stalling angle"
        ]
    , "The purpose of aerodynamic balancing of control surfaces is to" ~>
      Multichoice
        [
        ]

        "lighten the in-flight loads on the control column"

        [
          "prevent the control surface from fluttering"
        , "to increase the rate at which the aircraft responds to control inputs"
        , "ensure the aircraft will be controllable at low airspeed"
        ]
    ]

bobTaitChapterRevisionA5 ::
  Exam
bobTaitChapterRevisionA5 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A5 Climbing and Descending")
    [
      "During an approach, what is the effect of a sudden decrease in headwind component on IAS and rate of descent?" ~>
      Multichoice
        [
          "IAS and rate of descent would both increase"
        , "IAS and rate of descent would both decrease"
        ]

        "IAS would decrease and rate of descent would increase"

        [
          "IAS would increase and rate of descent would decrease"
        ]
    , "Partial extension of flap in a glide at constant indicated air speed will result in" ~>
      Multichoice
        [
        ]

        "an increase in rate and angle of descent"

        [
          "a decrease in rate and angle of descent"
        , "an increase in rate of descent and a decrease in angle of descent"
        , "an increase in angle of descent and a decrease in rate of descent"
        ]
    , "A wind shear on final approach which produces a sudden drop in headwind component will result in" ~>
      Multichoice
        [
        ]

        "a drop in IAS and a decrease in lift"

        [
          "a rise in IAS and a decrease in lift"
        , "a drop in IAS and an increase in lift"
        , "a rise in IAS and an increase in lift"
        ]
    , "Extension of flap in a glide at a constant indicated air speed will result in" ~>
      Multichoice
        [
          "less drag and a steeper approach path"
        , "less drag and a shallower approach path"
        ]

        "more drag and a steeper approach path"

        [
          "more drag and a shallower approach path"
        ]
    , "If the pilot maintains the same IAS, which of the following would produce the steepest angle of descent?" ~>
      Multichoice
        [
          "an increase in power and an increase in flap extension"
        ]

        "a decrease in power and an increase in flap extension"

        [
          "an increase in power and a decrease in flap extension"
        , "a decrease in power and a decrease in flap extension"
        ]
    , "Which of the following would cause climb performance to decrease?" ~>
      Multichoice
        [
          "a reduction in aircraft weight"
        , "an increase in thrust available"
        ]

        "a turn during the climb"

        [
          "an increase in headwind component"
        ]
    , "Which of the following describes the effect of a headwind on a descent at constant IAS?" ~>
      Multichoice
        [
          "the angle of descent remains the same but the rate of descent increases"
        , "the angle of descent decreases but the rate of descent remains the same"
        , "the angle of descent increases but the rate of descent increases"
        ]

        "the angle of descent increases but the rate of descent remains the same"

        [
        ]
    , "An aircraft is trimmed to cruise in straight and level flight with no flap extended. Which of the following would result if 20deg flap is extended and the aircraft is re-trimmed to maintain straight and level flight with no change in power?" ~>
      Multichoice
        [
          "the cruising IAS will increase and the nose attitude will be lower"
        ]

        "the cruising IAS will decrease and the nose attitude will be lower"

        [
          "the cruising IAS will increase and the nose attitude will be higher"
        , "the cruising IAS will decrease and the nose attitude will be higher"
        ]
    , "Compared to nil wind conditions, which of the following effects will be noticed when climbing into a headwind?" ~>
      Multichoice
        [
          "both the rate and angle of climb will increase"
        , "angle of climb will increase and rate of climb will decrease"
        , "both the rate and angle of climb will decrease"
        ]

        "angle of climb will increase and rate of climb will remain unaltered"

        [
        ]
    , "The recommended IAS to achieve the maximum angle of climb at sea-level is 60kt for a particular aircraft. Which of the following would reduce the angle of climb?" ~>
      Multichoice
        [
        ]

        "a decrease in IAS"

        [
          "a headwind"
        , "an increase in air density"
        , "a reduction in take-off weight"
        ]
    , "If nose attitude and power are kept constant, which of the following would have the effect of decreasing the angle of climb?" ~>
      Multichoice
        [
          "an increased headwind component"
        , "a decrease in aircraft weight"
        ]

        "an increase in air temperature"

        [
          "a decrease in density height"
        ]
    , "Comparing the best rate of climb speed to the best angle of climb speed. The best rate of climb occurs" ~>
      Multichoice
        [
        ]

        "at a lower nose attitude and higher airspeed"

        [
          "at a higher nose attitude and higher airspeed"
        , "at a lower nose attitude and lower airspeed"
        , "at a higher nose attitude and lower airspeed"
        ]
    , "Which of the following would produce the highest reading on the vertical speed indicator?" ~>
      Multichoice
        [
          "climbing at the IAS for best angle of climb?"
        , "climbing at the IAS lower than that recommended for the best angle of climb?"
        ]

        "climbing at the IAS for best rate of climb?"

        [
          "climbing at the IAS lower than that recommended for the best rate of climb?"
        ]
    , "Which of the following describes the effect of a tailwind on climb performance?" ~>
      Multichoice
        [
          "angle of climb is lower and rate of climb is higher"
        , "angle of climb is higher and rate of climb is higher"
        ]

        "angle of climb is lower and rate of climb is unchanged"

        [
          "angle of climb is higher and rate of climb is unchanged"
        ]
    , "Which of the following combination of factors would require highest nose attitude in level flight?" ~>
      Multichoice
        [
        ]

        "an increase in weight and a decrease in airspeed"

        [
          "a decrease in weight and a decrease in airspeed"
        , "an increase in weight and an increase in airspeed"
        , "a decrease in weight and an increase in airspeed"
        ]
    ]

bobTaitChapterRevisionA6 ::
  Exam
bobTaitChapterRevisionA6 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "A6 Turning, Stalling, Spins and Spirals")
    [
      "An aerofoil is said to be at its stalling angle if any increase or decrease in angle of attack" ~>
      Multichoice
        [
          "produces less lift and less drag"
        , "produces a lower lift/drag ratio"
        ]

        "produces less lift"

        [
          "produces more drag"
        ]
    , "If angle of attack is increased from zero to beyond the stalling angle at constant indicated air speed, the magnitude of the force of drag" ~>
      Multichoice
        [
          "increases then decreases"
        , "decreases continuously"
        , "decreases then increases"
        ]

        "increases continuously"

        [
        ]
    , "Any increase in the angle of attack of an aerofoil up to the stalling angle, at constant indicated air speed causes" ~>
      Multichoice
        [
        ]

        "lift to increase and drag to increase"

        [
          "lift to increase and drag to decrease"
        , "lift to decrease and drag to increase"
        , "lift to decrease and drag to decrease"
        ]
    , "A pilot must maintain a back pressure on the control column during a level balanced turn. This is necessary to" ~>
      Multichoice
        [
        ]

        "increase the angle of attack"

        [
          "create extra drag to slow the aircraft down during the turn"
        , "allow the elevators to help turn the aircraft in the desired direction"
        , "improve the visibility in the direction of the turn"
        ]
    , "If an aircraft is slipping during a level turn, a remedy would be" ~>
      Multichoice
        [
        ]

        "apply more rudder in the direction of turn"

        [
          "apply less rudder in the direction of turn"
        , "apply more bank in the direction of turn"
        , "apply greater back pressure on control column"
        ]
    , "Which of the following would be the best definition of the stalling speed?" ~>
      Multichoice
        [
          "the speed which requires the lowest angle of attack in level flight"
        ]

        "the speed which requires the stalling angle to produce the necessary lift"

        [
          "the speed at which the aircraft should become airborne during take-off"
        , "the speed which produces the best angle of climb after take-off"
        ]
    , "Which of the following would decrease the IAS at which the stall occurred?" ~>
      Multichoice
        [
          "an increase in angle of bank during a turn"
        ]

        "an increase in flap extension during level flight"

        [
          "a decrease in engine power"
        , "a strong headwind"
        ]
    , "If the aircraft suffers a wing-drop at the point of stall due to turbulent conditions, the correct control input to safely recover would be to" ~>
      Multichoice
        [
          "maintain strong back pressure and apply full opposite aileron"
        , "maintain strong back pressure and apply opposite rudder"
        , "relax the back pressure and apply full opposite aileron"
        ]

        "relax the back pressure and apply opposite rudder"

        [
        ]
    , "You are about to take off from a runway immediately after a heavy jet has taken off from the same runway. Which of the following actions would give the best change of avoiding its wake turbulence?" ~>
      Multichoice
        [
          "taking off beyond its lift-point, using a shallower angle of climb and maintaining runway heading"
        , "taking off before its lift-off point, using a steeper angle of climb and turning to remain down wind of its flight path"
        , "taking off before its lift-off point, using a steeper angle of climb and maintaining runway heading"
        ]

        "taking off before its lift-off point, using a steeper angle of climb and turning to remain up wind of its flight path"

        [
        ]
    , "Compared to nil wind conditions, which of the following effects will be noticed when climbing into a headwind?" ~>
      Multichoice
        [
          "both the rate and angle of climb will increase"
        , "angle of climb will increase and rate of climb will decrease"
        , "both the rate and angle of climb will decrease"
        ]

        "angle of climb will increase and rate of climb will remain unaltered"

        [
        ]
    , "Wingtip vortices in level flight will be strongest when" ~>
      Multichoice
        [
          "weight is increased and speed is increased"
        ]

        "weight is increased and speed is decreased"

        [
          "weight is decreased and speed is decreased"
        , "weight is decreased and speed is increased"
        ]
    , "You are about to take off from a runway immediately after a heavy jet aircraft has taken off from the same runway. Which of the following is the most dangerous hazard that could affect your take-off?" ~>
      Multichoice
        [
          "difficulty in directional control caused by the blast of the jet engine exhaust"
        ]

        "turbulence after lift-off caused by the wing-tip vortices of the larger aircraft"

        [
          "difficulty in directional control during the take-off run caused by the wing-tip vortices of the jet"
        , "reduction in engine power because of the heated air behind the departing jet"
        ]
    , "As the angle of bank in a level turn is increased, the load factor" ~>
      Multichoice
        [
          "increases and the stalling speed decreases"
        ]

        "increases and the stalling speed increases"

        [
          "decreases and the stalling speed decreases"
        , "decreases and the stalling speed increases"
        ]
    , "During a 60deg bank level turn, the load factor will be" ~>
      Multichoice
        [
          "the same as in level flight"
        ]

        "twice that in level flight"

        [
          "40% greater than in level flight"
        , "40% less than in level flight"
        ]
    , "During a 60deg level turn, the stalling speed will be" ~>
      Multichoice
        [
          "the same as in level flight"
        , "twice that in level flight"
        ]

        "40% greater than in level flight"

        [
          "40% less than in level flight"
        ]
    , "When turning at low level on a very windy day a pilot may notice" ~>
      Multichoice
        [
        ]

        "an illusion of slipping or skidding when turning cross wind"

        [
          "an increased rate of turn when turning upwind"
        , "difficulty balancing the turn"
        , "a tendency to overbank"
        ]
    , "One result of the application of abrupt back pressure on the control column during a dive would be" ~>
      Multichoice
        [
          "a decrease in the angle of attack and less lift"
        , "a decrease in the angle of attack and more lift"
        ]

        "an increase in the angle of attack and possibly a stall"

        [
          "an decrease in the angle of attack and possibly a stall"
        ]
    , "Wake turbulence would be most severe behind" ~>
      Multichoice
        [
        ]

        "a heavy aircraft flying slowly"

        [
          "a light aircraft flying slowly"
        , "a heavy aircraft flying fast"
        , "a light aircraft flying fast"
        ]
    ]

bobTaitChapterRevisionGK1 ::
  Exam
bobTaitChapterRevisionGK1 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "GK1 Engines and Fuel systems")
    [
      "The onset of detonation in an engine is indicated by" ~>
      Multichoice
        [
        ]

        "vibration, rising temperatures and reduced indicated air speed"

        [
          "vibration, falling temperatures and reduced indicated air speed"
        , "vibration, rising temperatures and increased indicated air speed"
        , "vibration, falling temperatures and increased indicated air speed"
        ]
    , "Applying carburettor heat when ice is present in the carburettor is accompanied by" ~>
      Multichoice
        [
          "a drop in engine RPM with no further change"
        ]

        "a drop in engine RPM followed by a rise"

        [
          "an immediate rise in engine RPM"
        , "an initial rise in engine RPM followed by a drop"
        ]
    , "The application of carburettor heat when no ice is present in the carburettor is accompanied by" ~>
      Multichoice
        [
        ]

        "a drop in engine RPM with no further change"

        [
          "a drop in engine RPM followed by a rise"
        , "an immediate rise in engine RPM"
        , "an initial rise in engine RPM followed by a drop"
        ]
    , "The reason for filling the tanks of an aircraft which is left parked over night in cold weather is" ~>
      Multichoice
        [
        ]

        "to prevent water from collecting in the tank due to condensation"

        [
          "to increase the weight of the aircraft in case of strong wind gusts"
        , "to ensure you don't get caught if the price of fuel goes up overnight"
        , "to prevent contamination from entering the tank through the vent"
        ]
    , "The pressure exerted on the piston during the power stroke increases as the amount of gas induced" ~>
      Multichoice
        [
        ]

        "increases and combustion temperature increases"

        [
          "decreases and combustion temperature increases"
        , "increases and combustion temperature decreases"
        , "decreases and combustion temperature decreases"
        ]
    , "As the throttle is moved towards the fully open position" ~>
      Multichoice
        [
          "manifold pressure increases and gas flow decreases"
        , "manifold pressure decreases and gas flow increases"
        ]

        "manifold pressure increases and gas flow increases"

        [
          "manifold pressure decreases and gas flow decreases"
        ]
    , "The best action to take at the onset of detonation in an engine is" ~>
      Multichoice
        [
          "lean the mixture and reduce the power"
        , "lean the mixture and increase the power"
        , "decrease the indicated air speed and maintain the power"
        ]

        "select mixture fully rich and decrease the power"

        [
        ]
    , "The colour of 100 octane low lead [100 LL] aviation fuel is" ~>
      Multichoice
        [
          "green"
        , "red"
        ]

        "blue"

        [
          "clear"
        ]
    , "Which of the following is the correct sequence for the four strokes of a four-stroke internal combustion engine?" ~>
      Multichoice
        [
          "induction, compression, exhaust and power"
        , "induction, power, compression and exhaust"
        , "induction, exhaust, compression and power"
        ]

        "induction, compression, power and exhaust"

        [
        ]
    , "If AVTUR was misidentified as AVGAS and added to the tanks of a piston engine aircraft, which of the following would be the most likely result?" ~>
      Multichoice
        [
          "engine damage due to the extra power generated at take-off"
        , "reduced power output and low engine temperature"
        , "increased power output and high engine temperature"
        ]

        "reduced power output due to severe detonation"

        [
        ]
    , "If a blockage in a fuel tank vent in a gravity fed system is not discovered and removed before flight" ~>
      Multichoice
        [
          "there will be no effect providing the aircraft remains at low level"
        , "the engine will not start"
        , "fuel is likely to be lost overboard during flight"
        ]

        "the engine may fail completely due to fuel starvation"

        [
        ]
    , "The octane rating of a fuel is a measure of" ~>
      Multichoice
        [
          "its specific gravity"
        ]

        "its resistance to detonation"

        [
          "its resistance to vaporisation"
        , "its anti-misting properties in the event of fire"
        ]
    ]

bobTaitChapterRevisionGK2 ::
  Exam    
bobTaitChapterRevisionGK2 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "GK2 Engine Systems and Instruments")
    [
      "Which of the following most completely describes the function of the oil system?" ~>
      Multichoice
        [
          "it keeps the engine clean"
        , "it provides lubrication"
        , "it helps to cool the engine"
        ]

        "it does all of the above"

        [
        ]
    , "Which of the following is a likely result of operating an engine with oil temperature too low?" ~>
      Multichoice
        [
        ]

        "higher than normal oil pressure and poor lubrication due to high oil viscosity"

        [
          "lower than normal oil pressure and poor lubrication due to low oil viscosity"
        , "lower than normal oil pressure and poor lubrication due to high oil viscosity"
        , "higher than normal oil pressure and poor lubrication due to low oil viscosity"
        ]
    , "The heat collected by the oil as it passes through the engine is dissipated to the outside airflow at the" ~>
      Multichoice
        [
          "oil filter"
        , "oil pump"
        ]

        "oil cooler"

        [
          "oil sump"
        ]
    , "Which of the following in-flight indications would suggest the most serious threat to the safety of an aircraft and its passengers?" ~>
      Multichoice
        [
          "high oil pressure"
        , "high oil temperature"
        ]

        "no oil pressure"

        [
          "low oil temperature"
        ]
    , "Which of the following is a reason for a warm-up period before take-off?" ~>
      Multichoice
        [
          "to allow the oil pump to build up sufficient oil pressure"
        ]

        "to ensure the oil is brought to the correct viscosity"

        [
          "to allow any dirty oil to be cleaned by the oil filter"
        , "to ensure the spark plugs become hot enough for proper combustion of the fuel"
        ]
    , "Which of the following is true of magneto ignition?" ~>
      Multichoice
        [
        ]

        "the spark is produced mechanically without any outside electrical power"

        [
          "the magneto cannot operate unless the aircraft's electrical system is switched on"
        , "magneto ignition does not require any shielding to prevent radio interference"
        , "magneto ignition makes it safe to handle the propellor when the engine is stopped"
        ]
    , "During a pre take-off check, the pilot notices no drop in RPM when the switches are moved from 'both' to 'R' and the engine continues to run smoothly. A likely cause would be" ~>
      Multichoice
        [
          "the right magneto is in excellent condition"
        ]

        "there is a fault in the ignition switch or wiring"

        [
          "there is a fault in the right magneto"
        , "the spark plugs are fouled with oil"
        ]
    , "Oil fouling of spark plugs is most likely" ~>
      Multichoice
        [
          "after long periods of operation at high power output"
        , "when the engine is first started and the oil is cold"
        , "when the oil is overdue for changing"
        ]

        "after long periods of operation at taxi or idle power"

        [
        ]
    , "The purpose of an impulse coupling is to" ~>
      Multichoice
        [
          "provide a hotter spark when the engine is at full power"
        , "prevent oil fouling of the spark plugs"
        ]

        "provide sparks when the engine is being cranked during start up"

        [
          "allow both magnetos to fire at once"
        ]
    , "Is the following statement true or false? If the aircraft electrical system is turned off and the magneto switches are selected to 'off, it is safe to handle the propellor because there is no danger of 'kick back' when the switches are off." ~>
      Multichoice
        [
          "the statement is true"
        ]

        "the statement is false"

        [
        ]
    ]

bobTaitChapterRevisionGK3 ::
  Exam
bobTaitChapterRevisionGK3 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "GK3 Engine Systems and Instruments")
    [
      "Which of the following is a limitation of the directional gyro?" ~>
      Multichoice
        [
          "the directional gyro is more difficult to read accurately"
        , "the directional gyro wobbles about in turbulent conditions"
        ]

        "the directional gyro does not automatically line up with north, it must be set by reference to a compass"

        [
          "the directional gyro reads incorrectly during turns"
        ]
    , "When setting a directional gyro, care should be taken to ensure that" ~>
      Multichoice
        [
          "the aircraft is facing north"
        , "the aircraft is not losing or gaining any height"
        , "the aircraft's electrical system is turned off"
        ]

        "the aircraft is in steady wings-level flight in smooth air"

        [
        ]
    , "Which of the following instruments rely on the static vent to give a reliable reading?" ~>
      Multichoice
        [
        ]

        "Airspeed Indicator, Altimeter and Vertical Speed Indicator"

        [
          "Airspeed Indicator, Artificial Horizon and Vertical Speed Indicator"
        , "Turn and Balance Indicator, Artificial Horizon and Altimeter"
        , "Vertical Speed Indicator, Turn and Balance Indicator and Airspeed Indicator"
        ]
    , "What is the significant of the white arc on the face of the airspeed indicator?" ~>
      Multichoice
        [
          "it indicates the fastest and slowest speeds at which it is safe to fly"
        , "it indicates the safe climbing speeds for use after take-off"
        , "it indicates the range of approach speeds that may be used for landing"
        ]

        "it indicates the safe speeds at which the aeroplane may be operated with flap extended"

        [
        ]
    , "VNE is the maximum indicated airspeed at which" ~>
      Multichoice
        [
          "the flaps may be lowered"
        , "a turn may be commenced"
        ]

        "the aircraft may operate under any circumstances"

        [
          "the aircraft may operate in turbulence"
        ]
    , "The function of a battery is to" ~>
      Multichoice
        [
        ]

        "store electrical energy for the system to use when the alternator is not operating"

        [
          "supply the current to allow the spark plugs to generate a spark"
        , "supply all the electrical demands of the system during flight"
        , "to keep the alternator fully charged"
        ]
    , "The function of an alternator is to" ~>
      Multichoice
        [
        ]

        "supply all the electrical demands of the system during flight"

        [
          "supply current to the spark plugs during start-up"
        , "to act as a back-up in case the battery fails"
        , "to store electrical energy when the engine is not running"
        ]
    , "The normal reading on a centre zero ammeter during flight should be" ~>
      Multichoice
        [
          "zero"
        , "a charge indication which depends on the number of electrical loads turned on"
        ]

        "a constant small charge"

        [
          "a discharge depending on the number of electrical loads turned on"
        ]
    , "Continued cranking of an engine which is difficult to start could result in" ~>
      Multichoice
        [
          "damage to engine components due to low oil pressure during start-up"
        , "a fire risk due to surplus fuel vapour flowing through the engine"
        , "stone damage to the propellor"
        ]

        "overheating and damage to the battery and/or starter motor"

        [
        ]
    , "Which of the following describes the difference between a circuit breaker and a fuse?" ~>
      Multichoice
        [
          "a circuit breaker interrupts the current while a fuse does not"
        , "a circuit breaker is not suitable for use when the current flow is high, while a fuse is"
        , "a circuit breaker does not interrupt the current flow while a fuse does"
        ]

        "a circuit breaker may be reset after it has been 'tripped' but a fuse cannot"

        [
        ]
    ]

bobTaitChapterRevisionBAP ::
  Exam
bobTaitChapterRevisionBAP =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Basic Aeroplane Performance")
    [
      "An aircraft with this loading system has 200lbs on the rear seats. The moment index generated by this weight would be closest to" ~>
      Multichoice
        [
          "30"
        ]

        "25.2"

        [
          "12.5"
        , "33"
        ]
    , "The combined weight of the pilot and front passenger in an aircraft with this loading system is 340lbs. The moment index generated by this weight would be closest to" ~>
      Multichoice
        [
          "3"
        , "309"
        , "134"
        ]

        "31"

        [
        ]
    , "An aircraft which uses this loading system is loaded as follows. Basic empty weight, 1240lb, 100.80 moment index. Row 1, Pilot and passenger 339lbs. Row 2, empty. Baggage, 50lbs. Fuel at take-off 37 US gal." ~>
      Multichoice
        [
        ]

        "This aircraft WILL remain within balance limits at all stages of the flight"

        [
          "This aircraft WILL NOT remain within balance limits at all stages of the flight"
        ]
    , "An aircraft with this loading system is loaded so that its gross weight is 1950lbs and the total moment index is 170 index units. The position of the centre of gravity with this load configuration in inches aft of the datum is closest to" ~>
      Multichoice
        [
        ]

        "87 inches aft of the datum"

        [
          "67 inches aft of the datum"
        , "101 inches aft of the datum"
        , "120 inches aft of the datum"
        ]
    , "An aircraft with this loading system has been loaded so that its take-off totals are as follows. Gross Weight=2100lbs. Total moment index 196. If the baggage compartment is full, what is the minimum weight which must be removed to ensure that the aircraft is within balance limits for take-off?" ~>
      Multichoice
        [
          "none"
        ]

        "25lbs"

        [
          "50lbs"
        , "100lbs"
        ]
    ]

bobTaitChapterRevisionHPr1 ::
  Exam
bobTaitChapterRevisionHPr1 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 1 Human Performance")
    [
      "The most common cause of pilot incapacitation during flight is" ~>
      Multichoice
        [
          "heart attack"
        , "dehydration"
        , "blocked sinus passages"
        ]

        "food or drink poisoning [gastro-intestinal disorders]"

        [
        ]
    , "The symptoms of dehydration include" ~>
      Multichoice
        [
          "profuse sweating"
        ]

        "drying of the nasal passages and a prickly sensation in the eyes"

        [
          "'runny' eyes and nose"
        , "stomach cramps and a high temperature"
        ]
    , "The CASA has recommended a period on the ground after making a blood donation of at least" ~>
      Multichoice
        [
          "six hours"
        ]

        "twenty-four hours"

        [
          "twelve hours"
        , "forty-eight hours"
        ]
    , "CAR 256 requires *total* abstinence by all pilots from *any* alcohol for a period of" ~>
      Multichoice
        [
        ]

        "eight hours before departure"

        [
          "24 hours before departure"
        , "eight hours before commencing duty"
        , "12 hours before departure"
        ]
    , "Which of the following is true of drugs taken by pilots for medical reasons before flight?" ~>
      Multichoice
        [
          "any drugs that are available 'over the counter' can be considered safe"
        , "drugs can be considered safe if they are prescribed by a doctor"
        , "drugs can be considered safe unless there is a warning on the packet"
        ]

        "the opinion of a DAME should be sought before using *any* drug"

        [
        ]
    , "Discomfort or pain in the ears or sinuses is *most likely* during" ~>
      Multichoice
        [
          "long periods of cruising flight at high altitude"
        ]

        "high rates of descent in unpressurised aircraft"

        [
          "high rates of climb in unpressurised aircraft"
        , "long slow descents in unpressurised aircraft"
        ]
    , "If an otherwise healthy pilot suffers a bout of dizzy spells, the appropriate action required by CAR 6.16 would be" ~>
      Multichoice
        [
        ]

        "notify a DAME and refrain from all flying until cleared"

        [
          "notify a DAME and refrain from all commercial flying until cleared"
        , "notify a DAME and refrain from all instrument flying until cleared"
        , "resume flying providing there is no recurrence within twenty-four hours"
        ]
    , "Pilots can best assist airsick passengers by" ~>
      Multichoice
        [
          "warning them that they will have to clean up the mess if they vomit"
        , "descending quickly to land before they become sick"
        , "reassuring them that airsickness is nothing to worry about"
        ]

        "flying as smoothly as possible and making all manoeuvres and descents gentle"

        [
        ]
    , "Too much salt in the diet can lead to" ~>
      Multichoice
        [
          "excessive sweating"
        , "increased chance of catching cold"
        ]

        "high blood pressure"

        [
          "low blood pressure"
        ]
    , "Foods most likely to produce intestinal gases are" ~>
      Multichoice
        [
          "salty foods"
        ]

        "green vegetables"

        [
          "foods high in sugar"
        , "chicken"
        ]
    ]

bobTaitChapterRevisionHPr2 ::
  Exam
bobTaitChapterRevisionHPr2 =    
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 2 Human Performance")
    [
      "The period of validity of a medical certificate for a pilot over forty years of age, unless otherwise cancelled or suspended is" ~>
      Multichoice
        [
        ]

        "2 years for student and private pilot licences and 1 year for commercial licences"

        [
          "2 years for student and 1 year for private pilot licences and commercial licences"
        , "1 year for student and 2 years for private pilot licences and commercial licences"
        , "2 years for all classes of licence"
        ]
    , "Hyperventilation is likely to result in" ~>
      Multichoice
        [
        ]

        "a change in the acidity of the blood caused by a drop in carbon dioxide levels"

        [
          "severe headaches and vomiting"
        , "sinus pain and discomfort in the middle ear"
        , "lightheadedness associated with an increase in carbon dioxide levels"
        ]
    , "Abdominal pain caused by the expansion of gases within the body is most likely to be associated with" ~>
      Multichoice
        [
        ]

        "a rapid climb in a high performance unpressurised aircraft"

        [
          "a rapid descent in a high performance unpressurised aircraft"
        , "a rapid climb in a high performance pressurised aircraft"
        , "a rapid descent in a high performance pressurised aircraft"
        ]
    , "At an altitude of 18000ft the ambient atmospheric pressure is approximately" ~>
      Multichoice
        [
          "one third of its sea-level value"
        ]

        "one half of its sea-level value"

        [
          "two thirds of its sea-level value"
        , "one tenth of its sea-level value"
        ]
    , "At a depth of about 30 feet, the underwater pressure is closest to" ~>
      Multichoice
        [
          "one and a half times normal sea-level atmospheric pressure"
        ]

        "two times normal sea-level atmospheric pressure"

        [
          "three times normal sea-level atmospheric pressure"
        , "one half of normal sea-level atmospheric pressure"
        ]
    , "Decompression sickness or the 'bends' is caused by" ~>
      Multichoice
        [
          "difficulty experienced in breathing under the high ambient pressure which occurs in deep water"
        , "the excess oxygen which dissolves in the blood during a deep dive"
        ]

        "the release of dissolved nitrogen from the blood during a rapid ascent from deep water"

        [
          "cramps caused by the low ambient temperature which occurs in deep water"
        ]
    , "A SCUBA diver has just completed a dive which did require decompression stops but was of less than four hours duration. The minimum recommended period which should be allowed before flying is" ~>
      Multichoice
        [
          "4 hours"
        ]

        "12 hours"

        [
          "24 hours"
        , "48 hours"
        ]
    , "Which of the following would be an appropriate way to combat the effects of hyperventilation?" ~>
      Multichoice
        [
          "breathe more rapidly"
        ]

        "make a conscious effort to slow down the breathing rate"

        [
          "take a deep breath and hold it"
        , "breathe more deeply"
        ]
    , "Hyperventilation is caused by" ~>
      Multichoice
        [
          "too much carbon monoxide in the air"
        , "too much carbon dioxide in the air"
        ]

        "breathing too fast and/or too deep for the body's requirements"

        [
          "holding the breath too long"
        ]
    , "Alcohol can still be detected in the blood for" ~>
      Multichoice
        [
        ]

        "14 hours after ingestion"

        [
          "14 hours after the blood alcohol level has reached .05"
        , "24 hours after ingestion"
        , "24 hours after the blood alcohol level has reached .05"
        ]
    , "Which of the following would be an appropriate remedy for a person who is suffering the effects of hyperventilation?" ~>
      Multichoice
        [
          "hold the breath for about 40 seconds"
        ]

        "breathe into a paper bag"

        [
          "increase the rate and depth of breathing"
        , "use a nasal decongestant"
        ]
    , "If it becomes necessary to take sleeping tablets, how much time should be allowed before flying?" ~>
      Multichoice
        [
          "12 hours"
        ]

        "24 hours"

        [
          "none if they provided a good night's sleep"
        , "8 hours"
        ]
    , "If a pilot completed a SCUBA dive which required decompression stops during ascent and was more than 4 hours duration, which of the following would apply?" ~>
      Multichoice
        [
          "a rest at sea-level of at least 24 hours is requred before flying"
        ]

        "a rest at sea-level of at least 48 hours is requred before flying"

        [
          "a rest at sea-level of at least 12 hours is requred before flying"
        , "a rest at sea-level of at least 6 hours is requred before flying"
        ]
    ]

bobTaitChapterRevisionHPr3 ::
  Exam
bobTaitChapterRevisionHPr3 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 3 Human Performance")
    [
      "Which of the following is a function of the middle ear?" ~>
      Multichoice
        [
          "to protect the eardrum from the intrusion of foreign bodies"
        ]

        "to amplify the vibrations arriving at the eardrum and transfer them to the cochlea"

        [
          "to convert the mechanical energy of vibrations to nerve impulses"
        , "to assist in maintaining the sense of balance"
        ]
    , "Which of the following is a function of the inner ear?" ~>
      Multichoice
        [
          "to protect the eardrum from the intrusion of foreign bodies"
        , "to amplify the vibrations arriving at the eardrum and transfer them to the cochlea"
        ]

        "to convert the mechanical energy of vibrations to nerve impulses and assist in maintaining the sense of balance"

        [
          "to equalise the pressure on either side of the eardrum"
        ]
    , "Which of the following mediates the sense of balance?" ~>
      Multichoice
        [
          "the cochlea"
        ]

        "the semicircular canals"

        [
          "the hammer, anvil and stirrup"
        , "the eustachian tube"
        ]
    , "Deterioration of hearing caused by exposure to loud noise usually occurs first" ~>
      Multichoice
        [
        ]

        "in the high frequency range, above normal speech"

        [
          "in the frequency range of normal speech, 300 to 5000Hz"
        , "in the frequency range below normal speech"
        , "across the whole range of audible frequencies"
        ]
    , "Which of the following is the minimum noise level that would mark the onset of temporary or permanent deafness after prolonged exposure?" ~>
      Multichoice
        [
        ]

        "85dB"

        [
          "110dB"
        , "140dB"
        , "160dB"
        ]
    , "Which of the following is the function of the eustachian tube in the human respiratory system?" ~>
      Multichoice
        [
          "it connects the middle ear to the outer ear"
        , "it connects the middle ear to the inner ear"
        ]

        "it connects the middle ear to the throat cavity"

        [
          "it connects the inner ear to the throat cavity"
        ]
    , "Hearing loss due to long-term exposure to high noise levels is usually first noticed in the loss of sensitivity to" ~>
      Multichoice
        [
        ]

        "high frequency sounds"

        [
          "low frequency sounds"
        , "medium frequency sounds"
        , "all frequencies of sound"
        ]
    , "Which part of the ear is most affected by pressure differences when the eustachian tube is blocked?" ~>
      Multichoice
        [
          "the outer ear"
        ]

        "the middle ear"

        [
          "the inner ear"
        , "the ear lobe"
        ]
    ]

bobTaitChapterRevisionHPr4 ::
  Exam
bobTaitChapterRevisionHPr4 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 4 Human Performance")
    [
      "The two difference types of light sensitive elements on the retina are classified as" ~>
      Multichoice
        [
          "rods which are sensitive to colour and cones which work best in dim light"
        ]

        "cones which are sensitive to colour and rods which work best in dim light"

        [
          "rods and cones both of which respond equally to all lighting conditions"
        , "rods and cones both of which are responsible for colour vision"
        ]
    , "The blind spot is" ~>
      Multichoice
        [
          "the area of the lens which is screened by the iris"
        , "the black spot in the centre of the coloured section of the eye"
        ]

        "at the junction of the optic nerve and the retina"

        [
          "an area on the cornea which does not respond to light"
        ]
    , "If a pilot whose eyes have fully adapter to darkness is exposed to a bright flash of light, the time required for dark adaptation to be re-established is most likely to be" ~>
      Multichoice
        [
          "3 minutes"
        , "5 minutes"
        , "15 minutes"
        ]

        "30 minutes"

        [
        ]
    , "The maximum distance at which a healthy eye in good lighting and contrast could identifiy a circle one metre in diameter is approximately" ~>
      Multichoice
        [
          "one kilometre"
        , "five kilometres"
        ]

        "2 nautical miles"

        [
          "five hundred metres"
        ]
    , "In level flight, a collision risk exists if a converging aircraft viewed from your cockpit appears to be" ~>
      Multichoice
        [
        ]

        "on the horizon and maintaining a constant position in your windscreen"

        [
          "below the horizon and moving closer to the centre of your windscreen"
        , "on the horizon and moving away from the centre of your windscreen"
        , "on the horizon"
        ]
    , "The most effective way to scan the sky for other aircraft during level flight is to" ~>
      Multichoice
        [
          "move the head in continuous arc from side to side"
        , "keep the head still and move the eyes continuously from side to side"
        , "do not look anywhere but straight ahead unless you detect movement"
        ]

        "move the head about 20deg or 30deg at a time, pausing after each movement to allow the peripheral vision to detect any movement"

        [
        ]
    , "Heavy rain may have the effect of making objects viewed through the windscreen appear," ~>
      Multichoice
        [
        ]

        "further away than they actually are"

        [
          "closer than they actually are"
        , "larger than they actually are"
        , "brighter than they actually are"
        ]
    ]

bobTaitChapterRevisionHPr5 ::
  Exam
bobTaitChapterRevisionHPr5 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 5 Human Performance")
    [
      "In sensing the orientation of the body in space, the brain assigns the highest priority to information coming from" ~>
      Multichoice
        [
        ]

        "the eyes"

        [
          "the inner ear"
        , "the middle ear"
        , "the proprioceptive system"
        ]
    , "Which of the following is the most likely cause of disorientation?" ~>
      Multichoice
        [
          "a change in the information coming from the inner ear"
        ]

        "a conflict or ambiguity in the information coming from visual, vestibular and postural cues"

        [
          "poor lighting reducing the amount of visual information received"
        , "a rapid transition from level flight to a steep climb"
        ]
    , "In the absence of reliable visual information, which of the following states of motion would be most difficult to differentiate?" ~>
      Multichoice
        [
          "a steep turn and a rapid deceleration in level flight"
        , "a rapid acceleration in level flight and a transition from level flight to a dive"
        ]

        "a rapid acceleration in level flight and a transition from level flight to a climb"

        [
          "a rapid deceleration in level flight and a transition from level flight to a climb"
        ]
    , "You are making an approach by day into a runway where the terrain slopes up from the threshold of intended landing. The most likely misjudgment during this approach would be" ~>
      Multichoice
        [
          "an overestimate of your height above the runway threshold"
        ]

        "an underestimate of your height above the runway threshold"

        [
          "an underestimate of your speed during the approach"
        , "an overestimate of the length of the runway"
        ]
    , "Which of the following organs mediates the sense of balance?" ~>
      Multichoice
        [
          "eustachian tube"
        , "the eardrum"
        ]

        "the semicircular canals"

        [
          "the cochlea"
        ]
    , "The susceptibility to disorientation is increased if the pilot is suffering from anxiety, stress or mental fatigue" ~>
      Multichoice
        [
          "this statement is false"
        ]

        "this statement is true"

        [
        ]
    ]

bobTaitChapterRevisionHPr6 ::
  Exam
bobTaitChapterRevisionHPr6 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 6 Human Performance")
    [
      "Which of the following will always be accompanied by an increase in positive G load?" ~>
      Multichoice
        [
          "increased IAS in a dive"
        , "any manoeuvre which requires extra power"
        ]

        "any manoeuvre which requires the pilot to pull back on the control column"

        [
          "any manoeuvre in which the aircraft's nose is above the horizon"
        ]
    , "In which of the following situations would a normal healthy person be most likely to experience loss of consciousness [G-LOC]?" ~>
      Multichoice
        [
          "eight G sustained for two seconds"
        , "three G sustained for five seconds"
        ]

        "seven G sustained for seven seconds"

        [
          "two G sustained for fifteen seconds"
        ]
    , "Which of the following is an effect of a sustained negative G load?" ~>
      Multichoice
        [
        ]

        "a slowing of the heart beat"

        [
          "a decreased flow of oxygenated blood to the brain"
        , "pooling of blood in the abdomen and legs"
        , "an irregular heart beat"
        ]
    , "At about which sustained G load will a healthy person be likely to begin to experience grey out?" ~>
      Multichoice
        [
          "1.5G"
        , "2.5G"
        ]

        "3.5G"

        [
          "4.5G"
        ]
    , "At about which sustained G load will a healthy person be likely to begin to experience black out?" ~>
      Multichoice
        [
          "7G"
        ]

        "5G"

        [
          "3.5G"
        , "2.5G"
        ]
    , "Which of the following would most likely lead to carbon monoxide contamination of the cockpit atmosphere?" ~>
      Multichoice
        [
          "excessive use of carburettor heat"
        , "an electrical short circuit"
        , "flying low in a very thick smoke haze"
        ]

        "a leak in the engine exhaust system"

        [
        ]
    , "Which of the following is *not* a symptom of carbon monoxide poisoning?" ~>
      Multichoice
        [
          "headache and fatigue"
        ]

        "a feeling of euphoria"

        [
          "discomfort in breathing"
        , "impairment of vision and mental confusion"
        ]
    , "Which of the following would be suitable treatment for a person suffering from carbon monoxide poisoning?" ~>
      Multichoice
        [
        ]

        "administer oxygen"

        [
          "give plenty of water"
        , "give a sleeping pill"
        , "have them re-breathe their exhaled breath from a bag placed over the nose and mouth"
        ]
    ]

bobTaitChapterRevisionHPr7 ::
  Exam
bobTaitChapterRevisionHPr7 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 7 Human Performance")
    [
      "The transfer of oxygen to the blood stream during respiration depends most directly upon" ~>
      Multichoice
        [
          "the breathing rate"
        ]

        "the partial pressure of oxygen within the lungs"

        [
          "the percentage of lung capacity achieved with each breath [depth of breathing]"
        , "the volume of oxygen inhaled with each breath"
        ]
    , "If a pilot suspects that he/she is suffering the effects of hypoxia, the most appropriate remedy would be" ~>
      Multichoice
        [
          "voluntarily increase the breathing rate to increase the oxygen uptake"
        , "voluntarily increase the depth of breathing to induce more oxygen into the lungs"
        , "voluntarily increase both the rate and depth of breathing"
        ]

        "use supplemental oxygen or descend immediately to a lower level"

        [
        ]
    , "Which of the following is *not* normally a symptom of hypoxia?" ~>
      Multichoice
        [
          "cyanosis"
        , "a feeling of euphoria"
        ]

        "severe headache"

        [
          "erratic behaviour"
        ]
    , "CAO 20.4.7 requries that supplemental oxygen be provided and used by the pilot at all times during flight above" ~>
      Multichoice
        [
          "20000 feet"
        ]

        "10000 feet"

        [
          "14000 feet"
        , "8000 feet"
        ]
    , "One of the effects of hypoxia is a degradation of night vision. This effect begins at about" ~>
      Multichoice
        [
        ]

        "4000 feet"

        [
          "10000 feet"
        , "14000 feet"
        , "20000 feet"
        ]
    , "Oxygen is transported around the body by" ~>
      Multichoice
        [
        ]

        "haemoglobin"

        [
          "white blood cells"
        , "plasma"
        , "the nervous system"
        ]
    , "A heavy smoker will suffer the effects of hypoxia at a lower altitude than a non-smoker. While flying at an altitude of 5000 feet, a heavy smoker could be experiencing the same degree of hypoxia as a non-smoker at" ~>
      Multichoice
        [
          "5000 feet"
        ]

        "10000 feet"

        [
          "15000 feet"
        , "20000 feet"
        ]
    , "Hypoxia may be caused by" ~>
      Multichoice
        [
          "flying with a head cold"
        ]

        "flying at an altitude where the partial pressure of oxygen is too low"

        [
          "breathing too quickly and/or too deeply for the requirements of the body"
        , "flying after a period of underwater diving"
        ]
    , "Hypoxia is *always* accompanied by" ~>
      Multichoice
        [
          "high temperature and vomiting"
        , "dizziness"
        , "profuse sweating"
        ]

        "a degradation in the performance of a pilot"

        [
        ]
    , "Cyanosis can be identified by" ~>
      Multichoice
        [
          "rapid breathing"
        ]

        "a blue coloration of the fingernail beds and lips"

        [
          "slurred speech"
        , "difficult in maintaining balance"
        ]
    ]

bobTaitChapterRevisionHPr8 ::
  Exam
bobTaitChapterRevisionHPr8 =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "REVISION 8 Human Performance")
    [
      "During which period of the day will the average person find it most difficult to stay awake?" ~>
      Multichoice
        [
          "midday to 2 pm"
        , "10 pm to 2 am"
        ]

        "3 am to 5 am"

        [
          "9 am to midday"
        ]
    , "Which of the following is true regarding transmeridian dyschronism [jet lag]." ~>
      Multichoice
        [
        ]

        "flying east requires the body's rhythms to advance"

        [
          "flying west requires the body's rhythms to advance"
        , "flying east requires the body's rhythms to delay"
        , "flying north in summer requires the body's rhythms to advance"
        ]
    , "Which of the following would be most vulnerable to fatigue?" ~>
      Multichoice
        [
        ]

        "clear thinking"

        [
          "hovering a helicopter"
        , "seeing and hearing clearlythe attention span"
        , "speaking clearly"
        ]
    , "The lapse in performance standards which occurs daily in mid afternoon is known as" ~>
      Multichoice
        [
          "the post-meridian depression"
        ]

        "the post-prandial dip"

        [
          "the siesta syndrome"
        , "the afternoon lapse phase"
        ]
    , "Which of the following describes the effect of excess stress on the attention span?" ~>
      Multichoice
        [
        ]

        "the attention span is reduced"

        [
          "the attention span is increased"
        , "the attention span is not affected by excess stress"
        , "the attention span reduces initially but soon returns to normal"
        ]
    , "Chronic fatigue differs from acute fatigue in that" ~>
      Multichoice
        [
          "it can usually be relieved by a good night's sleep"
        , "it is always the result of overexertion"
        , "it is easily treated by sleeping pills"
        ]

        "it is usually life-style related and the cause may not be obvious"

        [
        ]
    , "Which of the following is true of acute stress?" ~>
      Multichoice
        [
          "it always has a negative effect on performance and should be avoided at all costs"
        , "it has no affect on performance"
        ]

        "moderate levels of stress can enhance the dynamics of coping with a particular situation"

        [
          "the higher the stress levels, the better the performance becomes"
        ]
    , "A common source of human error is the false hypothesis. Under certain conditions this is more or less likely than at other times. From the following list, select the situation *least likely* to result in a person arriving at a false hypothesis." ~>
      Multichoice
        [
          "when expectancy of an event is high"
        , "after a period of intense concentration"
        , "during a period of preoccupation with another problem"
        ]

        "in the event of an engine failure"

        [
        ]
    , "The illustration at left serves to illustrate the perceptual error known as" ~>
      Multichoice
        [
          "the Coriolis illusion"
        , "the autokinetic illusion"
        , "somatogravic illusion"
        ]

        "mind set"

        [
        ]
    ]

bobTaitChapterRevisionHPTEM ::
  Exam
bobTaitChapterRevisionHPTEM =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "TEM Human Performance")
    [
      "Select the item which best describes an external threat" ~>
      Multichoice
        [
          "a noisy intoxicated passenger"
        , "a pilot suffering from the effects of a hangover"
        ]

        "another aircraft entering the runway while you are on late final"

        [
          "difficulty in understanding the transmission of a foreign pilot"
        ]
    , "Select the item which best describes an indesired aircraft state" ~>
      Multichoice
        [
        ]

        "arrival over the threshold too high and too fast on a landing approach"

        [
          "failing to notice a damaged tyre during a daily inspection"
        , "failure to realise that a destination aerodrome requires an alternate"
        , "aircraft overdue for its annual inspection"
        ]
    , "An example of an expected threat is" ~>
      Multichoice
        [
        ]

        "thunderstorms forecast on the TAF"

        [
          "engine failure in flight"
        , "becoming lost in flight"
        , "being diverted in flight by ATC"
        ]
    , "For the flight crew, the three basic components in the TEM model are" ~>
      Multichoice
        [
        ]

        "threats, errors and undesired aircraft states"

        [
          "threats, errors and anticipated aircraft states"
        , "threats, flight crew human resources and aircraft states"
        , "errors, flight crew human resources and undesired aircraft states"
        ]
    , "Which of the following would be classified as an external threat" ~>
      Multichoice
        [
        ]

        "pressure to meet timetables"

        [
          "pilot fatigue"
        , "health and fitness"
        , "lack of familiarity with other crew members"
        ]
    , "An example of a latent threat is" ~>
      Multichoice
        [
          "undercarriage will not retract in flight"
        , "wind gusts exceeding the aircraft's cross wind limitations for landing"
        ]

        "poor aircraft equipment design"

        [
          "unexpected high traffic volume in the terminal area"
        ]
    , "Entering the incorrect way-point data while operating in a stressful cockpit environment is an example of" ~>
      Multichoice
        [
          "an environmental threat leading to a configuration error"
        ]

        "an organisational threat leading to a committed error"

        [
          "an expected threat leading to an unexpected error"
        , "an unexpected threat leading to an expected error"
        ]
    , "The three primary categories of error in the TEM model are" ~>
      Multichoice
        [
          "loss of heading control, loss of attitude control and loss of airspeed control"
        , "navigational error, radio frequency error and navigation aid error"
        , "crew resource error, airtrafffic control error and ground handling error"
        ]

        "aircraft handling errors, procedural errors and communication errors"

        [
        ]
    , "Undesired aircraft states are categorised by the TEM model as" ~>
      Multichoice
        [
        ]

        "aircraft handling, ground handling and incorrect aircraft configuration"

        [
          "aircraft ground handling, vertical navigation and incorrect inflight configuration"
        , "vertical navigation, ground handling and inflight navigation"
        , "aircraft configuration, ground handling and inflight aircraft handling"
        ]
    , "Track and speed deviation are examples of" ~>
      Multichoice
        [
          "ground navigation state"
        ]

        "aircraft handling state"

        [
          "horizontal navigation state"
        , "aircraft configuration state"
        ]
    , "Unauthorised penetration of controlled airspace is an example of an undesired" ~>
      Multichoice
        [
          "ground navigation state"
        ]

        "aircraft handling state"

        [
          "air navigation state"
        , "navigation configuration state"
        ]
    , "Being positioned at the incorrect holding point prior to take-off is an example of an undesired" ~>
      Multichoice
        [
          "aircraft handling state"
        , "aircraft ground configuration state"
        , "situational awareness state"
        ]

        "ground navigation state"

        [
        ]
    , "Incorrect navigation aid setting is an example of" ~>
      Multichoice
        [
          "ground navigation state"
        ]

        "aircraft navigation state"

        [
          "horizontal navigation state"
        , "situational awareness state"
        ]
    , "With regard to TEM, the use of a checklist prior to take-off is an example of" ~>
      Multichoice
        [
          "a desirable aircraft state"
        , "a safety state"
        ]

        "a countermeasure"

        [
          "a safety tactic"
        ]
    , "When considering the risk any threat imposes you should consider" ~>
      Multichoice
        [
          "the probability of encountering the threat irrespective of the consequences"
        , "the seriousness of the consequences irrespective of the probability of encountering the threat"
        ]

        "the probability of encountering the threat and the seriousness of the consequences"

        [
          "the probability of encountering the threat at any stage during the flight"
        ]
    , "When an undesired aircraft state is identified the primary task should be" ~>
      Multichoice
        [
          "identify the error which led to the undesired aircraft state"
        , "identify and correct the error which led to the undesired aircraft state"
        ]

        "deal with the undesired aircraft state and return to controlled stabilised flight"

        [
          "advise ATC of the undesired aircraft state"
        ]
    , "One measure of the effectiveness of actions taken by a crew to manage threats is" ~>
      Multichoice
        [
          "the accuracy of the crew's recall of events during de-briefing"
        , "the speed with which the crew acted to manage the threat"
        ]

        "whether the threat was detected in time for the crew to respond appropriately"

        [
          "whether an undesired aircraft state was avoided"
        ]
    , "The most proactive option in thread management is to" ~>
      Multichoice
        [
          "anticipate the recovery action required if the threat occurs"
        ]

        "anticipate and avoid the threat altogether"

        [
          "take corrective action once the threat has occurred"
        , "concentrate on management of any undesired aircraft state that may result"
        ]
    , "Mismanaged threats usually lead to" ~>
      Multichoice
        [
        ]

        "errors which are then linked to undesired aircraft states"

        [
          "undesired aircraft states which are then linked to errors"
        , "aircraft handling errors which then lead to environmental errors"
        , "diversion from standard operating procedures"
        ]
    , "With regard to TEM, a cockpit systems failure warning light is an example of" ~>
      Multichoice
        [
          "a proactive decision making process"
        ]

        "a systemic-based countermeasure"

        [
          "an undesired aircraft state management device"
        , "a handling error countermeasure"
        ]
    ]

bobTaitChapterRevisionAirLaw ::
  Exam
bobTaitChapterRevisionAirLaw =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Air Law")
    [
      "When two powered aircraft are converging at the same level, the aircraft that should give way is" ~>
      Multichoice
        [
        ]

        "the one which has the other on its right"

        [
          "the one which has the other on its left"
        ]
    , "If the pilot of a powered aircraft sights a glider converging from his left at the same height" ~>
      Multichoice
        [
          "the aircraft which has the other on its right should give way"
        ]

        "the pilot of the powered aircraft should give way"

        [
          "the glider should give way"
        , "both aircraft should turn to the right"
        ]
    , "A pilot must check that all flight controls are free to move in the correct sense and to the full extent of their travel" ~>
      Multichoice
        [
          "only before the first flight of each day"
        , "during the take-off roll"
        ]

        "immediately before take-off"

        [
          "before the aircraft commences to taxi"
        ]
    , "During the take-off run it is necessary to check the operation of" ~>
      Multichoice
        [
          "the primary flight controls"
        ]

        "the air speed indicator"

        [
          "the radio"
        , "the brakes"
        ]
    , "The fuel system must be inspected for the presence of water" ~>
      Multichoice
        [
          "before taxiing for take-off"
        , "before starting the engine"
        ]

        "before the first flight of each day and after each refuelling"

        [
          "only after refuelling"
        ]
    , "Refuelling of aircraft must not commence unless the fuel tank filler cap or vents are at least metres away from" ~>
      Multichoice
        [
          "any sealed building"
        ]

        "any other parked aircraft"

        [
          "members of the public"
        , "any unsealed building"
        ]
    , "An aircraft being refuelled must be parked" ~>
      Multichoice
        [
          "as close as possible to the fire extinguisher"
        , "so that it is facing into the wind"
        , "with the park brake on and the wheels chocked"
        ]

        "so that it can be rapidly moved if it becomes necessary"

        [
        ]
    , "An aircraft engine must not be started within 5 metres of" ~>
      Multichoice
        [
        ]

        "any sealed building"

        [
          "any exposed public area"
        , "any unsealed building"
        , "any other aircraft"
        ]
    , "Except for take-off, landing or by stress of weather, an aircraft must not fly over any town or populous area at a" ~>
      Multichoice
        [
          "1500 ft AGL"
        , "1500 ft AMSL"
        ]

        "1000 ft AGL"

        [
          "1000 ft AMSL"
        ]
    , "Except by stress of weather or during take-off or landing, an aircraft must not fly over any area at a height lower than" ~>
      Multichoice
        [
          "1500 ft AGL"
        , "1500 ft AMSL"
        ]

        "1000 ft AGL"

        [
          "1000 ft AMSL"
        ]
    , "For a flight in a training area over sea-level terrain and outside controlled airspace at 3500 ft, the required visibility and vertical separation from the cloud base is" ~>
      Multichoice
        [
          "8 km and 1000 ft"
        , "5 km and 500 ft"
        ]

        "5 km and 1000 ft"

        [
          "5 km and clear of cloud"
        ]
    , "A recommended means for checking for the presence of water in aviation gasoline, is to drain a small sample of fuel from the fuel drain" ~>
      Multichoice
        [
          "into a clean container and check for a cloudy colour"
        , "into a clean container and check for the presence of globules of water in the bottom of the sample"
        ]

        "into a container which already contains a sample of pure fuel and check for a visible demarcation between the two fluids or for globules of water in the bottom of the sample"

        [
          "remove the fuel filter and check for the presence of water"
        ]
    , "Fuel tank vents must be checked for freedom from obstruction" ~>
      Multichoice
        [
          "before each flight"
        ]

        "before the first flight of each day"

        [
          "before each take-off"
        , "after each refuelling"
        ]
    , "Two aircraft are on converging courses and there is danger of collision. The ultimate responsibility to avoid collision is on:" ~>
      Multichoice
        [
          "the pilot who has the other on his right"
        , "the pilot who has the other on his left"
        , "the pilot of the slower aircraft"
        ]

        "both pilots are responsible"

        [
        ]
    , "Select the conditions which the pilot must maintain when a Special VFR [SVFR] clearance to enter a Class D CTR is issued." ~>
      Multichoice
        [
          "clear of cloud and 5000m visibility"
        , "1500m from cloud and 1600m visibility"
        ]

        "clear of cloud and 1600m visibility"

        [
          "1000ft vertically from cloud and 3000m visibility"
        ]
    , "During take-off from a licenced aerodrome your aircraft suffered a bird strike to the windscreen. The aircraft suffered no damage as a result of the collision. As a pilot in command you should" ~>
      Multichoice
        [
          "report the incident immediately by phone and follow up with a written report within 72 hours"
        ]

        "not make an immediate report but make a written report within 72 hours"

        [
          "report the incident immediately by phone and follow up with a written report within 48 hours"
        , "make no report if no damage was done to the aircraft"
        ]
    ]

bobTaitChapterRevisionMeteorology ::
  Exam
bobTaitChapterRevisionMeteorology =
  Exam
    "Bob Tait RPL Study Guide"
    (Just "A study guide for the Recreational Pilot Licence")
    (Just "Meteorology")
    [
      "Which conditions are most likely to produce radiation fog?" ~>
      Multichoice
        [
          "thin overcast with no wind"
        , "broken cloud with no wind"
        ]

        "nil cloud and light wind"

        [
          "scattered cumulus with light wind"
        ]
    , "During what stage of a thunderstorm would lightning be most frequent?" ~>
      Multichoice
        [
          "before any precipitation"
        , "during the most rapid growth"
        ]

        "at the mature stage"

        [
          "during the dissipating stage"
        ]
    , "The lifting of fog may be caused by" ~>
      Multichoice
        [
          "a fall of temperature"
        , "the formation of upper cloud"
        ]

        "an increase in wind strength"

        [
          "an increase in humidity"
        ]
    , "Which phenomena in a thunderstorm causes low level wind shear at some distance away from the cell?" ~>
      Multichoice
        [
          "updraft"
        ]

        "downdraft"

        [
          "hail"
        , "turbulence"
        ]
    , "Where are tornadoes most likely to be encountered?" ~>
      Multichoice
        [
          "over hot desert country"
        , "with mountain waves"
        , "strong low pressure systems"
        ]

        "wide spread severe thunderstorm activity"

        [
        ]
    , "Which process is involved in the formation of hoar frost?" ~>
      Multichoice
        [
          "accumulation of ice crystals suspended in the atmosphere"
        , "liquid water freezing on impact with the aircraft surface"
        ]

        "deposition of ice directly from the water vapour mixed with the air"

        [
          "freezing of large supercooled water droplets"
        ]
    , "The condition most likely to produce advection fog is" ~>
      Multichoice
        [
          "cold air passing over a cold surface"
        , "warm air passing over a warm surface"
        , "cold air passing over a warm surface"
        ]

        "warm air passing over a cold surface"

        [
        ]
    , "One of the prerequisites for the formation of a thunderstorm is" ~>
      Multichoice
        [
        ]

        "the presence of moist air through a considerable depth of the atmosphere"

        [
          "a rapid drop in surface temperature with an increase in wind strength"
        , "a gradual increase in cloud cover above 20000ft"
        , "a cold isothermal later through a considerable depth of the atmosphere"
        ]
    , "Advection fog forms when" ~>
      Multichoice
        [
          "cold air passes over a cold surface"
        , "cold air passes over a warm surface"
        , "warm air passes over a warm surface"
        ]

        "warm air passes over a cold surface"

        [
        ]
    , "Dust storms are most likely to produce which of the following hazards?" ~>
      Multichoice
        [
          "severe visibility restriction in a localised area below 10000ft"
        , "severe visibility restriction over a widepsread area below 10000ft"
        ]

        "severe visibility restriction over a widepsread area to heights above 10000ft"

        [
        ]
    , "The most dangerous effect of a mature thunderstorm is" ~>
      Multichoice
        [
        ]

        "turbulence"

        [
          "lightning"
        , "hail"
        , "icing"
        ]
    , "Slant visibility in fog causes" ~>
      Multichoice
        [
          "reduced visibility towards the sun"
        ]

        "the runway is visible from 'over the top' but not in the circuit area or on final"

        [
          "better visibility looking up than down"
        , "runway visual range is less than meteorological visibility"
        ]
    , "The runway at a coastal aerodrome runs parallel to the coast. When would the crosswind component be likely to be strongest?" ~>
      Multichoice
        [
          "early in the morning"
        , "late at night"
        ]

        "in the early afternoon"

        [
          "at sunset"
        ]
    ]

airspeedindicatorExam ::
  Exam
airspeedindicatorExam =
  Exam
    "Airspeed Indicator Exam"
    (Just "A self-made exam")
    Nothing
    [
      "What is the significance of the red band?" !-
      DirectAnswer
        "Vne (never exceed speed)."
    , "Where is the never exceed speed indicated?" !-
      DirectAnswer
        "End of yellow band, start of red band."
    , "What is the significance of the yellow band?" !-
      DirectAnswer
        "Vno (maximum structural cruise) to Vne (never exceed)."
    , "What is the significance of the white band?" !-
      DirectAnswer
        "Vs0 (stall speed with full flaps) to Vfe (maximum speed with flaps extended)."
    , "Where is the stall speed with full flaps indicated?" !-
      DirectAnswer
        "Start of white band."
    , "Where is the maximum speed with flaps extended indicated?" !-
      DirectAnswer
        "End of white band."
    , "What is the significance of the green band?" !-
      DirectAnswer
        "Vs1 (stall speed with no flap, no gear) to Vno (maximum structural cruise)."
    , "Where is the stall speed with no flap indicated?" !-
      DirectAnswer
        "Start of green band."
    , "Where is the maximum structural cruise indicated?" !-
      DirectAnswer
        "End of green band, start of yellow band."
    ]

airspeedsExam ::
  Exam
airspeedsExam =
  Exam
    "Airspeeds Exam"
    (Just "A self-made exam")
    Nothing
    [
      "What is meant by Vx?" !-
      DirectAnswer
        "Best angle of climb."
    , "What V-speed is best angle of climb?" !-
      DirectAnswer
        "Vx"
    , "What is meant by Vy?" !-
      DirectAnswer
        "Best rate of climb."
    , "What V-speed is best rate of climb?" !-
      DirectAnswer
        "Vy"
    , "What is meant by Va?" !-
      DirectAnswer
        "Maximum maneuvering speed; speed of maximum application of a flight control."
    , "What V-speed is maximum maneuvering speed?" !-
      DirectAnswer
        "Va"
    , "What is meant by Vb?" !-
      DirectAnswer
        "Maximum turbulence penetration speed."
    , "What V-speed is maximum turbulence penetration speed?" !-
      DirectAnswer
        "Vb"
    , "What is meant by Vno?" !-
      DirectAnswer
        "Maximum structural cruise speed."
    , "What V-speed is maximum structural cruise speed?" !-
      DirectAnswer
        "Vno"
    , "What is meant by Vne?" !-
      DirectAnswer
        "Never exceed speed."
    , "What V-speed is never exceed speed?" !-
      DirectAnswer
        "Vne"
    , "What is meant by Vs0?" !-
      DirectAnswer
        "Stall speed in landing configuration; full flap, gear extended."
    , "What V-speed is stall speed in landing configuration?" !-
      DirectAnswer
        "Vs0"
    , "What is meant by Vs1?" !-
      DirectAnswer
        "Stall speed in clean configuration; no flap, no gear."
    , "What V-speed is stall speed in clean configuration?" !-
      DirectAnswer
        "Vs1"
    ]


c172Rairspeeds ::
  Exam
c172Rairspeeds =
  Exam
    ""
    Nothing
    Nothing
    [
      "What is the Vne for C172R?" !-
      DirectAnswer
        "163KIAS"
    , "What is the Vno for C172R?" !-
      DirectAnswer
        "129KIAS"
    , "What is the Va for C172R at 2450lbs?" !-
      DirectAnswer
        "99KIAS"
    , "What is the Va for C172R at 2200lbs?" !-
      DirectAnswer
        "94KIAS"
    , "What is the Va for C172R at 1600lbs?" !-
      DirectAnswer
        "82KIAS"
    , "What is the Vfe (flap 10) for C172R?" !-
      DirectAnswer
        "110KIAS"
    , "What is the Vfe (flap >10) for C172R?" !-
      DirectAnswer
        "85KIAS"
    , "What is the EFATO speed (no flap) for C172R?" !-
      DirectAnswer
        "65KIAS"
    , "What is the EFATO speed (flap) for C172R?" !-
      DirectAnswer
        "60KIAS"
    , "What is the best glide speed for C172R?" !-
      DirectAnswer
        "65KIAS"
    , "What is the prec-search speed for C172R?" !-
      DirectAnswer
        "60KIAS"
    , "What is the no power landing speed (no flap) for C172R?" !-
      DirectAnswer
        "65KIAS"
    , "What is the no power landing speed (flap) for C172R?" !-
      DirectAnswer
        "60KIAS"
    , "What is the Vx for C172R?" !-
      DirectAnswer
        "60KIAS"
    , "What is the Vy for C172R?" !-
      DirectAnswer
        "79KIAS"
    , "What is the short-field take-off speed for C172R?" !-
      DirectAnswer
        "57KIAS"
    , "What is the stall speed (no flap) for C172R?" !-
      DirectAnswer
        "44KIAS"
    , "What is the stall speed (flap 10) for C172R?" !-
      DirectAnswer
        "37KIAS"
    , "What is the stall speed (flap 30) for C172R?" !-
      DirectAnswer
        "33KIAS"
    , "What is the maximum crosswind for C172R?" !-
      DirectAnswer
        "15 knots"
    ]

c172Sairspeeds ::
  Exam
c172Sairspeeds =
  Exam
    ""
    Nothing
    Nothing
    [      
      "What is the Vne for C172S?" !-
      DirectAnswer
        "163KIAS"
    , "What is the Vno for C172S?" !-
      DirectAnswer
        "129KIAS"
    , "What is the Va for C172S at 2550lbs?" !-
      DirectAnswer
        "105KIAS"
    , "What is the Va for C172S at 2200lbs?" !-
      DirectAnswer
        "98KIAS"
    , "What is the Va for C172S at 1600lbs?" !-
      DirectAnswer
        "90KIAS"
    , "What is the Vfe (flap 10) for C172S?" !-
      DirectAnswer
        "110KIAS"
    , "What is the Vfe (flap >10) for C172S?" !-
      DirectAnswer
        "85KIAS"
    , "What is the EFATO speed (no flap) for C172S?" !-
      DirectAnswer
        "70KIAS"
    , "What is the EFATO speed (flap) for C172S?" !-
      DirectAnswer
        "65KIAS"
    , "What is the best glide speed for C172S?" !-
      DirectAnswer
        "68KIAS"
    , "What is the prec-search speed for C172S?" !-
      DirectAnswer
        "65KIAS"
    , "What is the no power landing speed (no flap) for C172S?" !-
      DirectAnswer
        "70KIAS"
    , "What is the no power landing speed (flap) for C172S?" !-
      DirectAnswer
        "65KIAS"
    , "What is the Vx for C172S?" !-
      DirectAnswer
        "62KIAS"
    , "What is the Vy for C172S?" !-
      DirectAnswer
        "74KIAS"
    , "What is the short-field take-off speed for C172S?" !-
      DirectAnswer
        "61KIAS"
    , "What is the stall speed (no flap) for C172S?" !-
      DirectAnswer
        "48KIAS"
    , "What is the stall speed (flap 10) for C172S?" !-
      DirectAnswer
        "43KIAS"
    , "What is the stall speed (flap 30) for C172S?" !-
      DirectAnswer
        "40KIAS"
    , "What is the maximum crosswind for C172S?" !-
      DirectAnswer
        "15 knots"
    ]

form61_1486_1495 ::
  Exam
form61_1486_1495 =
  Exam
    "Form 61-1486 and 61-1495"
    (Just "A self-made exam")
    Nothing
    [
      "What are the privileges and limitations of the recreational pilot licence with aeroplane category rating?" !-
      DirectListAnswer
        [
          "can carry passengers if at least 3 take-offs and landings within previous 90 days *[CASR61.395]*"
        , "pilot holds and carries *[CASR61.420(b)]* class 1 or 2 medical certificate or RAMCP under conditions in subparagraph 2 *[CASR61.405]*"
        , "pilot must carry documents; licence, medical certificate, maintenance release, pilot operating handbook"
        , "must have English proficiency assessment *[CASR61.422]*"
        , "pilot only registered aircraft *[CASR61.425]*"
        , "airspace within 25nm of departure aerodrome, within flight training area and direct between departure aerodrome and flight training area *[CASR61.427]*"
        , "single-engine aircraft, MTOW <= 1500kg, day VFR, private operation or flight training *[CASR61.460]*"
        ]
    , "What are the drug and alcohol regulations?" !-
      DirectListAnswer
        [
          "8 hours from consumption of alcohol to departure *[CASR99]*"
        , "not intoxicated; 0.02 grams per 210 litres of breath *[CASR99]*"
        , "crew will not consume while on board *[CASR99]*"
        , "consult DAME regarding use of drugs *[CASR99]*"
        ]
    , "What are the VFR aircraft instrument requirements?" !-
      DirectListAnswer
        [
          "airspeed indicator *[CAO20.18(10) and CAO20.18(Appendix 1)]*"
        , "altimeter *[CAO20.18(10) and CAO20.18(Appendix 1)]*"
        , "direct or remote magnetic compass *[CAO20.18(10) and CAO20.18(Appendix 1)]*"
        , "timepiece with hours, minutes, seconds *[CAO20.18(10) and CAO20.18(Appendix 1)]*"
        ]
    , "What are the emergency equipment requirements?" !-
      DirectListAnswer
        [
          "life jackets when over water and out of glide *[CAO20.11(5.1)]*"
        , "sufficient life raft(s) when minimum of (30 minutes cruise) and (100nm) *[CAO20.11(5.2.1)]*"
        , "if life raft required, 1 ELT and pyro distress signals *[CAO20.11(6.1)]*"
        , "if more than one life raft required, then >= 2 (approved ELT under reg 252A) transmitters 121.5MHz and 243MHz and stowed ready for use *[CAO20.11(6.1)]*"
        , "single-engine, over water, not equipped with radio or incapable of air-to-ground radio, not required to carry a life raft, shall carry ELT (121.5MHz and 243MHz approved under reg 252A) *[CAO20.11(6.2)]*"
        ]
    , "What emergency procedures must the passengers be briefed on *[CAO20.11(14)]*?" !-
      DirectListAnswer
        [
          "smoking requirements *[CAO20.11(14.1.1)]*"
        , "use of seat belts *[CAO20.11(14.1.1)]*" 
        , "location of emergency exits *[CAO20.11(14.1.1)]*"
        , "use of oxygen where applicable *[CAO20.11(14.1.1)]*"
        , "use of floatation devices where applicable *[CAO20.11(14.1.1)]*"
        , "stowage of luggage *[CAO20.11(14.1.1)]*"
        , "onboard survival equipment *[CAO20.11(14.1.1)]*"
        ]
    , "What are the fuel and oil requirements for flight?" !-
      DirectListAnswer
        [
          "PiC must take steps to ensure sufficient fuel and oil *[CAR1988(234)]*"
        , "Fixed fuel reserve for VFR, aeroplane, piston-engine: 45 minutes *[CAAP 234-1(1)]*"
        , "5-8 quarts of oil *[Cessna 172 PoH Section 8 CAPACITY OF ENGINE SUMP]*"
        ]
    , "What are cargo and passenger loading requirements?" !-
      DirectListAnswer
        [
          "cargo on or above floor shall be restrained *[CAO20.16.2(3)]*"
        , "cargo shall not obstruct flight controls *[CAO20.16.2(4.1)]*, emergency exits *[CAO20.16.2(4.2)]*"
        , "cargo on a passenger seat shall evenly distribute weight, not exceeding 77kg *[CAO20.16.2(5.1)]* and restrained *[CAO20.16.2(5.2)]*"
        , "cargo on unoccupied control seat shall not exceed 77kg *[CAO20.16.2(6.2)]*, restrained *[CAO20.16.2(6.4)]*, flight controls removed if easy *[CAO20.16.2(6.3.1)]*, not interfere with aircraft operation *[CAO20.16.2(6.3)]*"
        , "seat belts during take-off, landing, < 1000ft AGL, turbulence *[CAO20.16.3(4.1)]*"
        , "one pilot crew wearing seat belt at all times *[CAO20.16.3(4.2)]*"
        , "seats upright during take-off and landing *[CAO20.16.3(5.1)]*"
        , "passenger in control seat must be given instruction, no interfere with flight controls *[CAO20.16.3(11.1)]*"
        , "two infants (<=3 years of age) may be carried on one seat with total weight <= 77kg *[CAO20.16.3(13.1)]*"
        ]
    , "What are the privileges and limitations of the single engine aeroplane class rating?" !-
      DirectListAnswer
        [
          "pilot licence must demonstrate competency of part 61 MoS *[CASR61.400]*"
        , "flight review every 24 months, within 3 months of expiry, and valid to the end of that month *[CASR61.745]*"
        ]
    , "What requirement applies to take-off and landing distances?" !-
      DirectAnswer
        "Add 15% to all take-off and landing distances for MTOW <= 2000kg *[CAO20.7.4(6.1)]*"
    , "What documents must be carried?" !-
      DirectListAnswer
        [
          "Maintenance release"
        , "Pilot Operating Handbook"
        , "Aviation Medical Certificate"
        , "Pilot Licence"
        ]
    ]

bobTaitChapterRevision ::
  [Exam]
bobTaitChapterRevision =
  [
    bobTaitChapterRevisionA1
  , bobTaitChapterRevisionA2
  , bobTaitChapterRevisionA3
  , bobTaitChapterRevisionA4
  , bobTaitChapterRevisionA5
  , bobTaitChapterRevisionA6
  , bobTaitChapterRevisionGK1
  , bobTaitChapterRevisionGK2
  , bobTaitChapterRevisionGK3
  , bobTaitChapterRevisionBAP
  , bobTaitChapterRevisionHPr1
  , bobTaitChapterRevisionHPr2
  , bobTaitChapterRevisionHPr3
  , bobTaitChapterRevisionHPr4
  , bobTaitChapterRevisionHPr5
  , bobTaitChapterRevisionHPr6
  , bobTaitChapterRevisionHPr7
  , bobTaitChapterRevisionHPr8
  , bobTaitChapterRevisionHPTEM
  , bobTaitChapterRevisionAirLaw
  , bobTaitChapterRevisionMeteorology
  ]

curtisPplTrial4 ::
  Exam
curtisPplTrial4 =
  Exam
    "Curtis Aviation Private Pilot Licence Trial Examination 4"
    (Just "Time Allowed: 3 hours 30 minutes. Pass: >= 70%")
    (Just "01 August 2015")
    [
      "An item of passenger briefing which is mandatory before take-off is:" ~>
      Multichoice
        [
        ]

        "The location of the emergency exits"

        [
          "The location of the fire extinguishers"
        , "The pilot actions in the event of an engine failure after take-off"
        , "The position of the body in the event of an emergency landing"
        ]
    , "A “PAN-PAN” call should be made if the pilot:" ~>
      Multichoice
        [
          "Is unable to comply with an ATC instruction"
        , "Has an intermittent transponder"
        ]

        "Is lost and experiencing difficulty in navigation"

        [
          "Is unable to raise flap after take-off at a CTAF aerodrome"
        ]
    , "You must not undertake a mercy flight if:" ~>
      Multichoice
        [
          "You are likely to operate in marginal VMC"
        , "You cannot comply with relevant regulations"
        , "Your aircraft is not radio-equipped"
        ]

        "There is an alternative means of achieving the same relief"

        [
        ]
    , "A prohibited area is defined as airspace in which flight by civil aircraft:" ~>
      Multichoice
        [
        ]

        "Is not permitted at any time and under any circumstances"

        [
          "Is not permitted during daylight hours"
        , "Is not permitted without prior approval"
        , "Is not permitted between the hours of 10pm and 6 am"
        ]
    , "For all flights, the pilot in command of an aircraft shall test the flight controls to the full limit of their travel:" ~>
      Multichoice
        [
          "Immediately before engine start"
        , "Immediately before starting to taxi"
        ]

        "Immediately before take-off"

        [
          "At the completion of the flight"
        ]
    , "A steady red light signal directed at an aircraft in flight signifies to the pilot that the:" ~>
      Multichoice
        [
          "Aerodrome is unsafe for landing"
        , "Aircraft should climb above circuit height & orbit the aerodrome"
        , "Aircraft’s undercarriage is not fully down – Go round"
        ]

        "Aircraft should give way to other aircraft & continue circling"

        [
        ]
    , "Select the option below which could best be described as an organisational threat" ~>
      Multichoice
        [
          "A flight into a high density traffic area"
        , "An air traffic controller whose speech is too fast to understand easily"
        , "Pilot proficiency degraded by lack of recent experience"
        ]

        "Provision of an instrument approach chart that is out of date"

        [
        ]
    , "A quantity of fuel likely to create a fire hazard has been spilled within 15 m of an aircraft. Fuelling of the aircraft may continue only after:" ~>
      Multichoice
        [
          "Two fire extinguishers have been positioned within 15m of the aircraft"
        , "Ground fuelling equipment is removed to more than 15m of the aircraft"
        ]

        "The spilled fuel is removed"

        [
          "All passengers have left the aircraft"
        ]
    , "A requirement for dropping operations is that:" ~>
      Multichoice
        [
          "A dispatcher must be provided with a seat"
        ]

        "Continuous sight of ground or water is maintained"

        [
          "All persons on board remain seated"
        , "There is no risk of articles falling outside the drop site"
        ]
    , "Which of the following restrictions applies to the consumption of alcoholic liquor by pilots:" ~>
      Multichoice
        [
          "It may not be consumed at any time"
        , "It may be consumed immediately prior to commencement of duty provided the capacity to act is not impaired"
        ]

        "It may be consumed up to, but not within, the 8 hours immediately preceding departure"

        [
          "It may be consumed in the 12 hours immediately preceding departure"
        ]
    , "The ARFOR indicates that there will be ‘BROKEN’ stratus with the cloud top at 3000 ft AMSL along your planned route OCTA. You plan VFR using radio navigation aids along this route. Your planned track is 067 degrees (M). To maintain VMC and conform to the ‘Table of Cruising Levels’ which of the following altitudes is the lowest that you can plan:" ~>
      Multichoice
        [
          "A030"
        , "A035"
        , "A040"
        , "A050"
        ]

        "A055"

        [
        ]
    , "At a non-controlled aerodrome in Class G airspace, the latest position at which an aircraft is permitted to join the circuit is the:" ~>
      Multichoice
        [
          "Upwind leg"
        , "Crosswind leg"
        ]

        "Downwind leg"

        [
          "Base leg"
        , "Final leg"
        ]
    , "You and three friends plan a shooting trip in a hired aircraft that you will pilot as a private flight. Which of the following conditions apply to the carriage of firearms:" ~>
      Multichoice
        [
          "Unloaded firearms may be carried if correctly packaged"
        , "Unloaded firearms may be carried in a luggage locker"
        ]

        "Specific written permission of CASA is required for the carriage of firearms and ammunition"

        [
          "Firearms may be carried provided they are licensed and you obtain permission from the aircraft owner or operator"
        ]
    , "When overflying a VOR station one cockpit indication you should expect is that:" ~>
      Multichoice
        [
          "A reciprocal track will be displayed in the course window"
        ]

        "The flag will change from “TO” to “FROM”"

        [
          "The “TO” flag will remain on until you set the reciprocal track"
        , "The station ident will change to a continuous audio signal"
        ]
    , "This question relates to a VFR flight by day proceeding more than 50 nm from the departure aerodrome. The maximum permitted crosswind component in an aeroplane’s flight manual is 15 kts. Which of the following weather conditions forecast for the destination at the ETA would necessitate the provision of an alternate:" ~>
      Multichoice
        [
          "15 kts steady crosswind"
        , "Visibility of 8 km"
        , "SCT cloud with a 1400 ft ceiling"
        ]

        "30% probability of visibility restricted to 7 km"

        [
        ]
    , "Refer Figure 5 (page 8, Work Booklet). Given the following details:\n* Runway = 08/26\n* Take-off distance available = 1200 metres\n* Surface = short wet grass\n* Slope = level\n* Pressure height = 4000 ft\n* Wind = 080/05 kts\n* Temperature = +28 Degrees C\nThe heaviest take-off weight permitted under the conditions given is closest to: " ~>
      Multichoice
        [
          "880 kg"
        ]

        "1030 kg"

        [
          "1055 kg"
        ]
    , "During summer in Australia, in what direction from the departure aerodrome will the end of daylight (in LMT) be earlier:" ~>
      Multichoice
        [
          "West"
        ]

        "North"

        [
          "South"
        , "East"
        ]
    , "Refer (BOURKE) WAC 3356 (page 30, Work Booklet). You are on a flight from ST GEORGE (YSGE) (2803S 14836E) to WALGETT (YWLG) (3002S 14807E). You departed YSGE at 0308UTC. At 0324UTC you crossed the railway line (approximately 35nm SE YSGE) and fixed your position between Bonathorne railway station and NOONDOO township. Using track error lines you estimate that the track error from YSGE to the 0324UTC position is 8 degrees right. You now alter HDG 16 degrees left to intercept the planned TR. At what time should the aircraft intercept the (planned) YSGE – YWLG track:" ~>
      Multichoice
        [
          "0332 UTC"
        ]

        "0340 UTC"

        [
          "0356 UTC"
        , "0337 UTC"
        ]
    , "Wind information given in aerodrome forecasts indicates the direction:" ~>
      Multichoice
        [
          "From which the wind is blowing in degrees magnetic"
        , "To which the wind is blowing in degrees magnetic"
        ]

        "From which the wind is blowing in degrees true"

        [
          "To which the wind is blowing in degrees true"
        ]
    , "Refer (TOWNSVILLE) WAC 3219 (page 27, Work Booklet). At 0120UTC you depart SPRING CREEK (1838S 14433E) to track direct to REID RIVER (1916S 14650E). At 0135UTC your position is on track north of Lucky Downs homestead (approximately 30nm from SPRING CREEK). From this position a HDG of 110(M) is maintained until you obtain a pinpoint at 0150UTC over Blue Range homestead (Approximately 60nm from SPRING CREEK). The HDG (M) required to track direct to REID RIVER from this position is closest to:" ~>
      Multichoice
        [
          "103(M)"
        , "106 M)"
        , "121(M)"
        ]

        "099(M)"

        [
        ]
    , "In the take-off configuration with landing gear extended an aeroplane with a maximum take-off weight not exceeding 5700 kg shall have the ability to achieve a minimum climb gradient of:" ~>
      Multichoice
        [
          "1.0%"
        , "3.2%"
        , "4.5%"
        ]

        "6.0%"

        [
        ]
    , "MERIMBULA (YMER) TAF YMER 092300Z 0012 29015KT 8000 –RASH SCT010 SCT040 FM04 26025KT 9999 BKN035 Depart YSWG 0000 – YSCB – Arrive YMER 0200 The Forecast wind velocity for arrival in the circuit area YMER is:" ~>
      Multichoice
        [
          "280 Deg (T), 10 kts"
        , "280 Deg (M), 25 kts"
        , "260 Deg (T), 25 kts"
        , "290 Deg (M), 15 kts"
        ]

        "290 Deg (T), 15 kts"

        [
        ]
    , "Your aircraft is fuelled with 55.5 US gallons of fuel. You allow 1.6 US gallons for start up, taxi and take-off. Your average fuel consumption rate in flight is 14.3 US gallons per hour. How much fuel in LITRES will you have used after 14 minutes of flight:" ~>
      Multichoice
        [
          "25 Litres"
        , "13 Litres"
        ]

        "19 Litres"

        [
          "22 Litres"
        ]
    , "The speed that shall only be exceeded with caution and in smooth air is called the:" ~>
      Multichoice
        [
          "Maximum structural cruising speed"
        , "Manoeuvring speed"
        ]

        "Turbulence penetration speed"

        [
          "Maximum flap lowering speed"
        ]
    , "The beginning of DAYLIGHT in UTC at FORREST WA (3050S 12807E) on the 7th February XXXX is closest to:" ~>
      Multichoice
        [
        ]

        "062035 UTC"

        [
          "070507 UTC"
        , "072035 UTC"
        , "062107 UTC"
        ]
    , "Given\n* Aerodrome elevation = 1500 ft\n* Cruise level = A065\n* Rate of Descent = 500 fpm\n* Ground Speed = 140 kts\n* Local QNH = area QNH\nTo arrive over the aerodrome at 1500 ft AGL the distance from the TOPD to your destination will be closest to:" ~>
      Multichoice
        [
          "13 nm"
        , "21 nm"
        , "28 nm"
        ]

        "16 nm"

        [
        ]
    , "Refer: ARFOR/TAF 6 Page 50 (Work Booklet) The cumulonimbus cloud to the west of Area 22 is forecast to occur:" ~>
      Multichoice
        [
          "As overcast cloud over the whole area"
        , "As individual cells"
        , "Occasionally up to 28000 feet"
        ]

        "As well separated cells"

        [
        ]
    , "Refer (BOURKE) WAC 3356 (page 30, Work Booklet). You plan to fly from ST GEORGE (YSGE) (2803S 14836E) to WALGETT (YWLG) (3002S 14807E). Your track YSGE to YWLG is 181 Degrees (M). The ARFOR indicates that there will be more that 4 oktas cloud with a base of 10,000 ft along the route. The highest altitude at which you may cruise VFR in accordance with the Table of Cruising Levels is:" ~>
      Multichoice
        [
          "A075"
        , "A095"
        ]

        "A085"

        [
          "A080"
        , "A090"
        ]
    , "Which of the following forecast conditions for a destination aerodrome will warrant the provision of an alternate for a VFR Day flight in an aeroplane:" ~>
      Multichoice
        [
          "Cloud exceeding 4 Oktas at 2000 ft AMSL"
        , "Visibility “9999”"
        , "TAF is endorsed with a 30% probability of dust which restricts visibility to 9 km"
        ]

        "Cross-wind component exceed the maximum allowed by the aeroplane’s flight manual"

        [
        ]
    , "If uncertain of position how can you track to an aerodrome with a VOR? Tune and indemnify the station; turn OBS till the:" ~>
      Multichoice
        [
          "CDI is centred and flag shows  “FROM”. Keep CDI centred by flying towards the needle if it wanders off centre"
        ]

        "CDI is centred and flag shows  “TO”. Keep CDI centred by flying towards the needle if it wanders off centre"

        [
          "Course window indicates “000” and flag shows “TO”. Maintain “000” in the course window and the CDI central"
        , "Course window indicates “000”. Turn the aircraft till the CDI centred and fly towards the needle if it wanders off centre"
        ]
    , "The following code refers to a layer of cloud as forecast in a TAF. “BKN030” – What is the amount of cloud in the layer:" ~>
      Multichoice
        [
          "5 to 8 OKTAS"
        , "3 to 4 OKTAS"
        , "4 to 7 OKTAS"
        , "2 to 5 OKTAS"
        ]

        "5 to 7 OKTAS"

        [
        ]
    , "Refer (TOWNSVILLE) WAC 3219 (Page 27, Work Booklet). What is the position (Latitude and Longitude) of ROLLINGSTONE (approximately 25 nm NW Townsville):" ~>
      Multichoice
        [
          "18 58S 146 23E"
        ]

        "19 03S 146 23E"

        [
          "19 30S 146 52E"
        ]
    , "Refer Loading System ALPHA (Page 10 and 11 Work Booklet). Given:\n* Aircraft basic index units = -190\n* Aircraft basic empty weight = 1018 kg\n* Row 1 – pilot and passenger = 200 kg\n* Row 2 – passenger, forward facing = 50 kg\n* Baggage rear compartment = 40 kg\n* Fuel = Full\nThe maximum weight of baggage, in Kg that may be carried in the nose compartment at take-off is closest to:" ~>
      Multichoice
        [
        ]

        "10 kg"

        [
          "35 kg"
        , "45 kg"
        , "20 kg"
        ]
    , "If other conditions remain the same, what effect will an increase in surface air temperature have on take-off distance required:" ~>
      Multichoice
        [
        ]

        "Distance required increases"

        [
          "Distance required is unchanged"
        , "Distance required is unchanged but take-off roll is increased"
        , "Distance required decreases"
        ]
    , "Refer (SYDNEY) WAC 3456 (centre-page, Work Booklet). The following details pertain to a direct flight from BATHURST (YBTH) (3325S 14939E) to DUBBO (YSDU) (3213S 14834E): ATD YBTH at 2305UTC, 2317UTC over HILL END town (approximately 25 nm from YBTH), constant HDG maintained since departure YBTH. The alteration of HDG required to regain planned TR to YSDU abeam WELLINGTON town is closest to:" ~>
      Multichoice
        [
          "10 degrees Left"
        ]

        "16 degrees Left"

        [
          "16 degrees Right"
        , "10 degrees Left"
        ]
    , "Which of the following errors or limitations is most commonly associated with the ADF/NDB system:" ~>
      Multichoice
        [
          "Scalloping"
        , "Bending"
        , "Line-of-sight range"
        ]

        "Thunderstorm interference"

        [
        ]
    , "Given:\n* Area forecast W/V = 115/45\n* TR (T) = 150 Degrees\n* Variation = 10 Degrees East\n* TAS = 150 kts\nDetermine the approximate HDG (M) and GS:" ~>
      Multichoice
        [
          "140 Degrees and 110 kt"
        ]

        "130 Degrees and 110 kt"

        [
          "130 Degrees and 120 kt"
        , "138 Degrees and 115 kt"
        ]
    , "Which of the following would be best described as an internal threat" ~>
      Multichoice
        [
          "Adverse weather"
        ]

        "Pilot fatigue"

        [
          "Operation at an unfamiliar aerodrome"
        , "Taxiing at an unfamiliar aerodrome with poor taxiway markings"
        ]
    , "Given W/V = 250/30 and runways available are 03/21 and 09/27, which runway has greatest headwind component for landing:" ~>
      Multichoice
        [
          "21"
        ]

        "27"

        [
          "03"
        , "09"
        ]
    , "Express in UTC - Six (6) PM Western Standard:" ~>
      Multichoice
        [
          "2000 UTC"
        , "0800 UTC"
        , "0830 UTC"
        ]

        "1000 UTC"

        [
        ]
    , "The following code refers to a layer of cloud as forecast in a TAF – FEW030. What is the amount of cloud in the layer:" ~>
      Multichoice
        [
          "2 to 3 OKTAS"
        , "0 to 2 OKTAS"
        ]

        "1 to 2 OKTAS"

        [
          "5 to 7 OKTAS"
        , "3 to 4 OKTAS"
        ]
    , "Refer Loading System ALPHA (pages 10 and 11, Work booklet). Given:\n* Basic index units = -190\n* Basic empty weight = 1016 kg\n* Row 1 (pilot and passenger) = 160 kg\n* Row 2 (forward facing passengers) = 110 kg\n* Baggage (total weight) = 95 kg\n* Baggage will not fit in the rear compartment, so may be loaded in Row 3 or the nose compartment.\nThe maximum weight of fuel, in kg, that may be carried at take-off is closed to:" ~>
      Multichoice
        [
          "210 kg"
        ]

        "252 kg"

        [
          "223 kg"
        ]
    , "Refer Figure 5 (page 8, Work booklet). Given:\n* Runway = 05/23\n* Take-off distance available = 1200 metres\n* Surface = short dry grass\n* Slope = 2% down to the NE\n* Pressure height = 1000 feet\n* Wind = 230/05 kts\n* Temperature = +28oC\n* Take-off Weight = 1055 kg\nThe minimum take-off distance required under the conditions given is closest to:" ~>
      Multichoice
        [
          "710 metres"
        , "1040 metres"
        , "1240 metres"
        ]

        "860 metres"

        [
        ]
    , "If approaching an NDB from the east, what approximate instrument indication would confirm that you are overflying the station? “Fixed card” ADF reading will change from:" ~>
      Multichoice
        [
          "270 to 090"
        , "180 to 000"
        ]

        "000 to 180"

        [
          "090 to 270"
        ]
    , "An effect of loading an aeroplane with its centre of gravity forward of the forward limit is that:" ~>
      Multichoice
        [
          "Tendency to stall is increased"
        , "Longitudinal stability is decreased"
        , "Spin recovery is inhibited"
        ]

        "Elevator effectiveness is reduced"

        [
        ]
    , "Refer ARFOR / TAF 6  (page50, Work booklet). The forecast visibility for area 22 is:" ~>
      Multichoice
        [
          "4000 metres with SH/DZ/TS"
        , "10 kilometres or more"
        ]

        "10 kilometres reducing to 4000 metres in SH/DZ/TS"

        [
          "4000 metres reducing in SH/DZ/TS"
        ]
    , "An aerofoil is said to be at its stalling angle if any increase or decrease in angle of attack results in: " ~>
      Multichoice
        [
          "Less lift and less drag"
        , "A lower L/D ratio"
        ]

        "Less lift"

        [
          "More drag"
        ]
    , "The condition most likely to produce advection fog is:" ~>
      Multichoice
        [
          "A cold air mass passing another cold surface"
        , "A cold air mass passing over a relatively warm surface"
        , "A warm air mass passing over another warm surface"
        ]

        "A warm air mass passing over a relatively cold surface"

        [
        ]
    , "An aircraft is on a glide approach at 65 kts. The pilot lowers full flap and maintains 65 kts. What is the effect on approach angle:" ~>
      Multichoice
        [
          "There is no effect on approach angle"
        ]

        "Approach angle steepens"

        [
          "Approach angle flattens"
        ]
    , "After take-off, what will be the effect on airspeed if you encounter wind shear where the headwind component increases:" ~>
      Multichoice
        [
        ]

        "An increase in airspeed and then it returns to normal"

        [
          "An increase in airspeed and then it remains higher"
        , "A loss of airspeed and then it returns to normal"
        , "No effect on airspeed"
        ]
    , "Refer to the total drag curve at Fig 40 (page 40, Work booklet). Which item of piston-engine aeroplane performance will occur at speed ‘X’:" ~>
      Multichoice
        [
        ]

        "Maximum still air range"

        [
          "Minimum rate of descent"
        , "Best endurance"
        , "Maximum rate of climb"
        ]
    , "If the indicated airspeed of an aeroplane is progressively reduced whilst maintaining level flight, induced drag will:" ~>
      Multichoice
        [
          "Decrease progressively"
        , "Decrease then increase"
        ]

        "Increase progressively"

        [
          "Increase then decrease"
        ]
    , "The maximum rate of climb speed will normally be achieved at:" ~>
      Multichoice
        [
          "The same as the speed giving the maximum angle of climb"
        ]

        "Greater than the speed giving maximum angle of climb"

        [
          "Greater than that recommended for normal climb"
        , "Less that the speed giving maximum angle of climb"
        ]
    , "Select the variable that would reduce the indicated stalling speed of an aeroplane in level flight:" ~>
      Multichoice
        [
        ]

        "A reduction in gross weight"

        [
          "An increase in air density"
        , "An increase in altitude"
        , "A reduction in power"
        ]
    , "Refer to Fig 49 (page 44, work booklet). The pressure pattern about point ‘B’ (east of Brisbane) is known as a:" ~>
      Multichoice
        [
        ]

        "High"

        [
          "Ridge"
        , "Low"
        , "Col"
        ]
    , "Operating an aircraft engine with the oil level below the specified minimum is likely to cause:" ~>
      Multichoice
        [
          "Oil foaming"
        , "Spark plug fouling"
        ]

        "Engine overheating"

        [
          "Excessive oil consumption"
        ]
    , "An aircraft piston engine will experience severe detonation when operated with:" ~>
      Multichoice
        [
        ]

        "A mixture of AVGAS and Aviation Turbine fuel"

        [
          "A Low power setting and carburettor heat full on"
        , "An over rich mixture during climb"
        , "Maximum continuous power in level flight"
        ]
    , "The effect of hoar frost forming on the wings of an aeroplane is:" ~>
      Multichoice
        [
        ]

        "Hazardous, because it can substantially increase stalling speed"

        [
          "Negligible, because it does not alter the aerodynamic shape"
        , "Negligible, because hoar frost weighs very little"
        , "Hazardous, because it can lock the aerodynamic controls"
        ]
    , "Refer to Fig 48 (page 44, work booklet). The pressure pattern about point ‘A’ (south of Tasmania) is known as a:" ~>
      Multichoice
        [
          "High"
        , "Col"
        , "Trough"
        ]

        "Low"

        [
        ]
    , "In comparison to a stall in straight and level flight, how will a sudden pull out from a dive affect the stall IAS and the stalling angle:" ~>
      Multichoice
        [
          "Stall IAS will increase; stalling angle will be less"
        , "Stall IAS and stalling angle will both increase"
        ]

        "Stall IAS will increase; stalling angle will remain constant"

        [
          "Stall IAS will remain constant; stalling angle will be less"
        ]
    , "A thin coating of clear ice on the leading edge of an aerofoil of an aircraft in flight will:" ~>
      Multichoice
        [
          "Initially increase the efficiency of the aerofoil by reducing drag"
        , "Has no appreciable effect as the decrease in lift is balanced by a corresponding decrease in drag"
        ]

        "Decrease lift and increase drag by changing the aerofoil characteristics"

        [
          "Initially increase the lift by increasing the camber"
        ]
    , "Which of the following is a likely reason for yaw to the right during the take-off roll in a single-engine aeroplane fitted with an engine, which rotates in a clockwise direction when viewed from the cockpit:" ~>
      Multichoice
        [
          "Torque effect"
        ]

        "A crosswind from the right"

        [
          "Slipstream effect"
        , "A crosswind from the left"
        ]
    , "Refer to Fig 49 (page 44, work booklet). The line ‘PR ‘ (west of Townsville) indicates the position of a:" ~>
      Multichoice
        [
          "Front"
        , "Ridge"
        , "Line of equal pressure"
        ]

        "Trough"

        [
        ]
    , "Hoar frost, which has formed on an aircraft, parked in the open overnight:" ~>
      Multichoice
        [
          "Does not affect performance"
        , "Will not affect performance sufficiently to cause any hazard"
        , "Will not affect performance, but may lock the controls solid"
        ]

        "May seriously affect performance and should always be removed before flight"

        [
        ]
    , "How will an increase in altitude affect the stall characteristics of an aeroplane:" ~>
      Multichoice
        [
          "The stalling angle will increase"
        , "The stall IAS will increase"
        ]

        "The stall IAS will remain the same"

        [
          "The stall angle will decrease"
        ]
    , "How will lift and darg vary if the angle of attach of an aerofoil is increased from 4 degrees up to the stalling angle:" ~>
      Multichoice
        [
          "Lift will increase and drag will decrease"
        , "Lift will decrease and drag will increase"
        ]

        "Lift and drag will both increase"

        [
          "Lift and drag will both decrease"
        ]
    , "The most significant effect on an aeroplane’s flight characteristics of NOT clearing frost from the wings prior to flight is that:" ~>
      Multichoice
        [
          "There is a marked deterioration in acceleration during the take-off run"
        , "The stalling angle will increase"
        ]

        "The indicated stalling speed will increase"

        [
          "Both the indicated stalling speed and stalling angle will increase"
        ]
    , "A characteristics of Rime Ice, which may be hazardous to aircraft, is that it:" ~>
      Multichoice
        [
          "Is likely to lock the control surfaces"
        ]

        "May block the pitot tube"

        [
          "Is the most difficult type of ice to remove"
        , "Spreads back over most of the aerofoil section"
        ]
    , "Unauthorised penetration of controlled airspace is an example of an undesired" ~>
      Multichoice
        [
          "Ground navigation state"
        ]

        "Aircraft handling state"

        [
          "Air navigation state"
        , "Navigation configuration state"
        ]
    , "An aeroplane takes off in calm conditions and climbs through a wind shear where it encounters a strong tail wind. What initial effect will this change in W/V have on IAS and flight path:" ~>
      Multichoice
        [
        ]

        "IAS will decrease; flight path will be shallower"

        [
          "IAS will decrease; flight path will be steeper"
        , "IAS will increase; flight path will be shallower"
        , "IAS will increase; flight path will be steeper"
        ]
    , "Flying conditions above a layer of small cumulus clouds are likely to be:" ~>
      Multichoice
        [
          "Bumpy"
        , "Smooth but strong updraughts"
        ]

        "Smooth"

        [
          "Very turbulent"
        ]
    , "A pilot is landing at a coastal aerodrome orientated east west, with the sea to the south, on a hot summer afternoon. What is the most likely wind direction at the aerodrome:" ~>
      Multichoice
        [
          "Northerly"
        , "Light and Variable"
        ]

        "Southerly"

        [
          "Westerly"
        ]
    , "Which hazard is normally associated with mountain waves:" ~>
      Multichoice
        [
        ]

        "Turbulence"

        [
          "Dust storms"
        , "Icing"
        , "Hail"
        ]
    , "Which of the following indications would positively confirm that an aeroplane is established in a spin and is not in a spiral dive:" ~>
      Multichoice
        [
          "Nose attitude"
        , "High rate of turn"
        , "Increasing airspeed"
        ]

        "Low airspeed"

        [
        ]
    , "The colour of 100LL aviation gasoline is:" ~>
      Multichoice
        [
          "Green"
        , "Pale Yellow"
        , "Red"
        ]

        "Blue"

        [
        ]
    , "The symptoms of dehydration include which of the following:" ~>
      Multichoice
        [
          "Profuse sweating"
        ]

        "Drying of nasal passages and a prickly sensation in the eyes"

        [
          "Runny eyes and nose"
        , "Stomach cramps and a high temperature"
        ]
    , "Which of the following would be an appropriate remedy for a person who is suffering the effects of hyperventilation:" ~>
      Multichoice
        [
          "Hold the breath for about 40 seconds"
        ]

        "Breathe into a paper bag"

        [
          "Increase the rate and depth of breathing"
        , "Use a nasal decongestant"
        ]
    , "Deterioration of hearing caused by exposure to loud noise usually occurs first:" ~>
      Multichoice
        [
        ]

        "In the high frequency range, above normal speech"

        [
          "In the frequency range of normal speech, 300 to 5000 Hz"
        , "In the frequency range below normal speech"
        , "Across the whole range of audible frequencies"
        ]
    , "In level flight, a collision risk exists if a converging aircraft viewed from the cockpit appears to be:" ~>
      Multichoice
        [
        ]

        "On the horizon and maintaining a constant position in your windscreen"

        [
          "Below the horizon and moving closer to the centre of your windscreen"
        , "On the horizon and moving away from the centre of your windscreen"
        , "On the horizon"
        ]
    , "The most effective way to scan the sky for other aircraft during level flight is to:" ~>
      Multichoice
        [
          "Move the head in a continuous arc from side to side"
        , "Keep the head still & move the eyes continuously from side to side"
        , "Do not look anywhere but straight ahead unless you detect movement"
        ]

        "Move the head about 20deg to 30deg at a time, pausing after each movement to allow the peripheral vision to detect any movement"

        [
        ]
    , "In the absence of reliable visual information, which of the following states of motion would be most difficult to differentiate:" ~>
      Multichoice
        [
          "A steep turn and a rapid deceleration in level flight"
        , "A rapid acceleration in level flight and a transition from level flight to a dive"
        ]

        "A rapid acceleration in level flight and a transition from level flight to a climb"

        [
          "A rapid deceleration in level flight and a transition from level flight to a climb"
        ]
    , "Which of the following is NOT a symptom of carbon monoxide poisoning:" ~>
      Multichoice
        [
          "Headache and fatigue"
        ]

        "A feeling of euphoria"

        [
          "Discomfort in breathing"
        , "Impairment of vision and mental confusion"
        ]
    , "Hypoxia may be caused by:" ~>
      Multichoice
        [
          "Flying with a head cold"
        ]

        "Flying at an altitude where the partial pressure of oxygen is too low"

        [
          "Breathing too quickly and/or too deeply for the requirements of the body"
        , "Flying after a period of underwater diving"
        ]
    , "For the flight crew, the three basic components in the TEM model are" ~>
      Multichoice
        [
        ]

        "Threats, errors and undesired aircraft state"

        [
          "Threats, errors, and anticipated aircraft states"
        , "Threats, flight crew human resources and aircraft states"
        , "Errors, fight crew human resources and undesired aircraft states"
        ]
    , "An example of an unexpected threat is" ~>
      Multichoice
        [
          "Thunderstorms forecast on the TAF"
        ]

        "Engine failure in flight"

        [
          "Becoming lost in flight"
        , "Being diverted in flight by ATC"
        ]
    ]

curtisPplTrial5 ::
  Exam
curtisPplTrial5 =
  Exam
    "Curtis Aviation Private Pilot Licence Trial Examination 5"
    (Just "Time Allowed: 3 hours 30 minutes. Pass: >= 70%")
    (Just "01 August 2015")
    [
      "The limitation imposed by CAR’s on turns on to the final approach is that the turn shall:" ~>
      Multichoice
        [
          "Not be commenced below 500ft. AGL."
        , "Be completed by 500ft. AGL."
        , "Permit a straight-in approach of at least 1,000m from the runway threshold."
        ]

        "Permit a straight-in approach of at least 500m from the perimeter of the aerodrome."

        [
        ]
    , "If climbing after take-off to A085 when should you change from local QNH to Area QNH:" ~>
      Multichoice
        [
        ]

        "At top of climb"

        [
          "When climbing through 3000 ft"
        , "At 1000 ft above the departure aerodrome"
        , "When climbing through 5000 ft"
        ]
    , "Which condition applies to flights within Restricted Area:" ~>
      Multichoice
        [
        ]

        "Flight within these areas are normally only permitted outside the hours of activity or with prior approval"

        [
          "Flights within these areas are not permitted under any circumstances"
        , "Flights by civil aircraft within these areas is not permitted"
        , "Prior approval to enter these areas is not necessary, but flights must proceed in accordance with specific procedures"
        ]
    , "You are flying home with your friend and his dog.  As pilot in command what are your legal responsibilities regarding the carriage of this dog:" ~>
      Multichoice
        [
        ]

        "The dog must be crated and kept in the baggage compartment"

        [
          "You are not responsible for the animal as it belongs to your friend."
        , "If carried in the passenger compartment, the dog must be restrained, muzzled and sitting on a moisture-absorbent mat"
        , "The dog must be restrained and tied next to your passenger."
        ]
    , "Who is responsible for declaring a bush fire evacuation flight as a mercy flight:" ~>
      Multichoice
        [
          "The local police"
        , "The head of the local fire department"
        ]

        "The Pilot in Command"

        [
          "The owner of the aircraft"
        ]
    , "The highest obstacle along an unpopulated route segment is 1500 ft AMSL. The minimum altitude at which a VFR aircraft may normally overfly the obstacle is:" ~>
      Multichoice
        [
          "3000 ft"
        , "500 ft"
        ]

        "2000 ft"

        [
          "1000 ft"
        ]
    , "A steady red light signal directed at an aircraft in flight signifies to the pilot that the:" ~>
      Multichoice
        [
        ]

        "Aircraft should give way to other aircraft and continue circling"

        [
          "Aerodrome is unsafe for landing"
        , "Aircraft should climb above the circuit height and orbit the aerodrome"
        , "Aircraft's undercarriage is not fully down - Go round"
        ]
    , "At what stage of a flight should you change from Area QNH to local QNH (if available) when descending from A075:" ~>
      Multichoice
        [
        ]

        "At top of descent"

        [
          "In the circuit area"
        , "When passing through 5000 ft"
        , "Within 3 nm of destination"
        ]
    , "A threat that is not immediately obvious to the pilot, such as an undercarriage lever situated adjacent to a flap lever, or manifold pressure and RMP gauges widely separated, is best described as:" ~>
      Multichoice
        [
        ]

        "A latent threat"

        [
          "An external threat"
        , "An anticipated threat"
        , "An internal threat"
        ]
    , "Which of the following restrictions applies to the consumption of alcoholic liquor by pilots:" ~>
      Multichoice
        [
          "It may be consumed immediately prior to commencement of duty provided the capacity to act is not impaired"
        , "It may not be consumed at any time."
        ]

        "It may be consumed up to, but no within, the 8 hours immediately preceding departure"

        [
          "It may not be consumed in the 12 hours immediately preceding flight departure."
        ]
    , "A SIGMET is transmitted to a pilot in flight advising of reported mountain wave activity. Select the description which best describes the category of this threat.:" ~>
      Multichoice
        [
          "External, unexpected, organisational"
        , "Internal, anticipated, environmental"
        ]

        "External, anticipated, environmental"

        [
          "Organisational, anticipated, latent"
        ]
    , "The holder of a pilot licence may not act as pilot in command of an aeroplane engaged in spinning practice unless:" ~>
      Multichoice
        [
          "Accompanied by a suitably qualified flight instructor."
        , "Accompanied by a safety pilot who is the holder of a commercial or higher class licence."
        , "The aeroplane is fitted with fully functional dual controls."
        ]

        "Certified by a suitably qualified flight instructor as being competent to recover from fully developed spins."

        [
        ]
    , "Refer to Figure 4 (page 7, Work Booklet). Given the following details:\n* Runway = 15/33\n* Landing distance available = 1300 metres\n* Slope = 2% down to the SE\n* Pressure height = 3000 ft\n* Wind = 150/15 kts\n* Temperature = +20 Degrees C\nThe Landing distance required is closest to:" ~>
      Multichoice
        [
          "720 metres"
        , "500 metres"
        , "540 metres"
        ]

        "630 metres"

        [
        ]
    , "Refer to Figure 4 (page 7, Work Booklet). Given the following details:\n* Runway = 09/27\n* Landing distance available = 1400 metres\n* Slope = Level\n* Pressure height = 5000 ft\n* Wind = 270/10 kts\n* Temperature = +15 Degrees C\nThe landing distance required under the conditions given is closest to:" ~>
      Multichoice
        [
          "670 metres"
        ]

        "600 metres"

        [
          "630 metres"
        , "700 metres"
        ]
    , "Refer to loading system ALPHA (pages 10 and 11, Work Booklet). Given:\n* Basic index units = -190\n* Basic empty weight = 1016 kg\n* Row 1 (pilot and passenger) = 160 kg\n* Row (forward facing passengers) = 110 kg\n* Baggage (total weight) = 95 kg\n* Baggage will not fit in the rear compartment, so may be loaded in Row 3 or the nose compartment.\nThe maximum weight of fuel, in kg, that may be carried at take-off is closest to:" ~>
      Multichoice
        [
        ]

        "252kg"

        [
          "210 kg"
        , "223 kg"
        ]
    , "Take-off distance required will increase if there is an increase in:" ~>
      Multichoice
        [
        ]

        "Surface air temperature"

        [
          "Air density"
        , "Percentage down-slope"
        , "Headwind component"
        ]
    , "Refer to Figure 6 (page 9, Work Booklet). Given the following details:\n* Runway = 04/22\n* Landing distance available = 1100 metres\n* Slope = Level\n* Pressure height = 3000 ft\n* Wind = 220/05 kts\n* Temperature = +20 Degrees C\nThe landing distance required under the conditions given is closest to:" ~>
      Multichoice
        [
          "550 metres"
        , "610 metres"
        ]

        "520 metres"

        [
          "500 metres"
        ]
    , "Refer loading system BRAVO (pages 12 and 13, Work Booklet). Given:\n* Aircraft empty weight = 1265 lbs with a moment of 101.5\n* Oil = 15 lbs with a moment of 0.5\n* Pilot = 170 lbs\n* Co-pilot = 155 lbs\n* Fuel = 110 litres\n* Baggage = Maximum permitted\nThe maximum permitted weight of passengers in the rear seats is closest to:" ~>
      Multichoice
        [
          "402 lbs"
        , "369 lbs"
        ]

        "302 lbs"

        [
        ]
    , "Given:\n* Pressure height = 6000 feet\n* OAT = +20 degrees C\n* CAS = 120 kts\nDetermine the TAS:" ~>
      Multichoice
        [
          "125 kts"
        , "130 kts"
        , "106 kts"
        ]

        "135 kts"

        [
        ]
    , "Your planned TR is 140 degrees (M). Your planned HDG is 135 degrees (M). Your departure aerodrome has a NDB. If you maintain HDG 135 degrees (M), which \"fixed card\" ADF indication would confirm that you are on TR:" ~>
      Multichoice
        [
        ]

        "185"

        [
          "180"
        , "175"
        , "315"
        , "135"
        ]
    , "If the ETA at destination falls within the forecast periods of that aerodrome's TAF with the following conditions, which of condition would ALWAYS require you to nominate an alternate:" ~>
      Multichoice
        [
        ]

        "PROVISIONAL"

        [
          "PROB"
        , "TEMPO"
        , "INTER"
        ]
    , "You plan to fly NARRABRI (NSW) (3019S 14950E) to ST GEORGE (QLD) (2803S 14807E). For the total flight, which ARFORS are required:" ~>
      Multichoice
        [
          "20, 22, 40"
        ]

        "20, 22, 41"

        [
          "20, 21, 22"
        , "20, 40, 41"
        ]
    , "Refer (BOURKE) WAC 3356 (page 30, Work Booklet). You are flying over WEE WAA township, approximately 20 nm WNW of NARRABRI (YNBR) (3019S 14950E), direct to ST GEORGE (YSGE) 2803S 14836E). At 2347 UTC you pinpoint your position over a minor road 5 nm northeast of MUNGINDI township (approximately 60 nm SE YSGE). You have been maintaining a constant HDG since WEE WAA township. The alteration of HDG required to track from the 2347 UTC position to YSGE is closest to:" ~>
      Multichoice
        [
          "16 degrees left"
        , "07 degrees left"
        , "05 degrees left"
        ]

        "12 degrees left"

        [
        ]
    , "Refer (BOURKE) WAC 3356 (page 30, Work Booklet). You plan direct WEE WAA (YWWA) (3015S 14924E), to ST GEORGE (YSGE) 2803S 14836E). At 0235 UTC you depart YWWA with a planned ground speed of 120 kts. At 0255 UTC you pinpoint your position left of track over WOODVALE homestead and alter HDG 10 degrees right. At 0315 UTC you pinpoint your position over MUNGINDI township. The alteration of HDG required to track to YSGE from the 0315 UTC position is closest to:" ~>
      Multichoice
        [
          "15 degrees left"
        , "10 degrees left"
        ]

        "19 degrees left"

        [
          "04 degrees left"
        ]
    , "Given: Useable fuel available (excluding reserves) = 110 litres. Fuel flow = 38LPH, G/S = 105 kts. The maximum distance that can be flown is closest to:" ~>
      Multichoice
        [
          "135 nm"
        , "210 nm"
        ]

        "305 nm"

        [
          "365 nm"
        ]
    , "Given:\n* Aerodrome elevation = 1500 feet\n* Cruise level = A065\n* Rate of descent = 500fpm\n* GS = 140 kts\n* Local QNH = Area QNH\nTo arrive over the aerodrome at 1500 feet AGL the distance from the TOPD to your destination will be closest to:" ~>
      Multichoice
        [
          "28 nm"
        ]

        "16 nm"

        [
          "13 nm"
        , "21 nm"
        ]
    , "Given the following conditions for planning a route segment:\n* W/V = 050/25 kts\n* TR (M) = 283\n* TAS = 125 kts\n* Magnetic Variation = 5 Degrees West\nThe HDG (M) and GS will be closest to:" ~>
      Multichoice
        [
          "273 (M), 138 kts"
        , "293 (M), 110 kts"
        , "294 (M), 115 kts"
        ]

        "292 (M), 140 kts"

        [
        ]
    , "The following message was issued during a pre-flight briefing: METAR YMLT 2300UTC 0001 25004KT 9999 SCT025 SCT035 18/12 Q 1020. The message is:" ~>
      Multichoice
        [
        ]

        "A routine aerodrome meteorological report"

        [
          "An aerodrome forecast"
        , "A trend type forecast"
        , "An aerodrome report issued when conditions have deteriorated below specified limits"
        ]
    , "The purpose of establishing take-off safety speed is to:" ~>
      Multichoice
        [
          "Ensure the greatest rate of climb following take-off"
        , "Ensure the greatest climb gradient (angle of climb) following take-off"
        ]

        "Ensure that adequate control is available if engine failure occurs following take-off"

        [
          "Provide the lowest speed at which the aeroplane may become airborne"
        ]
    , "Given an elevation of 1500 feet, a QNH of 1028 hPa, and an air temperature of +35 degrees C, the density height will be closest to:" ~>
      Multichoice
        [
          "2200 ft"
        , "4300 ft"
        ]

        "3700 ft"

        [
          "4800 ft"
        ]
    , "The take-off distance will be decreased with an increase in:" ~>
      Multichoice
        [
          "Tailwind component"
        , "Stalling speed"
        ]

        "Atmospheric pressure"

        [
          "Density height"
        ]
    , "Soon after lifting off from the runway at the take-off safety speed an aeroplane experiences a sudden loss of both lift and airspeed. This could be caused by:" ~>
      Multichoice
        [
          "Entering ground effect"
        , "Climbing out of ground effect"
        , "A sudden increase in headwind component"
        ]

        "A sudden decrease in headwind component"

        [
        ]
    , "Given:\n* Pressure height = 9000 ft\n* Ambient temperature = -15 Degrees C\n* CAS = 170 kts\nThe TAS will be closest to:" ~>
      Multichoice
        [
        ]

        "190 kts"

        [
          "195 kts"
        , "202 kts"
        , "152 kts"
        ]
    , "Given:\n* W/V = 250(M)/30 kts\n* Runways available = 03/21 and 09/27\nWhich runway has the greatest headwind component for landing:" ~>
      Multichoice
        [
          "21"
        ]

        "27"

        [
          "09"
        , "03"
        ]
    , "Refer figure 30 (page 36, Work Booklet). Where \"D\" = 30 nm, Track error = 8 degrees. If a constant HDG has been maintained since 0100 (at \"A\") and the pilot alters HDG 12 degrees right at time 0112 (at \"B\"), what time should the pilot expect to intercept track:" ~>
      Multichoice
        [
          "0130"
        ]

        "0136"

        [
          "0148"
        , "0124"
        ]
    , "During winter in Australia, in what direction from the departure aerodrome will the end of daylight (in LMT) be earlier:" ~>
      Multichoice
        [
        ]

        "South"

        [
          "West"
        , "East"
        , "North"
        ]
    , "During summer in Australia, in what direction from the departure aerodrome will the end of daylight (in LMT) be earlier:" ~>
      Multichoice
        [
          "West"
        ]

        "North"

        [
          "South"
        , "East"
        ]
    , "Refer: ARFOR/TAF 2 (page 46 Work Booklet) Depart YSWG 0200 – YSCB – arrive YMER 0350. The forecast wind velocity at 10000 feet between YSCB and YMER is:" ~>
      Multichoice
        [
          "280 Deg (M), 30 kts"
        , "340 Deg (M), 40 kts"
        , "280 Deg (M), 30 kts reducing to zero"
        , "280 Deg (T), 30 kts"
        ]

        "340 Deg (T), 40 kts"

        [
        ]
    , "Refer: ARFOR/TAF 4 (page 48 Work Booklet) Depart YSWG 0000 – YSCB – arrive YMER 0200. The forecast wind velocity for arrival in the circuit area YMER is:" ~>
      Multichoice
        [
        ]

        "290 Deg (T), 15 kts"

        [
          "260 Deg (T), 25 kts"
        , "290 Deg (M), 15 kts"
        , "280 Deg (T), 10 kts"
        , "260 Deg (M), 25 kts"
        ]
    , "Which of the following is an example of a latent threat:" ~>
      Multichoice
        [
          "Committing a check list to memory"
        ]

        "A poorly written checklist"

        [
          "Engine failure in flight"
        , "An encounter with rime ice"
        ]
    , "Stalling speed is increased by an increase in:" ~>
      Multichoice
        [
          "Angle of attack"
        , "Power"
        , "Flap setting"
        ]

        "Load factor"

        [
        ]
    , "What effect will a headwind have on the rate of climb:" ~>
      Multichoice
        [
          "Rate of climb increases"
        , "Rate of climb decreases"
        ]

        "Rate of climb remains unchanged"

        [
        ]
    , "A type of threat which is best managed by maintaining a high skill level through practice and training is:" ~>
      Multichoice
        [
          "An anticipated threat"
        , "A latent threat"
        ]

        "An unexpected threat"

        [
          "An organisational threat"
        ]
    , "Climbing an aeroplane at a higher speed than that recommended for best rate of climb will result in:" ~>
      Multichoice
        [
          "An increases rate and decreased angle of climb"
        , "An increased rate and angle of climb"
        ]

        "A decreased rate and angle of climb"

        [
          "A decreased rate and increased angle of climb"
        ]
    , "Which of the following would be best described as an internal threat:" ~>
      Multichoice
        [
          "Flight into a high traffic density area"
        , "An air traffic controller with a heavy foreign accent"
        ]

        "A pilot with a tendency to be over confident"

        [
          "A maintenance release that is incorrectly filled in"
        ]
    , "Refer to Fig 48 (page 44 Work Booklet) The line J – K (just to the east of Melbourne) indicates the position of a:" ~>
      Multichoice
        [
        ]

        "Cold Front"

        [
          "Stationary Front"
        , "Warm Front"
        , "Occluded Front"
        ]
    , "Refer to Fig 49 (page 44 Work Booklet) The pressure pattern about the point “B” (east of Brisbane QLD) is known as a:" ~>
      Multichoice
        [
          "Ridge"
        ]

        "High"

        [
          "Col"
        , "Low"
        ]
    , "Flying conditions above a layer of small cumulus clouds are likely to be:" ~>
      Multichoice
        [
        ]

        "Smooth"

        [
          "Bumpy"
        , "Very Turbulent"
        , "Smooth but with strong updraughts"
        ]
    , "Which of the following procedures would be most effective in rectifying a high cylinder head temperature indication:" ~>
      Multichoice
        [
          "Maintain power setting, enrich the mixture"
        ]

        "Decrease power setting, enrich the mixture"

        [
          "Maintain the power setting, lean the mixture"
        , "Decrease power setting, lean the mixture"
        ]
    , "During a full power check before take-off, at an aerodrome with a high density altitude, some engine roughness is detected. The roughness disappears as the mixture is leaned slightly. Under these circumstances the take-off should:" ~>
      Multichoice
        [
          "Be made with full rich mixture, and throttle retarded to the point of smooth running"
        , "Not be attempted until a LAME has rectified the problem"
        , "Be made with full throttle, full rich mixture, and the roughness accepted for the take-off run"
        ]

        "Be made with full throttle, and the mixture leaned to the point of smooth running"

        [
        ]
    , "During flight, a centre-zero ammeter shows an abnormally high positive reading for an extended period of time. The correct interpretation of this is that:" ~>
      Multichoice
        [
        ]

        "The battery is being overcharged and may boil"

        [
          "Although the reading is unusual, the electrical system will be unaffected"
        , "Instruments requiring electrical power will overread slightly"
        , "The alternator has failed and the battery is powering the electrical system"
        ]
    , "An aerofoil is said to be its stalling angle if any increase or decrease in angle of attack results in:" ~>
      Multichoice
        [
          "Less lift and less drag"
        , "More drag"
        ]

        "Less lift"

        [
          "A lower L/D ratio"
        ]
    , "A result of exceeding the stalling angle is that:" ~>
      Multichoice
        [
          "Air pressure below the wing is less than the pressure above the wing"
        , "Air pressure above the wing exceeds atmospheric pressure"
        ]

        "Lift decreases and drag increases"

        [
          "The wing no longer produces any lift"
        ]
    , "Which of the following effects will result from moving the centre of gravity aft:" ~>
      Multichoice
        [
          "The indicated stalling speed will increase"
        , "Greater elevator deflection is required to achieve a given change in attitude"
        , "The stalling angle will increase"
        ]

        "Longitudinal stability will deteriorate"

        [
        ]
    , "At which IAS should a piston-engine aeroplane be flown to achieve maximum endurance in level flight:" ~>
      Multichoice
        [
          "The speed which gives the best lift-drag ratio"
        ]

        "The speed requiring minimum power"

        [
          "The lowest possible speed"
        , "The speed requiring minimum thrust"
        ]
    , "From the list below, select the variable, which would result in an increase in the rate of climb:" ~>
      Multichoice
        [
          "Lowering take-off flap"
        ]

        "A reduction in gross weight"

        [
          "An increase in headwind component"
        , "A decrease in Air density"
        ]
    , "Which of the following is a likely reason for a yaw to the right during the take-off roll in a single-engine aeroplane fitted with an engine, which rotates in a clockwise direction when viewed from the cockpit:" ~>
      Multichoice
        [
          "A crosswind from the left"
        , "Slipstream effect"
        ]

        "A crosswind from the right"

        [
          "Torque effect"
        ]
    , "During flight, drag always acts parallel to the:" ~>
      Multichoice
        [
          "Thrust line"
        ]

        "Relative airflow"

        [
          "Wing chord"
        , "Longitudinal axis"
        ]
    , "What effect will a headwind have on the angle and rate of climb:" ~>
      Multichoice
        [
          "The angle and rate will both increase"
        , "The angle will decrease and the rate will increase"
        ]

        "The angle will increase and the rate will remain unchanged"

        [
          "The angle will increase and the rate will decrease"
        ]
    , "An increase in which of the parameters listed below will always result in an increase in Induced Drag:" ~>
      Multichoice
        [
          "Aspect ratio"
        , "L/D ratio"
        , "IAS"
        ]

        "Angle of attack"

        [
        ]
    , "Refer: to the L/D ratio graph at Fig 42 (page 40 Work Booklet) At what speed would an aeroplane be operating if flying at angle of attack ‘X’:" ~>
      Multichoice
        [
          "The maximum level flight speed"
        , "The minimum level flight speed"
        ]

        "The speed for maximum glide range"

        [
          "The speed for maximum angle of climb"
        ]
    , "If the indicated stalling speed of an aircraft is 60kt in straight and level flight what is the indicated stalling speed in a balanced 60 degree turn:" ~>
      Multichoice
        [
          "75 kts"
        , "60 kts"
        , "70 kts"
        ]

        "85 kts"

        [
        ]
    , "Which hazard is normally associated with mountain waves:" ~>
      Multichoice
        [
        ]

        "Turbulence"

        [
          "Icing"
        , "Duststorms"
        , "Hail"
        ]
    , "Which of the following conditions is most likely to result in thunderstorms:" ~>
      Multichoice
        [
        ]

        "High relative humidity at the surface, a lifting mechanism and conditional instability"

        [
          "High relative humidity at the surface, subsidence and a neutral atmosphere"
        , "Low relative humidity at the surface, a lifting mechanism and instability"
        , "High relative humidity at the surface, a lifting mechanism and a stable atmosphere"
        ]
    , "The cloud type which is indicative of mountain waves, when seen above a mountain range is" ~>
      Multichoice
        [
          "Cumulus"
        ]

        "Lenticular"

        [
          "Stratus"
        , "Cirrus"
        ]
    , "A pilot’s susceptibility to visual illusions during approaches will be increased by:" ~>
      Multichoice
        [
          "The amount and nature of the pilots flying experience"
        ]

        "Fatigue"

        [
          "Good night adaptation"
        , "The presence of good glideslpoe guidance"
        ]
    , "Regarding drugs, which can be taken without the approval of a DAME:" ~>
      Multichoice
        [
          "Only prescription drugs are permitted"
        , "Only non-prescription are permitted"
        , "Only legal drugs are permitted"
        ]

        "No drugs are permitted"

        [
        ]
    , "Hyperventilation may be caused by:" ~>
      Multichoice
        [
        ]

        "Overbreathing"

        [
          "Underbreathing"
        , "Breathing too much carbon dioxide"
        , "Breathing too much carbon monoxide"
        ]
    , "The ARFOR indicates that there will be ‘BROKEN’ stratus with the cloud top at 3000 feet AMSL along your planned route OCTA.  You plan VFR using radio navigation aids along this route.  Your planned track is 085 Deg (M).  To maintain VMC and conform to the ‘Table of Cruising Levels’ what is the lowest altitude that you can plan:" ~>
      Multichoice
        [
          "A035."
        , "A030."
        , "A050."
        ]

        "A055."

        [
        ]
    , "If you observe that the cloud amount is significantly greater than that is forecast, the type of report you should submit is called a:" ~>
      Multichoice
        [
          "SIGMET"
        , "TRENT"
        , "SPECI"
        ]

        "SHORT AIREP"

        [
        ]
    , "In the vicinity of an aerodrome within an ADIZ an aircraft is intercepted and is directed to land at the aerodrome.  To show that this is understood and will be complied with the pilot of the intercepted aircraft should, by day:" ~>
      Multichoice
        [
          "Activate the aircraft’s transponder identification function."
        ]

        "Lower the aircraft’s landing gear and follow the intercepting aircraft."

        [
          "Raise the aircraft landing gear while passing over the runway at a height of between 1000 and 2000 ft. and circle the aerodrome"
        , "Rock the aircraft’s wings and follow the intercepting aircraft."
        ]
    , "A pilot is put under pressure from his employer to get back to base before his tour of duty expires. This is an example of:" ~>
      Multichoice
        [
          "An external threat"
        , "An anticipated threat"
        , "An environmental threat"
        ]

        "A latent threat"

        [
        ]
    , "An aircraft piston engine will experience severe detonation when operated with:" ~>
      Multichoice
        [
          "Maximum continuous power in level flight"
        ]

        "A mixture of AVGAS and ATUR fuel"

        [
          "An over rich mixture during climb"
        , "A low power setting and carburettor heat full on"
        ]
    , "Optical illusions such as “black hole” effect and sloping runways are examples of:" ~>
      Multichoice
        [
          "An internal threat"
        , "An anticipated threat"
        ]

        "A latent environmental threat"

        [
          "A latent internal threat"
        ]
    , "An increase in headwind component, while gliding at the recommended best gliding speed, would result in:" ~>
      Multichoice
        [
          "A decrease in the rate of descent"
        , "An increase in the glide range"
        , "An increase in the rate of descent"
        ]

        "A decrease in the glide range"

        [
        ]
    ]

curtisPplTrial6 ::
  Exam
curtisPplTrial6 =
  Exam
    "Curtis Aviation Private Pilot Licence Trial Examination 6"
    (Just "Time Allowed: 3 hours 30 minutes. Pass: >= 70%")
    (Just "01 August 2015")
    [
      "Lanes of entry are established to provide VFR aircraft with a:" ~>
      Multichoice
        [
          "Traffic information service when entering or departing a general aviation control zone"
        , "Discrete route for overflying a primary control zone"
        ]

        "Passage to and from a general aviation control zone without infringing controlled airspace"

        [
          "Corridor in controlled airspace where ATC service is provide for entry to or departure from a general aviation control zone"
        ]
    , "You are ready to take-off at a non-controlled aerodrome from a runway 1000 metres in length. A twin-engine aircraft weighing over 2000 kg has just departed from the same runway. You should not commence your take-off until the other aircraft is at least:" ~>
      Multichoice
        [
          "1800 metres from your proposed take-off point"
        , "600 metres from your proposed take-off point"
        , "500 feet above ground level"
        ]

        "Crossing the upwind end of the runway"

        [
        ]
    , "You are asked to fly a critically ill person to a major city hospital for urgent medical treatment. The patient’s doctor confirms that the flight is vital to save loss of life. You may declare the flight as a mercy flight only if:" ~>
      Multichoice
        [
        ]

        "You will be unable to comply with applicable regulations and orders"

        [
          "You have obtained clearance from the nearest airways operations unit"
        , "The hospital is aware of the flight"
        , "You have confirmed that the aeroplane is authorised to conduct aerial work or charter"
        ]
    , "Which condition applies to flights within Restricted Areas:" ~>
      Multichoice
        [
        ]

        "Flights within these areas are normally only permitted outside the hours of activity or with prior approval"

        [
          "Flights by civil aircraft within these areas is not permitted"
        , "Prior approval to enter these areas is not necessary, but flights must proceed in accordance with specific procedures"
        , "Flights within these areas are not permitted under any circumstances"
        ]
    , "Which of the following is an example of a latent threat:" ~>
      Multichoice
        [
          "Committing a check list to memory"
        ]

        "A poorly written checklist"

        [
          "Engine failure in flight"
        , "An encounter with rime ice"
        ]
    , "A prohibited area is defined as airspace in which flights by civil aircraft is:" ~>
      Multichoice
        [
          "Not permitted without prior approval"
        , "Not permitted during daylight hours"
        , "Not permitted between 10pm and 6am local time"
        ]

        "Not permitted at any time and under any circumstances"

        [
        ]
    , "When is the pilot in command reqiuired to inspect and test the aircraft’s fuel system for the presence of water?:" ~>
      Multichoice
        [
          "Before the first flight of the day and after each flight"
        , "Before each flight"
        , "Before and after each refuelling"
        ]

        "Before the first flight of the day and after each refuelling"

        [
        ]
    , "A pilot who is running well behind schedule for the day, forgets to turn on the anti-icing system and consequently suffers severe airframe icing. This situation demonstrates:" ~>
      Multichoice
        [
        ]

        "A latent threat leading to handling error"

        [
          "A latent threat leading to a communication error"
        , "An internal threat leading to a handling error"
        , "An anticipated threat leading to a procedural error"
        ]
    , "You and your friends are planning a shooting trip in a hired aircraft that you pilot as a Private flight. Which of the following conditions apply to the carriage of firearms:" ~>
      Multichoice
        [
          "Unloaded firearms may be carried but the carriage of ammunition is prohibited by IATA (or OCAO) regulations"
        , "No special conditions apply if the firearms are licensed and you obtain permission from the aircraft owner or operator"
        ]

        "Specific written permission of CASA is required for the carriage of firearms and ammunition"

        [
          "Unloaded firearms and ammunition must be packed in accordance with IATA (or ICAO) technical instructions and carried in baggage compartment"
        ]
    , "One condition applying to the carriage of a stretcher patient in an aircraft is that:" ~>
      Multichoice
        [
          "A mercy flight must be declared"
        , "The patient must be accompanied by a nurse"
        ]

        "The stretcher must be secured to the aircraft"

        [
          "The aircraft must be an ambulance aircraft"
        ]
    , "In the vicinity of an aerodrome within an ADIZ an aircraft is intercepted and is directed to land at the aerodrome.  To show that this is understood and will be complied with the pilot of the intercepted aircraft should, by day:" ~>
      Multichoice
        [
        ]

        "Lower the aircraft’s landing gear and follow the intercepting aircraft"

        [
          "Rock the aircraft’s wings and follow the intercepting aircraft"
        , "Raise the aircraft landing gear while passing over the runway at a height of between 1000 and 2000 ft. and circle the aerodrome"
        , "Activate the aircraft’s transponder identification function"
        ]
    , "Which of the following passenger safety precautions must be taken while refuelling an aircraft with Avgas:" ~>
      Multichoice
        [
          "Passengers may remain on board but the a/c must be tied down"
        , "Passengers shall not embark or disembark"
        , "The cabin doors must be kept shut"
        ]

        "Passengers shall not remain on board"

        [
        ]
    , "Refer to Figure 3 (page 6, Work Booklet). Given the following details:\n* Runway = 05/23\n* Take-off distance available = 1300 metres\n* Surface = Long Wet Grass\n* Slope = Level\n* Pressure height = 3000 ft\n* Wind = 230/10 kts\n* Temperature = +15 Degrees C\n* Take-off Weight = 1055 kg\nThe Take-off distance required is closest to:" ~>
      Multichoice
        [
        ]

        "1030 metres"

        [
          "750 metres"
        , "1180 metres"
        , "830 metres"
        ]
    , "Refer to loading system CHARLIE (pages 14, 15 and 16, Work Booklet). Given:\n* Aircraft empty weight = 689 kg\n* Empty aircraft index units = 19522\n* Oil = 7 kg\n* Row 1 (pilot and passenger) = 110 kg\n* Baggage (baggage compartment) = 75 kg\n* Fuel (Litres) = 140 Lts\nThe maximum passenger weight, in kg, that may be carried in Row 2 at take-off is closest to:" ~>
      Multichoice
        [
        ]

        "110 kg"

        [
          "154 kg"
        , "135 kg"
        , "94 kg"
        ]
    , "Refer to loading system ALPHA (pages 10 and 11, Work Booklet). Given:\n* Basic index units = -200\n* Basic empty weight = 1015 kg\n* Row 1 (pilot and passenger) = 160 kg\n* Row (forward facing passengers) = 145 kg\n* Nose baggage compartment = 45 kg\n* Rear baggage compartment = 45 kg\nThe maximum weight of fuel, in kg, that may be carried at take-off is closest to:" ~>
      Multichoice
        [
          "223 kg"
        , "252 kg"
        ]

        "210 kg"

        [
        ]
    , "Given:\n* Pressure height = 6000 feet\n* OAT = +20 degrees C\n* CAS = 120 kts\nDetermine the TAS:" ~>
      Multichoice
        [
        ]

        "135 kts"

        [
          "125 kts"
        , "130 kts"
        , "106 kts"
        ]
    , "You plan to conduct a PVT VFR flight by day from TOWNSVILLE (QLD) (1915S 14646E) to SPRINGCREEK (QLD) (1838S 14433E) to INGHAM (QLD) (1840S 14609E). For the total flight, which ARFOR are required:" ~>
      Multichoice
        [
          "45"
        ]

        "44"

        [
          "40"
        , "43"
        ]
    , "Refer (TOWNSVILLE) WAC 3219 (page 27) Work Booklet). What is the position (Latitude and Longitude) of ROLLINGSTONE (approximately 25 nm NW of Townsville):" ~>
      Multichoice
        [
        ]

        "19 03S 146 23E"

        [
          "19 30S 146 52E"
        , "18 58S 146 23E"
        ]
    , "Refer (TOWNSVILLE) WAC 3219 (page 27, Work Booklet). At 2342 UTC you set course over ROLLINGSTONE (approximately 25 nm NW of TOWNSVILLE), to track direct to SPRING CREEK (1838S 14433E). Hdg 270 (M) is maintained until you pinpoint your position over Mt Lyall (south of Camel Creek Landing Area) at 0003 UTC. The HDG required to track direct to SPRING CREEK from this position is closest to:" ~>
      Multichoice
        [
          "248 degrees"
        ]

        "292 degrees"

        [
          "286 degrees"
        , "281 degrees"
        ]
    , "The beginning of daylight in UTC at FORREST WA (3050S 12807E) on the 7th February is closest to:" ~>
      Multichoice
        [
        ]

        "062035 UTC"

        [
          "062107 UTC"
        , "072035 UTC"
        , "070507 UTC"
        ]
    , "Refer (BOURKE) WAC 3356 (page 30, Work Booklet). You plan to fly from NARRABRI (YNBR) (3019S 14950E) direct to ST GEORGE (YSGE) 2803S 14836E). To avoid clouds after departing YNBR you divert left of planned TR along the YNBR – WALGETT railway line. At WEE WAA township (approximately 20 nm WNW YNBR) you decide to track direct to YSGE. The ARFOR wind for your cruise level is 100/20 kts, and your TAS is 140 kts. The HDG (M) to steer to YSGE is closest to:" ~>
      Multichoice
        [
          "330 degrees"
        , "322 degrees"
        , "348 degrees"
        ]

        "338 degrees"

        [
        ]
    , "The following message was issued during a pre-flight briefing: METAR YMLT 2300UTC 0001 25004KT 9999 SCT025 SCT035 18/12 Q 1020. The message is:" ~>
      Multichoice
        [
          "An aerodrome forecast"
        ]

        "A routine aerodrome meteorological report"

        [
          "An aerodrome report issued when conditions have deteriorated below specified limits"
        , "A trend type forecast"
        ]
    , "In the take-off configuration with the landing gear extended an aeroplane with a maximum take-off weight not exceeding 5700 kg shall have the ability to achieve a minimum climb gradient of:" ~>
      Multichoice
        [
          "4.5%"
        ]

        "6.0%"

        [
          "1.0%"
        , "3.2%"
        ]
    , "Soon after lifting off from the runway at the take-off safety speed an aeroplane experiences a sudden loss of both lift and airspeed. This could be caused by:" ~>
      Multichoice
        [
          "Climbing out of ground effect"
        ]

        "A sudden decrease in headwind component"

        [
          "A sudden increase in headwind component"
        , "Entering ground effect"
        ]
    , "Given the following: Beginning of daylight HAY (3432S 14450E) is 0615LMT; End of daylight AYERS ROCK (2515S 13059E) is 1715LMT; Planned landing time is 30 minutes before end of daylight. What is the approximate time available, for a VFR flight from HAY to AYERS ROCK:" ~>
      Multichoice
        [
        ]

        "11 hr 25 min"

        [
          "11 hr 55 min"
        , "12 hr 35 min"
        , "11 hr 00 min"
        ]
    , "Given: W/V = 250(M)/30 kts. Runways available = 03/21 and 09/27. Which runway has the greatest headwind component for landing:" ~>
      Multichoice
        [
          "09"
        , "21"
        , "03"
        ]

        "27"

        [
        ]
    , "This question relates to a VFR flight by day proceeding more than 50 nm from the departure aerodrome. The maximum permitted crosswind component in the aeroplane’s flight manual is 15 kt. Which of the following weather conditions forecast for the destination at the ETA would necessitate the provision of an alternate:" ~>
      Multichoice
        [
          "Visibility of 8 km"
        , "3/8 cloud with a 1400 ft ceiling"
        , "15 kt steady crosswind"
        ]

        "30% probability of visibility restricted to 7 km"

        [
        ]
    , "What is the approximate time available for a VFR flight on 5th January, from PORT HEDLAND (YPPD) WA (2023S 11838E) to CEDUNA (YCDU) SA (3208S 13343E) if: The beginning of daylight YPPD is 0500 LMT. The end of daylight YCUD is 1938 LMT. You plan to land YCUD 20 minutes before the end of daylight:" ~>
      Multichoice
        [
          "10 hr"
        , "14 hr 15 min"
        ]

        "13 hr 15 min"

        [
          "12 hr 45 min"
        ]
    , "Given: W/V = 050(M)/30 kts. What crosswind component would you expect for a landing on runway 35:" ~>
      Multichoice
        [
          "26 kt from the left"
        , "15 kt from the left"
        ]

        "26 kt from the right"

        [
          "15 kt from the right"
        ]
    , "Refer figure 31 (page 36, Work Booklet). Where \"D\" = 30 nm, Track error = 6 degrees. If a constant HDG has been maintained since 0300 (at \"A\") and the pilot alters HDG 9 degrees left at time 0318 (at \"B\"), what time should the pilot expect to intercept track:" ~>
      Multichoice
        [
        ]

        "0354"

        [
          "0412"
        , "0336"
        , "0327"
        ]
    , "Refer figure 34 (page 37, Work Booklet). Where “D1” = 60nm, “D2” = 20 nm. The aeroplane is estimated to arrive at “C” at 0215. It is on time at “A” but 3 minutes late at check point “X”. The revised estimate “C” is:" ~>
      Multichoice
        [
        ]

        "0219"

        [
          "0218"
        , "0221"
        , "0216"
        ]
    , "Refer: ARFOR/TAF 2 (page 46 Work Booklet). Depart YSWG 0200 – YSCB – arrive YMER 0350. The freezing level and visibility forecast for the YSWG-YSCB route segment of the flight is:" ~>
      Multichoice
        [
        ]

        "Freezing level 12000 ft, visibility 10 km or more reducing to 5000M in showers/rain and reducing to 3000M in thunderstorms"

        [
          "Freezing level 12000 ft, visibility 5000M reducing to 3000M in thunderstorms"
        , "Freezing level 9500 ft, visibility 10 km or more reducing to 5000M in showers/rain and reducing to 3000M in thunderstorms"
        ]
    , "Refer: ARFOR/TAF 4 (page 48 Work Booklet). Depart YSWG 0000 – YSCB – arrive YMER 0200. The forecast wind velocity for arrival in the circuit area YMER is:" ~>
      Multichoice
        [
          "260 Deg (T), 25 kts"
        , "290 Deg (M), 15 kts"
        ]

        "290 Deg (T), 15 kts"

        [
          "280 Deg (T), 10 kts"
        , "260 Deg (M), 25 kts"
        ]
    , "What effect does an increase in angle of bank have on stalling speed:" ~>
      Multichoice
        [
        ]

        "Stalling speed increases"

        [
          "Stalling speed decreases"
        , "Stalling speed remains the same"
        ]
    , "In a climb from level flight, the altimeter reading remains unchanged. A likely cause is a blocked:" ~>
      Multichoice
        [
          "Vacuum pump"
        ]

        "Static vent"

        [
          "Pitot tube"
        ]
    , "The stalling speed of an aeroplane is decreased by an increase in:" ~>
      Multichoice
        [
          "Headwind component"
        , "Angle of bank"
        , "Load factor"
        ]

        "Power"

        [
        ]
    , "After being distracted by difficult radio communications, you deviate from your assigned level in controlled airspace. A countermeasure which would most likely have avoided this situation would be called:" ~>
      Multichoice
        [
          "a planning countermeasure requiring the carriage of a second radio"
        ]

        "an execution countermeasure requiring you manage the workload"

        [
          "a review countermeasure that would have allowed you to ignore the height and concentrate on the radio"
        , "a procedural countermeasure that would have dealt with the radio problem"
        ]
    , "Refer to Fig 48 (page 44 Work Booklet). The pressure pattern about the point “C” (in western Queensland) is known as a:" ~>
      Multichoice
        [
        ]

        "Col"

        [
          "High"
        , "Ridge"
        , "Low"
        ]
    , "A pilot who resorts to assertiveness to address a problem in flight is employing:" ~>
      Multichoice
        [
          "An execution countermeasure"
        , "A planning countermeasure"
        , "A handling countermeasure"
        ]

        "A review countermeasure"

        [
        ]
    , "A characteristic of Rime ice, which may be hazardous to aircraft, is that it:" ~>
      Multichoice
        [
          "Is the most difficult type to remove"
        ]

        "May block the pitot tube"

        [
          "Is likely to lock the control surfaces"
        , "Spreads back over most of the aerofoil section"
        ]
    , "Refer to Fig 49 (page 44 Work Booklet) The line P – R (west of Townsville) indicates the position of a:" ~>
      Multichoice
        [
          "Ridge"
        , "Front"
        ]

        "Trough"

        [
          "Line of equal pressure"
        ]
    , "Flying conditions above a layer of small cumulus clouds are likely to be:" ~>
      Multichoice
        [
          "Smooth but with strong updraughts"
        ]

        "Smooth"

        [
          "Very Turbulent"
        , "Bumpy"
        ]
    , "Which of the following conditions would give the best indication of an abnormally low oil level:" ~>
      Multichoice
        [
          "High oil temperature and high oil pressure"
        ]

        "High oil temperature and low oil pressure"

        [
          "Low oil temperature and low oil pressure"
        , "Low oil temperature and high oil pressure"
        ]
    , "Select the variable which would reduce the indicated stalling speed of an aeroplane in level flight:" ~>
      Multichoice
        [
        ]

        "A reduction in gross weigh"

        [
          "An increase in air density"
        , "A reduction in power"
        , "An increase in altitude"
        ]
    , "Which of the following effects will result from moving the centre of gravity aft:" ~>
      Multichoice
        [
          "The stalling angle will increase"
        , "The indicated stalling speed will increase"
        , "Greater elevator deflection is required to achieve a given change in attitude"
        ]

        "Longitudinal stability will deteriorate"

        [
        ]
    , "At which IAS should a piston-engine aeroplane be flown to achieve maximum endurance in level flight:" ~>
      Multichoice
        [
        ]

        "The speed requiring minimum power"

        [
          "The speed requiring minimum thrust"
        , "The speed which gives the best lift-drag ratio"
        , "The lowest possible speed"
        ]
    , "One effect of fully lowering plain flaps is to:" ~>
      Multichoice
        [
          "Increase stalling angle"
        , "Increase the climb angle"
        , "Decrease the rate of descent"
        ]

        "Decrease the stall speed"

        [
        ]
    , "Which of the following indications would positively confirm that an aeroplane is established in a spin and not in a spiral dive:" ~>
      Multichoice
        [
          "Increasing airspeed"
        ]

        "Low airspeed"

        [
          "High rate of turn"
        , "Nose attitude"
        ]
    , "Which statement correctly describes the effect of an increase in weight on the maximum glide range in still air:" ~>
      Multichoice
        [
        ]

        "Range remains the same, if angle of attack is maintained"

        [
          "Range increases, if IAS is reduced"
        , "Range remains the same, if IAS is maintained"
        , "Range reduces, if angle of attack is maintained"
        ]
    , "Consider the turn performance of two aeroplanes with different gross weights. If they both execute turns at the same IAS, altitude, and bank, the heavier aeroplane will have:" ~>
      Multichoice
        [
          "A greater turn rate than the lighter aeroplane"
        ]

        "The same turn rate and radius as the lighter aeroplane"

        [
          "A greater turn radius than the lighter aeroplane"
        , "A lower turn rate and greater turn radius than the lighter aeroplane"
        ]
    , "Which of the following would indicate a sudden decrease in headwind component during a descent:" ~>
      Multichoice
        [
        ]

        "A decrease in indicated airspeed and an increase in rate of descent"

        [
          "An increase in indicated airspeed and decrease in rate of descent"
        , "An increase in both indicated airspeed and rate of descent"
        , "A decrease in both indicated airspeed and rate of descent"
        ]
    , "An aeroplane takes off in calm conditions and climbs through a wind shear where it encounters a strong tail wind. What initial effect will this change in W/V have on IAS and flight path:" ~>
      Multichoice
        [
          "IAS will decrease; flight path will be steeper"
        , "IAS will increase; flight path will be shallower"
        ]

        "IAS will decrease; flight path will be shallower"

        [
          "IAS will increase; flight path will be steeper"
        ]
    , "During a climb at maximum power, the IAS is progressively reduced from the maximum rate of climb speed to the stalling speed. The effect on the angle of climb is that it will:" ~>
      Multichoice
        [
          "Decrease then increase"
        ]

        "Increase then decrease"

        [
          "Decrease continually"
        , "Increase continually"
        ]
    , "An increase in which of the parameters listed below will always in an increase in Induced drag:" ~>
      Multichoice
        [
          "L/D ratio"
        ]

        "Angle of attack"

        [
          "Aspect ratio"
        , "IAS"
        ]
    , "When is turbulence associated with thunderstorm most severe:" ~>
      Multichoice
        [
        ]

        "During the mature stage"

        [
          "During the dissipating stage"
        , "During the growing or cumulus stage"
        , "Just before the mature stage"
        ]
    , "The cloud type which is indicative of mountain waves, when seen above a mountain range is:" ~>
      Multichoice
        [
          "Cirrus"
        ]

        "Lenticular"

        [
          "Stratus"
        , "Cumulus"
        ]
    , "Which hazard to flight is most likely in the rotor zone associated with mountain waves:" ~>
      Multichoice
        [
          "Icing"
        , "Line squall"
        ]

        "Severe turbulence"

        [
          "Hail"
        ]
    , "Squall lines are most often associated with:" ~>
      Multichoice
        [
          "Mountain waves"
        , "Heavy continuous rain"
        ]

        "Thunderstorms"

        [
          "Sea breezes"
        ]
    , "When mountain waves exist, rotor zones may occur and are most commonly found:" ~>
      Multichoice
        [
        ]

        "Beneath the crests of the waves"

        [
          "Just above the crests of the waves"
        , "In the troughs of the waves"
        , "Directly above the mountain range"
        ]
    , "The onset of a light wind during the formation of a radiation inversion will cause the inversion to:" ~>
      Multichoice
        [
        ]

        "Increase in depth"

        [
          "Become stronger"
        , "Decrease in depth"
        , "Dissipate"
        ]
    , "What is the major hazard to a landing aircraft associated with a microburst:" ~>
      Multichoice
        [
          "Reduced visibility"
        , "Heavy precipitation"
        , "Wind shear and updraughts"
        ]

        "Wind shear and down draughts"

        [
        ]
    , "Which of the following conditions will cause hypoxia:" ~>
      Multichoice
        [
          "When the ratio of helium to nitrogen pressure at high altitudes increases"
        , "When carbon dioxide levels in the lungs are lower than normal"
        ]

        "When the partial pressure of oxygen at high altitude decreases"

        [
          "When the carbon dioxide in the atmosphere is excessive"
        , "When nitrogen content of the air at high altitude decreases"
        ]
    , "Which of the following statements is true for many types of cold cures:" ~>
      Multichoice
        [
          "They may have side effects which strain the heart when flying at altitude"
        , "They may inflame the Eustachian tube, possibly leading to middle ear infections"
        ]

        "They contain anti-histamine, which may cause drowsiness and impair performance"

        [
          "They contain ephedrine, which may cause vision problems when flying at altitude"
        ]
    , "Which of the following would be an example of an unexpected environmental threat:" ~>
      Multichoice
        [
          "Take-off from an aerodrome with a high density altitude"
        , "Operation from an aerodrome with a high density altitude"
        , "Operating at an unfamiliar aerodrome"
        ]

        "An encounter with hoar frost on descent in clear air"

        [
        ]
    , "Responsibilities of Flight Service towards a SARTIME aircraft operating B050 would include the:" ~>
      Multichoice
        [
          "Provision of traffic separation"
        ]

        "Receipt of SARWATCH cancellation"

        [
          "Receipt of position reports"
        , "Provision of traffic information"
        ]
    , "A pilot who has failed to apply carburettor heat during a glide has committed:" ~>
      Multichoice
        [
        ]

        "Handling error"

        [
          "Procedural error"
        , "Communication error"
        , "Execution countermeasure"
        ]
    , "The cloud type most likely to be associated with heavy continuous rain is:" ~>
      Multichoice
        [
          "Stratus"
        , "Cumulonimbus"
        ]

        "Nimbostratus"

        [
          "Altostratus"
        ]
    , "You are planning a flight in a four (4) seater dual control aeroplane with the following passengers: 2 adults and 2 children aged 5 and 8 weighing 20 kg and 40 kg respectively. Assuming aircraft weight limitations will not be exceeded, which of the following limitations apply and why:" ~>
      Multichoice
        [
        ]

        "Four passengers may be carried because the children weigh less than 77 kg and can occupy one seat"

        [
          "Only two passengers may be carried because a passenger shall not occupy a control seat"
        , "Only three passengers may be carried because there are only four seats"
        ]
    , "Refer to Fig 48 (page 44 Work Booklet). The line J – K (just to the east of Melbourne) indicates the position of a:" ~>
      Multichoice
        [
          "Warm Front"
        , "Stationary Front"
        ]

        "Cold Front"

        [
          "Occluded Front"
        ]
    , "A pilot who has planned to cruise at 5500 feet is given an initial clearance to climb to 4500 feet in CTA. Because of poor radio reception, he reads back 5500 feet. This is an example of:" ~>
      Multichoice
        [
          "A procedural error"
        , "A handling error"
        ]

        "A communication error"

        [
          "A planning error"
        ]
    ]

curtisPplTrialExams ::
  [Exam]
curtisPplTrialExams =
  [
    curtisPplTrial4
  , curtisPplTrial5
  , curtisPplTrial6
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
  , taitPreRPL
  , taitPart13Meteorology
  , airborneAviationPreSolo
  , airborneAviationAreaSolo
  , airborneAviationMeteorology
  , frol
  , flightOneFROL
  , airspeedindicatorExam
  , airspeedsExam
  ] ++
  atcFROL ++
  [
    instruments
  ] ++
  bobTaitChapterRevision ++
  [ 
    c172Rairspeeds
  , c172Sairspeeds
  , form61_1486_1495
  ] ++
  curtisPplTrialExams

main ::
  IO ()
main =
  do a <- getArgs
     let action = case a of
                    [] -> markdownExams
                    (_:_) -> flashcardExams
     putStrLn (action exams)   
