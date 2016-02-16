module Main where

import Control.Monad.State
import Data.List
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
  l ++ x : r

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
                   PrePart61 -> "~~" ++ q ++ "~~"
        in show n ++ ". " ++ q' ++ "\n" ++
           (intercalate "\n" . zipWith (\n c -> "  " ++ show n ++ ". " ++ c) [1..] . choiceList . onFocus (\a -> "**" ++ a ++ "**") $ m)
  in intercalate "\n"
      [
        "# " ++ t1 ++ "\n" ++ 
        maybe "" (\t -> "## " ++ t ++ "\n\n") t2 ++ 
        maybe "" (\t -> "#### " ++ t ++ "\n\n") t3
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
                   PrePart61 -> "~~" ++ q ++ "~~"
        in replicate 40 '-' ++
           "\n\n" ++              
           q' ++
           "\n" ++
           let z = number m
               display (c, s) = c : ". " ++ s
           in (intercalate "\n" . choiceList . fmap display $ z) ++
              "\n\n" ++
              display (focus z) ++
              "\n\n"
  in intercalate "\n"
      [
        "# " ++ t1 ++ "\n" ++ 
        maybe "" (\t -> "## " ++ t ++ "\n\n") t2 ++ 
        maybe "" (\t -> "#### " ++ t ++ "\n\n") t3
      , intercalate "\n" (fmap flashcardQuestion (zip [1..] qs))
      ]

flashcardExams ::
  [Exam]
  -> String
flashcardExams =
  intercalate ("\n\n" ++ replicate 40 '=' ++ "\n\n") . fmap flashcardExam

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
    , "During a climbing turn you mustbe careful of:" ~>
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
  Exam
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
        , "1,000ft onthe area QNH."
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
    -- todo
    ]

exams ::
  [Exam]
exams =
  [
    atcPreCircuit
  , atcPreArea
  , atcBAK
  ]

main ::
  IO ()
main =
  do a <- getArgs
     let action = case a of
                    [] -> markdownExams
                    (_:_) -> flashcardExams
     putStrLn (action exams)   
