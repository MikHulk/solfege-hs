module Solfege
  ( NoteName(..)
  , Accidental(..)
  , Alteration(..)
  , fifthCycle
  , noteCycle
  , raise
  , Pitch(..)
  )
where

data NoteName
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Show, Read, Eq)

data Accidental
  = Flat
  | Sharp
  -- | DoubleFlat
  -- | DoubleSharp
  -- | HalfFlat
  -- | HalfSharp

instance Show Accidental where
  show Sharp = "♯"
  show Flat = "♭"
  -- show DoubleSharp = "𝄪"
  -- show HalfSharp = "𝄲"
  -- show DoubleFlat = "𝄫"
  -- show HalfFlat = "𝄳"

data Alteration
  = Tone
  | Semitone
  -- | Quartertone

data Pitch
  = Pitch NoteName (Maybe Accidental)

instance Show Pitch where
  show (Pitch note optAcc) =
    case optAcc of
      (Just alt) -> (show note) ++ (show alt)
      Nothing -> show note

fifthCycle :: [NoteName]
fifthCycle = cycle [ F, C, G, D, A, E, B ]

noteCycle :: [NoteName]
noteCycle = cycle [C, D, E, F, G, A, B ]

raise :: Alteration -> Pitch -> Pitch
raise Semitone (Pitch note (Just Flat)) =
  Pitch note Nothing
raise Semitone (Pitch B optAcc) =
  Pitch C optAcc
raise Semitone (Pitch E optAcc) =
  Pitch F optAcc
raise Semitone (Pitch note Nothing) =
  Pitch note (Just Sharp)
raise Semitone (Pitch note (Just Sharp)) =
  Pitch nextNote Nothing
  where nextNote = head $ drop 1 $ dropWhile ((/=) note) noteCycle
  
raise Tone (Pitch B (Just Flat)) =
  Pitch C Nothing
raise Tone (Pitch E (Just Flat)) =
  Pitch F Nothing
raise Tone (Pitch B (Just Sharp)) =
  raise Tone $ Pitch C Nothing
raise Tone (Pitch E (Just Sharp)) =
  raise Tone $ Pitch F Nothing
raise Tone (Pitch B Nothing) =
  Pitch C (Just Sharp)
raise Tone (Pitch E Nothing) =
  Pitch F (Just Sharp)
raise Tone (Pitch A (Just Sharp)) =
  Pitch B Nothing
raise Tone (Pitch D (Just Sharp)) =
  Pitch F Nothing
raise Tone (Pitch note optAcc) =
  Pitch nextNote optAcc
  where nextNote = head $ drop 1 $ dropWhile ((/=) note) noteCycle
-- raise Quartertone (Pitch note Nothing) =
--   Pitch note (Just HalfSharp)
-- raise Quartertone (Pitch B (Just HalfSharp) =
--   Pitch C Nothing
-- raise Quartertone (Pitch E (Just HalfSharp) =
--   Pitch F Nothing
-- raise Quartertone (Pitch note (Just HalfSharp) =
--   Pitch note (Just Sharp)
