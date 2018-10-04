data Eon = Hadean | Archean | Proterozoic | Phanerozoic deriving (Eq, Ord, Enum)
instance Show Eon where
  show (Hadean) = "Hadean (4.6 billion to 4.0 billion years ago)"
  show (Archean) = "Archean (4.0 billion to 2.5 billion years ago)"
  show (Proterozoic) = "Proterozoic (2.5 billion to 541 million years ago)"
  show (Phanerozoic) = "Phanerozoic (541 million years ago to present)"
yearToEon :: Int -> Eon
yearToEon value
  | value > 2017 || value < -4600000000 = error "wrong value"
  | value >= -4600000000 || value < -4000000000 = Hadean
  | value >= -4000000000 || value < -2500000000 = Archean
  | value >= -2500000000 || value < -541000000  = Proterozoic
  | value >= -541000000 || value <= 2017        = Phanerozoic
