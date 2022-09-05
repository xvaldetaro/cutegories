module Platform.Util.Similarity where

import Effect (Effect)

-- | Returns the Ratings for all strings sorted by most similar to least similar with.
-- | EXCLUDES SELF FROM ARRAY OF STRINGS IF SELF IS THERE!
foreign import findBestMatch :: String -> Array String -> Ratings

type Rating = { target :: String, rating :: Number}
type Ratings = Array Rating

foreign import randomLetter :: Effect String
