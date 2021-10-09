module MoodSwing where
import Text.Show (Show)
data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood Woot = Blah
