module Functions.Insult
    ( insult
    ) where

import Control.Monad.State (liftIO)
import qualified BotTypes as BT
import Data.Random (sample)
import Data.Random.Extras (choice)

insult :: BT.BotFunction
insult = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: insult <usernick> - Will send an insult to usernick"
    , BT.name = "Insulter"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        (first:"insult":_:[]) -> return $ first == (nick ++ ":")
        _ -> return False
shouldRun _ = return False

run :: BT.Message -> BT.Net String
run (BT.PrivMsg _ _ msg) = case words msg of
    (_:_:nick:[]) -> let myinsult = liftIO $ sample (choice insults)
        in myinsult >>= \i -> return $ nick ++ " " ++ i
    _ -> return "Parse error"
run _ = fail "run should only run on PrivMsg's"

{- Insults come from http://www.gotlines.com/insults/ -}
insults :: [String]
insults =
    [ "Is your ass jealous of the amount of shit that just came out of your mouth?"
    , "I'm not saying I hate you, but I would unplug your life support to charge my phone."
    , "Roses are red, violets are blue, I have 5 fingers, the 3rd ones for you."
    , "I wasn't born with enough middle fingers to let you know how I feel about you."
    , "I bet your brain feels as good as new, seeing that you never use it."
    , "Your birth certificate is an apology letter from the condom factory."
    , "Yo're so ugly, when your mom dropped you off at school she got a fine for littering."
    , "You must have been born on a highway because that's where most accidents happen."
    , "If you are going to be two faced, at least make one of them pretty."
    , "What's the difference between you and eggs? Eggs get laid and you don't."
    , "I'm jealous of all the people that haven't met you!"
    , "If laughter is the best medicine, your face must be curing the world."
    , "You bring everyone a lot of joy, when you leave the room."
    , "I'd like to see things from your point of view but I can't seem to get my head that far up my ass."
    , "I could eat a bowl of alphabet soup and shit out a smarter statement than that."
    , "You're the reason they invented double doors!"
    , "If I wanted to kill myself I'd climb your ego and jump to your IQ."
    , "Two wrongs don't make a right, take your parents as an example."
    , "You shouldn't play hide and seek, no one would look for you."
    , "Your family tree must be a cactus because everybody on it is a prick."
    , "If you're gonna be a smartass, first you have to be smart. Otherwise you're just an ass."
    , "There's only one problem with your face, I can see it."
    , "You're so ugly, when you popped out the doctor said 'Aww what a treasure' and your mom said 'Yeah, lets bury it.'"
    , "It's better to let someone think you are an Idiot than to open your mouth and prove it."
    , "I don't exactly hate you, but if you were on fire and I had water, I'd drink it."
    , "Maybe if you ate some of that makeup you could be pretty on the inside."
    , "Somewhere out there is a tree, tirelessly producing oxygen so you can breathe. I think you owe it an apology."
    , "At least when I do a handstand my stomach doesn't hit me in the face."
    , "Shut up, you'll never be the man your mother is."
    , "The only way you'll ever get laid is if you crawl up a chicken's ass and wait."
    ]
