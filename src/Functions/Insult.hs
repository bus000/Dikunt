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
        [first, "insult", _] -> return $ first == (nick ++ ":")
        _ -> return False
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) = case words msg of
    [_, _, nick] -> let myinsult = liftIO $ sample (choice insults)
        in myinsult >>= \i -> BT.privmsgs $ nick ++ " " ++ i
    _ -> BT.privmsgs "Parse error"
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
    , "Roses are red violets are blue, God made me pretty, what happened to you?"
    , "If you really want to know about mistakes, you should ask your parents."
    , "Hey, you have somthing on your chin... no, the 3rd one down"
    , "I have neither the time nor the crayons to explain this to you."
    , "You're so ugly you scare the shit back into people."
    , "What are you going to do for a face when the baboon wants his butt back?"
    , "How many times do I have to flush to get rid of you?"
    , "I'd slap you, but shit stains."
    , "If I gave you a penny for your thoughts, I'd get change."
    , "You're the reason the gene pool needs a lifeguard."
    , "You're so fat the only letters of the alphabet you know are KFC."
    , "I may love to shop but I'm not buying your bullshit."
    , "Why don't you slip into something more comfortable -- like a coma."
    , "You have two brains cells, one is lost and the other is out looking for it."
    , "If I were to slap you, it would be considered animal abuse!"
    , "Oh my God, look at you. Was anyone else hurt in the accident?"
    , "Well I could agree with you, but then we'd both be wrong."
    , "You're not funny, but your life, now that's a joke."
    , "It looks like your face caught on fire and someone tried to put it out with a hammer."
    , "Don't feel sad, don't feel blue, Frankenstein was ugly too."
    , "The last time I saw a face like yours I fed it a banana."
    , "Do you know how long it takes for your mother to take a crap? Nine months."
    , "What are you doing here? Did someone leave your cage open?"
    , "Do you still love nature, despite what it did to you?"
    , "I'll never forget the first time we met, although I'll keep trying."
    , "You're so fat, you could sell shade."
    , "Why don't you check eBay and see if they have a life for sale."
    , "You are proof that God has a sense of humor."
    , "I'd like to kick you in the teeth, but that would be an improvement!"
    , "You're so ugly, when you got robbed, the robbers made you wear their masks."
    , "You're so ugly, the only dates you get are on a calendar."
    , "There are more calories in your stomach than in the local supermarket!"
    , "You're as useless as a knitted condom."
    , "If you spoke your mind, you'd be speechless."
    , "You're so fat you need cheat codes to play Wii Fit"
    , "You look like something I'd draw with my left hand."
    , "You didn't fall out of the stupid tree. You were dragged through dumbass forest."
    , "So you've changed your mind, does this one work any better?"
    , "You're so ugly, when you threw a boomerang it didn't come back."
    , "You're as bright as a black hole, and twice as dense."
    , "If I wanted to hear from an asshole, I'd fart."
    , "You're so ugly, you scared the crap out of the toilet."
    , "Shock me, say something intelligent."
    , "If your brain was made of chocolate, it wouldn't fill an M&M."
    , "I can explain it to you, but I can�t understand it for you."
    , "It's kinda sad watching you attempt to fit your entire vocabulary into a sentence."
    , "I fart to make you smell better."
    , "You are proof that evolution CAN go in reverse."
    , "Looks like you traded in your neck for an extra chin!"
    , "You do realize makeup isn't going to fix your stupidity?"
    , "You're so ugly you make blind kids cry."
    , "I love what you've done with your hair. How do you get it to come out of the nostrils like that?"
    , "You're a person of rare intelligence. It's rare when you show any."
    , "You're the best at all you do - and all you do is make people hate you."
    , "You're so fat, when you wear a yellow rain coat people scream 'taxi''."
    , "I heard your parents took you to a dog show and you won."
    , "Some drink from the fountain of knowledge; you only gargled."
    , "Your parents hated you so much your bath toys were an iron and a toaster"
    , "Learn from your parents' mistakes - use birth control!"
    , "Your face makes onions cry."
    , "I thought you were attractive, but then you opened your mouth."
    , "Your hockey team made you goalie so you'd have to wear a mask."
    , "Which sexual position produces the ugliest children? Ask your mother."
    , "Why dont you shut up and give that hole in your face a chance to heal."
    , "If my dog had your face, I would shave his butt and make him walk backwards."
    , "You're so stupid you tried to wake a sleeping bag."
    , "You stare at frozen juice cans because they say, 'concentrate'."
    , "I don't know what makes you so stupid, but it really works!"
    , "Ever since I saw you in your family tree, I've wanted to cut it down."
    , "I heard you went to a haunted house and they offered you a job."
    , "Aww, it's so cute when you try to talk about things you don't understand."
    , "When was the last time you could see your whole body in the mirror?"
    , "If brains were dynamite you wouldn't have enough to blow your nose."
    , "With a face like yours, I'd wish I was blind."
    , "I'm no proctologist, but I know as asshole when I see one."
    , "Just wait till you can't fit your hand in the Pringles tubes, then where will you get your daily nutrition from?"
    , "If assholes could fly, this place would be an airport!"
    , "You so ugly when who were born the doctor threw you out the window and the window threw you back!"
    , "Looks aren't everything; in your case, they aren't anything"
    , "You're so ugly, you had tinted windows on your incubator."
    , "You get as much action as a nine button on a microwave."
    , "We all sprang from apes, but you didn't spring far enough."
    , "It's better to keep your mouth shut and give the 'impression' that you're stupid than to open it and remove all doubt."
    , "So, a thought crossed your mind? Must have been a long and lonely journey."
    , "Am I getting smart with you? How would you know?"
    , "You only annoy me when you're breathing."
    , "You're so ugly Hello Kitty said goodbye to you."
    , "You have enough fat to make another human."
    , "You fear success, but really have nothing to worry about."
    , "You're so dumb, your dog teaches you tricks."
    ]
