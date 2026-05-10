import Data.List (maximumBy)
import Data.Ord (comparing)

type Nickname    = String
type Energy      = Int
type Subscribers = Int
type TotalHours  = Int
type Charisma    = Int

type StreamerData = (Nickname, Energy, Subscribers, TotalHours, Charisma)

newtype Streamer = Streamer { run :: forall a. (StreamerData -> a) -> a }

streamer :: StreamerData -> Streamer
streamer (nick, en, subs, hrs, char) = Streamer (\msg -> msg (nick, en, subs, hrs, char))

energyPerHour :: Energy
energyPerHour = 10

getSubs :: Streamer -> Subscribers
getSubs s = run s (\(_, _, sbs, _, _) -> sbs)

stream :: Streamer -> Int -> Streamer
stream s n = run s (\(nick, en, subs, hrs, char) -> 
    let maxHours    = en `div` energyPerHour
        actualHours = min n maxHours
        newEnergy   = max 0 (en - actualHours * energyPerHour)
        newSubs     = subs + actualHours * char
        newTotal    = hrs + actualHours
    in streamer (nick, newEnergy, newSubs, newTotal, char))

rest :: Streamer -> Streamer
rest s = run s (\(nick, en, subs, hrs, char) -> 
    streamer (nick, 100, subs, hrs, char))

playRound :: Streamer -> Streamer
playRound s = run s (\(_, en, _, _, _) -> 
    if en >= energyPerHour then stream s 1 else rest s)

showInfo :: Streamer -> String
showInfo s = run s (\(nick, en, subs, hrs, char) -> 
    "Стример: " ++ nick ++ "\n" ++ 
    " Энергия: " ++ show en ++ "/100\n" ++ 
    " Подписчики: " ++ show subs ++ "\n" ++
    " Всего часов: " ++ show hrs ++ "\n" ++
    " Харизма: " ++ show char ++ "\n")

marathon :: [Streamer] -> Int -> [Streamer]
marathon streamers rounds = iterate (map playRound) streamers !! rounds

main :: IO ()
main = do
    let s1 = streamer ("s1", 100, 0, 0, 7)
    let s2 = streamer ("s2", 20, 0, 10, 5)
    let s3 = streamer ("s3", 50, 0, 10, 8)
    let results = marathon [s1, s2, s3] 7
    let winner = maximumBy (comparing getSubs) results
    putStrLn $ "Победитель: " ++ run winner (\(n, _, _, _, _) -> n)
