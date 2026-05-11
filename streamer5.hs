-- Задание 5

import Data.List (maximumBy)
import Data.Ord (comparing)

data Platform = Twitch | YouTube | VKPlay | Kick | TikTok | Trovo
    deriving (Show, Eq)
    
type Nickname    = String
type Energy      = Int
type Subscribers = Int
type TotalHours  = Int
type Charisma    = Int
type StreamerData = (Nickname, Energy, Subscribers, TotalHours, Charisma, Platform)

data Streamer = Streamer 
    { 
        nick      :: Nickname,
        en        :: Energy,
        subs      :: Subscribers,
        hrs       :: TotalHours,
        char      :: Charisma,
        plat      :: Platform,
    } deriving (Show)
    
energyPerHour :: Energy
energyPerHour = 10

stream :: Streamer -> Int -> Streamer
stream s n = 
    let maxHours    = en s `div` energyPerHour
        actualHours = min n maxHours
    in s {
          en   = max 0 (en s - actualHours * energyPerHour),
          subs = subs s + actualHours * char s,
          hrs  = hrs s + actualHours 
        }

rest :: Streamer -> Streamer
rest s = s { en = 100 }

playRound :: Streamer -> Streamer
playRound s = if en s >= energyPerHour then stream s 1 else rest s

showInfo :: Streamer -> String
showInfo s = 
    "Стример: " ++ nick s ++ "\n" ++ 
    " Энергия: " ++ show (en s) ++ "/100\n" ++ 
    " Подписчики: " ++ show (subs s) ++ "\n" ++
    " Всего часов: " ++ show (hrs s)++ "\n" ++
    " Харизма: " ++ show (char s) ++ "\n" ++
    " Платформа: " ++ show (plat s)

marathon :: [Streamer] -> Int -> [Streamer]
marathon streamers rounds = iterate (map playRound) streamers !! rounds