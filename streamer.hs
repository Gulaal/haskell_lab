-- Задание 1

streamer :: (String, Int, Int, Int) -> ((String, Int, Int, Int) -> a) -> a
streamer (nick, en, subs, hrs) = \msg -> msg (nick, en, subs, hrs)

getNickname s = s (\ (n, _, _, _) -> n)

getEnergy s = s (\ (_, e, _, _) -> e)

getSubscribers s = s (\ (_, _, sbs, _) -> sbs)

getTotalHours s = s (\ (_, _, _, hrs) -> hrs)

energyPerHour :: Int
energyPerHour = 10

subsPerHour :: Int
subsPerHour = 5

stream s n = s (\ (nick, en, subs, hrs) ->
    let maxHours = en `div` energyPerHour
        actualHours = min n maxHours
        newEnergy = max 0 (en - actualHours * energyPerHour)
        newSubs = subs + actualHours * subsPerHour
        newTotal = hrs + actualHours
    in streamer (nick, newEnergy, newSubs, newTotal)
    )

rest s = s (\ (nick, en, subs, hrs) -> streamer (nick, 100, subs, hrs))

showInfo s = s (\ (nick, en, subs, hrs) ->
    "Стример: " ++ nick ++ "\n" ++
    "  Энергия: " ++ show en ++ "/100\n" ++
    "  Подписчики: " ++ show subs ++ "\n" ++
    "  Всего часов стримов: " ++ show hrs
    )
    