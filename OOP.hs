-- DESIGN FIGHT ROBOTS!!!

-- constructor
robot (name, attack, hp) = \message -> message (name, attack, hp)


-- accessors

name (n, _, _ ) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

	-- -- getters
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

	-- -- setters
setName aRobot newName = aRobot (\(n, a, h ) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h ) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h ) -> robot (n, a, newHP))


printRobot aRobot = aRobot (\(n, a, h) ->
  n ++ " attack:" ++ (show a) ++ " health:" ++ (show h))


-- methods

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

-- state after hit
afterHit = damage killerRobot 90

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0
-- instaces
killerRobot = robot ("killer", 25, 200)
gentleGiant = robot ("Mister Gentle", 10, 300)

