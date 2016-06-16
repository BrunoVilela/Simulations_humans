
system.time(test1 <- BuildWorld(10, 0.5))
system.time(test2 <- BuildWorld2(10, 0.5))

test1 == test2


