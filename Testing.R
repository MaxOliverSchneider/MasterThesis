#Testing
print("Hello Test")

L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
d <- data.frame(x = 1, y = 1:10, fac = fac)


write.csv(d, "/home/ubuntu/Rprojects/firstTest.csv", row.names = TRUE)

print("printing successful?")
