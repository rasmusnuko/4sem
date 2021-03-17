#### Exercise 6-6 ####
vec <- c(1, -3, 7, -5, 10)
mean <- mean(vec)
maxValue <- max(vec)
min <- min(vec)

print("====EXERCISE 6-6B====")
print("Vector:")
print(vec)
print('Mean value:')
print(mean)
print('Max value:')
print(maxValue)
print('Min value:')
print(min)

print("")
print("====EXERCISE 6-6C====")
vec2 <- vec[1:2]
print("Getting subset of vector")
print(vec2)
vec2[3] = 42
print("Inserting 42 as third index")
print(vec2)


print("")
print("====EXERCISE 6-6D====")
newVec = c(5, 3, 2, 1, 9)
print("New random vector")
print(newVec)
print("Sum of vectors")
print(vec + newVec)


print("")
print("====EXERCISE 6-6E====")
randVec = rnorm(10)
print("Random vector")
print(randVec, digits=3)
print("Mean of random vector")
print(mean(randVec))
print("Last five indecies of random vector")
print(randVec[6:10], digits=3)
