# Quizz week 3

library(datasets)
data(iris)
head(iris)
?iris

# Q1
tapply(iris$Sepal.Length, iris$Species, mean)
round(tapply(iris$Sepal.Length, iris$Species, mean), 0) # rounded whole number

# Q2
apply(iris[, 1:4], 2, mean)

# Q3
data(mtcars)
head(mtcars)
?mtcars

tapply(mtcars$mpg, mtcars$cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)

# Q4
avg_hp <- tapply(mtcars$hp, mtcars$cyl, mean)
abs(avg_hp["8"] - avg_hp["4"])
round(abs(avg_hp["8"] - avg_hp["4"]), 0) # rounded
