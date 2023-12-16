library(ggplot2)

roll <- function(die) {
  s <- sample(die, 2, replace = TRUE)
  sum(s)
}

die <- 1:10
x <- roll(die)

print(x)

rolls <- replicate(10000, roll(die))
qplot(rolls, binwidth = 1)
