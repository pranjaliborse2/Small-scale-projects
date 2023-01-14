require("animation")
n.balls <- 10000
n.rows <- 20
x <- c()
for (i in 1:n.balls) {
  path <- sample(x=c(-0.5,0.5),size=(n.rows-1),replace=TRUE)
  bin <- sum(path)
  x<- c(x,bin)
}
hist(x)