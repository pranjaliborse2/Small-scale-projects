rm(list=ls())
source("lab5_sample.R")
# Make a plot.

sl.rmse <- function(params, time, sea.levels) { # don't forget the curly braces
  # Step 1: Pick apart the vector params into its individual values.
  a <- params[1]
  b <- params[2]
  c <- params[3]
  t.0 <- params[4]
  sl <- a*((time-t.0)^2)+b*(time-t.0)+c
  rmse= sqrt(mean((sea.levels-sl)^2))
  return(rmse)
}
test.rmse <- sl.rmse(params = c(0, 0, -100, 1800), time = t,
                     sea.levels = obs.sl)
print(test.rmse)
