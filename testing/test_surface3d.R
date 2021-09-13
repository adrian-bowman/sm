#     Checks on the sm.surface3d function

library(sm)
if (reinstall) devtools::install("sm")

test_label("Trawl data (example)", test.prompt)
with(trawl, {
   Zone93    <- (Year == 1 & Zone == 1)
   Position  <- cbind(Longitude - 143, Latitude)
   model1 <- sm.regression(Position[Zone93,], Score1[Zone93],
                           h= c(0.1, 0.1), display = "rgl", xlab="Longitude - 143")
   model2 <- sm.regression(Position[Zone93,], Score1[Zone93],
                           h= c(0.2, 0.2), display = "none")
   sm.surface3d(model2$eval.points, model2$est, model1$scaling, col = "red")
})
