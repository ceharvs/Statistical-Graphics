library(ggplot2)
library(gridExtra)
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p2 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p2 <- p2 + facet_grid(cyl ~ .)
grid.arrange(p1, p2, ncol=1)

For this I need the x axes of the top and bottom graphs to line up, however because of the strip to the left, the faceted graph is narrower than the top graph.
I can make the strip invisible using: