install.packages("tlp")
library(tlp)

# create a data frame with some data
df <- data.frame(
  Name = c("John", "Sarah", "Mike", "Karen"),
  Score = c(85, 95, 75, 80)
)

# create a TLP with red, yellow, and green lights based on the scores
my_tlp <- create_tlp(df$Score, limits = c(70, 80), colors = c("red", "yellow", "green"))

# add the names to the TLP
add_labels(my_tlp, df$Name)

# plot the TLP
plot(my_tlp)