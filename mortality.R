outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Coerce character values to numeric
outcome[, 11] <- as.numeric(outcome[, 11])

# Plot
hist(outcome[, 11])