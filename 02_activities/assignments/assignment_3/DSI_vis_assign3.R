library(tidyr)
library(dplyr)

# Read in the data of Toronto Island Ferry Ticket Counts
# https://open.toronto.ca/dataset/toronto-island-ferry-ticket-counts/
# This dataset provides ferry ticket sales and ticket redemptions.
ferry <- read.csv("Toronto_Island_Ferry_Ticket_Counts.csv",
                  header = TRUE,
                  sep = ",")

head(ferry)

# Seperate the column "Timestamp" into six columns
# "year", "month", "day", "hour", "min", "sec"
ferry_sep <- ferry %>%
  separate(Timestamp, into = c("year", "month", "day.hour", "min", "sec"))

ferry_sep <- ferry_sep %>%
  separate(day.hour, into = c("day", "hour"), sep = "T")

# Sum of Redemption.Count of each month
Redemption.Count.month <- aggregate(Redemption.Count ~ month,
                                    data = ferry_sep, FUN = sum)

# Sum of Sales.Count of each month
Sales.Count.month <- aggregate(Sales.Count ~ month,
                                    data = ferry_sep, FUN = sum)

# Create a pie chart showing the Redemption.Count of each month
pie.Redemption.month <- pie(
  Redemption.Count.month$Redemption.Count,
  labels = Redemption.Count.month$month,
  main = "Monthly ticket redemptions of Toronto Island Ferry",
  col = rainbow(12)
)

# Create a pie chart showing the Sales.Count of each month
pie.Sales.month <- pie(
  Sales.Count.month$Sales.Count,
  labels = Sales.Count.month$month,
  main = "Monthly ticket sales of Toronto Island Ferry",
  col = rainbow(12)
)
