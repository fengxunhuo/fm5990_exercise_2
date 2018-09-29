setwd("D:/Documents/University Life/Graduate/Year1/FM5990/R/Third Lecture")

# Load the library that we need
library(tidyverse)
# Load the data we need
df_occ <- read_csv("data_occ_201808.csv")
df_etfs <- read_csv("data_etf_list.csv")

# create a monthly volume report
df_monthly_volume <- df_occ %>% group_by(underlying) %>% 
                     summarise(monthly_volumn = sum(quantity))

# SPY vs SPX : SPX is an index, whereas SPY is an ETF. That means that SPX value is determined directly by
# the value of the underlying stocks comprising the S&P 500. 
# SPX and SPY will be close in value and move in tandem, but not exactly.

# VIX vs VXX : VIX is a popular measure of the stock market's expectation of volatility implied by S&P 500 index options.
# The VXX is an ETN designed to track VIX futures. Investing in VXX is essentially equivalent to exposure to 
# daily rolling long position in the first and second month VIX futures contracts.

# Those volatility-related funds has character "volatility" in the segment column.

# Convert all uppercase letters in the segment column to all lowercase letters.
df_etfs$segment <- base::tolower(df_etfs$segment)

# Isolate volatility etfs into a dataframe called df_vol_etfs.
df_vol_etfs <- filter(df_etfs, str_detect(df_etfs$segment, "volatility"))

# Create a list of top 100 most liquid non-volatility ETFs.
df_top_100 <- (df_etfs %>% filter( !(symbol %in% df_vol_etfs$symbol)) %>% arrange(spread))[1:100,]

# The length of distinct segments
nrow(distinct(df_top_100, segment))

# Drop the " % " in the expense_ratio column
df_top_100$expense_ratio = substr(df_top_100$expense_ratio,1,4)

# Transfer the expense_ratio into numeric tyoe and then group by segment and calculate all the expense_ratio of each segment
df_etf_expenseratio <- df_top_100 %>% group_by(segment) %>% 
                       summarise(all_expense_percent = sum(as.numeric(expense_ratio))*0.01)






