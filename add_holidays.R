# read in training data
flight.data.tr = read.csv('dep2015.csv')

# read in test data
flight.data.te = read.csv('dep2016_visible.csv')

# November and December get a score of 2
flight.data.tr$HOLIDAY = ifelse(flight.data.tr$MONTH == 11 | flight.data.tr$MONTH == 12, 2, 0)
flight.data.te$HOLIDAY = ifelse(flight.data.te$MONTH == 11 | flight.data.te$MONTH == 12, 2, 0)

# Weekends get a score of 1
flight.data.tr$HOLIDAY = ifelse(flight.data.tr$DAY_OF_WEEK == 6 | flight.data.tr$DAY_OF_WEEK == 7, 1, 0)
flight.data.te$HOLIDAY = ifelse(flight.data.te$DAY_OF_WEEK == 6 | flight.data.te$DAY_OF_WEEK == 7, 1, 0)

write.csv(flight.data.tr, 'dep2015_holidays.csv')
write.csv(flight.data.te, 'dep2016_holidays.csv')