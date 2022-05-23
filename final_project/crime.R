setwd("C:/Users/Antonio/Desktop/650W/final_project") 
crime <- read.csv("NYPD_Complaint_Data_Historic.csv", stringsAsFactors = FALSE)


crime_dat <- data.table(crime)


crime_dat2 <- crime_dat[, .(RPT_DT, LAW_CAT_CD, BORO_NM, Latitude, Longitude, ADDR_PCT_CD)]
crime_dat2$RPT_DT <- as.numeric(as.Date(crime_dat2$RPT_DT, "%m/%d/%Y"))
# if we need to convert back to date objects use as.Date(housing_data$date_of_sale,"1970-01-01")
crime_dat2 <- crime_dat2[BORO_NM == 'QUEENS' & 16830 < RPT_DT & RPT_DT<17199]

#Number of reported crimes by precinct in Queens 2016-2017
crime_dat3 <- crime_dat2[,.(num_crimes=.N),by=ADDR_PCT_CD]
colnames(crime_dat3)[1] <- 'prec_num'
#save to file for future use
write.csv(crime_dat3,file="crime_qns.csv")
