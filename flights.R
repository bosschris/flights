#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα
#load dataset
setwd("?????PATH????")
flights<-read.csv('DelayedFlights.csv')
#Προσθήκη στήλης ημερομηνίας YYYYMMDD
flights$date<-flights$DayofMonth+flights$Month*100+flights$Year*10000

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
count_NAs<-nrow(flights)-nrow(flights[rowSums(is.na(flights)) != ncol(flights),]);
print(paste0("The count of rows that are empty in all columns is: ",count_NAs))
#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
#create vector with date (YY-MM-DD) and arrival delay - then find the date with max frequency
df_2<-flights[,c(2,3,4,16,31)]
df_2<-df_2[df_2$ArrDelay>0, ]
df_2<-table(df_2$date)
day_with_most_delays<-names(df_2[df_2==max(df_2)])
print(paste0("The day with most delayed flights is: ", substr(as.character(day_with_most_delays),7,8)," and the month is ",substr(as.character(day_with_most_delays),5,6), " of year :",substr(as.character(day_with_most_delays),1,4)))
#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
df_3<-flights[,c(2:4,16,31)]
df_3<-df_3[df_3$ArrDelay>0, ]
df_3<-df_3[df_3$Year==2008,]
df_3<-df_3[(df_3$Month>5),]
df_3<-df_3[(df_3$Month<9),]
#εύρεση μέσων καθυστερήσεων για κάθε ημέρα
df_3_agg<-aggregate(cbind(ArrDelay)~ date,df_3, mean)
#εύρεση μέσης ημερήσιας καθυτέρησης
daily_average<-mean(df_3_agg$ArrDelay)
print(paste0("The daily average delay for summer months of 2008 is: ",daily_average, " mins"))
#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
df_4<-flights[,c(10,24)]

df_4<-table(df_4)
df_4_agg<-data.frame(df_4)
df_4_agg<-df_4_agg[df_4_agg$CancellationCode=="B",]
airline_name<-as.character(df_4_agg[df_4_agg$Freq==max(df_4_agg$Freq),1])
print(paste0("The airline with most delays of type B is : ",airline_name))
#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
df_5<-flights[,c(11,16)]
df_5<-df_5[df_5$ArrDelay>0, ]
df_5<-table(df_5$FlightNum)
flight_number<-names(df_5[df_5==max(df_5)])
print(paste0("The flight number with greatest number of delays is: ",flight_number))
#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
df_6<-flights[,c(16,19,20)]
df_6<-df_6[df_6$ArrDelay>0, ]
df_6_table<-table(df_6$Dest)
#find destination with most delays
destination_6<-as.factor(names(df_6_table[df_6_table==max(df_6_table)]))
#select the most distant destination
df_6<-df_6[df_6$Dest %in% destination_6,]
df_6<-as.character(df_6[df_6$Distance==max(df_6$Distance),2])
df_6<-unique(df_6)
print(paste0("The destination with longer distance and most delays is : ",df_6))
#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)
df_7<-flights[,c(16,19,23)]
df_7<-df_7[df_7$Cancelled==0,]
Dest_name<-as.character(df_7[df_7$ArrDelay %in% max(df_7$ArrDelay,na.rm=TRUE),2])
print(paste0("The destination with longest delay is: ",Dest_name))
#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
df_8<-flights[,c(10,16,30)]
df_8<-df_8[df_8$ArrDelay>0, ]
df_8<-df_8[is.na(df_8$LateAircraftDelay) == 0, ]

df_8_agg<-aggregate(cbind(LateAircraftDelay)~ UniqueCarrier,df_8, mean)
name_8<-as.character(df_8_agg[df_8_agg$LateAircraftDelay==max(df_8_agg$LateAircraftDelay),1])
print(paste0("The company with longest delays due to aircraft delay is : ",name_8))
#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα
df_9<-flights[,c(2:4,23,24,31)]
df_9<-df_9[df_9$CancellationCode=='A',]
df_9<-df_9[df_9$DayofMonth==13,]
#eisagogi stilis count = 1 oste na ektelestei meta h sum sto aggregate gia na ginei to count
df_9$count<-1

df_9<-aggregate(cbind(count)~ date,df_9, sum)
df_9_av<-mean(df_9$count)
print(paste0("Number of Type-A cancelations of 13th day of each month is : ",df_9$count))
print(paste0("Average number of Type-A cancelation of 13th day of each month is : ",df_9_av))
#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
df_10<-flights[,c(31,16)]
df_10<-df_10[df_10$ArrDelay>0, ]
df_10<-df_10[df_10$date>20080409,]
df_10<-df_10[df_10$date<20080424,]
df_10_average<-mean(df_10$ArrDelay,na.rm=TRUE)
print(paste0("The average flight delay  between 2008/04/09 and 2008/04/24 is : ",df_10_average))
#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
df_11<-flights[,c(2,3,6,29)]
df_11<-df_11[df_11$SecurityDelay!=0,]
df_11<-df_11[df_11$DepTime>0600,]
df_11<-df_11[df_11$DepTime<1400,]
month_11<-df_11[df_11$SecurityDelay %in% max(df_11$SecurityDelay,na.rm=TRUE),2]
year_11<-df_11[df_11$SecurityDelay %in% max(df_11$SecurityDelay,na.rm=TRUE),1]
print(paste0("The month with the longest delay due to security check between  06.00 and 14.00 is : ",month_11, " of year :",year_11))
#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
df_12<-flights[,c(10,16,11,31)]
df_12<-df_12[df_12$ArrDelay<0, ]
df_12<-df_12[df_12$date>20081031,]
df_12<-df_12[df_12$date<20081111,]
name_12<-unique(df_12[df_12$ArrDelay %in% min(df_12$ArrDelay, na.rm=TRUE),c(1,3)])
print(paste0("The flight with the earliest arrival has number : ",name_12$FlightNum," from company: ", name_12$UniqueCarrier))
#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
df_13<-flights[,c(18,26,31)]
df_13<-df_13[df_13$date>20080809,]
df_13<-df_13[df_13$date<20080830,]
df_13<-df_13[df_13$CarrierDelay>30,]
df_13<-table(df_13$Origin)
airport<-names(df_13[df_13==max(df_13)])
print(paste0("The airport with most delays longer than 30 mins due to air carrier is : ",airport))
#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
df_14<-flights[,c(11,17,23,25)]
df_14<-df_14[df_14$Cancelled==0,]
df_14<-df_14[df_14$Diverted==1,]
diverted_flights<-df_14$FlightNum
diverted_flights_minutes<-df_14$DepDelay
print(paste0("The flight: ",diverted_flights, " was diverted but not cancelled, with total delay: ", diverted_flights_minutes," minutes"))

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
df_15<-flights[,c(2:4,16,31)]
df_15<-df_15[df_15$ArrDelay>0,]
#dimiourgia stilis imerominias morfis YYYYMM
df_15$Year_month<-df_15$Month+df_15$Year*100
df_15_agg<-aggregate(cbind(ArrDelay)~ Year_month,df_15, sd)
uncertain_15<-as.character(df_15_agg[df_15_agg==max(df_15_agg),1])
print(paste0("The most uncertain month was : ",substr(uncertain_15,5,6), " from year :", substr(uncertain_15,1,4)))





