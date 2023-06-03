##Time to 50% germination.

#Download the file
#the data can be downloaded from osf: https://osf.io/nu7mv/

T50.df <- read.csv("~/Master/Dataset/Tfemtidata.csv", header = TRUE, sep = ";")

T50.df <- T50.df %>% 
  rename(Days_since_start = ï..Days_since_start) 


#removing faulty measurements the days since germination: 
#17fjern, 21fjern, 24fjern, 28fjern

T50.df <- T50.df[-c(4,5,7,8), ] 


#making the days numeric

T50.df$Days_since_start <- as.numeric(as.character(T50.df$Days_since_start))


#Finding the time to 50% germination for each petri dish.
#all numbers written in a new file.

WP1_1_1.3 <- t50(germ.counts = T50.df$X1_1.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")


WP2_1_1.3 <- t50(germ.counts = T50.df$X1_1.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_1_16.2 <- t50(germ.counts = T50.df$X1_16.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")

WP4_1_16.2 <- t50(germ.counts = T50.df$X1_16.2._WP4, 
                  intervals = T50.df$Days_since_start, method = "coolbear")

WP1_1_3.1 <- t50(germ.counts = T50.df$X1_3.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_1_3.1 <- t50(germ.counts = T50.df$X1_3.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP3_1_3.1 <- t50(germ.counts = T50.df$X1_3.1._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_1_3.3 <- t50(germ.counts = T50.df$X1_3.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")


WP1_1_5.3 <- t50(germ.counts = T50.df$X1_5.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_1_5.3 <- t50(germ.counts = T50.df$X1_5.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_1_6.3 <- t50(germ.counts = T50.df$X1_6.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_1_6.3 <- t50(germ.counts = T50.df$X1_6.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP5_1_8.1 <- t50(germ.counts = T50.df$X1_8.1._WP5, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_2_2.1 <- t50(germ.counts = T50.df$X2_2.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_2_2.1 <- t50(germ.counts = T50.df$X2_2.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_2_3.2 <- t50(germ.counts = T50.df$X2_3.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_2_3.2 <- t50(germ.counts = T50.df$X2_3.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_2_4.2 <- t50(germ.counts = T50.df$X2_4.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP1_2_5.3 <- t50(germ.counts = T50.df$X2_5.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP2_2_5.3 <- t50(germ.counts = T50.df$X2_5.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")

WP3_2_5.3 <- t50(germ.counts = T50.df$X2_5.3._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_2_6.1 <- t50(germ.counts = T50.df$X2_6.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_2_6.1 <- t50(germ.counts = T50.df$X2_6.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_2_14.2 <- t50(germ.counts = T50.df$X2_14.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_2_14.2 <- t50(germ.counts = T50.df$X2_14.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_2_14.2 <- t50(germ.counts = T50.df$X2_14.2._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_2_6.2 <- t50(germ.counts = T50.df$X2_6.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_2_6.2 <- t50(germ.counts = T50.df$X2_6.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP4_2_6.2 <- t50(germ.counts = T50.df$X2_6.2._WP4, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_2_14.3 <- t50(germ.counts = T50.df$X2_14.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_2_14.3 <- t50(germ.counts = T50.df$X2_14.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_2_15.1 <- t50(germ.counts = T50.df$X2_15.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_2_15.1 <- t50(germ.counts = T50.df$X2_15.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_13.1 <- t50(germ.counts = T50.df$X3_13.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_13.2 <- t50(germ.counts = T50.df$X3_13.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_13.2 <- t50(germ.counts = T50.df$X3_13.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_3_13.2 <- t50(germ.counts = T50.df$X3_13.2._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_2.3 <- t50(germ.counts = T50.df$X3_2.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_2.3 <- t50(germ.counts = T50.df$X3_2.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_13.3 <- t50(germ.counts = T50.df$X3_13.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_13.3 <- t50(germ.counts = T50.df$X3_13.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_4.1 <- t50(germ.counts = T50.df$X3_4.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_4.1 <- t50(germ.counts = T50.df$X3_4.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_3_4.1 <- t50(germ.counts = T50.df$X3_4.1._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_14.2 <- t50(germ.counts = T50.df$X3_14.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_15.1 <- t50(germ.counts = T50.df$X3_15.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_15.1 <- t50(germ.counts = T50.df$X3_15.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_3_15.1 <- t50(germ.counts = T50.df$X3_15.1._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_9.2 <- t50(germ.counts = T50.df$X3_9.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_9.2 <- t50(germ.counts = T50.df$X3_9.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_3_18.3 <- t50(germ.counts = T50.df$X3_18.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_3_18.3 <- t50(germ.counts = T50.df$X3_18.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_1.1 <- t50(germ.counts = T50.df$X4_1.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_2.2 <- t50(germ.counts = T50.df$X4_2.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_2.3 <- t50(germ.counts = T50.df$X4_2.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_2.3 <- t50(germ.counts = T50.df$X4_2.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_3.2 <- t50(germ.counts = T50.df$X4_3.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_3.2 <- t50(germ.counts = T50.df$X4_3.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP5_4_3.2 <- t50(germ.counts = T50.df$X4_3.2._WP5, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_3.3 <- t50(germ.counts = T50.df$X4_3.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_3.3 <- t50(germ.counts = T50.df$X4_3.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_17.1 <- t50(germ.counts = T50.df$X4_17.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_17.1 <- t50(germ.counts = T50.df$X4_17.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_4.1 <- t50(germ.counts = T50.df$X4_4.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_4.1 <- t50(germ.counts = T50.df$X4_4.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_4_4.1 <- t50(germ.counts = T50.df$X4_4.1._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_17.2 <- t50(germ.counts = T50.df$X4_17.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_17.2 <- t50(germ.counts = T50.df$X4_17.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_11.3 <- t50(germ.counts = T50.df$X4_11.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_11.3 <- t50(germ.counts = T50.df$X4_11.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_6.3 <- t50(germ.counts = T50.df$X4_6.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_6.3 <- t50(germ.counts = T50.df$X4_6.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_14.3 <- t50(germ.counts = T50.df$X4_14.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_14.3 <- t50(germ.counts = T50.df$X4_14.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_15.1 <- t50(germ.counts = T50.df$X4_15.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_15.1 <- t50(germ.counts = T50.df$X4_15.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_15.2 <- t50(germ.counts = T50.df$X4_15.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_4_15.2 <- t50(germ.counts = T50.df$X4_15.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_4_18.3 <- t50(germ.counts = T50.df$X4_18.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_13.1 <- t50(germ.counts = T50.df$X5_13.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_13.2 <- t50(germ.counts = T50.df$X5_13.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_13.2 <- t50(germ.counts = T50.df$X5_13.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_5_13.2 <- t50(germ.counts = T50.df$X5_13.2._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_3.3 <- t50(germ.counts = T50.df$X5_3.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_3.3 <- t50(germ.counts = T50.df$X5_3.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_17.1 <- t50(germ.counts = T50.df$X5_17.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_4.3 <- t50(germ.counts = T50.df$X5_4.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_4.3 <- t50(germ.counts = T50.df$X5_4.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_5_4.3 <- t50(germ.counts = T50.df$X5_4.3._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_11.3 <- t50(germ.counts = T50.df$X5_11.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_11.3 <- t50(germ.counts = T50.df$X5_11.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_14.1 <- t50(germ.counts = T50.df$X5_14.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_5_11.3 <- t50(germ.counts = T50.df$X5_11.3._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_6.1 <- t50(germ.counts = T50.df$X5_6.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_6.1 <- t50(germ.counts = T50.df$X5_6.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_14.2 <- t50(germ.counts = T50.df$X5_14.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_5_14.2 <- t50(germ.counts = T50.df$X5_14.2._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_18.1 <- t50(germ.counts = T50.df$X5_18.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_6.2 <- t50(germ.counts = T50.df$X5_6.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_6.2 <- t50(germ.counts = T50.df$X5_6.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_5_18.1 <- t50(germ.counts = T50.df$X5_18.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_7.1 <- t50(germ.counts = T50.df$X5_7.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_5_9.1 <- t50(germ.counts = T50.df$X5_9.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_1.3 <- t50(germ.counts = T50.df$X6_1.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_1.3 <- t50(germ.counts = T50.df$X6_1.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_13.2 <- t50(germ.counts = T50.df$X6_13.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_13.2 <- t50(germ.counts = T50.df$X6_13.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_2.2 <- t50(germ.counts = T50.df$X6_2.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_2.2 <- t50(germ.counts = T50.df$X6_2.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_4.3 <- t50(germ.counts = T50.df$X6_4.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_4.3 <- t50(germ.counts = T50.df$X6_4.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_5.2 <- t50(germ.counts = T50.df$X6_5.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_5.2 <- t50(germ.counts = T50.df$X6_5.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_6_5.2 <- t50(germ.counts = T50.df$X6_5.2._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_7.1 <- t50(germ.counts = T50.df$X6_7.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_15.3 <- t50(germ.counts = T50.df$X6_15.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_15.3 <- t50(germ.counts = T50.df$X6_15.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_9.3 <- t50(germ.counts = T50.df$X6_9.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_6_18.3 <- t50(germ.counts = T50.df$X6_18.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_6_18.3 <- t50(germ.counts = T50.df$X6_18.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_6_18.3 <- t50(germ.counts = T50.df$X6_18.3._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_1.1 <- t50(germ.counts = T50.df$X7_1.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_1.1 <- t50(germ.counts = T50.df$X7_1.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_1.2 <- t50(germ.counts = T50.df$X7_1.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_1.2 <- t50(germ.counts = T50.df$X7_1.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_7_1.2 <- t50(germ.counts = T50.df$X7_1.2._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_13.3 <- t50(germ.counts = T50.df$X7_13.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_13.3 <- t50(germ.counts = T50.df$X7_13.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_3.2 <- t50(germ.counts = T50.df$X7_3.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_3.2 <- t50(germ.counts = T50.df$X7_3.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_4.3 <- t50(germ.counts = T50.df$X7_4.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_5.2 <- t50(germ.counts = T50.df$X7_5.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_5.2 <- t50(germ.counts = T50.df$X7_5.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_17.3 <- t50(germ.counts = T50.df$X7_17.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_7_17.3 <- t50(germ.counts = T50.df$X7_17.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP4_7_17.3 <- t50(germ.counts = T50.df$X7_17.3._WP4, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_7_9.2 <- t50(germ.counts = T50.df$X7_9.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_1.2 <- t50(germ.counts = T50.df$X8_1.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_1.2 <- t50(germ.counts = T50.df$X8_1.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP3_8_1.2 <- t50(germ.counts = T50.df$X8_1.2._WP3, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_16.2 <- t50(germ.counts = T50.df$X8_16.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_16.2 <- t50(germ.counts = T50.df$X8_16.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_4.1 <- t50(germ.counts = T50.df$X8_4.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_5.1 <- t50(germ.counts = T50.df$X8_5.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_5.1 <- t50(germ.counts = T50.df$X8_5.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_6.2 <- t50(germ.counts = T50.df$X8_6.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_6.2 <- t50(germ.counts = T50.df$X8_6.2._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_14.3 <- t50(germ.counts = T50.df$X8_14.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_14.3 <- t50(germ.counts = T50.df$X8_14.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP4_8_7.2 <- t50(germ.counts = T50.df$X8_7.2._WP4, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_7.2 <- t50(germ.counts = T50.df$X8_7.2._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_15.1 <- t50(germ.counts = T50.df$X8_15.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_15.2 <- t50(germ.counts = T50.df$X8_15.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_15.2 <- t50(germ.counts = T50.df$X8_15.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_8_15.3 <- t50(germ.counts = T50.df$X8_15.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_8_15.3 <- t50(germ.counts = T50.df$X8_15.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_2.3 <- t50(germ.counts = T50.df$X9_2.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_2.3 <- t50(germ.counts = T50.df$X9_2.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_13.3 <- t50(germ.counts = T50.df$X9_13.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_13.3 <- t50(germ.counts = T50.df$X9_13.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_9_13.3 <- t50(germ.counts = T50.df$X9_13.3._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_17.1 <- t50(germ.counts = T50.df$X9_17.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_17.1 <- t50(germ.counts = T50.df$X9_17.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_9_17.1 <- t50(germ.counts = T50.df$X9_17.1._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_17.2 <- t50(germ.counts = T50.df$X9_17.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_5.1 <- t50(germ.counts = T50.df$X9_5.1._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_5.1 <- t50(germ.counts = T50.df$X9_5.1._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP4_9_5.1 <- t50(germ.counts = T50.df$X9_5.1._WP4, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_5.3 <- t50(germ.counts = T50.df$X9_5.3._WP1, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_5.3 <- t50(germ.counts = T50.df$X9_5.3._WP2, 
                 intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_17.3 <- t50(germ.counts = T50.df$X9_17.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_17.3 <- t50(germ.counts = T50.df$X9_17.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP4_9_17.3 <- t50(germ.counts = T50.df$X9_17.3._WP4, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_14.2 <- t50(germ.counts = T50.df$X9_14.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_18.1 <- t50(germ.counts = T50.df$X9_18.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_18.1 <- t50(germ.counts = T50.df$X9_18.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_9_15.3 <- t50(germ.counts = T50.df$X9_15.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_9_15.3 <- t50(germ.counts = T50.df$X9_15.3._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_9_15.3 <- t50(germ.counts = T50.df$X9_15.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_1.1 <- t50(germ.counts = T50.df$X10_1.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_16.1 <- t50(germ.counts = T50.df$X10_16.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_1.1 <- t50(germ.counts = T50.df$X10_1.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_1.2 <- t50(germ.counts = T50.df$X10_1.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_1.2 <- t50(germ.counts = T50.df$X10_1.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_1.3 <- t50(germ.counts = T50.df$X10_1.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_1.3 <- t50(germ.counts = T50.df$X10_1.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_16.2 <- t50(germ.counts = T50.df$X10_16.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_16.2 <- t50(germ.counts = T50.df$X10_16.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_4.2 <- t50(germ.counts = T50.df$X10_4.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_4.2 <- t50(germ.counts = T50.df$X10_4.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_5.1 <- t50(germ.counts = T50.df$X10_5.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_5.1 <- t50(germ.counts = T50.df$X10_5.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP3_10_5.1 <- t50(germ.counts = T50.df$X10_5.1._WP3, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_5.2 <- t50(germ.counts = T50.df$X10_5.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_5.2 <- t50(germ.counts = T50.df$X10_5.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_17.3 <- t50(germ.counts = T50.df$X10_17.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_17.3 <- t50(germ.counts = T50.df$X10_17.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_11.3 <- t50(germ.counts = T50.df$X10_11.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_14.1 <- t50(germ.counts = T50.df$X10_14.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_14.1 <- t50(germ.counts = T50.df$X10_14.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_11.3 <- t50(germ.counts = T50.df$X10_11.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP3_10_11.3 <- t50(germ.counts = T50.df$X10_11.3._WP3, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_6.3 <- t50(germ.counts = T50.df$X10_6.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_6.3 <- t50(germ.counts = T50.df$X10_6.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_7.3 <- t50(germ.counts = T50.df$X10_7.3._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_7.3 <- t50(germ.counts = T50.df$X10_7.3._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_10_8.1 <- t50(germ.counts = T50.df$X10_8.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_10_8.1 <- t50(germ.counts = T50.df$X10_8.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_11_10.1 <- t50(germ.counts = T50.df$X11_10.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_11_10.1 <- t50(germ.counts = T50.df$X11_10.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_11_10.3 <- t50(germ.counts = T50.df$X11_10.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_11_10.3 <- t50(germ.counts = T50.df$X11_10.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_11_12.1 <- t50(germ.counts = T50.df$X11_12.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_11_12.1 <- t50(germ.counts = T50.df$X11_12.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP4_11_12.1 <- t50(germ.counts = T50.df$X11_12.1._WP4, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_11_12.3 <- t50(germ.counts = T50.df$X11_12.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_11_12.3 <- t50(germ.counts = T50.df$X11_12.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_10.1 <- t50(germ.counts = T50.df$X12_10.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_10.1 <- t50(germ.counts = T50.df$X12_10.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_11.1 <- t50(germ.counts = T50.df$X12_11.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_11.1 <- t50(germ.counts = T50.df$X12_11.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_11.2 <- t50(germ.counts = T50.df$X12_11.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_11.2 <- t50(germ.counts = T50.df$X12_11.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_12.1 <- t50(germ.counts = T50.df$X12_12.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_12.1 <- t50(germ.counts = T50.df$X12_12.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP3_12_12.1 <- t50(germ.counts = T50.df$X12_12.1._WP3, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_12.2 <- t50(germ.counts = T50.df$X12_12.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_12.2 <- t50(germ.counts = T50.df$X12_12.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_12_12.3 <- t50(germ.counts = T50.df$X12_12.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_12_12.3 <- t50(germ.counts = T50.df$X12_12.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_13_10.2 <- t50(germ.counts = T50.df$X13_10.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_13_10.2 <- t50(germ.counts = T50.df$X13_10.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_13_12.2 <- t50(germ.counts = T50.df$X13_12.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_10.1 <- t50(germ.counts = T50.df$X14_10.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_10.1 <- t50(germ.counts = T50.df$X14_10.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_10.2 <- t50(germ.counts = T50.df$X14_10.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_10.2 <- t50(germ.counts = T50.df$X14_10.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP3_14_10.2 <- t50(germ.counts = T50.df$X14_10.2._WP3, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_10.3 <- t50(germ.counts = T50.df$X14_10.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP3_14_10.3 <- t50(germ.counts = T50.df$X14_10.3._WP3, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_10.3 <- t50(germ.counts = T50.df$X14_10.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP4_14_10.3 <- t50(germ.counts = T50.df$X14_10.3._WP4, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_11.1 <- t50(germ.counts = T50.df$X14_11.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_11.1 <- t50(germ.counts = T50.df$X14_11.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_11.2 <- t50(germ.counts = T50.df$X14_11.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_12.1 <- t50(germ.counts = T50.df$X14_12.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_12.2 <- t50(germ.counts = T50.df$X14_12.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_12.2 <- t50(germ.counts = T50.df$X14_12.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_14_12.3 <- t50(germ.counts = T50.df$X14_12.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_14_12.3 <- t50(germ.counts = T50.df$X14_12.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_15_10.3 <- t50(germ.counts = T50.df$X15_10.3._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_15_10.3 <- t50(germ.counts = T50.df$X15_10.3._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_15_11.1 <- t50(germ.counts = T50.df$X15_11.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_15_11.1 <- t50(germ.counts = T50.df$X15_11.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP3_15_11.1 <- t50(germ.counts = T50.df$X15_11.1._WP3, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_15_12.1 <- t50(germ.counts = T50.df$X15_12.1._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_15_12.1 <- t50(germ.counts = T50.df$X15_12.1._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_16_10.2 <- t50(germ.counts = T50.df$X16_10.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_16_10.2 <- t50(germ.counts = T50.df$X16_10.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_17_2.1 <- t50(germ.counts = T50.df$X17_2.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_18_2.2 <- t50(germ.counts = T50.df$X18_2.2._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_18_2.2 <- t50(germ.counts = T50.df$X18_2.2._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_18_11.2 <- t50(germ.counts = T50.df$X18_11.2._WP1, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP2_18_11.2 <- t50(germ.counts = T50.df$X18_11.2._WP2, 
                   intervals = T50.df$Days_since_start, method = "coolbear")
WP1_19_2.1 <- t50(germ.counts = T50.df$X19_2.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_19_2.1 <- t50(germ.counts = T50.df$X19_2.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP2_19_9.1 <- t50(germ.counts = T50.df$X19_9.1._WP2, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
WP1_19_9.1 <- t50(germ.counts = T50.df$X19_9.1._WP1, 
                  intervals = T50.df$Days_since_start, method = "coolbear")
