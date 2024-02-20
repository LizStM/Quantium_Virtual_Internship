## Data analysis on customer segments
library(tidyverse)

Data <- read.csv('QVI_data.csv')

#Total sales by LIFESTAGE and PREMIUM_CUSTOMER

lifestage_customer <- Data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(sales = sum(TOT_SALES)) %>%
  arrange(LIFESTAGE, PREMIUM_CUSTOMER)
  
ggplot(data = lifestage_customer,
       aes(x = PREMIUM_CUSTOMER, y = sales, fill = LIFESTAGE)) +
  geom_col(position = 'dodge') + 
  labs(x ='Type of customer', y ='Sales',
       title ='Sales', subtitle ='by life stage and customer') +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title.position = 'plot', plot.title = element_text(size = 15, fac = 'bold'),
  plot.subtitle = element_text(size = 12,fac = 'bold',hjust = 0.15))

# NUMBER OF TRANSACTIONS by lifestage and premium_customer

n_transactions <- Data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(nbr_transaction = n())

ggplot(data = n_transactions,
       aes(x= PREMIUM_CUSTOMER, y = nbr_transaction, fill = LIFESTAGE)) +
  geom_col(positio = 'dodge') +
  labs(x ='Type of customer', y ='Number of transactions',
       title ='Number of transactions', subtitle ='by life stage and  type of customer') +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title.position = 'plot', plot.title = element_text(size = 15, fac = 'bold'),
        plot.subtitle = element_text(size = 12,fac = 'bold',hjust = 0.15))

#Number of customers by LIFESTAGE and PREMIUM_CUSTOMER

data_customer <- read.csv('QVI_purchase_behaviour.csv')

n_customer <- data_customer %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(nbr_customers = n()) %>%
  arrange(LIFESTAGE, PREMIUM_CUSTOMER)

ggplot(data = n_customer,
       aes(x= PREMIUM_CUSTOMER, y = nbr_customers, fill = LIFESTAGE)) +
  geom_col(positio = 'dodge') +
  labs(x ='Type of customer', y ='Number of customers',
       title ='Number of customers', subtitle ='by life stage and  type of customer') +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title.position = 'plot', plot.title = element_text(size = 15, fac = 'bold'),
        plot.subtitle = element_text(size = 12,fac = 'bold',hjust = 0.15))

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER

n_units <- Data %>% 
  group_by(LYLTY_CARD_NBR, LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg = mean(PROD_QTY)) %>%
  arrange(LIFESTAGE, PREMIUM_CUSTOMER)
n_unitsT <- n_units %>% 
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg2 = mean(avg))

ggplot(data = n_unitsT,
       aes(x= PREMIUM_CUSTOMER, y = avg2, fill = LIFESTAGE)) +
  geom_col(positio = 'dodge') +
  labs(x ='Type of customer', y ='quantity',
       title ='Average number of units', subtitle ='per customer by LIFESTAGE and PREMIUM_CUSTOMER') +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title.position = 'plot', plot.title = element_text(size = 15, fac = 'bold'),
        plot.subtitle = element_text(size = 12,fac = 'bold',hjust = 0.15))

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER

Data$UNIT_PRICE <- Data$TOT_SALES/Data$PROD_QTY

price_unit <- Data %>%
  group_by(LIFESTAGE,PREMIUM_CUSTOMER) %>%
  summarise(avg_price = mean(UNIT_PRICE))

ggplot(data = price_unit,
       aes(x= PREMIUM_CUSTOMER, y = avg_price, fill = LIFESTAGE)) +
  geom_col(positio = 'dodge') +
  labs(x ='Type of customer', y ='price',
       title ='Average price per unit', subtitle ='by LIFESTAGE and PREMIUM_CUSTOMER') +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title.position = 'plot', plot.title = element_text(size = 15, fac = 'bold'),
        plot.subtitle = element_text(size = 12,fac = 'bold',hjust = 0.15))

#Perform an independent t-test between mainstream vs premium and budget midage
#and young singles and couples

midage_mainstream_premium <- Data[Data$LIFESTAGE == 'MIDAGE SINGLES/COUPLES' &
                                          (Data$PREMIUM_CUSTOMER == 'Mainstream' |
                                           Data$PREMIUM_CUSTOMER == 'Premium'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = midage_mainstream_premium)
#t = 14.059, df = 15715, p-value < 2.2e-16


midage_mainstream_budget <- Data[Data$LIFESTAGE == 'MIDAGE SINGLES/COUPLES' &
                                    (Data$PREMIUM_CUSTOMER == 'Mainstream' |
                                       Data$PREMIUM_CUSTOMER == 'Budget'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = midage_mainstream_budget)
#t = -13.46, df = 8422.7, p-value < 2.2e-16


midage_premium_budget <- Data[Data$LIFESTAGE == 'MIDAGE SINGLES/COUPLES' &
                                   (Data$PREMIUM_CUSTOMER == 'Premium' |
                                      Data$PREMIUM_CUSTOMER == 'Budget'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = midage_premium_budget)
#t = -1.3537, df = 9975.6, p-value = 0.1758  <-------

#_ _ _ _ _ _ _ _

young_mainstream_premium <- Data[Data$LIFESTAGE == 'YOUNG SINGLES/COUPLES' &
                                    (Data$PREMIUM_CUSTOMER == 'Mainstream' |
                                       Data$PREMIUM_CUSTOMER == 'Premium'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = young_mainstream_premium)
#t = 24.777, df = 8897.4, p-value < 2.2e-16


young_mainstream_budget <- Data[Data$LIFESTAGE == 'YOUNG SINGLES/COUPLES' &
                                   (Data$PREMIUM_CUSTOMER == 'Mainstream' |
                                      Data$PREMIUM_CUSTOMER == 'Budget'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = young_mainstream_budget)
#t = -29.522, df = 15099, p-value < 2.2e-16


young_premium_budget <- Data[Data$LIFESTAGE == 'YOUNG SINGLES/COUPLES' &
                                (Data$PREMIUM_CUSTOMER == 'Premium' |
                                   Data$PREMIUM_CUSTOMER == 'Budget'),11:13]
t.test(UNIT_PRICE ~ PREMIUM_CUSTOMER, data = young_premium_budget)
#t = -0.43028, df = 12477, p-value = 0.667 <-------




#Deep dive into Mainstream, young and midage singles/couples 
#brands that these two customer segments prefer


#list of young customers and Brand they bought
transaction_Customer_Young <- Data[Data$LIFESTAGE == 'YOUNG SINGLES/COUPLES' &
                               Data$PREMIUM_CUSTOMER == 'Mainstream',c(1,10)]
str(transaction_Customer_Young)

#Single to basket format
library(plyr)
brands_customer <- ddply(transaction_Customer_Young, c("LYLTY_CARD_NBR"), 
                           function(df1)paste(df1$BRAND, collapse = ","))
brands_customer$LYLTY_CARD_NBR <- NULL
write.csv(brands_customer, "Brands_Young.csv" , quote = FALSE, row.names = FALSE)


#reading the document with brands
#install.packages('arules',dependencies = TRUE)
library(arules)
transactionYoung <- read.transactions(file = "Brands_Young.csv",
                                   format = "basket",
                                   sep = ",",
                                   header = TRUE)

#Frequency of brands 
itemFrequencyPlot(transactionYoung, topN= 23, type='absolute')#all Brands in Data(23)
itemFrequencyPlot(transactionYoung, topN= 10, type='absolute')#all Brands in Data(23)

#asociation rules
soporte <- 100 / dim(transactionYoung)[1]
rules <- apriori(transactionYoung, parameter = list(supp= soporte,
                                                    conf= 0.7,
                                                    minlen= 1,
                                                    maxlen = 5,
                                                    target = "frequent itemset"))
#target = "rules" doesn't create any rule 
str(rules)

summary(rules)
inspect(rules)

duplicated(rules)

top_20_items <- sort(rules, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_items)

df_top20 <- as(top_20_items, Class = "data.frame")
write.csv(df_top20, "top20_young.csv")

ggplot(data = df_top20,
       aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frequent sets", x = "sets") +
  theme_bw()



#--------------pack size
#Single to basket format
packsize_young <- ddply(transaction_Customer_Young, c("LYLTY_CARD_NBR"), 
                         function(df1)paste(df1$PACK_SIZE, collapse = ","))
packsize_young$LYLTY_CARD_NBR <- NULL
write.csv(packsize_young, "PackSize_Young.csv" , quote = FALSE, row.names = FALSE)


#reading the document with brands
#install.packages('arules',dependencies = TRUE)
packsizeYoung <- read.transactions(file = "PackSize_Young.csv",
                                      format = "basket",
                                      sep = ",",
                                      header = TRUE)

#Frequency of brands 
itemFrequencyPlot(packsizeYoung, topN= 20, type='absolute')#all sizes in Data(20)
itemFrequencyPlot(packsizeYoung, topN= 10, type='absolute')

#asociation rules
soportePackYoung <- 100 / dim(packsizeYoung)[1]
rulesSizeYoung <- apriori(packsizeYoung, parameter = list(supp= soportePackYoung,
                                                    conf= 0.7,
                                                    minlen= 1,
                                                    maxlen = 5,
                                                    target = "frequent itemset"))
#target = "rules" doesn't create any rule 
summary(rulesSizeYoung)
inspect(rulesSizeYoung)

duplicated(rulesSizeYoung)

top_20_sizes <- sort(rulesSizeYoung, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_sizes)

df_top20sizes <- as(top_20_sizes, Class = "data.frame")
write.csv(df_top20sizes, "top20Sizes_young.csv")

ggplot(data = df_top20sizes,
       aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frequent sets (size)", x = "sets") +
  theme_bw()




#---------------------------------------------
#Midage

#list of midage customers and Brand they bought
transaction_Customer_Midage <- Data[Data$LIFESTAGE == 'MIDAGE SINGLES/COUPLES' &
                                     Data$PREMIUM_CUSTOMER == 'Mainstream',c(1,9,10)]
str(transaction_Customer_Midage)

#Single to basket format
brands_midage <- ddply(transaction_Customer_Midage, c("LYLTY_CARD_NBR"), 
                         function(df1)paste(df1$BRAND, collapse = ","))
brands_midage$LYLTY_CARD_NBR <- NULL
write.csv(brands_midage, "Brands_Midage.csv" , quote = FALSE, row.names = FALSE)


#reading the document with brands
transactionMidage <- read.transactions(file = "Brands_Midage.csv",
                                      format = "basket",
                                      sep = ",",
                                      header = TRUE)

#Frequency of brands 
itemFrequencyPlot(transactionMidage, topN= 23, type='absolute')#all Brands in Data(23)
itemFrequencyPlot(transactionMidage, topN= 10, type='absolute')#all Brands in Data(23)

#asociation rules
soporteBrandMidage <- 100 / dim(transactionMidage)[1]
rulesBrandMidage <- apriori(transactionMidage, parameter = list(supp= soporteBrandMidage,
                                                    conf= 0.7,
                                                    minlen= 1,
                                                    maxlen = 5,
                                                    target = "frequent itemset"))
#target = "rules" doesn't create any rule 
summary(rulesBrandMidage)

duplicated(rulesBrandMidage)

top20_items_Midage <- sort(rulesBrandMidage, by = "support", decreasing = TRUE)[1:20]
inspect(top20_items_Midage)

df_top20_Midage <- as(top20_items_Midage, Class = "data.frame")
write.csv(df_top20_Midage, "top20_midage.csv")

ggplot(data = df_top20_Midage,
       aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frequent sets (brand)", x = "sets") +
  theme_bw()



#--------------pack size
#Single to basket format
packsize_midage <- ddply(transaction_Customer_Midage, c("LYLTY_CARD_NBR"), 
                        function(df1)paste(df1$PACK_SIZE, collapse = ","))
packsize_midage$LYLTY_CARD_NBR <- NULL
write.csv(packsize_midage, "PackSize_Midage.csv" , quote = FALSE, row.names = FALSE)


#reading the document with pack sizes
packsizeMidage <- read.transactions(file = "PackSize_Midage.csv",
                                   format = "basket",
                                   sep = ",",
                                   header = TRUE)

#Frequency of brands 
itemFrequencyPlot(packsizeMidage, topN= 20, type='absolute')#all sizes in Data(20)
itemFrequencyPlot(packsizeMidage, topN= 10, type='absolute')

#asociation rules
soportePackMidage <- 100 / dim(packsizeMidage)[1]
rulesSizeMidage <- apriori(packsizeMidage, parameter = list(supp= soportePackMidage,
                                                          conf= 0.7,
                                                          minlen= 1,
                                                          maxlen = 5,
                                                          target = "frequent itemset"))

summary(rulesSizeMidage)
duplicated(rulesSizeMidage)

top_20_sizes_M <- sort(rulesSizeMidage, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_sizes_M)

df_top20sizesMidage <- as(top_20_sizes_M, Class = "data.frame")
write.csv(df_top20sizesMidage, "top20Sizes_Midage.csv")

ggplot(data = df_top20sizesMidage,
       aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frequent sets (size)", x = "sets") +
  theme_bw()




