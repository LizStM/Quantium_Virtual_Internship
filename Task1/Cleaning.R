# Quantium Virtual Intership

#install.packages("ggmosaic")

library(data.table)
library(ggplot2)
library(readr)
library(ggmosaic)

# Reading data
filePath <- "C:/Users/Liz/Documents/Virtual_Intership/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))



# Examining transaction data
head(transactionData)
str(transactionData)

# Convert DATE to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")


# verify information - examine PROD_NAME
unique(transactionData[, PROD_NAME])

productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words') #name the column

# Removing digits and special character
patterns <- c('^[123456789]','&')
words2 <- productWords [! grepl (paste(patterns, collapse='|'), productWords$words),]
# most common words and sorting them
commonWords <- words2[, .(.N), .(words)][order(-N)]

# Remove Salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]# new column that marks a salsa product
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]


# Summarise the data to check for nulls and possible outliers
summary(transactionData) #there is a case where 200 packets of chips are bought in one transaction.
transactionData[PROD_QTY>10]#two transaction by the same customer
transactionData[LYLTY_CARD_NBR==226000]
# Removing these two transactions
transactionData <- transactionData[LYLTY_CARD_NBR!=226000]


#Re-examine transaction data
summary(transactionData)
countByDate<- transactionData[, .(.N), .(DATE)]#a missing date

s <- as.Date('2018-07-01')
e <- as.Date('2019-06-30')
completeDates <- data.table(seq(from=s, to=e, by=1))

countByDate2 <- merge(countByDate, completeDates,by.x="DATE", by.y="V1", all=T)


#Ploting the dates
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(countByDate2, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


dec_jan = countByDate2[DATE>as.Date('2018-12-15') & DATE<as.Date('2019-01-15')]

ggplot(dec_jan, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#check Pack size
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
pack_sizes <- transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

ggplot(transactionData, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 10, col='black', fill='green', alpha=0.4) +
  labs(x = "Pack Size", y = "Number of transactions", title = "Transactions by pack size")

#Checking Brands
library(stringr)
transactionData[, BRAND := str_split(transactionData$PROD_NAME, " ", simplify = TRUE)[,1] ] #First word
unique(transactionData[, BRAND])

transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "WW", BRAND := "Woolworths"]
transactionData[BRAND == "NCC", BRAND := "Natural"]
transactionData[BRAND == "Grain", BRAND := "GrnWves"]



#Examining customer data
head(customerData)
summary(customerData)

#merge both datasets
data <- merge(transactionData, customerData, all.x = TRUE)

#checking for nulls.
sum(is.na(data))

#Save 'data'
#filePath <- "C:/Users/Liz/Documents/Virtual_Intership/"
#fwrite(data, paste0(filePath,"QVI_data.csv"))

