#Let's repeat finding the control store and assessing the impact of the trial for
#each of the other two trial stores.
#Store 88
### Load required libraries
library(data.table)
library(ggplot2)
library(tidyr)

filePath <- "C:/Users/Liz/Documents/Virtual_Internship/"
data <- fread(paste0(filePath,"QVI_data.csv"))

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Select control stores
data[, YEARMONTH := format(DATE,'%Y%m')]

### Measure calculations for each store and month
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = .N/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY))
                        , .(YEARMONTH , STORE_NBR)][order(YEARMONTH)]

### Filter to the stores with full observation periods and the pre-trial period
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]


## function to calculate correlation
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique( inputTable[, STORE_NBR ] )
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == i, ..metricCol],
                                                        inputTable[STORE_NBR == storeComparison, ..metricCol])
    )
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure, use.names = FALSE)
  }
  return(calcCorrTable)
}

### Create a function to calculate a standardised magnitude distance for a measure, looping through each control store

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison
                                   , "Store2" = i
                                   , "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH]
                                   , "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)]
                                                     - inputTable[STORE_NBR == i, eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  # Standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                                .(Store1, Store2)]
  return(finalDistTable)
}


## Use the function to calculate correlations against store 86 using total sales and number of customers.
trial_store <- 88
corr_nSales <- calculateCorrelation(preTrialMeasures, 'totSales', trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, 'nCustomers', trial_store)

### Then, use the functions for calculating magnitude.
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

### combine all the scores calculated to create a composite score to rank on.
corr_weight <- 0.5

score_nSales <- merge(corr_nSales, magnitude_nSales, by = c('Store1','Store2'))[
  , scoreNSales := corr_weight*corr_measure + corr_weight*mag_measure]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c('Store1','Store2'))[
  , scoreNCust := corr_weight*corr_measure + corr_weight*mag_measure]


### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers, by = c('Store1','Store2'))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#The store with the highest score is then selected as the control store since it is
#most similar to the trial store.

controlStores <- score_Control[finalControlScore > 0.75, .(Store2, finalControlScore)]
controlStores
control_store <- 237

### Visual checks on trends based on the drivers
measureOverTimeSales <- copy(measureOverTime)
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
            ifelse(STORE_NBR == control_store, "Control", "Other stores"))
            ][, totalSales := mean(totSales), by = c("YEARMONTH","Store_type")
            ][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1, sep = "-"),"%Y-%m-%d")
            ][as.numeric(YEARMONTH) < 201903 , ]

ggplot(pastSales, aes(TransactionMonth, totalSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#Next, number of customers.
measureOverTimeCusts <- copy(measureOverTime)
pastCustomers <- measureOverTimeCusts[,Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                            ifelse(STORE_NBR == control_store, "Control", "Other stores"))][
                                                              , totalCustomers := mean(nCustomers), by = c('YEARMONTH','Store_type')][
                                                                , TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1, sep = "-"),"%Y-%m-%d")][
                                                                  as.numeric(YEARMONTH) < 201903 , ]

ggplot(pastCustomers, aes(TransactionMonth, totalCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of customers", title = "Number of customers by month")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Assessment of trial

### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
                                                   YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
                                                                                                         YEARMONTH < 201902, sum(totSales)]

### Apply the scaling factor
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]


### Calculate the percentage difference between scaled control sales and trial sales
trialSales <- measureOverTimeSales[STORE_NBR == trial_store,]

percentageDiff <- merge(scaledControlSales[,c('YEARMONTH', 'controlSales')],
                        trialSales[,c('YEARMONTH','totSales')],
                        by = 'YEARMONTH')[, percentageDiff := (abs(controlSales-totSales)/
                                                                 ((controlSales+totSales)/2))]

###Let's see if the difference is significant!
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

# Calculate the t-values for the trial months. 
percentageDiff[, tValue := abs(controlSales-totSales)/stdDev][
  , TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1,
                                      sep = "-"),"%Y-%m-%d")]


### Trial and control store total sales
pSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                      ifelse(STORE_NBR == control_store, "Control", "Other stores"))][
                                                        , totalSales := mean(totSales), by = c("YEARMONTH","Store_type")][
                                                          , TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1, sep = "-"),"%Y-%m-%d")][
                                                            Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pSales_Controls95 <- pSales[Store_type == "Control",
][, totalSales := totalSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence
interval"]

#### Control store 5th percentile
pSales_Controls5 <- pSales[Store_type == "Control",
][, totalSales := totalSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
interval"]

trialAssessment <- rbind(pSales, pSales_Controls95, pSales_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totalSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#The results show that the trial in store 88 is significantly different to its
#control store in the trial period as the trial store performance lies outside of
#the 5% to 95% confidence interval of the control store in two of the three trial
#months.
#::::::::::::::::::::::::::::::::::::::::::::
###Let's have a look at assessing this for number of customers as well.

scalingFactorForControlCustomers <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

### Apply the scaling factor
scaledControlCusts <- measureOverTimeCusts[STORE_NBR == control_store, 
][ ,controlCustomers := nCustomers * scalingFactorForControlCustomers]


### Calculate the percentage difference between scaled control customers and trial customers
trialCusts <- measureOverTimeCusts[STORE_NBR == trial_store,]

percentageDiffCusts <- merge(scaledControlCusts[,c('YEARMONTH', 'controlCustomers')],
                             trialCusts[,c('YEARMONTH','totalCustomers')],
                             by = 'YEARMONTH')[, percentageDiff := (abs(controlCustomers-totalCustomers)/
                                                                      ((controlCustomers+totalCustomers)/2))]


###Let's see if the difference is significant

stdDevCust <- sd(percentageDiffCusts[YEARMONTH < 201902 , percentageDiff])

### Calculate the t-values for the trial months.

percentageDiffCusts[, tValue := abs(controlCustomers-totalCustomers)/stdDevCust
][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1,
                                      sep = "-"),"%Y-%m-%d")]


### Trial and control store number of customers
pCusts <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                      ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totalCustomers2 := mean(totalCustomers), by = c("YEARMONTH","Store_type")
][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH)%/%100, as.numeric(YEARMONTH)%%100, 1, sep = "-"),"%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pCusts_Controls95 <- pCusts[Store_type == "Control",
][, totalCustomers2 := totalCustomers2 * (1 + stdDevCust * 2)
][, Store_type := "Control 95th % confidence
interval"]

#### Control store 5th percentile
pCusts_Controls5 <- pCusts[Store_type == "Control",
][, totalCustomers2 := totalCustomers2 * (1 - stdDevCust * 2)
][, Store_type := "Control 5th % confidence
interval"]

trialAssessmentCusts <- rbind(pCusts, pCusts_Controls95, pCusts_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessmentCusts, aes(TransactionMonth, totalCustomers2, color = Store_type)) +
  geom_rect(data = trialAssessmentCusts[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total customers", title = "Total customers by month")

#Total number of customers in the trial period for the trial store is significantly
#higher than the control store for two out of three months, which indicates a
#positive trial effect.


#We've found control stores 233, 155, 237 for trial stores 77, 86 and 88 respectively.
#The results for trial stores 77 and 88 during the trial period show a significant
#difference in at least two of the three trial months but this is not the case for
#trial store 86. Overall, the trial shows a significant increase in
#sales.
