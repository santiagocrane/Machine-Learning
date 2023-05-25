#' Author: Alvaro
#' Date: Mar 12, 2023
#' Purpose: Fundraising PreProcessing

# Setwd
setwd("~/Hult_Visualizing-Analyzing-Data-with-R/PersonalFiles")
options(scipen=999)



# Libs
library(caret)
library(rpart.plot)
library(vtreat)
library(dplyr)
library(ggplot2)
library(skimr)
options(scipen = 999)

# Read the Data
customers <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv')
axiom <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdAxiomData.csv')
credit <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdCreditData.csv')
vehicle <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdVehicleData.csv')
prospects <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/ProspectiveCustomers.csv')

# Create a Dataframe with all the Information
all_data <- merge(customers,axiom, by = "HHuniqueID", all.x = TRUE)
all_data <- merge(all_data,credit, by = "HHuniqueID", all.x = TRUE)
all_data <- merge(all_data,vehicle, by = "HHuniqueID", all.x = TRUE)

summary(all_data)

################################# EDA Train #####################################################

# Clean the Dataframe, assign NA to all Missing Values
all_data[all_data == ""] <- NA
all_data[all_data == " "] <- NA
all_data[all_data == "NA"] <- NA
all_data[all_data == "N/A"] <- NA


skim(all_data)

missing_count <- data.frame(variable = names(all_data), count = colSums(is.na(all_data)))

# Plotting a Bar Chart for Counting Missing Values per Variables
ggplot(missing_count, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "navy") +
  ggtitle("Number of Missing Values per Variable") +
  xlab("Variables") +
  ylab("Count of Missing Values") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Looking at the Data
table(all_data$past_Outcome)

table(all_data$Education)

table(all_data$Job)

table(all_data$Communication)

table(all_data$annualDonations)


#1. Cleaning annualDonations
all_data$annualDonations <- gsub(",", "", all_data$annualDonations)  # Remove comma
all_data$annualDonations <- gsub("\\$", "", all_data$annualDonations)  # Remove dollar sign
all_data$annualDonations <- as.numeric(all_data$annualDonations)

all_data$annualDonations[is.na(all_data$annualDonations)] <- 0

#2. Cleaning Communication
# Finding the Mode for the Categorical Values
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

communication_mode <- Mode(all_data$Communication)
communication_mode

# Filling NA values in the Communication column with "celullar"
all_data$Communication <- ifelse(is.na(all_data$Communication), "cellular", all_data$Communication)

#3. Cleaning past_Outcome
# Filling NA values in past_Outcome with "other"
all_data$past_Outcome <- ifelse(is.na(all_data$past_Outcome), "other", all_data$past_Outcome)


#4. Remove Column "EstRace" from the DataFrame 
all_data <- subset(all_data, select = -EstRace)


#5, Remove Missing Value Rows from Job
all_data <- all_data[!is.na(all_data$Job), ]


#6. Clean Education
Education_mode <- Mode(all_data$Education)
Education_mode

# Filling NA values in the Education column with "secondary"
all_data$Education <- ifelse(is.na(all_data$Education), "secondary", all_data$Education)


#7. Remove Missing Value Rows from carMake
all_data <- all_data[!is.na(all_data$carMake), ]


#8. Remove Missing Value Rows from carModel
all_data <- all_data[!is.na(all_data$carModel), ]


#9. Filling Missing Values of Car Year with the Average Car Year '2010'
all_data$carYr[is.na(all_data$carYr)] <- 2010


#10. Remove Column "CallStart" from the DataFrame 
all_data <- subset(all_data, select = -CallStart)

#11. Remove Column "CallEnd" from the DataFrame 
all_data <- subset(all_data, select = -CallEnd)

#12. Remove Column "dataID" from the DataFrame 
all_data <- subset(all_data, select = -dataID)

# Double Check for Missing Values
skim(all_data)

#13. Move column Y_AcceptedOffer to last column and replacing values for 'Yes' and 'No'
all_data <- cbind(all_data[, -grep("Y_AcceptedOffer", names(all_data))],
                  all_data["Y_AcceptedOffer"])

all_data$Y_AcceptedOffer <- gsub("Accepted", "Yes", all_data$Y_AcceptedOffer)
all_data$Y_AcceptedOffer <- gsub("DidNotAccept", "No", all_data$Y_AcceptedOffer)




################################# EDA Test #####################################################

# Create a Dataframe with all the Information for the Prospective Clients
m_test <- merge(prospects,axiom, by = "HHuniqueID", all.x = TRUE)
m_test <- merge(m_test,credit, by = "HHuniqueID", all.x = TRUE)
m_test <- merge(m_test,vehicle, by = "HHuniqueID", all.x = TRUE)

summary(m_test)

# Clean the Dataframe, assign NA to all Missing Values
m_test[m_test == ""] <- NA
m_test[m_test == " "] <- NA
m_test[m_test == "NA"] <- NA
m_test[m_test == "N/A"] <- NA

#1. Cleaning annualDonations
m_test$annualDonations <- gsub(",", "", m_test$annualDonations)  # Remove comma
m_test$annualDonations <- gsub("\\$", "", m_test$annualDonations)  # Remove dollar sign
m_test$annualDonations <- as.numeric(m_test$annualDonations)

m_test$annualDonations[is.na(m_test$annualDonations)] <- 0

#2. Remove Column "EstRace" from the DataFrame 
m_test <- subset(m_test, select = -EstRace)

#3. Remove Column "dataID" from the DataFrame 
m_test <- subset(m_test, select = -dataID)

#4. Move column Y_AcceptedOffer to last column
m_test <- cbind(m_test[, -grep("Y_AcceptedOffer", names(m_test))],
                m_test["Y_AcceptedOffer"])





################################# Modeling 1 #####################################################

# Split the Training Data
splitPercent <- round(nrow(all_data) %*% .80)
totalRecords <- 1:nrow(all_data)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- all_data[idx,]
testDat  <- all_data[-idx,]

# Treatment
names(trainDat)
informativeFeatures <- names(trainDat)[2:24]
targetVariable      <- names(trainDat)[25]
successClass        <- 'Yes'

informativeFeatures
targetVariable

# Design Plan
plan <- designTreatmentsC(trainDat, 
                          informativeFeatures,
                          targetVariable, 
                          successClass)

# Apply the variable treatment plan
treatedtrain <- prepare(plan, trainDat)
treatedtest <- prepare(plan, testDat)


# Running Linear Model
model1 <- glm(as.factor(Y_AcceptedOffer) ~ ., treatedtrain, family='binomial')

summary(model1)

# Apply Parsimonious
parismonyFit <- step(model1, direction = 'backward')

# Make Predictions for Testing
acceptProb1 <- predict(parismonyFit, treatedtest, type='response')

print(acceptProb1)


# Organize the predictions in a Dataframe




# Assess - calculate accuracy, plot the ROC and make a confusion matrix etc.  Lots of ways to assess a model!
cutoff <- 0.5
predClass1 <- ifelse(acceptProb1>=cutoff,'Yes','No')
confMat <- table(treatedtest$Y_AcceptedOffer, predClass1)
confMat

# Accuracy for Model 1
sum(diag(confMat))/sum(confMat)



################################# Modeling 2 #####################################################

# Fit a decision tree with caret
set.seed(1234)

# Force a Full Decision Tree
model2 <- rpart(as.factor(Y_AcceptedOffer) ~ ., 
                 data = treatedtrain, 
                 method = "class", 
                 minsplit = 1, 
                 minbucket = 1, 
                 cp=-1)


summary(model2)

# Make Predictions for Testing
acceptProb2 <- predict(model2, treatedtest)

# Get the results in a Dataframe
predDF2<-data.frame(class  = colnames(acceptProb2)[max.col(acceptProb2)],
                      actual = treatedtest$Y_AcceptedOffer)

# Confusion Matrix
confMat2 <- table(predDF2$class,predDF2$actual)
confMat2

# Accuracy for Model 2
sum(diag(confMat2))/sum(confMat2)


################################# Modeling 3 #####################################################


model3 <- train(factor(Y_AcceptedOffer)~.,
                       data = treatedtrain,
                       method = "rf",
                       verbose = FALSE,
                       ntree = 500,
                       tuneGrid = data.frame(mtry = 1))


# To get classes with 0.50 cutoff
acceptProb3 <- predict(model3,  treatedtest)

# Organize some test results
predDF3 <- data.frame(actual = treatedtest,
                      probs  = acceptProb3)

# Accuracy for Model 3
caret::confusionMatrix(acceptProb3, as.factor(treatedtest$Y_AcceptedOffer))




############# PREDICTIONS FOR PROSPECTIVES CLIENTS USING MODEL 1 and 3 ############################


# Apply the Variable Treatment Plan
predtest <- prepare(plan, m_test)

# Predict using Model 3
predRF <- predict(model3, predtest)

# Organize the Results in a Dataframe 'predtestDF1'
predtestDF1 <- data.frame(actual = predtest,
                         probs  = predRF)



# Predict using Model 1
predGLM <- predict(parismonyFit, predtest, type='response')

# Organize the Results in a Dataframe 'predtestDF2'
predtestDF2 <- data.frame(actual = predtest,
                      probs  = predGLM)


# Add the Both Results in the prospects Database to Determine the best candidates
m_test$Accept_Offer <- predtestDF1$probs 
m_test$Accept_Prob <- predtestDF2$probs


# Create a new dataframe filtering the 100 prospects
pros_clients <- subset(m_test, Accept_Offer == "Yes")
pros_clients <- pros_clients[order(-pros_clients$Accept_Prob), ]
pros_clients <- head(pros_clients, 100)

# Send the results to Excel
output_file <- "possible_prospects.csv"
write.csv(pros_clients, file = output_file, row.names = FALSE)






##### LOOK INTO THE DATA #####

# Outliers

# Create a boxplot for the age column 
ggplot(all_data, aes(x = "Age", y = Age)) +
  geom_boxplot() +
  ggtitle("Boxplot of Age") +
  xlab("") +
  ylab("Age") +
  theme(plot.title = element_text(size = 16, face = "bold"))


# Create a boxplot for the PrevAttempts column 
ggplot(all_data, aes(x = "Attempt", y = PrevAttempts)) +
  geom_boxplot() +
  ggtitle("Boxplot of Attempt") +
  xlab("") +
  ylab("Attempt") +
  theme(plot.title = element_text(size = 16, face = "bold"))


# Create a boxplot for the Car year column 
ggplot(all_data, aes(x = "carYr", y = carYr)) +
  geom_boxplot() +
  ggtitle("Boxplot of carYr") +
  xlab("") +
  ylab("carYr") +
  theme(plot.title = element_text(size = 16, face = "bold"))



# Analysis for Insight

# Counts of Prospects by Past Outcome
ggplot(data = pros_clients, aes(x = past_Outcome)) + 
  geom_bar(position = "dodge", stat = "count")  +
  ggtitle("Counts by Past Outcome") +
  scale_fill_manual(values = c("darkgreen", "navy")) + 
  ylab("Count") + 
  xlab("Past Outcome")

# Histogram of Age
ggplot(data = pros_clients, aes(x = Age)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  xlab("Age") + ylab("Count") +
  ggtitle("Age Distribution by Gender and Status") +
  theme_bw()


# Counts of Prospects by HH Insurance
ggplot(data = pros_clients, aes(x = HHInsurance)) + 
  geom_bar(position = "dodge", stat = "count")  +
  ggtitle("Counts by HHInsurance") +
  scale_fill_manual(values = c("darkgreen", "navy")) + 
  ylab("Count") + 
  xlab("HHInsurance")


# Counts of Prospects by car Maker
table_car_make <- table(pros_clients$carMake)
table_car_make <- sort(table_car_make, decreasing = TRUE)

table_car_make

# Histogram of Car Year
ggplot(data = pros_clients, aes(x = carYr)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  xlab("carYr") + ylab("Count") +
  ggtitle("carYr") +
  theme_bw()


# Histogram of DigitalHabits_5_AlwaysOn
ggplot(data = pros_clients, aes(x = DigitalHabits_5_AlwaysOn)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  xlab("Age") + ylab("Count") +
  ggtitle("DigitalHabits_5_AlwaysOn") +
  theme_bw()


