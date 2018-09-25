# Mentioned the commands which have been used to analyse few of the underlying concepts and steps as a comment in this file.

# Loading the dataset with na.strings=c("","NA") to replace the missing values with NA's
loan_dataset <- read.csv("loan.csv", na.strings=c("","NA"))


### Step_1: Identifying Data Issues like duplicate values, NA values, String manipulation, Date correction  and outliers etc.

# Part_A: Checking for duplicate values:
sum(duplicated(loan_dataset))
# As can be seen here there are no duplicate values in the dataset

# Part_B: Checking missing values: Already while loading the dataset used na.strings=c("","NA") as an argument to replace the missing values with NA's

# Part_C: Checking for NA Values
sum(is.na(loan_dataset))

# As we can there are lot of NA values in the dataset and moving forward we will have to handle them properly wherever needed.
# Digging more into this we can see that the some columns/attributes have only NA values. We can remove these columns as these columns will not impact any univariate or bivariate analysis.
loan_dataset <- loan_dataset[colSums(!is.na(loan_dataset)) > 0]

# As can be seen below 3 columns also have a lot of NA values (64% 93% & 97% respectively) so removing them.
sum(is.na(loan_dataset$mths_since_last_delinq))
sum(is.na(loan_dataset$mths_since_last_record))
sum(is.na(loan_dataset$next_pymnt_d))

loan_dataset <- loan_dataset[, !(colnames(loan_dataset) %in% c("mths_since_last_delinq", "mths_since_last_record", "next_pymnt_d"))]

# Part_D: Checking for Outliers in Annual Income Attributes
library(ggplot2)

# Checking max and min value in dataset
max(loan_dataset$annual_inc)
min(loan_dataset$annual_inc)

# Verifying boxplot for Annual Income
boxplot(loan_dataset$annual_inc, main="Outliers Detection")


# As can be seen from below frequency polygon graph it is very skewed towards the outlier values.
ggplot(loan_dataset, aes(x = annual_inc)) + geom_freqpoly()

# As can be seen from above that there are some outliers in Annual Income attributes of loan dataset. So we have to handle these outliers.
# For this we can use quantile function and calculate the elements which are lying in 1.5 times the IQR (Inter Quartile Range).
# We will do capping (replacing with standard values) for the observations outside these limits with 0.05 and 0.95 values ranges.

quantile_25_75 <- quantile(loan_dataset$annual_inc, probs=c(.25, .75), na.rm = T)
capping <- quantile(loan_dataset$annual_inc, probs=c(.05, .95), na.rm = T)
IQR_ranges <- 1.5 * IQR(loan_dataset$annual_inc, na.rm = T)
loan_dataset$annual_inc[loan_dataset$annual_inc < (quantile_25_75[1] - IQR_ranges)] <- capping[1]
loan_dataset$annual_inc[loan_dataset$annual_inc > (quantile_25_75[2] + IQR_ranges)] <- capping[2]

# Checking with boxplot
boxplot(loan_dataset$annual_inc, main="After removing Outliers")

# Now the graph is more streamlined
ggplot(loan_dataset, aes(x = annual_inc)) + geom_freqpoly()

# Part_E: Handling String and converting them to suitable formats. Using library(stringr)
# As we can see in "int_rate" the "%" sign has been included along with integer. This will cause some issue when using this column. So removing these characters.
library(stringr)
loan_dataset$int_rate <- str_replace_all(loan_dataset$int_rate, "%", "")
loan_dataset$revol_util <- str_replace_all(loan_dataset$revol_util, "%", "")

# Again in "emp_length" the No. of years have been mentioned with "<" sign, some "n/a" values and also some strings.
# Cleaning this data making this as only integers attributes. Also as mentioned in Data Dictionary replacing "< 1" year wiht 0.
loan_dataset$emp_length <- as.numeric(gsub("[^0-9\\.]", "", loan_dataset$emp_length))
loan_dataset$emp_length[loan_dataset$emp_length == "1"] <- "0"
# Line No. 66 will retain few NA's in the "emp_length" but since they are only around 2.7%, we can ignore this.

# Part_F: Removing values where there are many "0", "1" or any other entry which is same for all the rows

# The column mentioned below has some NA values and all other values as "0", again this doesn't helps in overall analysis. So removing them.
length(which(loan_dataset$collections_12_mths_ex_med == "0"))
sum(is.na(loan_dataset$collections_12_mths_ex_med))

# The column mentioned below has around 94%, 89%, 97%, 97%, 94%, 100%, 99%, 100%, 94%, 100% , 90%, 90%, 100%, 100% and 100% values as "0", "1", "f" or "INDIVIDUAL" respectively, again this doesn't helps in overall analysis. So removing them.
length(which(loan_dataset$pub_rec == "0"))
length(which(loan_dataset$delinq_2yrs == "0"))
length(which(loan_dataset$out_prncp == "0"))
length(which(loan_dataset$out_prncp_inv == "0"))
length(which(loan_dataset$total_rec_late_fee == "0"))
length(which(loan_dataset$acc_now_delinq == "0"))
length(which(loan_dataset$chargeoff_within_12_mths == "0"))
length(which(loan_dataset$delinq_amnt == "0"))
length(which(loan_dataset$pub_rec_bankruptcies == "0"))
length(which(loan_dataset$tax_liens == "0"))
length(which(loan_dataset$recoveries == "0"))
length(which(loan_dataset$collection_recovery_fee == "0"))
length(which(loan_dataset$policy_code == "1"))
length(which(loan_dataset$initial_list_status == "f"))
length(which(loan_dataset$application_type == "INDIVIDUAL"))

loan_dataset <- loan_dataset[, !(colnames(loan_dataset) %in% c("collections_12_mths_ex_med", "pub_rec", 
                "delinq_2yrs", "out_prncp", "out_prncp_inv", "total_rec_late_fee",
                "acc_now_delinq", "policy_code", "initial_list_status",
                "chargeoff_within_12_mths", "application_type", "delinq_amnt",
                "pub_rec_bankruptcies", "tax_liens", "recoveries", "collection_recovery_fee"))]

nrow(loan_dataset) * ncol(loan_dataset)
# After removing all of above and checking with "sum(is.na(loan_dataset))" we can see there are around 15K NA values,
# which are around 0.7% of whole dataset i.e. 2184435 entries, so these can be ignored.

# Part_G: Handling Date formats.

loan_dataset$issue_d <- paste("01-", loan_dataset$issue_d, sep="")
loan_dataset$issue_d <- as.Date(loan_dataset$issue_d, format = "%d-%b-%y")

loan_dataset$earliest_cr_line <- paste("01-", loan_dataset$earliest_cr_line, sep="")
loan_dataset$earliest_cr_line <- as.Date(loan_dataset$earliest_cr_line, format = "%d-%b-%y")

loan_dataset$last_pymnt_d <- paste("01-", loan_dataset$last_pymnt_d, sep="")
loan_dataset$last_pymnt_d <- as.Date(loan_dataset$last_pymnt_d, format = "%d-%b-%y")

loan_dataset$last_credit_pull_d <- paste("01-", loan_dataset$last_credit_pull_d, sep="")
loan_dataset$last_credit_pull_d <- as.Date(loan_dataset$last_credit_pull_d, format = "%d-%b-%y")

### Step_2: We have handled and like NA values, Missing values, Strings manipulation, Date correction and outliers etc. in Step_1 and replaced them proper business justifiable values.


### Step_3: Graph Plotting: We will be plotting some graphs to narrow down and find some variables/attributes which will help in deciding whom to grant loans in future.

## Part_A: Here we will be doing some univariate and segmented univariate analysis.

# Effect of "loan_amnt" on "loan_status"
ggplot(loan_dataset, aes(x = loan_amnt, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "loan_amnt") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "loan_amnt" ranging from 2k to 12K.

# Effect of "emp_length" on "loan_status"
ggplot(loan_dataset, aes(x = as.factor(emp_length), fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "emp_length") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "emp_length" < 1 year and for 10+ years.

# Effect of "home_ownership" on "loan_status"
ggplot(loan_dataset, aes(x = home_ownership, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "home_ownership") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "home_ownership" type of "MORTGAGE" and "RENT".

# Effect of "annual_inc" on "loan_status"
ggplot(loan_dataset, aes(x = annual_inc, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "annual_inc") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "annual_inc" in between 30K $ 60K. There is a spike at the end for around 140K this can be due the outlier treatment we did earlier.

# Effect of "verification_status" on "loan_status"
ggplot(loan_dataset, aes(x = verification_status, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "verification_status") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "verification_status" set as "Not Verified" and "Verified".

# Effect of "purpose" on "loan_status"
ggplot(loan_dataset, aes(x = purpose, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "purpose") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "purpose" equal "debt_consolidation".

# Effect of "addr_state" on "loan_status"
ggplot(loan_dataset, aes(x = addr_state, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "addr_state") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "addr_state" equal to "CA".

# Effect of "zip_code" on "loan_status"
ggplot(loan_dataset, aes(x = zip_code, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "zip_code") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "zip_code" mainly of 8 zipcodes like 945XXX, 917XXX etc.

# Effect of "dti" on "loan_status"
ggplot(loan_dataset, aes(x = dti, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "dti") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "dti" ranges from 12 to 20.

# Effect of "open_acc" on "loan_status"
ggplot(loan_dataset, aes(x = open_acc, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "open_acc") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "open_acc" i.e. no. of credit lines ranging from 5 to 17.

# Effect of "revol_bal" on "loan_status"
ggplot(loan_dataset, aes(x = revol_bal, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "revol_bal") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "revol_bal" ranging from 0 to 16K.

# Effect of "total_acc" on "loan_status"
ggplot(loan_dataset, aes(x = total_acc, fill = loan_status)) + 
  geom_histogram(data=subset(loan_dataset,loan_status=="Charged Off"), bins = 100) + 
  xlab(label = "total_acc") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "total_acc" i.e. no. of credit lines ranging from 6 to 23.

# Creative derived metric
loan_dataset$ecl_year <- format(loan_dataset$earliest_cr_line, "%y")

# Effect of "ecl_year" on "loan_status"
ggplot(loan_dataset, aes(x = ecl_year, fill = loan_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "earliest_cr_line year") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "earliest_cr_line" where it was opened in between 1994 to 2002.

## Part_B: Here we will be doing some bivariate analysis on all the variables/attributes analysed above.

# Bivariate analysis of Continuous Variables
bivariateContinuous <- loan_dataset[,which(colnames(loan_dataset) %in% 
            c('loan_amnt','emp_length','annual_inc','dti','open_acc', 'revol_bal', 'total_acc', 'ecl_year'))]

# Converting all the columns to "num" type for calculating the correlation
bivariateContinuous <- sapply(bivariateContinuous, function(x) as.numeric(as.character(x)))
correlation <- cor(bivariateContinuous,use = 'pairwise.complete.obs')
correlation

# As can be seen from correlation table following variables are somehow correlated:
  # a). "loan_amnt" and "annual_inc" with around 43.5% correlation.
  # b). "open_acc" and "total_acc" with around 68.7% correlation.

# Bivariate analysis of Categorical Variables.
# From the univarivate analysis above we have some categorical variables which we can do bivariate analysis on.
# "home_ownership", "verification_status", "purpose", "addr_state", "zip_code"

# Effect of "home_ownership" and "verification_status" on "loan_status"
ggplot(loan_dataset, aes(x = home_ownership, fill = verification_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "home_ownership") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "RENT" & "NOT VERIFIED" around 19.7%, "MORTGAGE" & "VERIFIED" around 17.8%.

# Effect of "purpose" and "home_ownership" on "loan_status"
ggplot(loan_dataset, aes(x = purpose, fill = home_ownership)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "purpose") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for "debt_consolidation" with "RENT" & "MORTGAGE" with around 25% and 20%.

# Effect of "addr_state" and "home_ownership" on "loan_status"
ggplot(loan_dataset, aes(x = addr_state, fill = home_ownership)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "addr_state") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are for for "CA" with "RENT" and "MORTGAGE" with around 13% & 5.7%.

# Effect of "purpose" and "verification_status" on "loan_status"
ggplot(loan_dataset, aes(x = purpose, fill = verification_status)) + 
  geom_bar(data=subset(loan_dataset,loan_status=="Charged Off")) + 
  xlab(label = "purpose") + ylab(label = "No. of Customers")
# As can be seen here, most of "Charged Off" loan are "debt_consolidation" with "VERIFIED" & "NOT VERIFIED" with around 20% & 16.7%.

### Step_4: Derived metrics

# We have derived a new column "ecl_year" which separates "year" from "earliest_cr_line" to analyse year wise credit line in Line 185
loan_dataset$ecl_year

# Right now we don't see any requirement of creating any other Business or Data driven new metrics for this analyses. If needed new metrics can be derived with some constraints.

# write.csv(loan_dataset, "loan_dataset.csv")

#################################### End of Case Study ###################################