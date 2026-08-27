library(readxl)
data <- read_excel("Online_Retail.xlsx")
head(data)
str(data)

summary(data)
# Remove Missing Values
pertacge_missing = (sum(is.na(data$CustomerID))/nrow(data))*100
pertacge_missing
pertage_missing_desc = (sum(is.na(data$Description))/nrow(data))*100
pertage_missing_desc

# Filter out rows where CustomerID is NA
clv_data <- data[!is.na(data$CustomerID), ]
colnames(clv_data)

# Format the date Column 
clv_data$InvoiceDate = as.Date(clv_data$InvoiceDate)

clv_data$Description = trimws(tolower(clv_data$Description))

clv_data$CustomerID <- as.factor(clv_data$CustomerID)

clv_data$Country <- trimws(tolower(clv_data$Country))

clv_data$Description <- trimws(tolower(clv_data$Description))


str(clv_data)


nrow(clv_data)
nrow(data) - nrow(clv_data)
#  Orignal Rows => 541,909
#  Rows => 406,829
#  Remaining useful Rows => 135,080 


# Now its time to indentify noise and outliers and remove them 


sum(clv_data$Quantity< -1)

summary(clv_data$Quantity[clv_data$Quantity < 0])

#find the customer who canceled the order for 74215 times (it is noise)

sum(clv_data$Quantity > 14000)
sum(clv_data$Quantity < -1500)

# 2. Apply your smart, data-driven threshold
clv_data <- clv_data[clv_data$Quantity >= -500 & clv_data$Quantity <= 500, ]

# 3. Check your final healthy row count
nrow(clv_data)
nrow(data) - nrow(clv_data)


sum(clv_data$UnitPrice>1000)


# We will Apply normalization techniques after Feature Engineering


# 1. Total Sum
clv_data$Total_Sum <- clv_data$Quantity * clv_data$UnitPrice

# 2. Latest date 
snapshot_date <- max(clv_data$InvoiceDate) + 1 # Base time for everyone 
snapshot_date
# 3. Extract Recency, Frequency, Monetary 
# Aggregate function ;syntax => aggregate = (formula,data,FUN)
recency <- aggregate(InvoiceDate ~ CustomerID, data = clv_data, FUN = function(x) as.numeric(snapshot_date - max(x)))
frequency <- aggregate(InvoiceNo ~ CustomerID, data = clv_data, FUN = length)
monetary <- aggregate(Total_Sum ~ CustomerID, data = clv_data, FUN = sum)

# Merge
rfm <- merge(recency, frequency, by = "CustomerID")
rfm <- merge(rfm, monetary, by = "CustomerID")

# Fix the Column names
colnames(rfm) <- c("CustomerID", "Recency", "Frequency", "Monetary")

#2. Data Normalization (Base R)
rfm_good <- rfm[rfm$Monetary > 0, ] # Reurning customers left negative values and log of negative 0 so  I hvae removed returning customers

# 2. Fresh log transformation
rfm_log <- rfm_good
rfm_log$Recency   <- log(rfm_good$Recency + 1)
rfm_log$Frequency <- log(rfm_good$Frequency + 1)
rfm_log$Monetary  <- log(rfm_good$Monetary + 1)

# Scaling (Z-score calculation: (x - mean) / sd)
rfm_scaled <- rfm_log
rfm_scaled$Recency <- (rfm_log$Recency - mean(rfm_log$Recency)) / sd(rfm_log$Recency)
rfm_scaled$Frequency <- (rfm_log$Frequency - mean(rfm_log$Frequency)) / sd(rfm_log$Frequency)
rfm_scaled$Monetary <- (rfm_log$Monetary - mean(rfm_log$Monetary)) / sd(rfm_log$Monetary)



# Check numbers of outliers in each feature
print(paste("Recency Outliers:", sum(abs(rfm_scaled$Recency) > 3)))
print(paste("Frequency Outliers:", sum(abs(rfm_scaled$Frequency) > 3)))
print(paste("Monetary Outliers:", sum(abs(rfm_scaled$Monetary) > 3)))

# Count total NAs in each column
colSums(is.na(rfm_scaled))

# Keep only rows where Z-scores are strictly within -3 and +3 standard deviations
rfm_final_clean <- rfm_scaled[abs(rfm_scaled$Frequency) <= 3 & 
                                abs(rfm_scaled$Monetary) <= 3, ]

# Verify the final healthy rows remaining for your model
nrow(rfm_final_clean)
nrow(clv_data)

# We have just 4,276 valuable customers
