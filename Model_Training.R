library(readxl)
install.packages("writexl")
library(writexl)
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
useless_rows = nrow(data) - nrow(clv_data)
print(useless_rows)
#  Orignal Rows => 541,909
#  Rows => 406,829
#  useless Rows => 135,080 


# Now its time to indentify noise and outliers and remove them 


sum(clv_data$Quantity< -1)

summary(clv_data$Quantity[clv_data$Quantity < 0])

#find the customer who canceled the order for 74215 times (it is noise)

sum(clv_data$Quantity > 14000)
sum(clv_data$Quantity < -1500)

# 2. Apply smart, data-driven threshold
clv_data <- clv_data[clv_data$Quantity >= -500 & clv_data$Quantity <= 500, ]

# 3. Check your final healthy row count
nrow(clv_data)
nrow(data) - nrow(clv_data)


sum(clv_data$UnitPrice>1000)


# We will Apply normalization techniques after Feature Engineering


# 1. Total Sum
clv_data$Total_Sum <- clv_data$Quantity * clv_data$UnitPrice

# -----------------------------
# Split into Past and Future Data
# -----------------------------
start_date <- min(clv_data$InvoiceDate)
end_date   <- max(clv_data$InvoiceDate)

total_days <- as.numeric(end_date - start_date)

cutoff_date <- start_date + round(total_days * 0.75)

past_data <- clv_data[clv_data$InvoiceDate < cutoff_date, ]

future_data <- clv_data[clv_data$InvoiceDate >= cutoff_date, ]


snapshot_date <- max(past_data$InvoiceDate) + 1
snapshot_date
data = past_data

future_data


recency <- aggregate(
  InvoiceDate ~ CustomerID,
  data = past_data,
  FUN = function(x) as.numeric(snapshot_date - max(x))
)


frequency <- aggregate(
  InvoiceNo ~ CustomerID,
  data = past_data,
  FUN = function(x) length(unique(x))
)


monetary <- aggregate(
  Total_Sum ~ CustomerID,
  data = past_data,
  FUN = sum
)

rfm <- merge(recency, frequency, by = "CustomerID")
rfm <- merge(rfm, monetary, by = "CustomerID")

colnames(rfm) <- c(
  "CustomerID",
  "Recency",
  "Frequency",
  "Monetary"
)


rfm_features <- rfm[rfm$Monetary > 0, ]

future_target <- aggregate(
  Total_Sum ~ CustomerID,
  data = future_data,
  FUN = sum
)

colnames(future_target) <- c(
  "CustomerID",
  "Future_Monetary"
)

future_target$Future_Monetary[
  future_target$Future_Monetary < 0
] <- 0

head(future_target)

model_data <- merge(
  rfm_features,
  future_target,
  by = "CustomerID",
  all.x = TRUE
)



model_data$Future_Monetary[
  is.na(model_data$Future_Monetary)
] <- 0

summary(model_data)
head(model_data)

nrow(model_data)

summary(model_data)
sum(is.na(model_data))

model_data

nrow(clv_data)
nrow(model_data)

length(unique(clv_data$CustomerID))

length(unique(past_data$CustomerID))

length(unique(future_data$CustomerID))

nrow(rfm_features)

nrow(model_data)



boxplot(model_data$Future_Monetary)


quantile(model_data$Future_Monetary,
         probs = c(0.90, 0.95, 0.99, 0.995, 1.00))
limit <- quantile(model_data$Future_Monetary, 0.99)

model_data_clean <- model_data[
  model_data$Future_Monetary <= limit,
]
nrow(model_data)-nrow(model_data_clean)

summary(model_data_clean)



set.seed(123)

train_index <- sample(
  1:nrow(model_data_clean),
  size = 0.8 * nrow(model_data_clean)
)


train_data <- model_data_clean[train_index, ]
test_data  <- model_data_clean[-train_index, ]

train_data$Monetary <- log1p(train_data$Monetary)
test_data$Monetary  <- log1p(test_data$Monetary)

train_data$Future_Monetary <- log1p(train_data$Future_Monetary)
test_data$Future_Monetary  <- log1p(test_data$Future_Monetary)


recency_mean <- mean(train_data$Recency)
recency_sd   <- sd(train_data$Recency)

frequency_mean <- mean(train_data$Frequency)
frequency_sd   <- sd(train_data$Frequency)

monetary_mean <- mean(train_data$Monetary)
monetary_sd   <- sd(train_data$Monetary)

train_data$Recency   <- (train_data$Recency - recency_mean) / recency_sd
train_data$Frequency <- (train_data$Frequency - frequency_mean) / frequency_sd
train_data$Monetary  <- (train_data$Monetary - monetary_mean) / monetary_sd

test_data$Recency   <- (test_data$Recency - recency_mean) / recency_sd
test_data$Frequency <- (test_data$Frequency - frequency_mean) / frequency_sd
test_data$Monetary  <- (test_data$Monetary - monetary_mean) / monetary_sd



model_lr <- lm(
  Future_Monetary ~ Recency + Frequency + Monetary,
  data = train_data
)

#Prediction
pred_log <- predict(model_lr, newdata = test_data)

pred <- expm1(pred_log)
actual <- expm1(test_data$Future_Monetary)


#Evaluation

rmse <- sqrt(mean((actual - pred)^2))
rmse
mae <- mean(abs(actual - pred))
mae

ss_res <- sum((actual - pred)^2)
ss_tot <- sum((actual - mean(actual))^2)

r2 <- 1 - (ss_res / ss_tot)
r2
summary(model_lr)

pred_log <- predict(model_lr, newdata = test_data)

pred <- expm1(pred_log)
actual <- expm1(test_data$Future_Monetary)


errors <- data.frame(actual, pred, error = actual - pred)
errors[order(-abs(errors$error)), ] |> head(10)

# Remove that one row and see how R² changes
errors_no_outlier <- errors[-which.max(abs(errors$error)), ]
ss_res_clean <- sum(errors_no_outlier$error^2)
ss_tot_clean <- sum((errors_no_outlier$actual - mean(errors_no_outlier$actual))^2)
r2_clean <- 1 - (ss_res_clean / ss_tot_clean)
r2_clean

# Select only numeric columns
numeric_cols <- model_data[, c("Recency", "Frequency", "Monetary", "Future_Monetary")]

cor_matrix <- cor(numeric_cols, use = "complete.obs")
cor_matrix





# Boxplots to see outliers visually
boxplot(model_data$Monetary, main = "Monetary - Outliers", horizontal = TRUE)
boxplot(model_data$Future_Monetary, main = "Future_Monetary - Outliers", horizontal = TRUE)

# Histograms for distribution shape
hist(model_data$Monetary, breaks = 50, main = "Monetary Distribution")
hist(model_data$Future_Monetary, breaks = 50, main = "Future_Monetary Distribution")




Q1 <- quantile(model_data$Monetary, 0.25)
Q3 <- quantile(model_data$Monetary, 0.75)
IQR_val <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR_val

upper_bound
sum(model_data$Monetary > upper_bound)
mean(model_data$Monetary > upper_bound) * 100

monetary_cap <- quantile(model_data$Monetary, 0.99)
future_cap <- quantile(model_data$Future_Monetary, 0.99)

model_data$Monetary <- pmin(model_data$Monetary, monetary_cap)
model_data$Future_Monetary <- pmin(model_data$Future_Monetary, future_cap)


monetary_cap <- quantile(model_data$Monetary, 0.99)
future_cap <- quantile(model_data$Future_Monetary, 0.99)

model_data_no_outliers <- model_data[
  model_data$Monetary <= monetary_cap &
    model_data$Future_Monetary <= future_cap,
]

nrow(model_data) - nrow(model_data_no_outliers)














set.seed(123)

train_index <- sample(
  1:nrow(model_data_no_outliers),
  size = 0.8 * nrow(model_data_no_outliers)
)

train_data <- model_data_no_outliers[train_index, ]
test_data  <- model_data_no_outliers[-train_index, ]

train_data$Monetary <- log1p(train_data$Monetary)
test_data$Monetary  <- log1p(test_data$Monetary)

train_data$Future_Monetary <- log1p(train_data$Future_Monetary)
test_data$Future_Monetary  <- log1p(test_data$Future_Monetary)

recency_mean <- mean(train_data$Recency)
recency_sd   <- sd(train_data$Recency)
frequency_mean <- mean(train_data$Frequency)
frequency_sd   <- sd(train_data$Frequency)
monetary_mean <- mean(train_data$Monetary)
monetary_sd   <- sd(train_data$Monetary)

train_data$Recency   <- (train_data$Recency - recency_mean) / recency_sd
train_data$Frequency <- (train_data$Frequency - frequency_mean) / frequency_sd
train_data$Monetary  <- (train_data$Monetary - monetary_mean) / monetary_sd

test_data$Recency   <- (test_data$Recency - recency_mean) / recency_sd
test_data$Frequency <- (test_data$Frequency - frequency_mean) / frequency_sd
test_data$Monetary  <- (test_data$Monetary - monetary_mean) / monetary_sd

model_lr_v2 <- lm(
  Future_Monetary ~ Recency + Frequency + Monetary,
  data = train_data
)

pred_log <- predict(model_lr_v2, newdata = test_data)
pred <- expm1(pred_log)
actual <- expm1(test_data$Future_Monetary)

rmse <- sqrt(mean((actual - pred)^2))
mae <- mean(abs(actual - pred))
r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)

rmse; mae; r2
summary(model_lr_v2)




# Test on unseen data
sample_customers <- data.frame(
  CustomerID = c("SAMPLE_1", "SAMPLE_2", "SAMPLE_3", "SAMPLE_4", "SAMPLE_5"),
  Recency   = c(5,    120,   250,   30,    80),
  Frequency = c(15,   3,     1,     8,     5),
  Monetary  = c(5000, 800,   150,   2500,  1200)
)

sample_customers

# Log-transform Monetary (Frequency and Recency are NOT log-transformed in your pipeline)
sample_customers$Monetary_log <- log1p(sample_customers$Monetary)

# Standardize using the SAME train-set mean/sd from before
sample_customers$Recency_scaled   <- (sample_customers$Recency - recency_mean) / recency_sd
sample_customers$Frequency_scaled <- (sample_customers$Frequency - frequency_mean) / frequency_sd
sample_customers$Monetary_scaled  <- (sample_customers$Monetary_log - monetary_mean) / monetary_sd


# Build a prediction dataframe matching the column names your model expects
pred_input <- data.frame(
  Recency   = sample_customers$Recency_scaled,
  Frequency = sample_customers$Frequency_scaled,
  Monetary  = sample_customers$Monetary_scaled
)

pred_log_sample <- predict(model_lr_v2, newdata = pred_input)
pred_sample <- expm1(pred_log_sample)

# Combine with original data for a readable result
result <- data.frame(
  CustomerID = sample_customers$CustomerID,
  Recency = sample_customers$Recency,
  Frequency = sample_customers$Frequency,
  Monetary = sample_customers$Monetary,
  Predicted_Future_Monetary = round(pred_sample, 2)
)

result








# What's the typical ratio of Future_Monetary to Monetary in your real training data?
summary(model_data_no_outliers$Future_Monetary / model_data_no_outliers$Monetary)


#Now Predict Future Income for next three months of each customer

# Prepare full model_data (or model_data_no_outliers) for prediction
# Apply the SAME transformations used in training

model_data_no_outliers$Monetary_log <- log1p(model_data_no_outliers$Monetary)

model_data_no_outliers$Recency_scaled   <- (model_data_no_outliers$Recency - recency_mean) / recency_sd
model_data_no_outliers$Frequency_scaled <- (model_data_no_outliers$Frequency - frequency_mean) / frequency_sd
model_data_no_outliers$Monetary_scaled  <- (model_data_no_outliers$Monetary_log - monetary_mean) / monetary_sd

# Build prediction input with matching column names
pred_input_all <- data.frame(
  Recency   = model_data_no_outliers$Recency_scaled,
  Frequency = model_data_no_outliers$Frequency_scaled,
  Monetary  = model_data_no_outliers$Monetary_scaled
)

# Predict
pred_log_all <- predict(model_lr_v2, newdata = pred_input_all)
model_data_no_outliers$Predicted_Future_Income <- expm1(pred_log_all)

export_data <- model_data_no_outliers[, c(
  "CustomerID", "Recency", "Frequency", "Monetary",
  "Predicted_Future_Income"
)]

colnames(export_data) <- c(
  "CustomerID", "Recency", "Frequency", "Monetary",
  "Future_Income_for_3_months"
)




#write_xlsx(export_data, "CLV_Predictions.xlsx")

