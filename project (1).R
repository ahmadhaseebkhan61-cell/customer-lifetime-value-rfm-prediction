library(readxl)
data = read_excel('Online_Retail.xlsx')
colnames(data)
names(data)
quantity_mean = mean(data$Quantity)
qty_median = median(data$Quantity)
print(qty_median)
print(quantity_mean)
names(data)
mean_unitprice = mean(data$UnitPrice)
print(mean_unitprice)
unitprice_median = median(data$UnitPrice)
print(unitprice_median)

var_qty = var(data$Quantity)
print(var_qty)
std_qty = sd(data$Quantity)
print(std_qty)

var_unitprice =var(data$UnitPrice)
print(var_unitprice)
std_unitprice = sd(data$UnitPrice)
print(std_unitprice)
names(data)
# country mode
mode_country = names(sort(table(data$Country),decreasing = TRUE))[1]
print(mode_country)

sapply(data,is.numeric)
numeric_cols = data[,sapply(data,is.numeric)]
numeric_cols
# IQR of QUantity
q1 = quantile(numeric_cols$Quantity,0.25,na.rm = TRUE)
q3 = quantile(numeric_cols$Quantity,0.75,na.rm = TRUE)
IQR_qty = q3-q1
print(IQR_qty)
# IQR of uint price
q1_up = quantile(numeric_cols$UnitPrice,0.25,na.rm = TRUE)
q3_up = quantile(numeric_cols$UnitPrice,0.75,na.rm = TRUE)
IQR_up = q3_up-q1_up
print(IQR_up)

# Detect outliers from quantity 
lower_bound_qty = q1-1.5*IQR_qty
upper_bound_qty = q3+1.5*IQR_qty
cat("Lower bound: ",lower_bound_qty)
cat("Upper bound: ",upper_bound_qty)

# Detect outliers from Unitprice
lower_bound_up = q1_up-1.5*IQR_up
upper_bound_up = q3_up+1.5*IQR_up
cat("Lower bound: ",lower_bound_up)
cat("Upper bound: ",upper_bound_up)

boxplot(numeric_cols$UnitPrice,main="Box plot of Unit price",ylab='Unit price')
boxplot(numeric_cols$Quantity,main="Box plot of Quantity",ylab = 'Quantity')

# Example for Quantity: Only plotting data within the calculated bounds
qty_filtered <- numeric_cols$Quantity[numeric_cols$Quantity >= lower_bound_qty & 
                                        numeric_cols$Quantity <= upper_bound_qty]

boxplot(qty_filtered, main="Box plot of Quantity (Filtered)", ylab="Quantity")
up_filtered = numeric_cols$UnitPrice[numeric_cols$UnitPrice >= lower_bound_up & numeric_cols$UnitPrice <= upper_bound_up]
boxplot(up_filtered,main="Box plot of Unit Price (Filtered)",ylab="Unit Price")


# Check symmetry for UnitPrice
hist(numeric_cols$UnitPrice,main="Histogram of Unit Price", 
     xlab="Unit Price", 
     col="skyblue", 
     breaks=30)

# Check symmetry for Quantity
hist(numeric_cols$Quantity,main="Histogram of Quantity", 
     xlab="Quantity", 
     col="skyblue", 
     breaks=30)
# Histogram of the filtered Unit Price (the one without extreme outliers)
hist(up_filtered, 
     breaks = 20, 
     main = "Filtered Unit Price Distribution", 
     xlab = "Unit Price", 
     col = "lightgreen")


# Histogram of the filtered Quantity (the one without extreme outliers)
hist(qty_filtered,breaks = 20,main="Filtered quantity Distribution",
     xlab="Quantity",col = "lightgreen")
# check justification for skewness
mean(up_filtered)
mean(qty_filtered)

median(up_filtered)
median(qty_filtered)

# For Unit Price, the mean ($2.5$) is greater than the median ($1.95$).
# For Quantity, the mean ($4.5$) is greater than the median ($3.0$).
# so Both are Positive Sekwed

