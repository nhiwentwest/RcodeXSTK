
################################################################################
#########  predefined functions and library installations  #####################
################################################################################

x <- c("dplyr", "ggplot2", "plotly","corrplot", "ggcorrplot", "readr", "mice",
       "Boruta", "car", "caret", "Metrics")
for (package_name in x) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}


# check invalid 
is_invalid <- function(num) num == "" | is.na(num) | num == "N/A" 


# print number of invalid observations 
check_missing <- function(frame) 
{
  percentages <- sapply(frame, 
                        function(col) sprintf("%.2f%%", sum(is_invalid(col)) / nrow(frame) * 100))
  print(percentages)
}


# remove features that contains >60% invalid data
remove_invalid_features <- function(data, threshold) 
{
  percentages <- apply(data, 2, 
                       function(col) sum(sapply(col, is_invalid)) / nrow(data) * 100)
  invalid_columns <- names(percentages[percentages > threshold])
  
  if (length(invalid_columns) > 0)
  {
    cat(sprintf("Number of invalid columns: %.0f\n", length(invalid_columns)))
    for (col in invalid_columns) 
      cat(sprintf("%s: %.2f%%\n", col, percentages[col]), sep = "")
    cat("\n")
  }
  else 
    cat(sprintf("There is no invalid columns with the threshold of %.2f.\n", threshold))
  data <- data[, !(names(data) %in% invalid_columns)]
}


convert_mb_to_kb <- function(x)
{
  for(i in seq_along(x))
  {
    first_number <- as.numeric(regmatches(x[i], regexpr("\\d+", x[i])))
    if (length(first_number) > 0)
    {
      if(grepl("MB", x[i]))
        x[i] <- first_number * 1024
      else 
        x[i] <- first_number
    }
  }
  x <- as.numeric(x)
  return(x)
}

convert_tb_to_gb <- function(x)
{
  for(i in 1:length(x))
  {
    if(grepl("GB", x[[i]]))
      x[[i]] <- as.numeric(gsub("GB", "", x[[i]]))
    else if (grepl("TB", x[[i]]))
      x[[i]] <- as.numeric(gsub("TB", "", x[[i]])) * 1024
  }
  return(x)
}


convert_ghz_to_mhz <- function(x)
{
  for(i in 1:length(x))
  {
    if(grepl("MHz", x[[i]]))
      x[[i]] <- as.numeric(gsub("MHz", "", x[[i]]))
    else if (grepl("GHz", x[[i]]))
      x[[i]] <- as.numeric(gsub("GHz", "", x[[i]])) * 1000
  }
  return(x)
}



convert_interval <- function(x) 
{
  x <- gsub("\\$", "", x)
  x <- gsub(",", "", x)
  y <- numeric(length(x))
  for(i in 1:length(x))
  {
    if (grepl("-", x[i]))
    {
      left <- as.numeric(sub("-.*", "", x[i]))
      right <- as.numeric(sub(".*-", "", x[i]))
      y[[i]] <- (left + right)/2
    }
    else y[[i]] <- as.numeric(x[i])
  }
  return(y)
}


trim <- function(dataframe, continuous) 
{
  for(var in continuous)
    if (var %in% names(dataframe))
      dataframe[[var]] <- sapply(dataframe[[var]], 
                                 function(col) as.numeric(gsub("[^0-9.]", "", col)))
  return(as.data.frame(dataframe))
}

sum_factors <- function(dataset, categoricals)
{
  factors <- dataset[categoricals] %>%
    mutate_all(as.factor)
  summary(factors)
}


process_launch_date <- function(data) {
  quart <- c(0.875, 0.625, 0.375, 0.125)
  data <- as.numeric(gsub("[^0-9.]", "", data))
  for (i in seq_along(data)) 
  {
    if (!is.na(data[i])) {
      extra <- abs(data[i]) %/% 100
      data[i] <- data[i] %% 100
      if (data[i] > 23) {
        data[i] <- 123 - data[i] + quart[extra]
      } else {
        data[i] <- 23 - data[i] + quart[extra]
      }
    }
  }
  return(data)
}

group_prod_collection <- function(data) {
  # for case-insensitive 
  lower_data <- tolower(data)
  res <- character(length(data))
  
  for (i in seq_along(data)) {
    if (grepl("with|series|family|scalable", lower_data[i])) {
      res[i] <- "IntelA Processors"
    } 
    else if (grepl("legacy", lower_data[i])) {
      res[i] <- "Legacy IntelA Processors"
    } 
    else if (grepl("(4th|5th|6th)", lower_data[i])) {
      res[i] <- "4th - 6th Generations"
    } 
    else if (grepl("7th", lower_data[i])) {
      res[i] <- "7th Generation"
    } 
    else if (grepl("8th", lower_data[i])) {
      res[i] <- "8th Generation"
    } 
    else {
      res[i] <- data[i]
    }
  }
  return(res)
}

group_memory_type <- function(data) {
  res <- character(length(data))
  
  for (i in seq_along(data)) {
    # Extract the first substring 
    first_substring <- regmatches(data[i], regexpr("DDR\\d*", data[i]))
    
    # Check if the first substring is not empty
    if (length(first_substring) > 0) {
      res[i] <- first_substring
    } 
  }
  return(res)
}

group_pci <- function(data) {
  res <- character(length(data))
  
  for (i in seq_along(data)) {
    first_number <- as.numeric(regmatches(data[i], regexpr("\\d+", data[i])))
    
    if (tolower(data[i]) %in% c("no", "none")) {
      res[i] <- "0"
    } 
    else if (length(first_number) > 0 && !is.na(first_number)) {
      if (grepl("2", data[i]) && grepl("3", data[i])) {
        res[i] <- "2.0/3.0"
      }
      else if (first_number %in% c(1, 2, 3)) {
        res[i] <- paste(first_number, collapse = ",")
      } 
      else {
        res[i] <- data[i]
      }
    } 
    else {
      res[i] <- data[i]
    }
  }
  
  return(res)
}

continuous <- c("Recommended_Customer_Price", "Processor_Base_Frequency", 
                "TDP", "Lithography", "Cache",
                "Max_Memory_Size", "Max_Memory_Bandwidth",
                "nb_of_Cores", "nb_of_Threads", 
                "Max_nb_of_Memory_Channels", "Max_nb_of_PCI_Express_Lanes", "Age",
                "Max_Turbo_Frequency", "Graphics_Base_Frequency", "Graphics_Max_Dynamic_Frequency"
)

categoricals <- c("Product_Collection", "Vertical_Segment", "Processor_Number",
                  "Status", "Bus_Speed", 
                  "Embedded_Options_Available", "Conflict_Free", 
                  "Memory_Types", "ECC_Memory_Supported",
                  "PCI_Express_Revision", "PCI_Express_Configurations_",
                  "Intel_Hyper_Threading_Technology_", 
                  "Intel_Virtualization_Technology_VTx_", 
                  "Intel_64_", "Instruction_Set", "Instruction_Set_Extensions",
                  "Idle_States", "Thermal_Monitoring_Technologies",
                  "Execute_Disable_Bit", "T", "Secure_Key"
)


################################################################################
#########  PREPROCESSING   ######################################################
################################################################################

dataset <- Intel_CPUs

summary(dataset)


n <- nrow(dataset)
dataset <- dataset[!(dataset$Graphics_Base_Frequency == "350.00 GHz" & dataset$Launch_Date == "04'16"), ]


dataset$Recommended_Customer_Price <- convert_interval(dataset$Recommended_Customer_Price)
dataset <- dataset[!is.na(dataset$Recommended_Customer_Price),]

cat("Number of removed observations:", n - nrow(dataset), "\nNumber of observations left:", nrow(dataset))

# remove features that contains over THRESHOLD % missing data.
check_missing(dataset)
threshold <- 60
dataset <- remove_invalid_features(dataset, threshold)

dataset[categoricals] <- dataset[categoricals] %>%
  mutate_all(as.factor)

summary(dataset)

# modify some continuous data

dataset$Launch_Date <- process_launch_date(dataset$Launch_Date)
names(dataset)[names(dataset) == 'Launch_Date'] <- 'Age'
dataset$Max_Memory_Size <- convert_tb_to_gb(dataset$Max_Memory_Size)
dataset$Processor_Base_Frequency <- convert_ghz_to_mhz(dataset$Processor_Base_Frequency)
dataset$Graphics_Max_Dynamic_Frequency <- convert_ghz_to_mhz(dataset$Graphics_Max_Dynamic_Frequency)

dataset$Cache <- convert_mb_to_kb(dataset$Cache)
dataset <- trim(dataset, continuous)

# group some categorical data

dataset$Product_Collection <- group_prod_collection(dataset$Product_Collection)
dataset$Memory_Types <- group_memory_type(dataset$Memory_Types)
dataset$PCI_Express_Revision <- group_pci(dataset$PCI_Express_Revision)

unusable <- c("Processor_Number", 
                           "T", "PCI_Express_Configurations_", "Bus_Speed",
                           "Instruction_Set_Extensions", "Conflict_Free")
dataset <- dataset %>%
  select(-one_of(unusable))
categoricals <- categoricals[!categoricals %in% unusable]
continuous <- continuous[!continuous %in% unusable]


dataset[categoricals] <- dataset[categoricals] %>%
  mutate_all(as.factor)
summary(dataset)
dataset[dataset == ""] <- NA

################################################################################
#########     IMPUTATION     ###################################################
################################################################################

# Multiple Imputation by Chained Equations
temp <- mice(dataset[!(names(dataset) == "Recommended_Customer_Price")], maxit = 10, meth = "cart")
densityplot(temp)

# change method because MICE was not appropriate
age <- replace(dataset$Age, is.na(dataset$Age), mean(dataset$Age, na.rm = TRUE))
plot_data <- data.frame(
  value = c(dataset$Age, age),
  variable = rep(c("Original", "Imputed"), each = nrow(dataset))
)


# then plot the mean-imputation method -> suitable
ggplot(plot_data, aes(x = value, color = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age (Original vs. Imputed)", x = "Age") +
  scale_fill_manual(values = c("Original" = "blue", "Imputed" = "magenta"))


# impute categorical data
complete_data <- complete(temp, 1)
complete_data$Age <- age
summary(complete_data)


y <- dataset$Recommended_Customer_Price
X <- complete_data[, !colnames(complete_data) %in% "Recommended_Customer_Price"]


set.seed(123)
boruta_train <- Boruta(X, y)
print(boruta_train)
plot(boruta_train)


z_scores <- getImpRfZ(X, y)
selected_features <- which(z_scores > 12)
print(names(X[selected_features]))
selected_data <- X[, c(selected_features)]
selected_data$Recommended_Customer_Price <- dataset$Recommended_Customer_Price
summary(selected_data)

################################################################################
########################  DESCRIPTIVE  #########################################
################################################################################

sel <- names(selected_data)
categoricals <- categoricals[categoricals %in% sel]
continuous <- continuous[continuous %in% sel]


cor_matrix <- cor(selected_data[continuous])
# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 45, tl.pos = "t", 
         diag = FALSE, # This ensures the diagonal labels are not shown
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.7) # Other parameters for better visualization





# Customize the labels on the left side
colnames_corrplot <- colnames(cor_matrix)
text(-0.5, 1:length(colnames_corrplot), labels = colnames_corrplot, adj = c(1, 0.5), srt = 45, xpd = TRUE, cex = 0.7)



################################################################################
############################  INFERENTIAL ######################################
################################################################################


summary(selected_data)
set.seed(123)
train_indices <- sample(1:nrow(selected_data), 0.6 * nrow(selected_data))
testing_set <- selected_data[-train_indices, ]
training_set <- selected_data[train_indices, ]


lm_model <- lm(Recommended_Customer_Price ~., data = training_set)
summary(lm_model)
vif_v <- vif(lm_model)
print(vif_v)
plot(lm_model)

actual_values <- testing_set$Recommended_Customer_Price
predictions <- predict(lm_model, newdata = testing_set)
plot(predictions)
plot(actual_values)

frame <- as.data.frame(actual_values)
frame$predicted <- predictions
print(sample_n(frame, 20))


#perform forward stepwise regression
intercept_only <- lm(Recommended_Customer_Price ~ 1, data=training_set)
SW <- lm(Recommended_Customer_Price ~ ., data=training_set)

fSW <- step(intercept_only, direction='forward', scope=formula(SW))
bSW <- step(SW, direction='backward', scope=formula(SW))
biSW <- step(intercept_only, direction='both', scope=formula(SW))

fSW$coefficients
bSW$coefficients
biSW$coefficients

plot(predict(fSW, newdata = testing_set))
plot(actual_values)

#polynomial regression
set.seed(20)


RMSE <- list()
MAE <- list()
R_squared <- list()


for (deg in 2:5) {
  
  model <- lm(formula = Recommended_Customer_Price ~ polym(Age, nb_of_Cores, Cache,
    TDP, Max_Memory_Size, Max_Memory_Bandwidth, Max_nb_of_PCI_Express_Lanes,
    nb_of_Threads, Processor_Base_Frequency, Max_Turbo_Frequency, 
    degree = deg, raw = T), data = training_set)
  
  predictions <- predict(model, testing_set)
  
  RMSE[[paste0("deg", deg, "poly")]] <- rmse(actual_values, predictions)
  MAE[[paste0("deg", deg, "poly")]] <- mae(actual_values, predictions)
  R_squared[[paste0("deg", deg, "poly")]] <- summary(model)$r.squared
}


RMSE$lm <- rmse(actual_values, predictions)
MAE$lm <- mae(actual_values, predictions)
cat("RMSE for lm model: ", RMSE$lm, "\n MAE for lm model: ", MAE$lm)


RMSE$SW <- rmse(actual_values, predict(fSW, newdata = testing_set))
MAE$SW <- mae(actual_values, predict(fSW, newdata = testing_set))


cat("RMSE for SW model: ", RMSE$SW, "\n MAE for SW model: ", MAE$SW)


metrics <- data.frame(RMSE())
po_model <- lm(formula = Recommended_Customer_Price ~ polym(Age, nb_of_Cores, Cache,
             TDP, Max_Memory_Size, Max_Memory_Bandwidth, Max_nb_of_PCI_Express_Lanes,
             nb_of_Threads, Processor_Base_Frequency, Max_Turbo_Frequency, 
             degree = 2, raw = T), data = training_set)
plot(predict(po_model, testing_set))

frame <- as.data.frame(actual_values)
frame$predicted <- predict(po_model, testing_set)
print(sample_n(frame, 20))

summary(RMSE)
sumaary(MAE)

print(R_squared)
print(RMSE)
print(MAE)
