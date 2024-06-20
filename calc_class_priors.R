Raw_data <- c(runif(40, 0.1, 1.5))
Class <- c("Green", "Red", "Yellow")
Class_mod <- sample(Class, length(Raw_data), replace = TRUE)
data_1 <- data.frame(Raw_data, Class_mod)

class_mod <- sample(Class, length(Raw_data), replace = TRUE)

class_mod

colnames(df_1) <-c("col1","col2")
unique_classes <- unique(df_1$col2)
unique_classes

calculate_params <- function(df_1) {
  colnames(df_1) <-c("col1","col2")
  unique_classes <- unique(df_1$col2)
  num_classes <- length(unique_classes)
  for(i in 1:num_classes){
    x <- subset(df_1, df_1$col2 == unique_classes[i])
    class_name <- unique_classes[i]
    class_num <- match(class_name,unique_classes)
    class_prior <- (nrow(x)) / (nrow(df_1))
    class_mean <- mean(x$col1)
    class_sd <- sd(x$col1)
    print(paste("Class", class_num, ":", class_name))
    print(paste("Prior probability:", class_prior))
    print(paste("Mean of observations:", class_mean))
    print(paste("St. dev of Observations: ", class_sd))
    cat("\n")
  }
}