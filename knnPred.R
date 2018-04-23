### Function: KNN with different distance metrics ####
## Inputs: 
# Validation/Test data: all columns can be included, Class can be assigned as a Factor
# Train data: all columns can be included, Class can be assigned as a Factor
# k value: default to 1
# metricD: a distance metric, default is "gower", can choose "euclidean", "manhattan" (using function "daisy" from "cluster")
# ref_class: specify the reference class, AS CHARACTER variable
# class_col: specify the column name AS CHARACTER that contains the Class variable

require(cluster)

knn_predict <- function(test_data, train_data, k_value = 1, metricD = "gower", ref_class = "", class_col = ""){
  # Initialise an empty prediction vector
  pred <- c()
  
  # LOOP-1 : Test/Validation Data
  for(i in c(1:nrow(test_data))){
    # distance and true class empty vectors
    distance  <- c()
    trueClass <- c()
    
    # good & bad variable initialization with 0 value
    # good for the ref_class
    good <- 0              
    bad  <- 0
    
    # LOOP-2 : Train Data 
    for(j in c(1:nrow(train_data))) {
      
      # Adding distance b/w test data and train data to the "distance" vector
      df <- rbind(test_data[i,], train_data[j,])
      distance <- c(distance, daisy(df, metric = metricD)[1])
      
      # Adding class variable of train data to the "trueClass" vector
      trueClass <- c(trueClass, as.character(train_data[j,][[class_col]]))
    }
    
    # Create a data frame containing the trueClass and distance columns
    knn.df <- data.frame(trueClass, distance) 
    
    # Sort the data frame based on distance calculated to get top K neighbours
    knn.df <- knn.df[order(knn.df$distance),]
    
    # Get the top K neighbours
    knn.df <- knn.df[1:k_value,]
    
    # LOOP-3: Loop over knn.df and Count classes of neighbours
    for(k in c(1:nrow(knn.df))){
      if(as.character(knn.df[k,"trueClass"]) == ref_class){
        good = good + 1
      }
      else
        bad = bad + 1
    }
    
    # Compares the number of neighbours with class labelled good or bad
    # If majority of neighbors are good then put ref_class in pred vector
    # If majority of neighbors are bad then put the other class in pred vector
    if(good > bad){
      pred <- c(pred, ref_class)
    }
    else if(good < bad){
      # Get the 2nd class from the data set
      classes <- as.vector(unique(train_data[[class_col]]))
      class_2 <- classes[!(classes %in% ref_class)]
      
      pred <- c(pred, class_2)
    }
  }
  return(pred)
}