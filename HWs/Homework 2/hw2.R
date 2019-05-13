###########################
# ALDA: hw2.R 
# Instructor: Dr. Thomas Price
# Mention your team details here
# Group 6 
# Shashank Shekhar - (sshekha4)
# Rahul Aettapu -(raettap)
# Purva Vasudeo - (ppvasude)
############################

require(caret)
require(rpart)


calculate_distance_matrix <- function(train_matrix, test_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  
  # INPUT:
  # Input: train_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # training rows (100 in the dataset supplied to you) and
  # sentence_length is the total # features (100 in the dataset supplied to you).
  # Input: test_matrix: type: matrix of size 50 x 100 (i.e, 50 rows, 100 features)
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine')
  
  # OUTPUT:
  # output: a 50 x 100 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every row in test to every row in train 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.

  distance_matrix = matrix(0L, nrow = nrow(test_matrix), ncol = nrow(train_matrix))
  # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(test_matrix))){
      for(j in seq(i, nrow(train_matrix))){
        #print(paste(i, j))
        distance_matrix[i,j] <- do.call(method_name, list(unlist(test_matrix[i,]), unlist(train_matrix[j,])))
        if(j < nrow(test_matrix)){
        distance_matrix[j,i] <- distance_matrix[i,j]
        }
      }
    }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  return(sqrt(sum((p-q)^2)))
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  dot_product <- sum(p*q)
  p_length <- sqrt(sum(p*p))
  q_length <- sqrt(sum(q*q))
  return((dot_product)/(p_length*q_length))
}

knn_classifier <- function(x_train, y_train, x_test, distance_method, k){
  # You will be IMPLEMENTING a KNN Classifier here
  
  # Build a distance matrix by computing the distance between every test sentence 
  # (row in training TF-IDF matrix) and training sentence (row in test TF-IDF matrix).
  # Use the above calculate_distance_matrix function to calculate this distance matrix (code already given to you).
  # You can re-use the calculate_euclidean and calculate_cosine methods from HW1 here.
  # Once the distance matrix is computed, for each row in the distance matrix, calculate the 'k' nearest neighbors
  # and return the most frequently occurring class from these 'k' nearest neighbors.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ('calcualte_euclidean' or 'calculate_cosine')
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # NOTE 1: Don't normalize the data before calculating the distance matrix
  
  # NOTE 2: For cosine, remember, you are calculating similarity, not distance. As a result, K nearest neighbors 
  # k values with highest values from the distance_matrix, not lowest. 
  # For euclidean, you are calculating distance, so you need to consider the k lowest values. 
  
  # NOTE 3:
  # In case of conflicts, choose the class with lower numerical value
  # E.g.: in 5NN, if you have 2 NN of class 1, 2 NN of class 2, and 1 NN of class 3, there is a conflict b/w class 1 and class 2
  # In this case, you will choose class 1. 
  
  # NOTE 4:
  # You are not allowed to use predefined knn-based packages/functions. Using them will result in automatic zero.
  # Allowed packages: R base, utils
  
  solution <- c() 
  
  #loop to get knn of each test point in test data
  for(test_row in c(1:nrow(x_test)))
  {
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    
    #Calculate distance between test row and training data using the given distance method
    for(train_row in c(1:nrow(x_train)))
    {
      if(distance_method == 'calculate_euclidean')
        df <- rbind(df,c(calculate_distance_matrix(x_train[train_row, ], x_test[test_row, ], distance_method),y_train[[train_row]])) #dataframe of distance,class
      # This will create a dist_vec with cosine difference so further calculation steps remain same as for euclidean
      else
        df <- rbind(df,c(1-calculate_distance_matrix(x_train[train_row, ], x_test[test_row, ], distance_method),y_train[[train_row]])) #dataframe of distance,class
    }
    
    #Give the columns respective names in the data frame
    colnames(df) <- c("distance", "class")
    
    #Sorting data according to distance
    df <- df[with(df, order(distance)), ]
    
    #Fetch top k neighbours from the data frame
    df <- df[1:k,]
    
    # Get a count of each class present in the dataframe using a table
    df<-as.data.frame(table(df$class))
    
    # First order based on descending frequencies , then sort ascending for class values
    df <- df[order(-df$Freq,df$Var1),]
    
    # Add the first element from matrix to solution which gives class for the given test point
    solution <- c(solution,as.matrix(df[1,1])) 
    solution <- factor(solution)
    
    
  }
  return (solution) 
  
  
  
  
}


knn_classifier_confidence <- function(x_train, y_train, x_test, distance_method='calculate_cosine', k){
  # You will be trying to build a modified KNN classifier using the paper given in the HW
  # While most of the implementation is similar to the KNN classifier, there is one additional step you need to do.
  # Read the HW PDF for more details about this method
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ( 'calculate_cosine')
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Read the NOTES from comments under knn_classifier.
  # Allowed packages: R base, utils
  
  solution <- c() 
  
  #loop to get knn of each test point in test data
  for(test_row in c(1:nrow(x_test)))
  {
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    
    #Calculate similarity between test row and training data using the given similarity method
    for(train_row in c(1:nrow(x_train)))
    {
      if(distance_method == 'calculate_euclidean')
        df <- rbind(df,c(calculate_distance_matrix(x_train[train_row, ], x_test[test_row, ], distance_method),y_train[[train_row]])) 
      # This will create a calculation with cosine similarity so further calculation steps remain same as for euclidean
      else
        df <- rbind(df,c(calculate_distance_matrix(x_train[train_row, ], x_test[test_row, ], distance_method),y_train[[train_row]])) 
    }
    
    #Give the columns respective names in the data frame
    colnames(df) <- c("similarity", "class")
    
    #Sorting data according to similarity
    if(distance_method == 'calculate_euclidean')
      df <- df[with(df, order(similarity,decreasing = 'false')), ]
    else
      df <- df[with(df, order(similarity,decreasing = 'true')), ]
    #print(df)
    #Get a total of all the cosine similarities
    df <- df[1:k,]
    total_cos_sim<-sum(df$similarity)
    #print(total_cos_sim)
    
    # Group and sum cosine similarities if the training data belongs to same class
    similarity_matrix<- aggregate(df$similarity, by=list(Category=df$class), FUN=sum)
    
    confidence_vector <- c()
    
    for(row in c(1:nrow(similarity_matrix)))
    {
      #Handling of 0 denominator
      if(total_cos_sim-similarity_matrix[row,2]!=0)
        confidence_vector <- c(confidence_vector,similarity_matrix[row,2]/(total_cos_sim-similarity_matrix[row,2]))
      # Basically, this means that all the k neighbours are of same class  
      else
        confidence_vector <- c(confidence_vector,similarity_matrix[row,2]) 
      
    }
    
    #Append confidence vector to similarity_matrix
    similarity_matrix$confidence_vector<- confidence_vector
    #print(similarity_matrix)
    
    # Append class of this test point to final solution according to the maximum similarity confidence
    solution <- c(solution,similarity_matrix[which.max(similarity_matrix$confidence_vector),1 ]) 
    
  }
  
  #print(solution)
  return (solution)
  
  
}

dtree <- function(x_train, y_train, x_test, n_folds){
  set.seed(123)
  # You will build a CART decision tree, then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, R Base, utils
  
  # HINT1: Make sure to read the documentation for the rpart package. Check out the 'rpart' and 'predict' functions.
  
  # HINT2: I've given you attributes and class labels as separate variables. Do you need to combine them 
  # into a data frame for rpart?
  td <- cbind(x_train,y_train)
  #td <- data.frame(x_train,y_train)
  
  names(td)[length(names(td))]<-"Class"
  #print(x_train)
  #dtree_fit <- train(Class ~., data = x_train, method = "rpart",parms = list(split = "gini"))
  dtree_fit <- rpart(Class ~., data = td,parms = list(split = "gini"))
  
  return(predict(dtree_fit, newdata = x_test,type = 'class'))
  
}


dtree_cv <- function(x_train, y_train, x_test, n_folds){
  set.seed(123)
  # You will build a decision tree and tune its parameters using n-fold crossvalidation on the *training* dataset,
  # then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, caret, R Base, utils
  
  # HINT1: Make sure to read the documentation for the caret package. Check out the 'train' and 'trainControl' functions.
  trctrl <- trainControl(method ="cv", number = n_folds)
  x_train <- cbind(x_train,y_train)
  names(x_train)[length(names(x_train))]<-"Class"
  #print(x_train)
  dtree_fit <- train(Class ~., data = x_train, method = "rpart",parms = list(split = "gini"),trControl = trctrl,tuneLength =20)
  return(predict(dtree_fit, newdata = x_test))
  
  
}


calculate_accuracy <- function(y_pred, y_true){
  # Given the following:
  
  # INPUT:
  # y_pred: predicted class labels (vector, each value of type factor)
  # y_true: ground truth class labels (vector, each value of type factor)
  
  # OUTPUT:
  # a list in the following order: [confusion matrix, overall accuracy], where confusion matrix is of class "table"
  # (see Figure 2 in the PDF for an example Confusion Matrix)
  # and overall accuracy is on a scale of 0-1 of type double
  # overall class accuracy = accuracy of all the classes
  
  # confusion matrix should have Prediction to the left, and Reference on the top.
  result <- confusionMatrix(table(y_pred,y_true),mode = 'prec_recall')
  #print(result$table)
  accuracy <- sum(diag(result$table))/sum(result$table)
  #print(accuracy)
  answer <- c(result$table, accuracy)
  #print(result)
  
  return(answer)
  
  
}

