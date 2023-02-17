predictive.variables <- c("x1", "x2", "x5", "x7")     # isolate the variables to test

combs <- lapply(seq_along(predictive.variables), n=4, combinations, v=predictive.variables)   #   generate combinations of models and format into list
combs.split <- lapply(combs, asplit, MARGIN = 1)
combs.list <- unlist(combs.split, recursive = FALSE, use.names = TRUE)

fits <- list()    #initialise lists for for loop
models <- list()
for (i in 1:15){      #for loop to fit the various models
  fits[[i]] <- glm(formula = y ~ ., data = table.b4[c("y", combs.list[[i]])])  # fit model
  models[[i]] <- formula(fits[[i]])     # save model formula
}
df <- data.frame(matrix(unlist(models), nrow=length(models), byrow=TRUE))   #   format model list into dataframe
AIClist <- lapply(fits, AIC, k=2)   #  calculate AIC and BIC for each model
BIClist <- lapply(fits, AIC, k = log(length(fits)))   
AICs <- unlist(AIClist)    
BICs <- unlist(BIClist)

Results <- cbind(df,AICs, BICs)     #   save models,AICs in dataframe for disply
SortedResults <- Results[order(Results$AICs, decreasing = FALSE),]  # Sort Results

print(SortedResults)
print(vif(fits[[5]]))
print(cooks.distance(fits[[5]]))

