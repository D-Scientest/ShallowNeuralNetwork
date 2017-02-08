splitBlocksCV = function(train_fold = 3, fold = 5, X, Y, realY = NULL, fold_idx = 3, break_point, seed = 2) {
  #################################################################################################
  ## Split the data into training and validation data set for use in kfold validation
  ## chunks data according to specfcific breakpoints and creates sets with these boundaries
  ## the default is 5-Fold validation, 60%/40% split
  ##
  ##  fold = number of kfold validations
  ##  train_fold = number of folds attributed to the training set
  ##  x,y = respective training sets
  ##  fold_idx = represents the index for randomized selection of sets, change with each i in loop
  ##  break_point = the list of breakpoints for the data set
  ##  seed = reprodicible seed, change if needed
  
  # X = TV_x; Y = TV_y; break_point = list(TV_breakPoint); seed = i
  # train_fold <- 3; fold <- 5; fold_idx <- 3
  
  set.seed(seed)
  comb = combn(fold, train_fold)[, sample(ncol(combn(fold, train_fold)))]
  train_index = comb[,fold_idx]
  
  #Create index for each group observation
  number_group_obs = length(break_point[[1]])
  group_obs = as.data.frame(cut(seq(nrow(X)), c(0,break_point[[1]]), labels = 1:number_group_obs))
  colnames(group_obs) = 'obs_idx'
  
  shuffle_index = sample(1:number_group_obs,number_group_obs)
  obs_fold_index = cbind(shuffle_index, rep(1:fold, each = (number_group_obs/fold)))
  colnames(obs_fold_index) = c('obs_idx','fold_idx')
  pair = merge(group_obs, obs_fold_index, by = 'obs_idx')
  train_indicator = pair$fold_idx %in% train_index
  
  if(is.null(realY)){
    ## return a list of matrices
    list(trainX = X[train_indicator,], trainY = Y[train_indicator,],
         validX = X[!train_indicator,], validY = Y[!train_indicator,])
  }else{
    list(trainX = X[train_indicator,], trainY = Y[train_indicator,],
         validX = X[!train_indicator,], validY = Y[!train_indicator,],
         realValidY = realY[!train_indicator,])
  }
}