#----------------------------------------------------------#
#                    K-fold cross-validation               #
#----------------------------------------------------------#
cv_group <- function(k, label, seed)  # labels are coded with 0 or -1 and 1
{
  set.seed(seed)
  temp <- data.frame(obs=c(1:length(label)), label=label)
  temp_classone <- temp[temp$label==0 | temp$label==-1,]
  temp_classtwo <- temp[temp$label==1,]
  n_classone <- rep(1:k, ceiling(nrow(temp_classone)/k))[1:nrow(temp_classone)]
  n_classtwo <- rep(1:k, ceiling(nrow(temp_classtwo)/k))[1:nrow(temp_classtwo)]
  temp_classone$k <- sample(n_classone, nrow(temp_classone), replace=F)
  temp_classtwo$k <- sample(n_classtwo, nrow(temp_classtwo), replace=F)
  x <- 1:k
  cvlist <- list()
  cvlist <- lapply(x, function(x) 
  {
    obs <- c(temp_classone$obs[temp_classone$k==x], temp_classtwo$obs[temp_classtwo$k==x])
    obs <- obs[order(obs)]
  })
}
#----------------------------------------------------------#
#                       Hold-out test                      #
#----------------------------------------------------------#
ho_group <- function(train_num, label, seed)  # labels are coded with 0 or -1 and 1
{
  set.seed(seed)
  temp <- data.frame(obs=c(1:length(label)), label=label)
  sample_rate <- train_num / nrow(temp)  # sample ratio
  temp_class_one <- temp[temp$label==0 | temp$label==-1,]
  temp_class_two <- temp[temp$label==1,]
  sample_num_one <- ceiling(nrow(temp_class_one) * sample_rate)
  sample_num_two <- train_num - sample_num_one
  train_index_one <- sample(temp_class_one$obs, sample_num_one, replace=F)
  train_index_two <- sample(temp_class_two$obs, sample_num_two, replace=F)
  train_index <- c(train_index_one, train_index_two)
  train_index <- train_index[order(train_index)]
  test_index <- setdiff(temp$obs, train_index)
  ho_list <- list()
  ho_list[[1]] <- train_index
  ho_list[[2]] <- test_index
  return(ho_list)
}
