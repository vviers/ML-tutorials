library("ggplot2")

# Test K-NN

df1 = mvtnorm::rmvnorm(1000, mean = c(7, -5), sigma = matrix(c(10, 0, 0, 10), 2, 2))
df2 = mvtnorm::rmvnorm(1000, mean = c(-1, 2.5), sigma = matrix(c(10, 0, 0, 10), 2, 2))
df = as.data.frame(rbind(cbind(df1, 1L), cbind(df2, -1L)))
colnames(df) <- c("x", "y", "class")

ggplot(df) +
  geom_point(aes(x = x, y = y, col = as.factor(class))) +
  scale_color_discrete()

# Split Train / Test
test <- sample(nrow(df), 666, replace = F)

df_test = df[test, ]
df_train = df[-test, ]

# ggplot(df_train) +
#   geom_point(aes(x = x, y = y, col = as.factor(class))) +
#   scale_color_discrete()
# 
# ggplot(df_test) +
#   geom_point(aes(x = x, y = y, col = as.factor(class))) +
#   scale_color_discrete()

# K-NN (K-Nearest Neighbours)

# distance between any two vectors
euclid_dist <- function(v1, v2){
  return(sqrt(sum((v1 - v2)^2)))
}

# distances between new data and the training data
distance_each <- function(new_data, old_data){
  return(apply(old_data, MARGIN = 1, FUN = euclid_dist, v2 = new_data))
}

# classifier function
classify <- function(new_vec, k){
  neighb <- order(distance_each(new_vec, df_train[, c("x", "y")]),
                decreasing = F)[1:k]
  return(sign(sum(df_train[neighb, "class"])))
}

# predict
df_test$class_pred <- apply(df_test[, c("x", "y")], MARGIN = 1, FUN = classify, k = 3)

# evaluate
sum(df_test$class == df_test$class_pred) / length(df_test$class)
# Using K = 3, the we get an accuracy of 93% !


# TUNING HYPERPARAMETERS
# How does accuracy vary with our choice of k ?
accuracies = c()

for (i in c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)){
  df_test$class_pred <- apply(df_test[, c("x", "y")], MARGIN = 1, FUN = classify, k = i)
  acc <- sum(df_test$class == df_test$class_pred) / length(df_test$class)
  accuracies <- append(accuracies, acc)
}

plot(x = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29), y = accuracies, pch = 15)


# Pick k = 15
# plot misclassified points
df_test$class_pred <- apply(df_test[, c("x", "y")], MARGIN = 1, FUN = classify, k = 15)

df_test$correct <- df_test$class == df_test$class_pred

ggplot(df_test) +
  geom_point(aes(x = x, y = y, col = as.factor(class_pred), shape = as.factor(correct), alpha = as.factor(!correct)),
             show.legend = T) +
  scale_color_discrete(name = 'Predicted Classes', guide = 'legend', labels = c('-1', "1")) +
  scale_shape_manual(name = 'Misclassified', guide = "legend", labels = c("Yes", "No"), values = c(4, 1)) +
  scale_alpha_discrete(range = c(.3, 1)) +
  guides(alpha = F) +
  ggtitle(label = "Misclassified Points - Testing Set", subtitle = "K-Nearest-Neighbours [K = 15]")
