# Ridge Regression, Lasso Regression

library(Ecdat)
library(data.table)
comp <- data.table(Computers)

names(comp)

# Using R
naive_fit <- lm(price ~ ., data = comp)
summary(naive_fit)


# Using Matrix Algebra
# Beta = ((X'X)^-1) X'Y

X = as.matrix(comp[, -c("price")])

# Transform Factors into dummy variables
# Define function
getBool <- function(x) {
  if (x == "yes"){
    return(1)
  } else return(0)
}

# Vectorize function
getBoolVectorized = Vectorize(getBool)

types = sapply(comp, class) # get the class of each `comp` column
factors = names(types[which(types == "factor")]) # extract factor column names

X[, factors] = apply(X[, factors], 2, getBoolVectorized)
X = apply(X, 2, function(x) as.numeric(x))

# add constant to design matrix
X = cbind(1, X)

# create target vector
Y = as.vector(comp[, price])

# solve: betas = ((X'X)^-1) X'Y
betas = solve((t(X) %*% X)) %*% t(X) %*% Y
betas 
# they are the same as those from `lm`

# Std. Errors
# Calculate the error vector
e = Y - (X %*% betas)
stderr = sqrt(diag(as.numeric(var(e)) * solve((t(X) %*% X))))
results = data.frame(coeffs = betas, std = stderr)
results$t_value = abs(results$coeffs / results$std)


