## Glmnet Analyses and Plotting

# Glmnet is a package that fits a generalized linear model via penalized maximum likelihood.

############################ Description, Sources, and Context ######################################
# If you receive - Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred
# The issue of 0/1 probabilities: it means your problem has separation or quasi-separation 
# (a subset of the data that is predicted perfectly and may be running a subset of the coefficients 
# out to infinity). That can cause problems, so you will want to look at the coefficients (especially 
#those that are large and have large uncertainty intervals) and at data with probability scores near 
# zero or one (or link-scores with large absolute values).

# https://community.rstudio.com/t/logistic-regression-model-glm-fit-fitted-probabilities-numerically-0-or-1-occurred/9828 ==> suggests giving glmnet a try - it introduces a regularization/constraints that can help a bit and should be performant.

# glmnet documentation: https://www.rdocumentation.org/packages/glmnet/versions/2.0-16/topics/glmnet

# Best technique = use all the variables in the model but restrict the coefficients (explained around 
# 7 min: https://www.youtube.com/watch?v=BU2gjoLPfDc) --> Ridge regression vs. Lasso regression

# glmnet and coordinate descent; solve the Lasso problem by ...

# (1) coordinate descent which means you got a whole bunch of parameters given a value of 
# lambda with a cost parameter and what we're going to do is optimize each parameter seperately 
# holding all the others fixed and cycle around until the cofficients stablize. This is efficient.
# (2) do this on a grid of lambda values, from  lambda max to lambda min 
# (3) can do this with a variety of loss functions and additive penalties 


# **Features of glmnet (18 minutes into video):**
# (i) models (including binomial)
# (ii) elastic net penalty includes ridge and lasso, and hybrids in between
# (iii) speed
# (iv) can handle large number of variables p
# (v) cross validation functions for all models
# (vi) can allow for spare matrix formats for X, and hence massive
# (vii) can provide lower and upper bounds for each coefficient e.g. positive lasso
# (viii) offsets (often used in Poisson models), penalty strengths (zero penalty means a 
# variable is always in the model), observation weights allowed, can fit no-intercept models, 
# session-wise parameters (using glmnet.options)

# Source 1: https://drsimonj.svbtle.com/ridge-regression-with-glmnet
# Source 2: https://community.rstudio.com/t/logistic-regression-model-glm-fit-fitted-probabilities-numerically-0-or-1-occurred/9828/14
# Source 3: https://www.youtube.com/watch?v=BU2gjoLPfDc
# Source 4: https://web.stanford.edu/~hastie/Papers/Glmnet_Vignette.pdf

######################################################################################################

# Data with 3 Explanatory Variables 
x <- data %>% 
  select(A,B,C,D) %>% 
  data.matrix()
y <- data$R

fit <- glmnet(x,y, family="binomial", alpha=1) # alpha = 1 for lasso, 0 for ridge
fit 
plot(fit, label=TRUE) # coefficient profile (that's plotted by the l1 norm of the coefficient vector) 

# X-AXIS: L1 Norm: Also known as Manhattan Distance or Taxicab norm . L1 Norm is the sum of the magnitudes 
# of the vectors in a space. It is the most natural way of measure distance between vectors, that 
# is the sum of absolute difference of the components of the vectors.

# Y-AXIS: The y axis indicates the number of nonzero coefficients at the current lambda, 
# which is the effective degrees of freedom (df) for the lasso.

summary(fit) # extract all of the fitted models
lambda.1se <- coef(fit,s=0.1) # extract coefficients at a single value of lambda

# lambda.1se | gives the most regularized model such that error is within one standard error 
# of the minimum

# Cross-Validation Curve
cv.fit <- cv.glmnet(x,y, family="binomial")
plot(cv.fit) 

# lambda.min | The lowest point in the curve indicates the optimal lambda: the log value of lambda 
# that best minimizes the error in cross-validation. We can extract this values as:
  
opt_lambda <- cv.fit$lambda.min
lambda.min <- coef(cv.fit, s = opt_lambda)

# Combining into a Matrix
coeffs <- cbind(lambda.1se, lambda.min)
colnames(coeffs)[1] <- "lambda.1se coeffs"
colnames(coeffs)[2] <- paste("opt_lambda =", opt_lambda, "coeffs")
print(coeffs)





