####Stuff that's not central to the plot:


#```{r}
model_T1_final <-glmer(flew_b~host_c*sex_c + mass_c + (1|population), 
                    family=binomial, data=data_T1) #  boundary (singular) fit: see ?isSingular error
summary(model_T1_final) 
## Did not change effect estimates or improve model fit
#```

## Glmnet Plotting for Trial 1

#```{r}
# Glmnet is a package that fits a generalized linear model via penalized maximum likelihood.

x <- data %>% 
  select(A,B,C,D) %>% 
  data.matrix()
y <- data$R

fit <- glmnet(x,y, family="binomial", alpha=1) # alpha = 1 for lasso, 0 for ridge
fit # as decrease lambda, the degrees of freedom increases
plot(fit, label=TRUE) # coefficient profile (that's plotted by the l1 norm of the coefficient vector) - 4 = D and 3 = C are near 0
# L1 Norm: Also known as Manhattan Distance or Taxicab norm . L1 Norm is the sum of the magnitudes of the vectors in a space. It is the most natural way of measure distance between vectors, that is the sum of absolute difference of the components of the vectors.
#```

#The axis above indicates the number of nonzero coefficients at the current $\lambda$, which is the effective degrees of freedom (df) for the lasso.

#```{r}
summary(fit) # extract all of the fitted models
lambda.1se <- coef(fit,s=0.1) # extract coefficients at a single value of lambda
lambda.1se
#```

#lambda.1se gives the most regularized model such that error is within one standard error of the minimum

#```{r}
# cross-validation curve
cv.fit <- cv.glmnet(x,y, family="binomial")
plot(cv.fit) 
#```
#Large error bars.

#The lowest point in the curve indicates the optimal lambda: the log value of lambda that best minimizes the error in cross-validation. We can extract this values as:

#```{r}
opt_lambda <- cv.fit$lambda.min
opt_lambda
#```

#```{r}
lambda.min <- coef(cv.fit, s = opt_lambda)
lambda.min 
#```

