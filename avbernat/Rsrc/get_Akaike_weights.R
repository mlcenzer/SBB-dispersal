
get_model_probs <- function(AIC) {
  
  # Akaike weights can be used in model averaging. 
  # They represent the relative likelihood of a model-
  # also known as the goodness of fit of a model.
  # It can be interpreted as the probability 
  # that a given model is the best approximating model.
  
  deltaAIC <- AIC - min(AIC)
  
  relative_likelihood <- exp(-1/2 * deltaAIC)
  
  Akaike_weights = relative_likelihood / sum(relative_likelihood) # relative likelihoods are normalized
  
  return(Akaike_weights) # Akaike weights can be interpreted as conditional probabilities for each model
  
}

# citation: Wagenmakes, E. & Farrell, S. "Notes and Comment: AIC model selection using Akaike weights".
#           Psychonomic Bulletin & Review. 2004, 11 (1), 192-196.
#           https://link.springer.com/content/pdf/10.3758/BF03206482.pdf

