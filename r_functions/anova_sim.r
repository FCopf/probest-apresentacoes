anova_sim = function(B = c(b0 = 50, b1 = 0, b2 = 0, b3 = 0, b4 = 0), 
                     k = 100, sd_residuo = 5, 
                     seed = 1){
  n = length(B) - 1
  X = gl(n, k, labels = LETTERS[1:n])
  dummy = model.matrix(~0 + X)
  betas = matrix(B)
  fx = as.matrix(data.frame(1, dummy)) %*% betas
  set.seed(seed)
  Y = fx + rnorm(n  = n * k, mean = 0, sd = sd_residuo)
  df = data.frame(X, Y)
  return(df)
}