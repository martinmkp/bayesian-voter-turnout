library(cmdstanr)
library(brms)
options(brms.backend='cmdstanr')
options(brms.file_refit='on_change')

# Data and model paths
# Here, it is assumed that the working dir is set to the current dir.
parent_dir <- dirname(getwd())
model_path <- file.path(parent_dir, 'models', 'fit_m1')
data_path <- file.path(parent_dir, 'data', 'data_preprocessed.csv')

# Load preprocessed dataset
df <- read.csv(data_path, header=TRUE, sep=';')

# Create a training dataset
set.seed(100) 
train_idx <- sort(sample.int(n=nrow(df), size=floor(0.9*nrow(df)), replace=F))
df_train <- df[train_idx, ]

# Priors
prior_m1 <- c(
  # Mean coefficients     
  prior(student_t(7, 0.4, 1), class='Intercept'),
  prior(student_t(7, -1, 1), class='b', coef='poverty_prop'),
  
  # Phi coefficients
  prior(student_t(7, 1.8, 1), class='Intercept', dpar='phi'),
  prior(student_t(7, 0, 1), class='b', coef='poverty_prop', dpar='phi')
)

# Model formula
formula_m1 <- bf(turnout_est ~ poverty_prop,
                 phi ~ poverty_prop)

# Fit the model
fit_m1 <- brms::brm(
  formula = formula_m1,
  prior = prior_m1,
  data = df_train,
  family = Beta(),
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  seed = 100,
  file = model_path,
  silent = 0
)