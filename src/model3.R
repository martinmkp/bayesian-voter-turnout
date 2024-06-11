library(cmdstanr)
library(brms)
options(brms.backend='cmdstanr')
options(brms.file_refit='on_change')

# Data and model paths
# Here, it is assumed that the working dir is set to the current dir.
parent_dir <- dirname(getwd())
model_path <- file.path(parent_dir, 'models', 'fit_m3')
data_path <- file.path(parent_dir, 'data', 'data_preprocessed.csv')

# Load preprocessed dataset
df <- read.csv(data_path, header=TRUE, sep=';')

# Create a training dataset
set.seed(100) 
train_idx <- sort(sample.int(n=nrow(df), size=floor(0.9*nrow(df)), replace=F))
df_train <- df[train_idx, ]

# Priors
prior_m3 <- c(
  # Mean coefficients     
  prior(normal(0.4, 1.5), class='Intercept'),
  prior(normal(-1, 1.5), class='b', coef='poverty_prop'),
  prior(normal(0.5, 1.5), class='b', coef='bsc_prop'),
  prior(normal(0.5, 1.5), class='b', coef='elderly_prop'),
  prior(normal(0.5, 1.5), class='b', coef='white_cvap_prop'),
  
  # Phi coefficients
  prior(normal(1.8, 1.5), class='Intercept', dpar='phi'),
  prior(normal(0, 1.5), class='b', coef='poverty_prop', dpar='phi'),
  prior(normal(0, 1.5), class='b', coef='bsc_prop', dpar='phi'),
  prior(normal(0, 1.5), class='b', coef='elderly_prop', dpar='phi'),
  prior(normal(0, 1.5), class='b', coef='white_cvap_prop', dpar='phi')
)

# Model formula
formula_m3 <- bf(turnout_est ~ poverty_prop + bsc_prop + elderly_prop + white_cvap_prop,
                 phi ~ poverty_prop + bsc_prop + elderly_prop + white_cvap_prop)

# Fit the model
fit_m3 <- brms::brm(
  formula = formula_m3,
  prior = prior_m3,
  data = df_train,
  family = Beta(),
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  seed = 100,
  file = model_path,
  silent = 0
)