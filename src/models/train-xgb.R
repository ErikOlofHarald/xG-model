library(here)
library(data.table)
library(xgboost)
library(xgboostExplainer)
library(rBayesianOptimization)


# Reading data ------------------------------------------------------------

shots <- readRDS(here("data", "processed", "model-data.rds"))
shots <- as.data.table(shots)


# Features ----------------------------------------------------------------

features <- names(shots)[c(3, 16:69)]
fchars <- names(shots)[lapply(shots, is.character) == TRUE]
fchars <- c(fchars, grep("zone", features, value = TRUE))
fchars <- intersect(features, fchars)

# n_shots: Total number of shots in an attack. Can't be used (leakage)
# second_start: Have match_second and ds
# zone_start_id: using coordinates
# zone_id: using coordinates
# pass_1_zone_id: using coordinates
# pass_2_zone_id: using coordinates
# shot_angle: using coordinates
# shot_dist: using coordinates
drops <- c("n_shots", "second_start", "zone_start_id", "zone_id",
  "pass_1_zone_id", "pass_2_zone_id", "shot_angle", "shot_dist")
features <- setdiff(features, drops)
fchars <- intersect(fchars, features)
fnums <- setdiff(features, fchars)


# Train and test data -----------------------------------------------------

test_idx <- which(shots$league == "Sweden. Allsvenskan" & shots$season == 2018)
train <- shots[-test_idx]
set.seed(1)
train <- train[sample(nrow(train), nrow(train))]  # Reshuffle data
test <- shots[test_idx]


# Folds -------------------------------------------------------------------

# Stratified by target
set.seed(1)
y <- train[["goal"]]
v <- seq(length(y))
s0 <- v[y == 0]
s0 <- split(s0, list(fold = sample(10, length(s0), replace = TRUE)))
s1 <- v[y == 1]
s1 <- split(s1, list(fold = sample(10, length(s1), replace = TRUE)))
folds <- Map("c", s1, s0)
folds_idx <- do.call("c", folds)
rm(y, v, s0, s1)


# Target encoding ---------------------------------------------------------

set.seed(1)

# Use each oof to calculate target encoding. Take mean over all folds
get_encoding <- function(x, y) {
  s <- sample(1:10, length(x), replace = TRUE)
  v <- aggregate(y, list(grp = x, k = s), mean, na.rm = FALSE)
  v <- aggregate(v$x, list(grp = v$grp), mean, na.rm = FALSE)
  setNames(v$x, v$grp)
}

set_encoding <- function(x, z) {
  unname(z[x])
}

# Use oof data to calculate target encoding for each fold
train_encoded <- vector("list", length(folds))
for (k in seq_along(folds)) {
  idx <- folds[[k]]
  d <- train[-idx]
  oof <- train[idx]
  y <- d[["goal"]]
  for (v in fchars) {
    dic <- get_encoding(d[[v]], y)
    oof[, (v) := set_encoding(get(v), dic)]
  }
  train_encoded[[k]] <- oof
}
train_encoded <- rbindlist(train_encoded)
train <- train[folds_idx]
rm(k, idx, d, oof, y, v, dic)


# Matrix for xgb model ----------------------------------------------------

dtrain <- xgb.DMatrix(
  data = data.matrix(train_encoded[, ..features]),
  label = train_encoded$goal,
  missing = NA
)


# Bayesian parameter optimisation -----------------------------------------

xgb_cv_bayes <- function(gamma, max_depth, min_child_weight, subsample, colsample_bytree) {
  params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.1,
    gamma = gamma, max_depth = max_depth, min_child_weight = min_child_weight, subsample = subsample,
    colsample_bytree = colsample_bytree)
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 2000,
    early_stopping_rounds = 5,
    verbose = 0L,
    maximize = TRUE,
    showsd = TRUE,
    folds = folds,
    metrics = list("auc"),
    prediction = TRUE
  )
  
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
    Pred = cv$pred)
}

bounds <- list(
  gamma = c(0, 25),
  max_depth = c(1L, 12L),
  min_child_weight = c(1L, 15L),
  subsample = c(0.7, 1),
  colsample_bytree = c(0.7, 1)
)

xgb_bayes_model <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = bounds,
  init_grid_dt = NULL,
  init_points = 10,  # number of random points to start search
  n_iter = 20, # number of iterations after initial random points are set
  acq = 'ucb',
  kappa = 2.5,
  eps = 0.0,
  verbose = TRUE
)


# Use tuned parameters to find nrounds ------------------------------------

params <- as.list(xgb_bayes_model$Best_Par)
params <- c(params, list(booster = "gbtree", objective = "binary:logistic", eta = 0.01))

cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,
  early_stopping_rounds = 5,
  print_every_n = 25,
  folds = folds,
  metrics = list("auc", "rmse"),
  prediction = TRUE
)



# Train model with final parameters ---------------------------------------

mod <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = cv$best_iteration
)


# Create explainer --------------------------------------------------------

explainer <- buildExplainer(mod, dtrain, "binary", 0.5)


# CV and test predictions -------------------------------------------------

get_encoding <- function(x, y) {
  v <- aggregate(y, list(grp = x), mean, na.rm = FALSE)
  setNames(v$x, v$grp)
}

set_encoding <- function(x, z) {
  unname(z[x])
}

# Use training data to calculate test target encoding
test_encoded <- copy(test)
for (v in fchars) {
  dic <- get_encoding(train[[v]], train[["goal"]])
  test_encoded[, (v) := set_encoding(get(v), dic)]
}

dtest <- xgb.DMatrix(
  data = data.matrix(test_encoded[, ..features]),
  missing = NA
)

preds_test <- copy(test)
preds_test <- preds_test[, pred := predict(mod, dtest)][, .(shot_id, pred)]
preds_cv <- copy(train)
preds_cv <- preds_cv[, pred := cv$pred][, .(shot_id, pred)]
preds <- rbind(preds_cv, preds_test)


# Save outputs ------------------------------------------------------------

saveRDS(cv, here("models", "cv.rds"))
saveRDS(mod, here("models", "mod.rds"))
saveRDS(colnames(dtrain), here("models", "features.rds"))
saveRDS(dtrain, here("models", "dtrain.rds"))
saveRDS(dtest, here("models", "dtest.rds"))
saveRDS(xgb_bayes_model, here("models", "xgb_bayes_model.rds"))

saveRDS(preds, here("data", "processed", "predictions.rds"))
saveRDS(explainer, here("data", "processed", "explainer.rds"))

