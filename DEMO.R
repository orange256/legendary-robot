# install packages
if(!require(devtools)) {
  install.packages("devtools"); require(devtools)}

devtools::install_github("ml2lab/seafood")
library(seafood)

# DEMO ==========================================================

# Step 1: Construct a 1-order CFA model ----
CFA_model <-'f1 =~ V1 + V2 +V3
f2 =~ V4 + V5 +V6
f3 =~ V7 + V8 +V9
f4 =~ V10 + V11 +V12
f5 =~ V13 + V14 +V15
f6 =~ V16 + V17 +V18
'

# Step 2-1: choose 'full' model method ----------------------------
full_result <- thx_seafood(data = demo_data,
                           model = CFA_model,
                           eta = c('f1','f2','f3'),
                           ksi = c('f4','f5','f6'),
                           method = "full"
                           )
# see model fit
summary(full_result$fit, fit.measures=TRUE)
# see new data
full_data <- full_result$fscore_2inter

# Step 2-2: choose 'marsh' model method ---------------------------
# add ID
ID_demo_data <- demo_data %>% cbind(abc=1:nrow(demo_data),.)

marsh_result <- thx_seafood(data = ID_demo_data,
                            model = CFA_model,
                            eta = c('f1','f2','f3'),
                            ksi = c('f4','f5','f6'),
                            method = "marsh",
                            ID = "abc"
                            )
summary(marsh_result$fit, fit.measures=TRUE)
marsh_data <- marsh_result$fscore_2inter

# Step 2-3: choose 'cross' model method -----------------------------------
# add ID
ID_missing <- demo_data %>% cbind(abc=1:nrow(demo_data),.)

# erasing some data (set to NA)
ID_missing[3,3] <- 888
ID_missing[4,2] <- NA

cross_result <- thx_seafood(data = ID_missing,
                            model = CFA_model,
                            eta = c('f1','f2','f3'),
                            ksi = c('f4','f5','f6'),
                            method = "cross",
                            product = c('f1f5','f2f4','f3f5'),
                            ID = "abc",
                            missing_value = 888
                            )

summary(cross_result$fit, fit.measures=TRUE)
cross_data <- cross_result$fscore_2inter

# Step 2-4: choose 'lms' model method -----------------------------------

# file will save in "D:/Mplus_in_R"
lms_result <- thx_seafood(data = ID_missing,
                          model = CFA_model,
                          eta = c('f1','f2','f3'),
                          ksi = c('f4','f5','f6'),
                          method = "lms",
                          DV = "V19",
                          ID = "abc",
                          missing_value = 888
                          )


lms_result$Summaries
lms_result$Parameters
lms_data <- lms_result$fscore_2inter


# end =====================================================================
