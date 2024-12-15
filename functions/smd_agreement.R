smd_agreement <- function(rct_estimate = NULL,
                          rct_lower = NULL,
                          rct_upper = NULL,
                          rwe_estimate = NULL,
                          rwe_lower = NULL,
                          rwe_upper = NULL
                          ){
  
  # numerator
  numerator <- (rct_estimate - rwe_estimate)
  
  # variances
  var_rct <- (rct_upper - rct_lower)/(2*1.96)
  var_rwe <- (rwe_upper - rwe_lower)/(2*1.96)
  
  # denominator
  denominator <- sqrt(var_rct^2 + var_rwe^2)
  
  # smd
  smd <- numerator/denominator
  
  return(smd)
  
}

# examples (taken from DUPLICATE)
# smd_agreement <- smd_agreement(
#   rct_estimate = log(0.87),
#   rct_lower = log(0.78),
#   rct_upper = log(0.97),
#   rwe_estimate = log(0.82),
#   rwe_lower = log(0.87),
#   rwe_upper = log(0.76)
#   )
# 
# smd_agreement <- smd_agreement(
#   rct_estimate = log(0.83),
#   rct_lower = log(0.73),
#   rct_upper = log(0.95),
#   rwe_estimate = log(0.69),
#   rwe_lower = log(0.59),
#   rwe_upper = log(0.81)
#   )
# 
# smd_agreement <- smd_agreement(
#   rct_estimate = log(1),
#   rct_lower = log(0.89),
#   rct_upper = log(1.12),
#   rwe_estimate = log(0.81),
#   rwe_lower = log(0.76),
#   rwe_upper = log(0.86)
#   )
