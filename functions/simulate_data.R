#' Simulates and artifical FLAURA EHR-derived dataset
#'
#' @description
#' Parameterized function to quickly create an artificial FLAURA
#' EHR-derived analytic cohort for analytic code development.
#'
#' @details
#' ...
#'
#' @param n_total integer, number of total patients
#' @param seed integer, seed for reproducibility
#' @param include_id logical, include a generated patientid variable
#' @param imposeNA logical, set covariates to missing
#' @param propNA numeric, proportion of missingness, needs to be between 0 and 1
#' 
#' @return data frame with simulated analytic cohort
#'
#' @importFrom dplyr select rename mutate
#' @importFrom tibble tibble
#' @importFrom mice ampute
#' @importFrom stats rbinom rnorm
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(encore.io)
#'
#' data_miss <- simulate_flaura(
#'   n_total = 3500, 
#'   seed = 41, 
#'   include_id = FALSE, 
#'   imposeNA = TRUE,
#'   propNA = .33
#'   )
#'   
#' head(data_miss)
#' 
#'}

simulate_data <- function(n_total = 3500, 
                          seed = 42,
                          include_id = TRUE,
                          imposeNA = TRUE,
                          propNA = NULL
                          ){
  
  # input checks
  assertthat::assert_that(inherits(n_total, "numeric"), msg = "<n_total> needs to be an integer")
  assertthat::assert_that(inherits(seed, "numeric"), msg = "<seed> needs to be an integer")
  assertthat::assert_that(inherits(include_id, "logical"), msg = "<seed> needs to be a logical")
  assertthat::assert_that(inherits(imposeNA, "logical"), msg = "<imposeNA> needs to be a logical")
  if(!is.null(propNA)) assertthat::assert_that(inherits(propNA, "numeric"), msg = "<propNA> needs to be numeric")
  if(!is.null(propNA)) assertthat::assert_that(propNA >= 0 & propNA <= 1, msg = "<propNA> needs to be a numeric between 0 and 1")
  if(!is.null(propNA)) assertthat::assert_that(imposeNA == TRUE, msg = "<propNA> is specified but imposeNA is FALSE")
  
  # set the seed
  set.seed(seed)
  
  # Build the total cohort
  cohort <- tibble::tibble(
    # patient id
    patientid = paste0("Patient", seq(1, n_total, 1)),
    # age
    dem_age_index_cont = stats::rnorm(n = n_total, mean = 69, sd = 8),
    # sex/male
    dem_sex_cont = stats::rbinom(n = n_total, size = 1, prob = .33),
    # smoking
    c_smoking_history = sample(c(TRUE, FALSE), size = n_total, replace = TRUE, prob = c(.44, .56)),
    # num met sites
    c_number_met_sites = sample(c(1, 2, 3, 4, 5), size = n_total, replace = TRUE, prob = c(.73, .22, .035, .006, .001)),
    # c_ecog_cont
    c_ecog_cont = stats::rbinom(n = n_total, size = 1, prob = .57),
    # c_stage_initial_dx_cont
    c_stage_initial_dx_cont = sample(c(4, 3, 2, 1), size = n_total, replace = TRUE, prob = c(.76, 0.2, 0.028, 0.012)),
    # dem_race
    dem_race = factor(sample(c("Asian", "White", "Other"), size = n_total, replace = TRUE, prob = c(.36, .62, .02))),
    # dem_region
    dem_region = factor(sample(c("Midwest", "Northeast", "South", "West"), size = n_total, replace = TRUE, prob = c(.14, .20, .39, .27))),
    # dem_ses
    dem_ses = sample(c(1, 2, 3, 4, 5), size = n_total, replace = TRUE, prob = c(.12, .15, .19, .24, .29)),
    # c_hemoglobin_g_dl_cont
    c_hemoglobin_g_dl_cont = stats::rnorm(n = n_total, mean = 12.90, sd = 1.3),
    # c_urea_nitrogen_mg_dl_cont
    c_urea_nitrogen_mg_dl_cont = stats::rnorm(n = n_total, mean = 2.71, sd = 0.3),
    # c_platelets_10_9_l_cont
    c_platelets_10_9_l_cont = stats::rnorm(n = n_total, mean = 259, sd = 53),
    # c_calcium_mg_dl_cont
    c_calcium_mg_dl_cont = stats::rnorm(n = n_total, mean = 2.23, sd = 0.04),
    # c_glucose_mg_dl_cont
    c_glucose_mg_dl_cont = stats::rnorm(n = n_total, mean = 4.64, sd = 0.11),
    # c_lymphocyte_leukocyte_ratio_cont
    c_lymphocyte_leukocyte_ratio_cont = stats::rnorm(n = n_total, mean = 2.87, sd = 0.17),
    # c_alp_u_l_cont
    c_alp_u_l_cont = stats::rnorm(n = n_total, mean = 4.48, sd = 0.13),
    # c_protein_g_l_cont
    c_protein_g_l_cont = stats::rnorm(n = n_total, mean = 68, sd = 4),
    # c_alt_u_l_cont
    c_alt_u_l_cont = stats::rnorm(n = n_total, mean = 2.89, sd = 0.33),
    # c_albumin_g_l_cont
    c_albumin_g_l_cont = stats::rnorm(n = n_total, mean = 40, sd = 3),
    # c_bilirubin_mg_dl_cont
    c_bilirubin_mg_dl_cont = stats::rnorm(n = n_total, mean = -0.92, sd = 1.28),
    # c_chloride_mmol_l_cont
    c_chloride_mmol_l_cont = stats::rnorm(n = n_total, mean = 102.0, sd = 3),
    # c_monocytes_10_9_l_cont
    c_monocytes_10_9_l_cont = stats::rnorm(n = n_total, mean = -0.51, sd = 0.25),
    # c_eosinophils_leukocytes_ratio_cont
    c_eosinophils_leukocytes_ratio_cont = stats::rnorm(n = n_total, mean = 0.69, sd = 0.59),
    # c_ldh_u_l_cont
    c_ldh_u_l_cont = stats::rnorm(n = n_total, mean = 1.69, sd = 0.05),
    # c_hr_cont
    c_hr_cont = stats::rnorm(n = n_total, mean = 4.43, sd = 0.04),
    # c_sbp_cont
    c_sbp_cont = stats::rnorm(n = n_total, mean = 4.85, sd = 0.09),
    # c_oxygen_cont
    c_oxygen_cont = stats::rnorm(n = n_total, mean = 97.00, sd = 0.01),
    # c_neutrophil_lymphocyte_ratio_cont
    c_neutrophil_lymphocyte_ratio_cont = stats::rnorm(n = n_total, mean = 1.29, sd = 0.38),
    # c_bmi_cont
    c_bmi_cont = stats::rnorm(n = n_total, mean = 3.23, sd = 0.13),
    # c_ast_alt_ratio_cont
    c_ast_alt_ratio_cont = stats::rnorm(n = n_total, mean = 0.12, sd = 0.29),
    # c_time_dx_to_index
    c_time_dx_to_index = stats::rnorm(n = n_total, mean = 44, sd = 17),
    # c_de_novo_mets_dx
    c_de_novo_mets_dx = sample(c(TRUE, FALSE), size = n_total, replace = TRUE, prob = c(.79, .21)),
    # c_height_cont
    c_height_cont = stats::rnorm(n = n_total, mean = 1.65, sd = 0.08),
    # c_weight_cont
    c_weight_cont = stats::rnorm(n = n_total, mean = 69, sd = 11),
    # c_dbp_cont
    c_dbp_cont = stats::rnorm(n = n_total, mean = 76, sd = 6),
    # c_year_index
    c_year_index = factor(sample(c("<2018", "2018+"), size = n_total, replace = TRUE, prob = c(.95, .05)))
    )
  
  # assign treatment probabilities
  cohort <- cohort |> 
    dplyr::mutate(
      # treat (to simplify categorical variables don't get betas)
      treat = stats::rbinom(
        n = n_total, 
        size = 1, 
        prob = locfit::expit(
          -5.8 +
          dem_age_index_cont * log(1.0071) + 
          dem_sex_cont * log(0.9804) + 
          c_smoking_history * log(0.7065) + 
          c_number_met_sites * log(0.9982) + 
          c_hemoglobin_g_dl_cont * log(1.0483) + 
          c_urea_nitrogen_mg_dl_cont * log(0.9198) + 
          c_platelets_10_9_l_cont * log(0.9998) + 
          c_calcium_mg_dl_cont * log(0.9118) + 
          c_glucose_mg_dl_cont * log(1.0954) + 
          c_lymphocyte_leukocyte_ratio_cont * log(1.1192) + 
          c_alp_u_l_cont * log(1.3126) + 
          c_protein_g_l_cont * log(1.0002) + 
          c_alt_u_l_cont * log(0.9679) + 
          c_albumin_g_l_cont * log(1.0296) + 
          c_bilirubin_mg_dl_cont * log(0.9126) + 
          c_chloride_mmol_l_cont * log(1.0076) + 
          c_monocytes_10_9_l_cont * log(1.3919) + 
          c_eosinophils_leukocytes_ratio_cont * log(0.9567) + 
          c_ldh_u_l_cont * log(0.8804) + 
          c_hr_cont * log(1.2264) + 
          c_sbp_cont * log(1.0647) + 
          c_oxygen_cont * log(1.00064) + 
          c_ecog_cont * log(0.7857) + 
          c_neutrophil_lymphocyte_ratio_cont * log(1.0758) + 
          c_bmi_cont * log(1.061) + 
          c_ast_alt_ratio_cont * log(1.0756) + 
          c_stage_initial_dx_cont * log(1.2177) + 
          c_time_dx_to_index * log(1.0001)
          )
        )
      )
  
  #summary(cohort$treat)
  
  # simulate HTE by race
  
  ## Asian patients
  cohort_Asian <- cohort |> 
    dplyr::filter(dem_race == "Asian")
  
  # assign betas for hazard model
  betas_os_Asian <- c(
    treat = log(1),
    dem_sex_cont = log(0.79),
    dem_age_index_cont = log(1.02),
    c_smoking_history = log(.70),
    c_ecog_cont = log(.7)
    )
  
  set.seed(seed)
  cohort_outcome_Asian <- cohort_Asian |> 
    dplyr::bind_cols(
      simsurv::simsurv(
        dist = "weibull",
        gammas = 1.2,
        lambdas = 0.01,
        betas = betas_os_Asian,
        x = cohort_Asian,
        maxt = 120
        )
      ) |> 
    dplyr::select(-id) |> 
    dplyr::rename(
      fu_itt_months = eventtime,
      death_itt = status
      )
  
  ## Non-Asian patients
  cohort_nonAsian <- cohort |> 
    dplyr::filter(dem_race != "Asian")
  
  betas_os_nonAsian <- c(
    treat = log(0.54),
    dem_sex_cont = log(0.79),
    dem_age_index_cont = log(1.02),
    c_smoking_history = log(.70),
    c_ecog_cont = log(.7)
    )
  
  set.seed(seed)
  cohort_outcome_nonAsian <- cohort_nonAsian |> 
    dplyr::bind_cols(
      simsurv::simsurv(
        dist = "weibull",
        gammas = 1.2,
        lambdas = 0.01,
        betas = betas_os_nonAsian,
        x = cohort_nonAsian,
        maxt = 120
      )
    ) |> 
    dplyr::select(-id) |> 
    dplyr::rename(
      fu_itt_months = eventtime,
      death_itt = status
      )
  
  # combine Asian and non-Asian
  cohort_outcome <- rbind(cohort_outcome_Asian, cohort_outcome_nonAsian)
  
  # summary(cohort_outcome$death_itt)
  # summary(cohort_outcome$fu_itt_months)
  # hist(cohort_outcome$fu_itt_months)
  
  if(!include_id) cohort_outcome <- cohort_outcome |> dplyr::select(-patientid)
  
  if(imposeNA){
    
    varComplete <-  c("patientid", "treat", "c_year_index", "fu_itt_months", "death_itt", "dem_age_index_cont", "dem_sex_cont")
    varNA <- setdiff(names(cohort_outcome), varComplete)
    
    # define column numbers for pattern and weight determination
    colsNA <- which(colnames(cohort_outcome) %in% varNA)
    
    # define missingness pattern
    default_pattern <- rep(1, ncol(cohort_outcome))
    pattern <- replace(default_pattern, colsNA, 0)
    
    set.seed(seed)
    cohort_outcome <- mice::ampute(
      data = cohort_outcome, 
      prop = propNA,
      patterns = pattern,
      mech = "MCAR"
      )$amp
    
  }
  
  return(cohort_outcome)
  
}

