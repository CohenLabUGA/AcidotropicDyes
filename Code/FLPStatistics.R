# ---- Load Required Libraries ----
# Libraries for data manipulation, statistical testing, and reading files

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(emmeans) 
library(multcomp)  


# ---- Define Function: Test Biomass Differences by Cruise ----
# This function performs either t-test or Wilcoxon tests across cruises
# for each biomass-related variable provided.

test_grazing <- function(data, variables, method = "t") {
  results <- list()
  
  for (var in variables) {
    y <- data[[var]]
    grp <- data$Cruise
    
    # Choose test
    if (method == "t") {
      test_result <- t.test(y ~ grp)
      pval <- test_result$p.value
      test_used <- "t-test"
    } else if (method == "wilcox") {
      test_result <- wilcox.test(y ~ grp)
      pval <- test_result$p.value
      test_used <- "Wilcoxon rank-sum"
    } else {
      stop("method must be either 't' or 'wilcox'")
    }
    
    # Significance stars
    significance <- if (pval < 0.05) {
      "*"
    } else {
      "ns"
    }
    
    # Save results
    results[[var]] <- list(
      test = test_used,
      p.value = round(pval, 6),
      significance = significance
    )
  }
  
  # Print summary
  cat("\n--- Biomass Cruise Comparison Results ---\n")
  for (var in names(results)) {
    r <- results[[var]]
    cat(paste0(var, " (", r$test, "): p = ", r$p.value, " [", r$significance, "]\n"))
  }
  
  return(invisible(results))
}
# ---- Read and Prepare Data ----
flp_all <- read_excel("Data/AllFLPData.xlsx")

flpfcm <- flp_all %>% filter(Cruise == "California Current System" |
                               (Cruise == "North East Shelf" & Method == "FlowCytometry"))

flpmicroscopy <- flp_all %>% filter(Cruise == "California Current System" |
                                      (Cruise == "North East Shelf" & Method == "Microscopy"))

nesflp <- flp_all %>% filter(Cruise == "North East Shelf")
lysodf <- read.csv("Data/AllCruiseLysoTracker.csv")

# All data are not normal, using Wilcox tests for all
# ---- Normality Testing ----
check_normality <- function(data, vars) {
  map_dfr(vars, ~{
    test <- shapiro.test(data[[.x]])
    tibble(variable = .x, W = test$statistic, p.value = test$p.value)
  })
}

shapiro_flpfcm <- check_normality(flpfcm, c("avpercent", "avconc", "avgrazing", "avnoBR"))
shapiro_flpmicroscopy <- check_normality(flpmicroscopy, c("avpercent", "avgrazing"))
shapiro_lysotracker <- check_normality(lysodf, c("avmixo", "avpercent"))

print(shapiro_flpfcm)
print(shapiro_flpmicroscopy)
print(shapiro_lysotracker)

# ---- Run Wilcox Tests ----
test_grazing(flpfcm, c("avpercent", "avconc", "avgrazing", "avnoBR"), method = "wilcox") # For NES FCM and CCS microscopy
test_grazing(flpmicroscopy, c("avpercent", "avgrazing"), method = "wilcox") #For NES microscopy and CCS microscopy
test_grazing(lysodf, c("avmixo", "avpercent"), method = "wilcox") # For LysoTracker values

# Test to see if FLP NES methods are significantly different
print(wilcox.test(avgrazing ~ Method, data = nesflp))

# Calculate ranges, averages, and standard deviations for variables in the FLP dataframe
flpdf <- read_excel("Data/AllFLPData.xlsx")
range_flp <- flpdf %>%
  group_by(Cruise, Method) %>%
  dplyr::summarise(mixoconc = mean(avconc, na.rm = TRUE),
                   sdconc = sd(avconc, na.rm = TRUE),
                   minconc = min(avconc, na.rm = TRUE),
                   maxconc = max(avconc, na.rm = TRUE),
                   
                   percent = mean(avpercent, na.rm = TRUE),
                   sdpercent = sd(avpercent, na.rm = TRUE),
                   minpercent = min(avpercent, na.rm = TRUE),
                   maxpercent = max(avpercent, na.rm = TRUE),
                   
                   grazing = mean(avgrazing, na.rm = TRUE),
                   sdgrazing = sd(avgrazing, na.rm = TRUE),
                   mingrazing = min(avgrazing, na.rm = TRUE),
                   maxgrazing = max(avgrazing, na.rm = TRUE),
                   
                   bacterivory = mean(avnoBR, na.rm = TRUE),
                   sdbacterivory = sd(avnoBR, na.rm = TRUE),
                   minbacterivory = min(avnoBR, na.rm = TRUE),
                   maxbacterivory = max(avnoBR, na.rm = TRUE))

# Calculate average, standard devation, and range for lysotracker mixotrophs
range_lyso <- lysodf %>%
  na.omit() %>%
  group_by(Cruise) %>%
  dplyr::summarise(mixoconc=mean(avmixo), sdmixoconc=sd(avmixo),
                   mixopercent=mean(avpercent), sdpercent=sd(avpercent), 
                   minmixo=min(avmixo), maxmixo=max(avmixo), 
                   minpercent=min(avpercent), maxpercent=max(avpercent))

