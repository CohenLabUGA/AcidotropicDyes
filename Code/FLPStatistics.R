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
# This function performs either ANOVA or Kruskal-Wallis tests across cruises
# for each biomass-related variable provided.
# Optionally performs post-hoc tests (Tukey HSD for ANOVA, pairwise Wilcoxon for Kruskal).

test_grazing <- function(data, variables, method = "kruskal", pairwise = TRUE) {
  results <- list()
  
  for (var in variables) {
    formula <- as.formula(paste(var, "~ Cruise"))
    
    test_result <- switch(method,
                          "anova"   = aov(formula, data = data),
                          "kruskal" = kruskal.test(formula, data = data)
    )
    
    pval <- if (method == "anova") {
      summary(test_result)[[1]][["Pr(>F)"]][1]
    } else {
      test_result$p.value
    }
    
    significance <- case_when(
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      TRUE         ~ "ns"
    )
    
    results[[var]] <- list(test = method,
                           p.value = round(pval, 6),
                           significance = significance)
    
    if (pairwise && pval < 0.05) {
      results[[var]]$pairwise <- switch(method,
                                        "anova"   = TukeyHSD(test_result)$Cruise,
                                        "kruskal" = pairwise.wilcox.test(data[[var]], data$Cruise, p.adjust.method = "BH")$p.value
      )
    }
  }
  
  # Print summary
  cat("\n--- Biomass Cruise Comparison Results ---\n")
  walk2(names(results), results, function(var, r) {
    cat(sprintf("%s (%s): p = %.3g [%s]\n", var, r$test, r$p.value, r$significance))
    if (!is.null(r$pairwise)) {
      cat("    Significant pairwise differences:\n")
      sig_pw <- if (r$test == "anova") {
        r$pairwise[r$pairwise[, "p adj"] < 0.05, , drop = FALSE]
      } else {
        r$pairwise[r$pairwise < 0.05, , drop = FALSE]
      }
      if (length(sig_pw) == 0) {
        cat("    - None\n")
      } else {
        print(sig_pw)
      }
    }
  })
  
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

# ---- Run Kruskal-Wallis Tests ----
test_grazing(flpfcm, c("avpercent", "avconc", "avgrazing", "avnoBR"), method = "kruskal") # For NES FCM and CCS microscopy
test_grazing(flpmicroscopy, c("avpercent", "avgrazing"), method = "kruskal") #For NES microscopy and CCS microscopy
test_grazing(lysodf, c("avmixo", "avpercent"), method = "kruskal") # For LysoTracker values

# Test to see if FLP NES methods are significantly different
print(kruskal.test(avgrazing ~ Method, data = nesflp))

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

