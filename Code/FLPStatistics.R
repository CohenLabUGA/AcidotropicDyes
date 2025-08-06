# ---- Load Required Libraries ----
# Libraries for data manipulation, statistical testing, and reading files

library(gtools)
library(grid)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)
library(dplyr)


# ---- Define Function: Test Biomass Differences by Cruise ----
# This function performs either ANOVA or Kruskal-Wallis tests across cruises
# for each biomass-related variable provided.
# Optionally performs post-hoc tests (Tukey HSD for ANOVA, pairwise Wilcoxon for Kruskal).

test_grazing <- function(data, variables, method = "kruskal", pairwise = TRUE) {
  results <- list()
  
  for (var in variables) {
    formula <- as.formula(paste(var, "~ Cruise"))
    
    # Perform ANOVA or Kruskal-Wallis
    test_result <- if (method == "anova") {
      aov(formula, data = data)
    } else {
      kruskal.test(formula, data = data)
    }
    # Extract p-value
    pval <- if (method == "anova") summary(test_result)[[1]][["Pr(>F)"]][1] else test_result$p.value
    
    # Determine significance symbol
    significance <- if (pval < 0.001) {
      "***"
    } else if (pval < 0.01) {
      "**"
    } else if (pval < 0.05) {
      "*"
    } else {
      "ns"
    }
    
    # Save results
    results[[var]] <- list(
      test = method,
      p.value = round(pval, 6),
      significance = significance
    )
    
    # Post-hoc pairwise testing
    if (pairwise && method == "kruskal" && pval < 0.05) {
      pw <- pairwise.wilcox.test(data[[var]], data$Cruise, p.adjust.method = "BH")
      results[[var]]$pairwise <- pw$p.value
    } else if (pairwise && method == "anova" && pval < 0.05) {
      tukey <- TukeyHSD(test_result)
      results[[var]]$pairwise <- tukey$Cruise
    }
  }
  
  # Print summary to console
  cat("\n--- Biomass Cruise Comparison Results ---\n")
  for (var in names(results)) {
    r <- results[[var]]
    cat(paste0(var, " (", r$test, "): p = ", r$p.value, " [", r$significance, "]\n"))
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
  }
  return(invisible(results))
}

# ---- Read and Prepare Data ----
# Load merged LysoTracker summary data for both cruises
flpfcm <- read_excel("Data/AllFLPData.xlsx") %>%
  filter(Cruise == "California Current System" |
           (Cruise == "North East Shelf" & Method == "FlowCytometry"))

flpmicroscopy <- read_excel("Data/AllFLPData.xlsx") %>%
  filter(Cruise == "California Current System" |
           (Cruise == "North East Shelf" & Method == "Microscopy"))

nesflp <- read_excel("Data/AllFLPData.xlsx") %>%
  filter(Cruise == "North East Shelf")

lysodf <- read.csv("Data/AllCruiseLysoTracker.csv")


# ---- Assess Normality for Each Biomass Variable ----

# Define variables to test
flpfcmvars <- c("avpercent", "avconc", "avgrazing", "avnoBR")
flpmicroscopyvars <-  c("avpercent", "avgrazing")
lysovars <- c("avmixo", "avpercent")

# Perform Shapiro-Wilk test for normality on each biomass variable

shapiro_flpfcm <- lapply(flpfcmvars, function(flpfcmvars) {
  test <- shapiro.test(flpfcm[[flpfcmvars]])
  data.frame(variable = flpfcmvars, 
             W = test$statistic, 
             p.value = test$p.value)
})

shapiro_flpmicroscopy <- lapply(flpmicroscopyvars, function(flpmicroscopyvars) {
  test <- shapiro.test(flpmicroscopy[[flpmicroscopyvars]])
  data.frame(variable = flpmicroscopyvars, 
             W = test$statistic, 
             p.value = test$p.value)
})

shapiro_lysotracker <- lapply(lysovars, function(lysovars) {
  test <- shapiro.test(lysodf[[lysovars]])
  data.frame(variable = lysovars, 
             W = test$statistic, 
             p.value = test$p.value)
})

shapiro.test(nesflp$avgrazing)

# Combine Shapiro results into one data frame
# Determine if data are normally distributed or not
shapiro_flpfcm <- do.call(rbind, shapiro_flpfcm)
print(shapiro_flpfcm)

shapiro_flpmicroscopy <- do.call(rbind, shapiro_flpmicroscopy)
print(shapiro_flpmicroscopy)

shapiro_lysotracker <- do.call(rbind, shapiro_lysotracker)
print(shapiro_lysotracker)

# ---- Run Kruskal-Wallis Tests Across Cruises ----
# Use nonparametric Kruskal-Wallis test (since variables are not normal)
test_grazing(flpfcm, flpfcmvars, method = "kruskal")
test_grazing(flpmicroscopy, flpmicroscopyvars, method = "kruskal")
test_grazing(lysodf, lysovars, method="kruskal")
kruskal_test_result <- kruskal.test(avgrazing ~ Method, data = nesflp)
print(kruskal_test_result)

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

