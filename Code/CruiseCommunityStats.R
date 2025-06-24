# ---- Load Required Libraries ----
# Libraries for data manipulation, plotting, statistical testing, and reading files

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
library(RColorBrewer)


# ---- Define Function: Test Biomass Differences by Cruise ----
# This function performs either ANOVA or Kruskal-Wallis tests across cruises
  # for each biomass-related variable provided.
# Optionally performs post-hoc tests (Tukey HSD for ANOVA, pairwise Wilcoxon for Kruskal).

test_biomass_by_cruise <- function(data, variables, method = "kruskal", pairwise = TRUE) {
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
df <- read.csv("Data/AllCruiseLysoTracker.csv")


# ---- Assess Normality for Each Biomass Variable ----

# Define variables to test
biomass_vars <- c("avsyn", "avnano", "avhetero")

# Perform Shapiro-Wilk test for normality on each biomass variable

shapiro_results <- lapply(biomass_vars, function(biomass_vars) {
  test <- shapiro.test(df[[biomass_vars]])
  data.frame(variable = biomass_vars, 
             W = test$statistic, 
             p.value = test$p.value)
})

# Combine Shapiro results into one data frame
shapiro_df <- do.call(rbind, shapiro_results)
print(shapiro_df)

# ---- Run Kruskal-Wallis Tests Across Cruises ----

# Use nonparametric Kruskal-Wallis test (since variables are not normal)
test_biomass_by_cruise(df, biomass_vars, method = "kruskal")


# ---- Bacteria Analysis (Separate File) ----

# Load bacterial concentration data (from flow cytometry or similar)

bacdf <- read_excel("Data/BacteriaConcentrations.xlsx")

# Test for normality in bacteria data
shapiro.test(bacdf$bacteria)

# Run Kruskal-Wallis test across cruises for bacterial abundance
test_biomass_by_cruise(bacdf, "bacteria", method="kruskal" )


# ---- Summarize Ranges and Averages for Biomass Metrics ----

# For nanoeukaryotes, heterotrophs, and Synechococcus
rangeav <- df %>%
  group_by(Cruise) %>%
  dplyr::summarise(nano=mean(avnano), sdnano=sd(avnano), 
                   minnano=min(avnano), maxnano=max(avnano), 
                   meanhetero=mean(avhetero, na.rm=TRUE),
                   sdhetero = sd(avhetero, na.rm = TRUE), 
                   minhetero = min(avhetero, na.rm = TRUE), 
                   maxhetero = max(avhetero, na.rm = TRUE),
                   syn=mean(avsyn), sdsyn=sd(avsyn), 
                   minsyn=min(avsyn), maxsyn=max(avsyn))
rangebac <- bacdf %>%
  group_by(Cruise) %>%
  dplyr::summarise(bac=mean(bacteria, na.rm = TRUE), 
                   sdbac=sd(bacteria, na.rm = TRUE), 
                   maxbac=max(bacteria, na.rm = TRUE), 
                   minbac=min(bacteria, na.rm = TRUE))
