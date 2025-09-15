# ---- Load Required Libraries ----
# Libraries for data manipulation, plotting, statistical testing, and reading files
library(readxl)
library(tidyr)
library(dplyr)
library(car)
library(broom)


# ---- Define Function: Test Biomass Differences by Cruise ----
# This function performs either ANOVA or Kruskal-Wallis tests across cruises
# for each biomass-related variable provided.
test_biomass_by_cruise <- function(data, variables, method = "kruskal") {
  results <- list()
  
  data$Cruise <- as.factor(data$Cruise)  # ensure factor
  
  for (var in variables) {
    formula <- as.formula(paste(var, "~ Cruise"))
    
    # Perform test
    if (method == "anova") {
      test_result <- aov(formula, data = data)
      pval <- summary(test_result)[[1]][["Pr(>F)"]][1]
    } else {
      test_result <- kruskal.test(formula, data = data)
      pval <- test_result$p.value
    }
    
    # Significance stars
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
  }
  
  # Print summary once
  cat("\n--- Biomass Cruise Comparison Results ---\n")
  for (var in names(results)) {
    r <- results[[var]]
    cat(paste0(var, " (", r$test, "): p = ", r$p.value, " [", r$significance, "]\n"))
  }
  
  return(invisible(results))
}
# Combine Shapiro results into one data frame
shapiro_df <- do.call(rbind, shapiro_results)
print(shapiro_df)
# All data is not normal, running non-parametric tests (Kruskal-Wallis)
# ---- Run Kruskal-Wallis Tests Across Cruises ----

# Use nonparametric Kruskal-Wallis test (since variables are not normal)
test_biomass_by_cruise(df, biomass_vars, method = "kruskal")
#kruskal-wallis is run for each biomass variable defined and reports if it's significantly different between NES and CCS cruises

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

