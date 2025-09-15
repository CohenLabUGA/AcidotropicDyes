# ---- Load Required Libraries ----
# Libraries for data manipulation, plotting, statistical testing, and reading files
library(readxl)
library(tidyr)
library(dplyr)
library(car)
library(broom)


# ---- Define Function: Test Biomass Differences by Cruise ----
# This function performs either t.test or Wilcox tests across cruises
# for each biomass-related variable provided.
test_biomass_by_cruise <- function(data, variables, method = "t") {
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

# Load merged LysoTracker summary data for both cruises
df <- read.csv("Data/AllCruiseLysoTracker.csv")


# ---- Assess Normality for Each Biomass Variable ----

# Define variables to test
biomass_vars <- c("avsyn", "avnano", "avhetero")

# Perform Shapiro-Wilk test for normality on each biomass variable
shapiro_results <- lapply(biomass_vars, function(var) {
  test <- shapiro.test(df[[var]])
  data.frame(variable = var, 
             W = test$statistic, 
             p.value = test$p.value)
})
# Combine Shapiro results into one data frame
shapiro_df <- do.call(rbind, shapiro_results)
print(shapiro_df)
# All data is not normal

# ---- Run Wilcox Tests Across Cruises ----

# Use nonparametric Wilcox test (since variables are not normal)
test_biomass_by_cruise(df, biomass_vars, method = "wilcox")
#Wilcox is run for each biomass variable defined and reports if it's significantly different between NES and CCS cruises

# ---- Bacteria Analysis (Separate File) ----

# Load bacterial concentration data (from flow cytometry or similar)

bacdf <- read_excel("Data/BacteriaConcentrations.xlsx")

# Test for normality in bacteria data
shapiro.test(bacdf$bacteria)

# Run Kruskal-Wallis test across cruises for bacterial abundance
test_biomass_by_cruise(bacdf, "bacteria", method="wilcox" )


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

