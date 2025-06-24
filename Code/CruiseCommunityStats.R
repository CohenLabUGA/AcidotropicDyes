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

test_biomass_by_cruise <- function(data, variables, method = "kruskal", pairwise = TRUE) {
  results <- list()
  
  for (var in variables) {
    formula <- as.formula(paste(var, "~ Cruise"))
    
    test_result <- if (method == "anova") {
      aov(formula, data = data)
    } else {
      kruskal.test(formula, data = data)
    }
    
    pval <- if (method == "anova") summary(test_result)[[1]][["Pr(>F)"]][1] else test_result$p.value
    
    significance <- if (pval < 0.001) {
      "***"
    } else if (pval < 0.01) {
      "**"
    } else if (pval < 0.05) {
      "*"
    } else {
      "ns"
    }
    
    results[[var]] <- list(
      test = method,
      p.value = round(pval, 6),
      significance = significance
    )
    
    if (pairwise && method == "kruskal" && pval < 0.05) {
      pw <- pairwise.wilcox.test(data[[var]], data$Cruise, p.adjust.method = "BH")
      results[[var]]$pairwise <- pw$p.value
    } else if (pairwise && method == "anova" && pval < 0.05) {
      tukey <- TukeyHSD(test_result)
      results[[var]]$pairwise <- tukey$Cruise
    }
  }
  
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

df <- read.csv("Data/AllCruiseLysoTracker.csv")


shapiro_results <- lapply(biomass_vars, function(biomass_vars) {
  test <- shapiro.test(df[[biomass_vars]])
  data.frame(variable = biomass_vars, 
             W = test$statistic, 
             p.value = test$p.value)
})
shapiro_df <- do.call(rbind, shapiro_results)

print(shapiro_df)

biomass_vars <- c("avsyn", "avnano", "avhetero")
test_biomass_by_cruise(df, biomass_vars, method = "kruskal")

bacdf <- read_excel("Data/BacteriaConcentrations.xlsx")
shapiro.test(bacdf$bacteria)

test_biomass_by_cruise(bacdf, "bacteria", method="kruskal" )

### Get range and av for each variable summed across cruises
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
