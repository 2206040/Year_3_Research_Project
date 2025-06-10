library(ggplot2)
library(ggpubr)
# Your data vectors (corrected)
PCF11_WT_SC35 <- c(0.738, 0.424, 0.563, 0.632, 0.357)
DIDR1_SC35 <- c(0.708, 0.740, 0.526, 0.432, 0.546)
DIDR2_SC35 <- c(0.305, 0.413, 0.461, 0.465, 0.358)
DSpacer2_SC35 <- c(0.479, 0.657, 0.728, 0.514, 0.645)
DIDR1_DIDR2_SC35 <- c(0.463, 0.310, 0.267, 0.554, 0.603)
DIDR2_DSpacer_SC35 <- c(0.360, 0.512, 0.222, 0.063, 0.265)
# Combine into dataframe
group <- c(
  rep("WT", length(PCF11_WT_SC35)),
  rep("DIDR1", length(DIDR1_SC35)),
  rep("DIDR2", length(DIDR2_SC35)),
  rep("DSpacer2", length(DSpacer2_SC35)),
  rep("DIDR1_DIDR2", length(DIDR1_DIDR2_SC35)),
  rep("DIDR2_DSpacer", length(DIDR2_DSpacer_SC35))
)
value <- c(
  PCF11_WT_SC35,
  DIDR1_SC35,
  DIDR2_SC35,
  DSpacer2_SC35,
  DIDR1_DIDR2_SC35,
  DIDR2_DSpacer_SC35
)
df <- data.frame(
  group = factor(group, levels = c("WT", "DIDR1", "DIDR2", "DSpacer2", "DIDR1_DIDR2", "DIDR2_DSpacer")),
  value = value
)
# Define comparisons (each mutant vs WT)
my_comparisons <- list(
  c("WT", "DIDR1"),
  c("WT", "DIDR2"),
  c("WT", "DSpacer2"),
  c("WT", "DIDR1_DIDR2"),
  c("WT", "DIDR2_DSpacer")
)
# Mann-Whitney U tests and collect p-values
mutants_list <- list(
  DIDR1 = DIDR1_SC35,
  DIDR2 = DIDR2_SC35,
  DSpacer2 = DSpacer2_SC35,
  DIDR1_DIDR2 = DIDR1_DIDR2_SC35,
  DIDR2_DSpacer = DIDR2_DSpacer_SC35
)
raw_pvalues <- sapply(mutants_list, function(x) wilcox.test(x, PCF11_WT_SC35)$p.value)
adjusted_pvalues <- p.adjust(raw_pvalues, method = "BH")
results_df <- data.frame(
  Comparison = paste(names(mutants_list), "vs WT"),
  Raw_p_value = raw_pvalues,
  Adjusted_p_value = adjusted_pvalues
)
print(results_df)
# Plot boxplot with significance stars
ggboxplot(df, x = "group", y = "value", fill = "group", palette = "jco") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label = "p.signif") +
  labs(title = "Correlation Coefficients: Plasmid-derived PCF11 vs Nuclear Speckles (SC-35)",
       y = "Correlation Coefficient", x = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))