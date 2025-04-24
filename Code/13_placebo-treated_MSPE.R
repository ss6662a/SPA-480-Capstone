
# Define pre and post periods (adjust if needed)
pre_period <- years < 2014
post_period <- years >= 2014

# Calculate treated gaps
gap_treated <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# MSPEs
mspe_treated_pre <- mean(gap_treated[pre_period]^2)
mspe_treated_post <- mean(gap_treated[post_period]^2)
mspe_ratio_treated <- mspe_treated_post / mspe_treated_pre



placebo_ratios <- c()

for (id in names(placebo_results)) {
  res <- placebo_results[[id]]
  gap_placebo <- res$dataprep$Y1plot - (res$dataprep$Y0plot %*% res$synth$solution.w)
  
  mspe_pre <- mean(gap_placebo[pre_period]^2)
  mspe_post <- mean(gap_placebo[post_period]^2)
  
  ratio <- mspe_post / mspe_pre
  placebo_ratios <- c(placebo_ratios, ratio)
}

# Combine ratios
all_ratios <- c(mspe_ratio_treated, placebo_ratios)

# Rank (lower rank = higher significance)
rank_treated <- sum(all_ratios >= mspe_ratio_treated)
pseudo_p_value <- rank_treated / length(all_ratios)

cat("Treated MSPE Ratio:", round(mspe_ratio_treated, 3), "\n")
cat("Number of placebos with equal or higher ratio:", rank_treated - 1, "\n")
cat("Total units:", length(all_ratios), "\n")
cat("Pseudo p-value:", round(pseudo_p_value, 3), "\n")



