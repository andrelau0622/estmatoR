# 加载包
library(estmatoR)

# 计算随机对照试验（均数比较）的样本量并可视化
n_rct_mean <- calc_rct_mean_sample_size(mu1 = 50, mu2 = 55, sd = 10)

# 计算随机对照试验（率比较）的样本量并可视化
n_rct_proportion <- calc_rct_proportion_sample_size(p1 = 0.5, p2 = 0.6)

# 计算队列研究（率比较）的样本量并可视化
n_cohort_proportion <- calc_cohort_proportion_sample_size(p1 = 0.4, p2 = 0.5)

# 计算病例对照研究的样本量并可视化
n_case_control <- calc_case_control_sample_size(p1 = 0.3, p2 = 0.4)

# 计算配对设计（均数比较）的样本量并可视化
n_paired_design <- calc_paired_design_mean_sample_size(mu1 = 20, mu2 = 22, sd = 3)

# 计算优效性试验的样本量并可视化
n_superiority_test <- calc_superiority_test_sample_size(p1 = 0.5, p2 = 0.7)

# 计算诊断试验的样本量并可视化
n_diagnostic_test <- calc_diagnostic_test_sample_size(sensitivity = 0.85, specificity = 0.90)

# 计算等效性试验的样本量并可视化
n_equivalence_test <- calc_equivalence_test_sample_size(p1 = 0.5, p2 = 0.6)

# 计算非劣效性试验的样本量并可视化
n_non_inferiority_test <- calc_non_inferiority_test_sample_size(p1 = 0.6, p2 = 0.4)




