# R/sample_size.R

# 加载必要的包
library(pwr)
library(ggplot2)

#' 计算随机对照试验的均数比较样本量并可视化
#'
#' @param mu1 第一组均值
#' @param mu2 第二组均值
#' @param sd 标准差
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_rct_mean_sample_size <- function(mu1, mu2, sd, alpha = 0.05, power = 0.80) {
  n <- pwr.t.test(d = (mu2 - mu1) / sd, sig.level = alpha, power = power, type = "two.sample")$n
  
  # 可视化样本量
  sd_values <- seq(1, 10, by = 0.5)
  sample_sizes <- sapply(sd_values, function(s) pwr.t.test(d = (mu2 - mu1) / s, sig.level = alpha, power = power, type = "two.sample")$n)
  
  df <- data.frame(sd = sd_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = sd, y = sample_size)) +
    geom_line(color = "blue") +
    labs(title = "随机对照试验：样本量随标准差变化的图", x = "标准差 (sd)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算随机对照试验的率比较样本量并可视化
#'
#' @param p1 第一组成功率
#' @param p2 第二组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_rct_proportion_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "blue") +
    labs(title = "随机对照试验：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算横断面调查（定性资料）的样本量并可视化
#'
#' @param p 预期成功率
#' @param alpha 显著性水平
#' @return 所需样本量
#' @export
calc_cross_sectional_proportion_sample_size <- function(p, alpha = 0.05) {
  n <- ceiling((qnorm(1 - alpha / 2)^2 * p * (1 - p)) / (0.05^2))
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- ceiling((qnorm(1 - alpha / 2)^2 * p_values * (1 - p_values)) / (0.05^2))
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "blue") +
    labs(title = "横断面调查（定性资料）：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算横断面调查（定量资料）的样本量并可视化
#'
#' @param mu 预期均值
#' @param sd 预期标准差
#' @param alpha 显著性水平
#' @return 所需样本量
#' @export
calc_cross_sectional_mean_sample_size <- function(mu, sd, alpha = 0.05) {
  n <- ceiling((qnorm(1 - alpha / 2) * sd / 0.05)^2)
  
  # 可视化样本量
  sd_values <- seq(1, 10, by = 0.5)
  sample_sizes <- ceiling((qnorm(1 - alpha / 2) * sd_values / 0.05)^2)
  
  df <- data.frame(sd = sd_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = sd, y = sample_size)) +
    geom_line(color = "red") +
    labs(title = "横断面调查（定量资料）：样本量随标准差变化的图", x = "标准差 (sd)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算队列研究（率比较）的样本量并可视化
#'
#' @param p1 第一组成功率
#' @param p2 第二组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_cohort_proportion_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "green") +
    labs(title = "队列研究：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算病例对照研究的样本量并可视化
#'
#' @param p1 病例组成功率
#' @param p2 对照组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_case_control_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.2p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.2p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "purple") +
    labs(title = "病例对照研究：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算配对设计（均数比较）的样本量并可视化
#'
#' @param mu1 第一组均值
#' @param mu2 第二组均值
#' @param sd 配对差标准差
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_paired_design_mean_sample_size <- function(mu1, mu2, sd, alpha = 0.05, power = 0.80) {
  n <- pwr.t.test(d = (mu2 - mu1) / sd, sig.level = alpha, power = power, type = "paired")$n
  
  # 可视化样本量
  sd_values <- seq(1, 10, by = 0.5)
  sample_sizes <- sapply(sd_values, function(s) pwr.t.test(d = (mu2 - mu1) / s, sig.level = alpha, power = power, type = "paired")$n)
  
  df <- data.frame(sd = sd_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = sd, y = sample_size)) +
    geom_line(color = "orange") +
    labs(title = "配对设计：样本量随标准差变化的图", x = "标准差 (sd)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算优效性试验的样本量并可视化
#'
#' @param p1 第一组成功率
#' @param p2 第二组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_superiority_test_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "cyan") +
    labs(title = "优效性试验：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算诊断试验的样本量并可视化
#'
#' @param sensitivity 灵敏度
#' @param specificity 特异度
#' @param alpha 显著性水平
#' @return 所需样本量
#' @export
calc_diagnostic_test_sample_size <- function(sensitivity, specificity, alpha = 0.05) {
  n <- ceiling((qnorm(1 - alpha / 2)^2 * (sensitivity * (1 - specificity))) / (0.05^2))
  
  # 可视化样本量
  sen_values <- seq(0.1, 0.9, by = 0.1)
  sample_sizes <- ceiling((qnorm(1 - alpha / 2)^2 * (sen_values * (1 - specificity))) / (0.05^2))
  
  df <- data.frame(sensitivity = sen_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = sensitivity, y = sample_size)) +
    geom_line(color = "magenta") +
    labs(title = "诊断试验：样本量随灵敏度变化的图", x = "灵敏度", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算等效性试验的样本量并可视化
#'
#' @param p1 第一组成功率
#' @param p2 第二组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_equivalence_test_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "brown") +
    labs(title = "等效性试验：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}

#' 计算非劣效性试验的样本量并可视化
#'
#' @param p1 第一组成功率
#' @param p2 第二组成功率
#' @param alpha 显著性水平
#' @param power 检验功效
#' @return 所需样本量
#' @export
calc_non_inferiority_test_sample_size <- function(p1, p2, alpha = 0.05, power = 0.80) {
  n <- pwr.p.test(h = ES.h(p1, p2), sig.level = alpha, power = power)$n
  
  # 可视化样本量
  p_values <- seq(0.01, 0.99, by = 0.01)
  sample_sizes <- sapply(p_values, function(p) pwr.p.test(h = ES.h(p, p2), sig.level = alpha, power = power)$n)
  
  df <- data.frame(p = p_values, sample_size = sample_sizes)
  
  ggplot(df, aes(x = p, y = sample_size)) +
    geom_line(color = "yellow") +
    labs(title = "非劣效性试验：样本量随成功率变化的图", x = "成功率 (p)", y = "样本量 (n)") +
    theme_minimal()
  
  return(n)
}
