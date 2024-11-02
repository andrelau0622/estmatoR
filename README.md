# estmatoR

`estmatoR` 是一个 R 包，用于计算九种不同类型的研究设计的样本量，包括随机对照试验、横断面调查、队列研究、病例对照研究、配对设计、优效性试验、诊断试验、等效性试验和非劣性试验。

## 研究类型简介

1. **随机对照试验（RCT）**：一种实验研究，随机分配参与者到试验组和对照组，以评估干预措施的效果。
   - **适用范围**：临床试验、药物试验。
   - **样本量计算公式**：
     - 比率比较：\[ n = \frac{(Z_{\alpha/2} + Z_{\beta})^2 \cdot (p_1(1-p_1) + p_2(1-p_2))}{(p_1 - p_2)^2} \]
     - 均数比较：\[ n = \frac{(Z_{\alpha/2} + Z_{\beta})^2 \cdot (2\sigma^2)}{( \mu_1 - \mu_2)^2} \]

2. **横断面调查**：在特定时间点对人群进行调查，收集定性和定量数据。
   - **适用范围**：流行病学研究、社会学研究。
   - **样本量计算公式**：
     - 定性资料：\[ n = \frac{Z^2 \cdot p(1-p)}{E^2} \]
     - 定量资料：\[ n = \frac{(Z_{\alpha/2} \cdot \sigma)^2}{E^2} \]

3. **队列研究**：跟踪一组人（队列），在时间上观察某些特征对结果的影响。
   - **适用范围**：长期健康研究、临床观察。
   - **样本量计算公式**：\[ n = \frac{(Z_{\alpha/2} + Z_{\beta})^2 \cdot p(1-p)}{(p_1 - p_2)^2} \]

4. **病例对照研究**：对比已有疾病的个体（病例）和无疾病个体（对照），回顾性评估潜在的影响因素。
   - **适用范围**：病因研究、流行病学调查。
   - **样本量计算公式**：\[ n = \frac{(Z_{\alpha/2} + Z_{\beta})^2 \cdot (p_{case}(1-p_{case}) + p_{control}(1-p_{control}))}{(p_{case} - p_{control})^2} \]

5. **配对设计**：将参与者成对分组，比较每对中的干预效果。
   - **适用范围**：小样本研究、临床试验。
   - **样本量计算公式**：\[ n = \frac{(Z_{\alpha/2} + Z_{\beta})^2 \cdot \sigma_d^2}{d^2} \]

6. **优效性试验**：评估干预措施是否优于现有标准。
   - **适用范围**：药物试验、治疗效果评估。
   - **样本量计算公式**：同 RCT 的公式。

7. **诊断试验**：评估测试的敏感性和特异性。
   - **适用范围**：医学诊断、筛查研究。
   - **样本量计算公式**：\[ n = \frac{(Z_{\alpha/2})^2 \cdot (p(1-p))}{(d^2)} \]

8. **等效性试验**：评估新干预措施是否与现有干预措施在效果上等效。
   - **适用范围**：药物替代研究。
   - **样本量计算公式**：与 RCT 类似，需设定等效界限。

9. **非劣效性试验**：证明新干预措施的效果不劣于现有措施的效果。
   - **适用范围**：新药开发、临床试验。
   - **样本量计算公式**：同等效性试验。

## 样本量计算原理

样本量计算的原理基于统计学，考虑以下因素：
- **效应大小**：干预措施或变量之间的预期差异。
- **显著性水平（α）**：通常设置为 0.05，表示接受错误拒绝原假设的概率。
- **检验功效（1-β）**：通常设置为 0.80 或 0.90，表示检验能够正确拒绝错误的原假设的概率。
- **样本变异性**：样本数据的分散程度。

根据不同的研究设计，`estmatoR` 包提供了相应的样本量计算方法。

## 包使用演示

### 安装包

首先，安装 `estmatoR` 包：

```R
# 安装 devtools 包，如果还没有安装
install.packages("devtools")

# 从本地或 GitHub 安装 estmatoR 包
devtools::install_github("andrelau0622/estmatoR")
library(estmatoR)

# 计算随机对照试验（率和均数比较）的样本量
n_rct_proportion <- calc_rct_proportion_sample_size(p1 = 0.5, p2 = 0.7)
n_rct_mean <- calc_rct_mean_sample_size(mu1 = 10, mu2 = 12, sd = 2)

# 计算横断面调查（定性资料）的样本量
n_cross_sectional_proportion <- calc_cross_sectional_proportion_sample_size(p = 0.5)
n_cross_sectional_mean <- calc_cross_sectional_mean_sample_size(mu = 10, sd = 2)

# 计算队列研究的样本量
n_cohort <- calc_cohort_sample_size(p = 0.3)

# 计算病例对照研究的样本量
n_case_control <- calc_case_control_sample_size(p_case = 0.4, p_control = 0.2)

# 计算配对设计的样本量
n_matched <- calc_matched_sample_size(d = 0.5, sd = 1)

# 计算优效性试验的样本量
n_superiority <- calc_superiority_sample_size(mu1 = 10, mu2 = 12, sd = 2)

# 计算诊断试验的样本量
n_diagnostic <- calc_diagnostic_sample_size(sensitivity = 0.8, specificity = 0.9)

# 计算等效性试验的样本量
n_equivalence <- calc_equivalence_sample_size(mu1 = 10, mu2 = 12, sd = 2, delta = 0.5)

# 计算非劣效性试验的样本量
n_non_inferiority <- calc_non_inferiority_sample_size(mu1 = 10, mu2 = 12, sd = 2, delta = 0.5)

# 可视化横断面调查样本量
plot_cross_sectional_sample_size(n_cross_sectional_proportion)
