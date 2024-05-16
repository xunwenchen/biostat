# 生物统计学/Biostatistics/Biometrics ----
# 非参检验/Nonparametric analysis examples 
# Instructor: Xun Wen Chen, Department of Ecology, College of Life Science and Technology, Jinan University, Guangzhou, China

# 
# 安装和加载所需程序包/install and load packages needed. Consult instructor if you are not sure
# 需要上网/need to be online

my_packages <- c('dplyr', 'randtests', 'tseries', 
                 'snpar', 'dunn.test', 'tidyverse', 'coin')

# 安装所需程序包/install required packages
if (!require(my_packages)) {
  install.packages(my_packages)
}

# 加载程序包/load required packages
lapply(my_packages, library, character.only = TRUE)


# 5.1. 游程检验/runs test ----
# ~ 例5-1/example 5-1 ----
# 生物统计学第六版/Biometrics 6th edn
# 野外观测某动物雄性/雌性出现次数/Occurrence of male and female of an animal
s_data <- c('F','M','M','M','M','M',
            'F','M','M','F','M','M',
            'M','M','F','M','F','M',
            'M','M','F','F','F','M','M','M')
# 把string数据格式变成factor格式/convert string to factor
s_data_fc <- as.factor(s_data)
# 进行游程检验/perform runs test using package 
runs.test(s_data_fc)
# p > 0.05 随机；p < 0.05 不随机

# ~ 灯泡例子/bulb example ----
# 模拟一批灯泡数据
bulb <- sample(c(0, 1), size = 1000, replace = TRUE, prob = c(0.1, 0.9))
# 查看前50个数据/check first 50 data
head(bulb, 50)
# 进行游程检验/perform runs test
snpar::runs.test(bulb)
# p > 0.05 随机；p < 0.05 不随机


# 5.2. 符号检验/sign test ----
# ~ 符号检验举例 Sign test example 1 ----
# 处理前vs处理后7个指标数值
before_trt <-c(80,76,65,67,66,54,55)
after_trt <- c(100,99,88,76,78,78,89)

# Perform the sign test
BSDA::SIGN.test(before_trt, after_trt)
# p > 0.05 随机；p < 0.05 不随机


# ~ 符号检验举例1（一个样本vs理论值）----
# 一个样本的数据
x <- c(50, 33, 34, 56, 54, 43, 56)

# 理论值
mu <- 20

# 进行符号检验
BSDA::SIGN.test(x, md = mu)



# ~ 符号检验举例1（一个样本vs理论值-手动计算）----
# 一个样本数据
x <- c(50, 33, 34, 56, 54, 43, 56)

# 理论值
mu <- 20

# 进行符号检验
signs <- sign(x - mu) # >0赋值1，=0赋值0，<0赋值-1
n_plus <- sum(signs == 1) # 7个“+”
n_minus <- sum(signs == -1) # 0个“-”

# 计算p值
p_value <- min(2 * pbinom(min(n_plus, n_minus), length(x), 0.5), 1)

# 输出结果
cat("Number of plus signs:", n_plus, "\n")
cat("Number of minus signs:", n_minus, "\n")
cat("p-value:", p_value, "\n")


# ~ 符号检验举例2（两个配对样本）----
# 数值为治愈率
medA <- c(94, 88, 83, 92, 87, 95, 90, 90, 86, 84) 
medB <- c(86, 84, 85, 78, 76, 82, 83, 84, 82, 83)
# 进行符号检验
BSDA::SIGN.test(medA, medB)
# p > 0.05 两种药物无差异；p < 0.05 两种药物有差异


# 5.3. 秩和检验/Wilcoxon test ----
# ~ Wilcoxon符号秩检验举例1（一个样本vs理论值）----
# 同样一组数据
x <- c(50, 33, 34, 56, 54, 43, 56)

# 理论值
mu <- 20

# 进行Wilcoxon符号秩检验
wilcox.test(x, mu = mu, paired = FALSE, alternative = "two.sided")



# ~ Wilcoxon符号秩检验举例2 (两个配对样本/成对数据)----
# ~~ 例5-6 两种药物数据，检验疗效是否有差异 ----
medA <- c(94, 88, 83, 92, 87, 95, 90, 90, 86, 84)
medB <- c(86, 84, 85, 78, 76, 82, 83, 84, 82, 83)

# 进行Wilcoxon符号秩检验
wilcox.test(medA, medB, paired = TRUE)

# 原始数据KW检验+事后比较
yield_spA <- c(58, 54, 50, 49)
yield_spB <- c(42, 38, 41, 36)
yield_spC <- c(35, 31, 34, 33)

# Combine the data into a list
data_list <- list(yield_spA, yield_spB, yield_spC)

# Perform Kruskal-Wallis test
kruskal.test(data_list)

# Load the necessary library
library(dunn.test)

# Perform Dunn's test for pairwise comparisons
dunn.test(data_list, method = "bonferroni")



# ~ 例5-7 Brown-Mood median test（两个独立样本） ----
# 测定A、B两个小麦品种株高(cm)
# 结果如下:
# A品种:69.8, 68.8, 67.5, 65.6, 65.5, 64.8, 64.0, 63.9, 62.0
# B品种:78.0, 75.4, 74.0, 71.2, 69.3, 68.0, 62.1
h_sp_A <- c(69.8, 68.8, 67.5, 65.6, 65.5, 64.8, 64.0, 63.9, 62.0)
h_sp_B <- c(78.0, 75.4, 74.0, 71.2, 69.3, 68.0, 62.1)
data_all <- c(h_sp_A, h_sp_B)
group <- factor(c(rep("spA", length(h_sp_A)), rep("spB", length(h_sp_B))))

# Perform the Brown-Mood median test
median_test(data_all ~ group)
independence_test(data_all ~ group, distribution="exact")


#~ Whitney-Mann-Wilcoxon 秩和检验 (两样本)----
# ~~~ 例5-8 ----
# 产虾数量数据输入
trt1 <- c(148, 143, 138, 145, 142)
trt2 <- c(139, 136, 141, 133, 140)
wilcox.test(trt1, trt2, alternative = 'greater')


# 5.4. H检验/Kruskal-Wallis rank-sum test（多样本）----
# ~~ 例：学习方法A vs B vs C （原始数据多样本）/learning strA vs B vs C ----
# 输入数据
scores_A <- c(85, 88, 92, 90, 87, 91, 93, 89, 86, 90)
scores_B <- c(78, 80, 82, 79, 81, 83, 80, 79, 78, 80)
scores_C <- c(90, 92, 94, 91, 93, 95, 92, 91, 90, 94)
mean(scores_A); mean(scores_B); mean(scores_C)

# 把所有数据放一起
scores_all <- c(scores_A, scores_B, scores_C)

# 给不同组命名
group <- factor(c(rep("str_A", length(scores_A)), 
                  rep("str_B", length(scores_B)), 
                  rep("str_C", length(scores_C))))

# 进行H检验/perform H-test
kruskal.test(scores_all ~ group)

# 事后比较/post-hoc test（确定到底那两组之间有差异）
# adjusts the p-value for multiple comparisons using the Bonferroni, Šidák, Holm, Holm-Šidák, Hochberg, Benjamini-Hochberg, or Benjamini-Yekutieli adjustment (see Details using "?dunn.test()" typed in the Console). The default is no adjustment for multiple comparisons.
# 'Bonferroni'方法最简单和最保守

dunn.test(scores_all, group, method = 'Bonferroni')



# ~~ 例：癌症治疗效果（频数表多样本）/Frequency table multi-groups ----
# 模拟数据/Hypothetical data
cancer_df <- data.frame(
  Group = c(rep("NoDrug", 3), 
            rep("Placebo", 3), 
            rep("Drug_A", 3), 
            rep("Drug_B", 3), 
            rep("Drug_C", 3)),
  Result = rep(c("Reduced", 
                 "Stable", 
                 "Progressed"), 5),
  Count = c(10, 15, 5, 12, 13, 
            5, 20, 8, 2, 18, 
            10, 2, 22, 6, 2))
View(cancer_df)

# Convert the frequency table to individual observations
cancer_df_exp <- cancer_df %>% 
  uncount(Count, .remove = FALSE)

# Perform the Kruskal-Wallis H test
kruskal.test(Count ~ Group, data = cancer_df_exp)
dunn.test(cancer_df_exp$Count, 
          cancer_df_exp$Group, 
          method = 'Bonferroni')


# 6.1. 列联表/contingency table(or crosstab) ----
# ~ 列联表举例1/example 1 of crosstab ----
gender <- c(rep("Male", 106), rep("Female", 117))
preference <- c(rep("PC", 50), rep("Mac", 56), rep("PC", 46), rep("Mac", 71))

# Create a data frame
data <- data.frame(gender, preference)

# Create the contingency table
table_gender <- table(data$gender, data$preference)

# Print the contingency table
print(table_gender)

# Plot the figure
barplot(table_gender, beside=TRUE, col=c("#006A7E", "#7FB4BE"),
        legend.text=c("Female", "Male"), args.legend=list(x="topright"))

# ~ chi-squared统计数计算 ----
chisq_test(table_gender)
chisq.test(table_gender)#用Yates方法矫正
chisq.test(table_gender, correct = FALSE)#去掉矫正后，结果和chisq_test()的计算结果一致

# 6.2. 拟合优度检验/goodness of fit test ----
# Observed counts
observed <- c(100, 50, 50, 120, 130, 150)

# Expected counts
expected <- c(100, 100, 100, 100, 100, 100)

# Perform Chi-Square goodness of fit test
test_result <- chisq.test(observed, p = expected/sum(expected))

# Print the test result
print(test_result)



# 6.3. 独立性检验/independence test ----
# 模拟数据两个区域，四种鸟品种各自数量/create a hypothetical dataset
# 模拟非独立数据
bird_raw_data <- data.frame(
  Species = c(rep("Species1", 50), 
              rep("Species2", 50), 
              rep("Species3", 50), 
              rep("Species4", 50)),
  Region = c(rep("Region1", 25), 
             rep("Region2", 25), 
             rep("Region1", 15), 
             rep("Region2", 35), 
             rep("Region1", 30), 
             rep("Region2", 20), 
             rep("Region1", 40), 
             rep("Region2", 10)))

# 模拟独立数据
bird_raw_data <- data.frame(
  Species = c(rep("Species1", 50), 
              rep("Species2", 50), 
              rep("Species3", 50), 
              rep("Species4", 50)),
  Region = c(rep("Region1", 25), 
             rep("Region2", 25), 
             rep("Region1", 25), 
             rep("Region2", 25), 
             rep("Region1", 25), 
             rep("Region2", 25), 
             rep("Region1", 25), 
             rep("Region2", 25)))

# 建立列联表/Create a contingency table
bird_crs_tab <- table(bird_raw_data$Species, 
                      bird_raw_data$Region)
print(bird_crs_tab)

# 画图看看/plot
barplot(bird_crs_tab, beside=TRUE)

# 卡方检验独立性/Perform a Chi-Square test of independence
chisq.test(bird_crs_tab)


# 6.4. Fisher精确检验/Fisher's exact test & McNemar检验/McNermar test ----
# ~ Fisher ----
# 例6-7
# 建立数据
data_surv <- matrix(c(4, 6, 1, 28), 
                    nrow = 2)

# Add column names
colnames(data_surv) <- c("trt_1", 
                         "trt_2")

# Add row names
rownames(data_surv) <- c("death", 
                         "survived")
data_surv

# Perform Fisher's Exact Test
fisher.test(data_surv)

# 可与chi-squared检验比较/compare with chi-squared test
# chi-squared检验获得的p值会比Fisher的小

# ~ McNemar ----

# 广告是否刺激购买
# buy or not buy before and after ad
# Create the data
data_McN <- matrix(c(20, 21, 
                     43, 18), nrow = 2)

# Add column names
colnames(data_McN) <- c("before_ad", 
                         "after_ad")

# Add row names
rownames(data_McN) <- c("buy", 
                         "do not buy")
              
               
data_McN

# Perform McNemar's Test
mcnemar.test(data_McN, correct = FALSE)




# 6.5. 列联表中的相关系数/coefficient ----
# 重新导入之前的数据
bird_raw_data <- data.frame(
  Species = c(rep("Species1", 50), 
              rep("Species2", 50), 
              rep("Species3", 50), 
              rep("Species4", 50)),
  Region = c(rep("Region1", 25), 
             rep("Region2", 25), 
             rep("Region1", 15), 
             rep("Region2", 35), 
             rep("Region1", 30), 
             rep("Region2", 20), 
             rep("Region1", 40), 
             rep("Region2", 10)))

# 建立列联表/Create a contingency table
bird_crs_tab <- table(bird_raw_data$Species, 
                      bird_raw_data$Region)
print(bird_crs_tab)

# calculate V coefficient
# Calculate chi-square statistic
chisq_test <- chisq.test(bird_crs_tab)

# Calculate Cramér's V
n <- sum(chisq_test$observed) # total observations
k <- min(dim(chisq_test$observed)) # number of rows or columns, whichever is less
V <- sqrt(chisq_test$statistic / (n * (k - 1)))

print(V)



# c coefficient

# V coefficient


# 6.6. 差分概率的置信区间与相对风险/ ----
# ~~ example 1 ----
# Hypothetical contingency table
# Group1 and Group2 could be any two categories you are comparing (e.g., treatment vs control, male vs female, etc.)
data <- matrix(c(50, 150, 100, 200), nrow = 2)
colnames(data) <- c("Success", "Failure")
rownames(data) <- c("Group1", "Group2")

# Print the contingency table
print(data)

# Perform a proportions test
prop.test(data)


# 习题/exercise ----

# 5.2 ----
species <- as.factor (c('b', 'a', 'b', 'b', 'a', 'a', 'a', 'b', 'a', 'b', 'b', 'a'))
yield <- c(23, 24, 18, 23, 19, 11, 6, 22, 14, 22, 27, 15)

error <- yield - mean(yield)
# use runs test to test the error of species is random
runs.test(factor(species), error)



# <<问题汇总/Question collection>> ----

# in-class questions ----
# ~~ 1. 数据的顺序是否会影响比较结果？ ----
# 学生在课上提出（例5-8，即PPT的41-41页），数据的顺序是否会影响检验结果，见以下数据。

# 第一次分析
trt1 <- c(148, 143, 138, 145, 142)
trt2 <- c(139, 136, 141, 133, 140)
wilcox.test(trt1, trt2, alternative = 'greater')

# 解答：Whitney-Mann-Wilcoxon秩和检验，检验两个独立的样品，数据的次序不影响检验结果，见下面分析：

# 打乱第二组数据的次序后进行第二次分析
trt1 <- c(148, 143, 138, 145, 142)
trt2 <- c(136,139, 133, 140, 141)
wilcox.test(trt1, trt2, alternative = 'greater')

# 上述两次检验结果完全一样，p值相同，即p=0.02778

# 值得注意的是，例5-6中两种药物数据，检验疗效是否有差异。数据属于成对样本，例如同一个人接受了不同药物治疗看效果，那两组数据之间就不是独立的，因此**数据的顺序会影响检验结果**

# 原始数据
medA <- c(94, 88, 83, 92, 87, 95, 90, 90, 86, 84)
medB <- c(86, 84, 85, 78, 76, 82, 83, 84, 82, 83)
# 进行Wilcoxon符号秩检验
wilcox.test(medA, medB, paired = TRUE)

# 打乱第二组数据，进行第二次检验
medA <- c(94, 88, 83, 92, 87, 95, 90, 90, 86, 84)
medB <- c(83, 76, 86, 78, 82, 83, 84, 84, 85, 82)
# 进行Wilcoxon符号秩检验
wilcox.test(medA, medB, paired = TRUE)

# 两次检验的的结果并不一样，一个p=0.01078，一个p=0.01431。此时，**数据的顺序会影响检验结果**

# ~~ 2. crosstab correlation coefficients calculation ----
# using the following crosstab to test independence and calculate correlation coefficients (including phi, c, and V)
ssr <- matrix(c(60, 0, 4, 8, 62, 109), nrow = 3)
rownames(ssr) <- c("1R", "2S", "3S")
colnames(ssr) <- c("resist", "suscept")
ssr
assocstats(ssr)



# ~~~~ change to a 2*2 crosstab for chi-squared test and calculate coefficients ----
ssr <- matrix(c(60, 4, 8, 171), nrow = 2)
rownames(ssr) <- c("R", "S")
colnames(ssr) <- c("resist", "suscept")
ssr
chisq.test(ssr)
assocstats(ssr)
