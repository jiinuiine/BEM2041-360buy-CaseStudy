# Importing the necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(cluster)
library(factoextra)
library(tidyr)


# Load the dataset
X360buy_SurveyData <- read_excel("Desktop/BEM2041 Marketing Analytics/Case Study Report/360buy_SurveyData.xlsx")

# Rename for convenience
df <- X360buy_SurveyData

# Inspect the dataset
View(df)
str(df)
summary(df)
colnames(df)

# Figure 1. Sample profile overview
df2 <- df

df2$CusGen <- factor(df2$CusGen, labels = c("Male", "Female"))
df2$LevEdn <- factor(df2$LevEdn,
                     labels = c("Bachelor's", "Master's+", "PhD"))

ggplot(df2, aes(x = CusGen, y = CusAgeYr, fill = LevEdn)) +
  geom_boxplot() +
  labs(
    x = "Gender",
    y = "Customer Age (Years)",
    title = "Customer Age Distribution by Gender and Education",
    subtitle = "Based on survey responses from 1,000 360buy customers"
  ) +
  scale_y_continuous(breaks = seq(20, 50, by = 5))

# Scale the data
# Cluster using standardised variables
m <- apply(df, 2, mean)
s <- apply(df, 2, sd)
scaled_data <- scale(df, m, s)

View(scaled_data)

# Figure 2. Scree plot
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Scree Plot for Optimal Number of Clusters")

# Hierarchical clustering using Euclidean distance and Ward.D2 method
dist_360 <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_360, method = "ward.D2")
clusters_k3 <- cutree(hc, k = 3)
clusters_k4 <- cutree(hc, k = 4)

# Table 1: Mean Values for k=4
df_k4 <- cbind(df, cluster = clusters_k4)
df_k4$cluster <- factor(df_k4$cluster)

segments_k4 <- df_k4 %>%
  group_by(cluster) %>%
  summarise_all(mean)

segments_k4 %>%
  gt() %>%
  tab_header(title = md('**Table 1. Mean Values for 4 Clusters**'))
View(segments_k4)

# Table 2: Mean Values for k=3
df_k3 <- cbind(df, cluster = clusters_k3)
df_k3$cluster <- factor(df_k3$cluster)

segments_k3 <- df_k3 %>%
  group_by(cluster) %>%
  summarise_all(mean)

segments_k3 %>%
  gt() %>%
  tab_header(title = md('**Table 2. Mean Values for 3 Clusters**'))
View(segments_k3)

# Table 3: Median Values for k=3
segments_k3_median <- df_k3 %>%
  group_by(cluster) %>%
  summarise_all(median)

segments_k3_median %>%
  gt() %>%
  tab_header(title = md('**Table 3. Median Values for 3 Clusters**'))
View(segments_k3_median)

# Figure 3: Hierarchical Clustering - Dendrogram (k=3 confirmed)
plot(hc, cex = 0.1, labels = FALSE,
     main = "Hierarchical Clustering - Dendrogram")
rect.hclust(hc, k = 3, border = 2:4)

# Figure 4: Relative Size of Clusters
table(df_k3$cluster)
percentages <- c(37.9, 29.4, 32.7)

prop_data <- data.frame(
  ClusterSizes = c(37.9, 29.4, 32.7),
  Clusters = c("Passive Browsers", "Technophiles", "Loyal Customers")
)

ggplot(prop_data, aes(x = "", y = percentages, fill = Clusters)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(percentages, "%")), fontface = "bold",
            position = position_stack(vjust = 0.5)) +
  labs(title = "Relative Size of 360buy Market Clusters",
       subtitle = "Based on the HCA using Euclidean Distance and the Ward.D2 method in R") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())

# Figure 5: Mean Cluster Characteristics with Standard Deviations
vars <- c("CusChoice", "ConstUp", "ReplacReminder",
          "ProdReturn", "ProInsuCov", "LevIncome", "LevEdn")

data <- data.frame(
  cluster = factor(c(1, 2, 3)),
  CusChoice = c(3.894459, 5.353741, 5.015291),
  ConstUp = c(3.443272, 5.272109, 4.388379),
  ReplacReminder = c(3.382586, 4.221088, 5.639144),
  ProInsuCov = c(2.638522, 4.761905, 4.403670),
  ProdReturn = c(3.213720, 2.962585, 5.275229),
  LevIncome = c(2.741425, 3.673469, 3.587156),
  LevEdn = c(1.527704, 1.561224, 1.434251)
)

sd_data <- data.frame(
  cluster = factor(c(1, 2, 3)),
  CusChoice = c(1.573405, 1.297426, 1.386751),
  ConstUp = c(1.518432, 1.200443, 1.354017),
  ReplacReminder = c(1.3128638, 1.6426787, 0.9836375),
  ProInsuCov = c(1.147205, 1.230074, 1.377643),
  ProdReturn = c(1.0410727, 1.1923940, 0.9926561),
  LevIncome = c(0.9181584, 0.9959869, 1.1972182),
  LevEdn = c(0.7740143, 0.6193632, 0.5205483)
)

sd_long <- pivot_longer(sd_data, -cluster,
                        names_to = "variable", values_to = "sd")

long_data <- pivot_longer(data, -cluster,
                          names_to = "variable", values_to = "value")

long_data_with_sd <- merge(long_data, sd_long, by = c("cluster", "variable"))
long_data_with_sd$sd <- as.numeric(long_data_with_sd$sd)

ggplot(long_data_with_sd, aes(x = cluster, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                position = position_dodge(width = 0.6),
                width = 0.2,
                color = "black",
                linewidth = 0.5,
                linetype = "dashed") +
  geom_text(aes(label = round(value, 2)),
            position = position_dodge(width = 0.6),
            vjust = -1.5,
            color = "black",
            size = 3,
            fontface = "bold") +
  labs(x = "Cluster", y = "Value",
       title = "Mean Cluster Characteristics with Standard Deviations",
       subtitle = "Based on the HCA using Euclidean Distance and the Ward.D2 method in R") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 7, by = 1))

# Figure 6: Income Levels by Cluster

income_cluster_counts <- df_k3 %>%
  group_by(LevIncome, cluster) %>%
  summarise(count = n(), .groups = "drop")

cluster_names <- c("Passive Browsers", "Technophiles", "Loyal Customers")

ggplot(income_cluster_counts, aes(x = factor(LevIncome), y = count, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3,
            color = "black",
            fontface = "bold") +
  labs(
    title = "Income Levels by Cluster",
    x = "Income Level",
    y = "Count",
    subtitle = "Based on the HCA using Euclidean Distance and the Ward.D2 method in R"
  ) +
  scale_x_discrete(labels = c(
    "Below £50K",
    "£51K–£100K",
    "£101K–£125K",
    "£126K–£150K",
    "Above £150K"
  )) +
  scale_fill_brewer(palette = "Set1", labels = cluster_names) +
  theme_minimal()

# Figure 7: Customer Account Ownership by Cluster
account_data <- df_k3 %>%
  mutate(CusAcct = factor(CusAcct, labels = c("No Account", "Has Account"))) %>%
  group_by(cluster, CusAcct) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(pct = round(count/sum(count)*100, 1))

ggplot(account_data, aes(x = cluster, y = pct, fill = CusAcct)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4, color = "white") +
  labs(x = "Cluster", y = "Percentage (%)",
       title = "Customer Account Ownership by Cluster",
       subtitle = "Based on HCA using Euclidean Distance and Ward.D2 Method") +
  scale_fill_manual(values = c("#FC4E07", "#00AFBB")) +
  theme_minimal() +
  theme(legend.title = element_blank())