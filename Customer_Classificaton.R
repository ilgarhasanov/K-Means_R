library(tidyverse)
library(factoextra)
library(NbClust)
library(cluster)
library(plotly)

cust <- read.csv("Mall_Customers.csv")
head(cust)
cust %>% glimpse()

cust$Gender <- cust$Gender %>% as_factor() %>% as.numeric()
df <- cust %>%  select(-CustomerID) %>% scale()


df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#7 clusters

df %>% fviz_nbclust(kmeans ,method = "silhouette") +
  labs(subtitle = "Silhouette")
#10 clusters

df %>% 
  fviz_nbclust(kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")
#7 clusters


set.seed(123)
k_means <- df %>% kmeans(centers = 7)


y_kmeans <- k_means$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 7,
                plotchar = F,
                main = 'Clusters of customers')


g <- cust %>% 
  ggplot(aes(Annual.Income..k.., Spending.Score..1.100.,
             color = y_kmeans)) +
  geom_point(,size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Annual Income", 
       y="Spending Score",
       title="Mall Customers",
       subtitle="7 clusters")

g %>% ggplotly(tooltip = "text")



