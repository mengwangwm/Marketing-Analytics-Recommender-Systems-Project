
# Generate customer table -- from last project --------------------------------------------------------


library(data.table)
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)
setwd('C:/Users/Meng Wang/Desktop/MKT_Analysis/Project/Segmentation/files')
product <- fread('product_table.csv') # original product table
transaction <- fread('transaction_table.csv') # original transaction table

# check missing value
str(transaction)
sapply(transaction,function(x) sum(is.na(x)))
sapply(product,function(x) sum(is.na(x)))
#hist(transaction[,sum(tran_prod_sale_amt),cust_id][,.(V1)])

# create new transaction id
transaction[,ti:=paste(tran_dt,store_id,cust_id)]

# Merge transaction data and product data
data <- merge(transaction,product,by = 'prod_id') 
#There are 500 transaction with products not inluded in product data

# create a binary variable to see which row is about discounted item
data[,onsale:=1]
data[tran_prod_sale_amt==tran_prod_paid_amt,onsale:=0]

colnames(data)


# generate the corresponding customer table with defined attributes
l <- data[,.(total_revenue=sum(tran_prod_paid_amt), # total revenue
             total_transaction=length(unique(ti)), # total number of transaction
             t_d_product=length(unique(prod_id)), # total number of distinct product purchased
             t_d_store=length(unique(store_id)), # total number of distinct stores visited 
             t_d_category=length(unique(category_id)), # total number of distinct categories purchased
             t_d_brand=length(unique(brand_desc)), # total number of distinct brands purchased
             p_o_discount_purchase=sum(onsale)/.N, # percentage of productes purchased on sale
             avg_discount_rate=-sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt)),
          cust_id] # the sales amount on sale over total sales amount 

# Generate Production Table -- from last project -------------------------------

# Products descriptive analysis 
## products with the best volumes (count and KG are separated)
prod_vol_count <- transaction %>% group_by(prod_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
prod_vol_KG <- transaction %>% group_by(prod_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")
intersect(prod_vol_count$prod_id,prod_vol_KG$prod_id)

## products with the best revenues
prod_rev <- transaction %>% group_by(prod_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))
## products with the most customers
prod_cust <- transaction %>% group_by(prod_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))
## products with the most stores
prod_cust <- transaction %>% group_by(store_id) %>% summarise(total_store = n_distinct(store_id)) %>% arrange(desc(total_store))
## products with the most transactions
prod_rev <- data %>% group_by(ti) %>% summarise(total_trans = n()) %>% arrange(desc(total_trans))


## subcategories with the best volumes (count and KG are separated)
subcategory_vol_count <- data %>% group_by(subcategory_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
subcategory_vol_KG <- data %>% group_by(subcategory_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")
## subcategories with the best revenues
subcategory_rev <- data %>% group_by(subcategory_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))
## subcategories with the best customers
subcategory_cust <- data %>% group_by(subcategory_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))

## categories with the best volumes (count and KG are separated)
category_vol_count <- data %>% group_by(category_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
category_vol_KG <- data %>% group_by(category_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")
## categories with the best revenues
category_rev <- data %>% group_by(category_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))
## categories with the best customers
category_cust <- data %>% group_by(category_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))

# Product clustering 
## create key attributes for product clustering
product_statistics <- data %>% group_by(prod_id) %>% summarise(total_revenue = sum(tran_prod_paid_amt), total_transact=n_distinct(ti), total_distinct_customer = n_distinct(cust_id), total_stores = n_distinct(store_id), avg_discount_rate = (abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt)))
discounted_product_cnt <- data %>% filter(tran_prod_discount_amt < 0) %>% group_by(prod_id) %>% summarise(discounted_tran_cnt=n_distinct(tran_id))

#the final product table. 
product_statistics <- inner_join(discounted_product_cnt,product_statistics, by="prod_id") %>% mutate(percentage_discount_product = discounted_tran_cnt/total_transact)

# remove unuseful data for clear view. 
rm(transaction)
rm(category_cust)
rm(category_rev)
rm(category_vol_count)
rm(category_vol_KG)
rm(subcategory_cust)
rm(subcategory_rev)
rm(subcategory_vol_count)
rm(subcategory_vol_KG)
rm(prod_cust)
rm(prod_rev)
rm(prod_vol_count)
rm(prod_vol_KG)
rm(discounted_product_cnt)


# Find toothpaste cherry pickers  --------------------------------------------------------------------

# find the total amount of transactions including toothpaste for each customer
tp_amt <- data %>% group_by(cust_id) %>% filter(category_desc_eng=="TOOTHPASTE") %>% summarise((tp_amt = length(ti)))
# find the total amount of transactions including colgate toothpaste for each customer
colgate_amt <- data %>% group_by(cust_id) %>% filter(category_desc_eng=="TOOTHPASTE" & brand_desc =="COLGATE") %>% summarise((tp_amt = length(ti)))
colnames(tp_amt) <- c("cust_id","tp_amt")
colnames(colgate_amt) <- c("cust_id","colgate_amt")
# merge aboving two attributes into the customer table 
l <- merge(tp_amt,l,all.y =T)
l <- merge(colgate_amt,l,all.y=T)
sapply(l,function(x) sum(is.na(x)))
l[is.na(l)] <- 0

# find the total value of transactions including toothpaste for each customer
tp_value <- data %>% group_by(cust_id) %>% filter(category_desc_eng=="TOOTHPASTE") %>% summarise(tp_value=sum(tran_prod_paid_amt))
# find the total value of transactions including colgate toothpaste for each customer
colgate_value <- data %>% group_by(cust_id) %>% filter(category_desc_eng=="TOOTHPASTE" & brand_desc =="COLGATE") %>% summarise(colgate_value=sum(tran_prod_paid_amt))
colnames(tp_value) <- c("cust_id","tp_value")
colnames(colgate_value) <- c("cust_id","colgate_value")
# merge aboving two attributes into the customer table 
l <- merge(tp_value,l,all.y =T)
l <- merge(colgate_value,l,all.y=T)
sapply(l,function(x) sum(is.na(x)))
l[is.na(l)] <- 0

# Calculate the ratio of the total amount of transactions including toothpaste (customer_level)
l$tp_amt_ratio <- l$tp_amt/l$total_transaction
# calculate the ratio of the total values of transactions inlcuding toothpaste (customer-level)
l$tp_value_ratio <- l$tp_value/l$total_revenue

# pick up only useful attributes to make the table for cherry pickers. (for potential later use)
cp <- l[,c(1,2,3,4,5,6,7,12,13,14,15)]

# finally formed the customer-level table with only 4 attributes: "p_o_discount_purchase", "avg_discount_rate", "tp_amt_ratio", "tp_value_ratio"
cpc <- cp[,c(1,8,9,10,11)]

cor(cpc[,2:5]) # calculate correlation

# create normalization function
standarlize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
# normalization
cpcs <- lapply(cpc,standarlize)
cpcs <- as.data.table(cpcs)
cpc <- as.data.table(cpc)
cpcs <- cbind(cpc[,.(cust_id)],cpcs[,-c('cust_id')])
# find the best k
fviz_nbclust(cpcs[, 2:5], kmeans, method = "wss", k.max = 10, print.summary = T)
fviz_nbclust(cpcs[, 2:5], kmeans, method = "silhouette", k.max = 10, print.summary = T) #3
fviz_nbclust(cpcs[, 2:5], kmeans, method = "gap_stat",nboot = 30, k.max = 10, print.summary = T) #2
# the best k we have is 3. 
set.seed(1234)
clusters <- kmeans(cpcs[, 2:5], 3)
clusters$size # 2936 1496 3488
clusters$centers # centric after normalization

# result from 3 cluster model
#p_o_discount_purchase avg_discount_rate tp_amt_ratio tp_value_ratio
#1             0.4174304         0.4931449   0.10839023     0.08491742
#2             0.3069617         0.3699543   0.29998016     0.22478482
#3             0.2211934         0.2638520   0.09218143     0.07319522
cpcs[,cluster_3:=clusters$cluster]
three_cluster <- cpcs[,-c('cust_id')][,lapply(.SD,mean),cluster_3][order(cluster_3)] # centric before normalization

set.seed(1234)
clusters_2 <- kmeans(cpcs[, 2:5], 2)
clusters_2$size # 2936 1496 3488
clusters_2$centers # centric after normalization
# result from 2 cluster model
#p_o_discount_purchase avg_discount_rate tp_amt_ratio tp_value_ratio
#1             0.4078850         0.4827280    0.1490015     0.11461285
#2             0.2256924         0.2705452    0.1274532     0.09888369
cpcs[,cluster_2:=clusters_2$cluster]
two_cluster <- cpcs[,-c('cust_id')][,lapply(.SD,mean),cluster_2][order(cluster_2)] # centric before normalization

hist(cpcs$p_o_discount_purchase)
hist(cpcs$avg_discount_rate)
hist(cpcs$tp_amt_ratio)
hist(cpcs$tp_value_ratio)

# targeted customers with target products ------------------------------------------------------

# filter out those cherry-pickers based on toothpaste likeness
cp_tp <- cpcs %>% filter(cluster_3 == 2)

# taregt customer list for cherry pickers
target_cust_list <- unique(cp_tp$cust_id)

write.csv(target_cust_list,"Cherry_pickers.csv")

# all following steps are aimed to explore the four sub-brand of toothpaste. 
ctp <- data %>% filter(category_desc_eng=="TOOTHPASTE" & brand_desc == "COLGATE")
table(ctp$sub_category_desc)
#PASTA DENTIFR BRANQ PASTA DENTIFR INFANT PASTA DENTIFR MEDICI PASTA DENTIFR TRADIC (61084)
#6880 11.26                1197 19.6                 702   1.15            52305 85.63

# filter out all colgate products
Colgate_info <- product %>% filter(prod_id %in% ctp$prod_id)
Colgate_info <- Colgate_info[,c(1,3)]
#merge it with product table. 
Colgate_info <- merge(Colgate_info, product_statistics,all.x=T)


# find the summary stats for each of the four sub-category. 
Colgate_info_BRANQ <- Colgate_info %>% filter(sub_category_desc=="PASTA DENTIFR BRANQ")
summary(Colgate_info_BRANQ[,3:8]) 
# discounted_tran_cnt total_revenue     total_transact   total_distinct_customer  total_stores   avg_discount_rate
# Mean   : 96.29      Mean   : 2880.1   Mean   : 974.6   Mean   : 657.3          Mean   :197.9   Mean   :0.30060 
Colgate_info_TRADIC <- Colgate_info %>% filter(sub_category_desc=="PASTA DENTIFR TRADIC")
summary(Colgate_info_TRADIC[,3:8]) 
# discounted_tran_cnt total_revenue     total_transact   total_distinct_customer  total_stores   avg_discount_rate
# Mean   :147.3       Mean   : 5824.3   Mean   :2241.2   Mean   :1206            Mean   :288.3   Mean   :0.24750 
Colgate_info_MEDICI <- Colgate_info %>% filter(sub_category_desc=="PASTA DENTIFR MEDICI")
summary(Colgate_info_MEDICI[,3:8]) 
# discounted_tran_cnt total_revenue     total_transact   total_distinct_customer  total_stores   avg_discount_rate
# Mean   :93          Mean   :2964   Mean   :689    Mean   :412             Mean   :230   Mean   :0.176  
Colgate_info_INFANT <- Colgate_info %>% filter(sub_category_desc=="PASTA DENTIFR INFANT")
summary(Colgate_info_INFANT[,3:8]) 
# discounted_tran_cnt total_revenue     total_transact   total_distinct_customer  total_stores   avg_discount_rate
# Mean   :65.0        Mean   : 877.2   Mean   :396.7   Mean   :282.3           Mean   :181.3   Mean   :0.15819 
