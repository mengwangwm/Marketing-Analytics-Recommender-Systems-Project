library(dplyr)

# merge transaction & product table
merge <- merge(transaction_table, product_table, by="prod_id")

#find someone who purchase 224 tubes of toothpaste in 2 years
crazy_customer1 <- transaction_table %>% filter(cust_id =="99569937")

crazy_customer <- merge %>% filter((cust_id == "4029893" | cust_id =="99569937") & category_desc_eng =="TOOTHPASTE")

#summary 7920 customer information
customer_info <- merge %>% select(cust_id, tran_dt) %>% group_by(cust_id) %>% mutate(visit=n_distinct(tran_dt)) %>% select(cust_id, visit) %>% distinct(cust_id, visit)
customer_info <- merge(customer_1_, customer_info, by="cust_id")

customer_pool <- merge(customer_buy, customer_info, by="cust_id")
#write.csv(customer_pool, "D:/Spring Semester/Marketing Analytics/recommender systems/project/customer_pool.csv")

#add visit frequency and discount times to the data
visit_tp <- merge %>% filter(category_desc_eng == "TOOTHPASTE") %>% select(cust_id, tran_dt) %>% group_by(cust_id) %>% mutate(visit_tp=n_distinct(tran_dt)) %>% select(cust_id, visit_tp) %>% distinct(cust_id, visit_tp)
visit_col <- merge %>% filter(category_desc_eng == "TOOTHPASTE" & brand_desc == "COLGATE") %>% select(cust_id, tran_dt) %>% group_by(cust_id) %>% mutate(visit_col=n_distinct(tran_dt)) %>% select(cust_id, visit_col) %>% distinct(cust_id, visit_col)

disc_tm_tp <- merge %>% filter(category_desc_eng == "TOOTHPASTE") %>% select(cust_id, tran_prod_offer_cts) %>% group_by(cust_id) %>% mutate(disc_tm_tp=sum(tran_prod_offer_cts!="0")) %>% distinct(cust_id, disc_tm_tp)
disc_tm_col <- merge %>% filter(category_desc_eng == "TOOTHPASTE" & brand_desc == "COLGATE") %>% select(cust_id, tran_prod_offer_cts) %>% group_by(cust_id) %>% mutate(disc_tm_col=sum(tran_prod_offer_cts!="0")) %>% distinct(cust_id, disc_tm_col)

customer_pool <- merge(customer_pool, visit_tp, by="cust_id")
customer_pool <- merge(customer_pool, visit_col, by="cust_id")
customer_buy <- merge(customer_buy, visit_tp, by="cust_id")
customer_buy <- merge(customer_buy, visit_col, by="cust_id")

customer_pool <- merge(customer_pool, disc_tm_tp, by="cust_id")
customer_pool <- merge(customer_pool, disc_tm_col, by="cust_id")
customer_buy <- merge(customer_buy, disc_tm_tp, by="cust_id")
customer_buy <- merge(customer_buy, disc_tm_col, by="cust_id")

#####find company customer and evalue them
#1 buy 20+ toothpaste
co.customer.1 <- filter(customer_buy,customer_buy$transaction_tp >= 20)
summary(co.customer$transaction_tp)
#2 see if they have lots of buyings in 2 years
co.customer.1 <- merge(co.customer.1, customer_info)
summary(co.customer.1)
summary(customer_info)

#statistics test
res <- t.test(customer_info$total_transaction, co.customer.1$total_transaction, var.equal = TRUE)
res

co.customer_2025 <- filter(customer_buy, customer_buy$transaction_tp < 25 & customer_buy$transaction_tp >= 20)
co.customer_2025 <- merge(co.customer_2025, customer_info)
co.customer_2535 <- filter(customer_buy, customer_buy$transaction_tp < 35 & customer_buy$transaction_tp >= 25)
co.customer_2535 <- merge(co.customer_2535, customer_info)

co.customer_35 <- filter(customer_buy,customer_buy$transaction_tp >= 35)
co.customer_35 <- merge(co.customer_35, customer_info)

res <- t.test(co.customer.1$visit, co.customer_35$visit, var.equal = TRUE)
res

#observe the correlations of those crazy customers' behavior
cor(co.customer_35$transaction_col, co.customer_35$avg_disc_rate_col, use = "complete.obs")
cor(co.customer_35$transaction_tp, co.customer_35$avg_disc_rate_col, use = "complete.obs")

cor(co.customer.1$transaction_col, co.customer.1$avg_disc_rate_other, use = "complete.obs")
cor(co.customer.1$transaction_tp, co.customer.1$avg_disc_rate_other, use = "complete.obs")

cor(customer_pool$transaction_col, customer_pool$avg_disc_rate_other, use = "complete.obs")
cor(customer_pool$transaction_tp, customer_pool$avg_disc_rate_other, use = "complete.obs")

cor((co.customer_70$transaction_col / co.customer_70$transaction_tp), co.customer_70$avg_disc_rate_other, use = "complete.obs")
cor((co.customer_35$transaction_col / co.customer_35$transaction_tp), co.customer_35$avg_disc_rate_other, use = "complete.obs")
cor((co.customer_2535$transaction_col / co.customer_2535$transaction_tp), co.customer_2535$avg_disc_rate_other, use = "complete.obs")
cor((co.customer_2025$transaction_col / co.customer_2025$transaction_tp), co.customer_2025$avg_disc_rate_other, use = "complete.obs")

summary(co.customer_35$avg_disc_rate_col) #0.259
summary(co.customer_35$avg_disc_rate_other) #0.156
summary(co.customer_35$avg_discount_rate) #0.163

summary(co.customer.1$avg_disc_rate_col) #0.257
summary(co.customer.1$avg_disc_rate_other) #0.161
summary(co.customer.1$avg_discount_rate) #0.163

summary(customer_pool$avg_disc_rate_col) #0.254
summary(customer_pool$avg_disc_rate_other) #0.153
summary(customer_pool$avg_discount_rate) #0.160

summary(co.customer_35$disc_tm_col) #11.88
summary(co.customer_35$disc_tm_tp) #18.29
summary(customer_pool$disc_tm_col) #5.54
summary(customer_pool$disc_tm_tp) #8.49

summary(co.customer_35$visit_col) #17.77
summary(co.customer_35$visit_tp) #34.18
summary(co.customer_35$visit) #348.5

summary(co.customer.1$visit_col) #13.57
summary(co.customer.1$visit_tp) #25.31
summary(co.customer.1$visit) #343.7

summary(customer_pool$visit_col) #8.481
summary(customer_pool$visit_tp) #15.61
summary(customer_pool$visit) #337.7

crazy <- customer_buy %>% filter(transaction_tp >= 36)
#customer_buy <- customer_buy %>% mutate(qty_tp=transaction_tp/visit_tp) %>% mutate(qty_col=transaction_col/visit_col)
#summary(customer_buy)


#NA transform to 0
customer_buy[is.na(customer_buy)] <- 0

#ADD the Colgate ratios to the column
customer_buy <- customer_buy %>% mutate(col_ratio = customer_buy$transaction_col / customer_buy$transaction_tp)
summary(customer_buy$col_ratio)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.2500  0.5000  0.5036  0.7600  1.0000
hist(customer_buy$col_ratio)

#find the loyal Colgate customers
customer_col_lover <- customer_buy %>% filter(transaction_tp < 8 & col_ratio >= 0.76)
col_lover_id <- customer_col_lover[,1]
write.csv(col_lover_id, "D:/Spring Semester/Marketing Analytics/recommender systems/project/col_lover_id.csv")

#find the Fans of competitors
customer_other_lover <- customer_buy %>% filter(transaction_tp >= 8 & col_ratio <= 0.25)
other_lover_id <- customer_other_lover[,1]
write.csv(other_lover_id, "D:/Spring Semester/Marketing Analytics/recommender systems/project/other_lover_id.csv")


#Look at their statistics to generate promotion plans
summary(customer_col_lover$avg_disc_rate_col)
summary(customer_buy$avg_disc_rate_col)

summary(customer_col_lover$avg_disc_rate_other)
summary(customer_buy$avg_disc_rate_other)

summary(customer_other_lover$avg_disc_rate_col)
summary(customer_buy$avg_disc_rate_col)

summary(customer_other_lover$avg_disc_rate_other)
summary(customer_buy$avg_disc_rate_other)

