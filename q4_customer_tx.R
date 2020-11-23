library(RODBC)
library(RWeka)
library(arules)
library(knitr)

db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=.\\foodmart2000-4.mdb")

sql4 <- "
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    ct.yearly_income,
    ct.gender,
    ct.occupation,
    pc.product_category as item
from sales_fact_1998 as ft, product as pd, product_class as pc, customer as ct
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id
and ft.customer_id = ct.customer_id
union all
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    ct.yearly_income,
    ct.gender,
    ct.occupation,
    pc.product_category as item
from sales_fact_dec_1998 as ft, product as pd, product_class as pc, customer as ct
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id
and ft.customer_id = ct.customer_id;"

max_columns <- 5

ft <- as.data.frame(sqlQuery(db, sql4))

for(ci in 1:max_columns){
  ft[,ci] <- as.character(ft[,ci])
}

head(ft)

columns <- c("tid")

cate_sql <- "select distinct product_category from product_class;"

cust_attrs <- c("yearly_income", "gender", "education", "occupation")

attr_sqls = paste("select distinct", cust_attrs, "from customer;")

all_attr_sqls = c(attr_sqls, cate_sql)

for( sql in all_attr_sqls){
  pc <- as.data.frame(sqlQuery(db, sql))
  pc[,1] <- as.character(pc[,1])
  columns <- c(columns, as.vector(t(pc)[1,]))
}

len <- length(columns)
# tids <- head(unique(ft$tid), n=5000)
tids <- unique(ft$tid)
df <- as.data.frame(matrix(ncol = length(columns), 
                           nrow=0, 
                           dimnames = list(NULL,columns)))

ridx <- 1
for(tid in tids){
  df[ridx, 1] = tid
  df[ridx, 2:len] = 0
  
  cust_indices <- 2:(length(cust_attrs) + 1)
  for(cust_idx in cust_indices){
    ca <- unique(as.vector(ft[ft$tid==tid, cust_idx]))
    df[ridx, ca] = 1
  }
  
  prod_idx <- max_columns
  pcs <- unique(as.vector(ft[ft$tid==tid, prod_idx]))    
  for(pc in pcs){
    df[ridx, pc] = 1
  }
  
  ridx <- ridx + 1
}

for (idx in 1:length(columns)){
  df[,idx] <- as.factor(df[,idx])
}

head(df)

write.arff(df[,2:len], file = 'q4_customer_tx.arff')
write.table(
  df[,2:len],
  'q4_customer_tx.csv',
  row.names = FALSE,
  col.names = TRUE,
  sep = ",",
  quote = FALSE)