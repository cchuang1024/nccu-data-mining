library(RODBC)
library(RWeka)
library(arules)
library(knitr)

db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=.\\foodmart2000-2.mdb")

sql2 <- "
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    pc.product_category as item
from sales_fact_1998 as ft, product as pd, product_class as pc
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id
union all
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    pc.product_category as item
from sales_fact_dec_1998 as ft, product as pd, product_class as pc
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id;"

ft <- as.data.frame(sqlQuery(db, sql2))
ft[, 1] <- as.character(ft[, 1])
ft[, 2] <- as.character(ft[, 2])

head(ft)

sql_pc <- "select distinct product_category from product_class; "

pc <- as.data.frame(sqlQuery(db, sql_pc))
pc[,1] <- as.character(pc[,1])

columns <- c("tid", as.vector(t(pc)[1,]))
len <- length(columns)
tids <- unique(ft$tid)
df <- as.data.frame(matrix(ncol = length(columns), 
                           nrow=0, 
                           dimnames = list(NULL,columns)))

ridx <- 1

for(tid in tids){
  df[ridx, 1] = tid
  df[ridx, 2:len] = 0
  
  pcs <- unique(as.vector(ft[ft$tid==tid,2]))
  
  for(pc in pcs){
    df[ridx, pc] = 1
  }
  
  ridx <- ridx + 1
}

for (idx in 1:length(columns)){
  df[,idx] <- as.factor(df[,idx])
}

head(df)

write.arff(df[,2:len], file = 'q2_fpgrowth.arff')
write.table(
  df[,2:len],
  'q2_fpgrowth.csv',
  row.names = FALSE,
  col.names = TRUE,
  sep = ",",
  quote = FALSE)