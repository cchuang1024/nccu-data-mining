library(RODBC)
library(RWeka)
library(arules)
library(knitr)

db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=.\\foodmart2000-5.mdb")

sql5_normal <- "
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    pc.product_category as item
from sales_fact_1998 as ft, product as pd, product_class as pc
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id;"

sql5_xmax <- "
select
    trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
    pc.product_category as item
from sales_fact_dec_1998 as ft, product as pd, product_class as pc
where pd.product_id = ft.product_id
and pd.product_class_id = pc.product_class_id;"

export_factor_table <- function(db, sql, file_name){
  max_columns <- 2
  
  ft <- as.data.frame(sqlQuery(db, sql))
  
  for(ci in 1:max_columns){
    ft[,ci] <- as.character(ft[,ci])
  }
  
  head(ft)
  
  columns <- c("tid")
  
  cate_sql <- "select distinct product_category from product_class;"
  pc <- as.data.frame(sqlQuery(db, cate_sql))
  pc[,1] <- as.character(pc[,1])
  columns <- c(columns, as.vector(t(pc)[1,]))
  
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
  
  write.arff(df[,2:len], file = paste(file_name, '.arff', sep = ""))
  write.table(
    df[,2:len],
    paste(file_name, '.csv', sep = ""),
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE)
}

export_factor_table(db, sql5_xmax, 'q5_tx_xmas')

# export_factor_table(db, sql5_normal, 'q5_tx_normal')