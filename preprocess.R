library(RODBC)

# 

connect <- function(db_file){
  db <- odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=", db_file, sep=""))
  return(db)
}

# sql1 <- "
# select
#     customer_id,
#     time_id,
#     store_id,
#     product_id as item
# from sales_fact_1998;"

query_transaction <- function(conn) {
  sql2 <- "
  select
      trim(str(ft.customer_id)) & '-' & trim(str(ft.time_id)) & '-' & trim(str(ft.store_id)) as tid,
      pc.product_category as item
  from sales_fact_1998 as ft, product as pd, product_class as pc
  where pd.product_id = ft.product_id
  and pd.product_class_id = pc.product_class_id;"
  
  tx <- as.data.frame(sqlQuery(conn, sql2))
  tx[, 1] <- as.character(tx[, 1])
  tx[, 2] <- as.character(tx[, 2])
  
  return (tx)
}

query_product_class <- function(conn) {
  sql_pc <- "select distinct product_category from product_class;"
  
  pc <- as.data.frame(sqlQuery(conn, sql_pc))
  pc[, 1] <- as.character(pc[, 1])
  
  return (pc)
}

build_init_df <- function(tx, pc){
  columns <- c("tid", as.vector(t(pc)[1,]))
  len <- length(columns)
  tids <- unique(tx$tid)
  
  df <- as.data.frame(matrix(ncol = length(columns), 
                             nrow=0, 
                             dimnames = list(NULL,columns)))
  
  ridx <- 1
  for(tid in tids){
    df[ridx, 1] = tid
    for(cidx in 2:len){
      df[ridx, cidx] = 0
    }
    
    pcs <- unique(as.vector(tx[tx$tid==tid,2]))
    
    for(pc in pcs){
      df[ridx, pc] = 1
    }
    
    ridx <- ridx + 1
  }
  
  return (df)
}

export_csv <- function(df){
  write.table(
    df,
    'sales_fact_1998.csv',
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE)
}

main <- function(){
  mdb <- ".\\foodmart2000.mdb" 
  conn <- connect(mdb)
  tx <- query_transaction(conn)
  pc <- query_product_class(conn)
  df <- build_init_df(tx, pc)
  
  export_csv(df)
}

main()
