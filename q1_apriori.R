library(RODBC)
library(RWeka)
library(arules)
library(knitr)

db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=.\\foodmart2000-1.mdb")

sql <- "
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

ft <- as.data.frame(sqlQuery(db, sql))
ft[, 1] <- as.character(ft[, 1])
ft[, 2] <- as.character(ft[, 2])

head(ft)

tids <- unique(ft$tid)
print(paste('tid length:', length(tids)))

max_len <- 0
for (tid in tids){
  items <- unique(ft[ft$tid==tid, 2])
  item_len <- length(items)
  max_len <- ifelse(item_len > max_len, item_len, max_len)
}
print(paste('max_len:', max_len))

header <- paste('I', 1:max_len, sep = '')
result_frame <- rbind(NULL)

for (tid in tids){
  items <- unique(ft[ft$tid==tid, 2])
  items_len <- length(items)
  diff_len <- max_len - items_len
  row <- c(items, rep('', diff_len))
  result_frame <- rbind(result_frame, row)
}

result_frame <- rbind(header, result_frame)
head(result_frame)

write.arff(result_frame, file = 'q1_apriori.arff')
write.table(
  result_frame,
  'q1_apriori.csv',
  row.names = FALSE,
  col.names = FALSE,
  sep = ",",
  quote = TRUE)