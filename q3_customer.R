library(RODBC)
library(RWeka)
library(arules)
library(knitr)

db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=.\\foodmart2000-3.mdb")

sql3 <- "
select distinct
    ct.customer_id,
    ct.state_province,
    ct.yearly_income,
    ct.gender,
    ct.total_children,
    ct.num_children_at_home,
    ct.education,
    ct.occupation,
    ct.houseowner,
    ct.num_cars_owned
from customer ct, (
  select 
    customer_id
  from sales_fact_1998
  union
  select
    customer_id
  from sales_fact_dec_1998) as tx
where ct.customer_id = tx.customer_id ;
"

ct <- as.data.frame(sqlQuery(db, sql3))

head(ct)

for(idx in 2:10){
  ct[,idx] <- as.factor(ct[, idx])
}



write.arff(ct[,2:10], file = 'q3_customer.arff')
write.table(
  ct[,2:10],
  'q3_customer.csv',
  row.names = FALSE,
  col.names = TRUE,
  sep = ",",
  quote = FALSE)