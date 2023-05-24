library(dplyr)
library(odbc)

project_base_dir <- "C:/Users/Derek/Desktop/GBA"

source(file.path(project_base_dir, "R/main.R"))

# This is a good start
# params2 <- config::get("databases", file = file.path(project_base_dir,"ini/GBA_DEV_DM.yml"), use_parent = FALSE)

params <- 
  get_params_from_file(
    file.path(project_base_dir,"ini/GBA_DEV_DM.ini"),
    setup_logging = FALSE,
    setup_db = FALSE
  )

load_libraries(params)

params <- connect_to_dbs(params)

eligibility <- get_eligibility_db(
  params=params
  ,roll_cols=T
  ,match_col_names=T
)

table_name="eligibility"
eligibility_db <- iSqlQuery(
  channel=params$db
  ,query=paste0("SELECT * FROM ",table_name)
  ,as.is=T
);

eligibility_db <- coerce_data_str(	
  import_data=eligibility_db
  ,import_data_name="eligibility"
  ,params=params
  ,as_is=T
);

con <- dbConnect(odbc::odbc(), dsn = params$db_dsn, uid=params$db_uid, pwd=params$db_pwd)
eb <- DBI::dbReadTable(con, table_name, as.is = TRUE) # Looks like these are "right" as read

eb_dt <- data.table(eb)
