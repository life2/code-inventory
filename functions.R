main_dir <- "C:/Users/Derek/Desktop/GBA"
library(fs)

dir_tree(main_dir, recurse = 1)

library(roxygen2)
library(stringr)

r_folder_name <- "R"

r_dir <- file.path(main_dir, r_folder_name)

load_order <- c("/R", "/R/model", "/R/arch", "/R/preprocessors", "/R/reporting",
                "/R/data", "/R/rules", "/R/rules/defs", "/R/validate", "/R/dao", 
                "/R/rule_defs", "/R/build_dataset")

blocks_to_drop <- c("/R/import", "/R/run", "/R/sandbox", "/R/sandbox/import", 
                    "/R/sandbox/reporting", "/R/sandbox/MJF", "/R/assessments")

# get parsed source into roxygen-friendly format
env <- new.env(parent = globalenv())
all_files <- list.files(r_dir, full.names = TRUE, recursive = TRUE, pattern = "[.]R")
block_names <- dirname(gsub(main_dir, "", all_files, fixed = TRUE))
blocks_to_drop <- block_names %in% blocks_to_drop
all_files <- all_files[!blocks_to_drop]
block_names <- block_names[!blocks_to_drop]

block_order <- sapply(block_names, function(x) which(load_order %in% x))
block_order <- order(block_order, all_files)

all_files <- all_files[block_order]
block_names <- block_names[block_order]

get_functions <- function(source_file, source_env = env) {
  
  blocks <- roxygen2::parse_file(source_file, source_env)
  
  get_title <- function(x) {
    tags <- sapply(x[["tags"]], "[[", "tag")
    call <- paste0("`", x[["call"]][2], "()` - ")
    has_title <- which(tags == "title")
    description <- ifelse(length(has_title),
                          x[["tags"]][[has_title]][["raw"]],
                          "")
    description <- str_squish(str_replace_all(description, "\n", " "))
    paste0(call, description)
  }
  
  sapply(blocks, get_title)
}

get_functions <- function(source_file, source_env = env) {
  
  blocks <- roxygen2::parse_file(source_file, source_env)
  
  get_title <- function(x) {
    tags <- sapply(x[["tags"]], "[[", "tag")
    call <- toString(x[["call"]][2])
    has_title <- which(tags == "title")
    description <- ifelse(length(has_title),
                          x[["tags"]][[has_title]][["raw"]],
                          "")
    description <- str_squish(str_replace_all(description, "\n", " "))
    data.frame(name = call, description = description)
  }
  
  #lapply(blocks, get_title)
  do.call(rbind, lapply(blocks, get_title))
}

all_functions <- do.call(rbind, lapply(all_files, get_functions))

im_db <- hsearch_db(package = "imidexR2")
im_db <- im_db$Base[, c("Name", "Title")]
names(im_db) <- c("name", "description")

all_functions <- rbind(all_functions, im_db)

write.csv(all_functions, "all_functions.csv", row.names = FALSE)

current_name <- ""
for (i in seq_along(all_functions)) {
  if(block_names[i] != current_name) {
    cat(paste0("### `", block_names[i], "`"))
    cat("\n\n")
  }
  current_name <- block_names[i]
  cat(paste0("#### `", basename(all_files[i]), "`"))
  cat("\n\n")
  cat(paste0("- ", all_functions[[i]]), sep = "\n")
  cat("\n\n")
}

to_install <- c("anytime", "caTools", "comorbidity", "data.table", "devtools", 
                "digest", "doSNOW", "ff", "foreach", "futile.logger", "geosphere", 
                "ggplot2", "gRain", "hash", "hexbin", "Hmisc", "latticeExtra", 
                "lme4", "lubridate", "medicalrisk", "mRMRe", "ngram", "nlme", 
                "openxlsx", "pcaPP", "PerformanceAnalytics", "plyr", "randomForest", 
                "rattle", "Rcmdr", "RCurl", "reshape2", "rJava", "RMySQL", "RODBC", 
                "roxygen2", "safer", "scales", "shiny", "Snowball", "SnowballC", 
                "sqldf", "stringdist", "stringi", "stringr", "timeSeries", "tm", 
                "topicmodels", "TTR", "twang", "xlsx", "XML", "xts", "zoo")