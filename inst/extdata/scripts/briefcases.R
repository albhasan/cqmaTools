# briefcases.R - Extract data from gas-analysis briefcases

#---- TODO ----
# - script description
# delete
library(devtools)
setwd("/home/lagee/Documents/ghProjects/cqmaTools")
devtools::load_all()

#---- Script setup ----
# base path
test_path <-   "~/Documents/alber/test"

# Path to flight logs
magicc_path <- "~/home 2/magicc/Malas"

stopifnot(all(sapply(c(test_path, magicc_path), dir.exists)))

#---- Script parameters ----
site <- toupper("SAN")
data_out_path <- file.path(test_path, "BCS_results") 
stopifnot(dir.exists(data_out_path))

#---- Log setup ----
require(log4r)
logger <- create.logger()
logfile(logger) <- file.path(data_out_path, "briefcases.log")
level(logger) <- "DEBUG"
info(logger, "Start! ###############################################")

#---- SCRIPT ----
debug(logger, paste0("site: ", site))
debug(logger, paste0("magicc_path: ", magicc_path))
debug(logger, paste0("data_out_path: ", data_out_path))

# get the files
pattern <- paste("^PFP_[0-9]{3,4}_", site, "_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.(TXT|txt)", sep = "")
file_vec <- list.files(magicc_path, recursive = TRUE,
                       pattern = pattern, 
                       full.names = TRUE)

# call the script
bc_list <- lapply(file_vec, cqmaTools::getHistoryAC)

bc_list <- lapply(bc_list, function(x){
  #x <- bc.list[[1]]
  #x <- bc.list[[81]]
  #x <- bc.list[[90]]
  #x <- bc.list[[91]]

  # Criteria for filtering invalid observations:
  # - flight plan has more than 12 observations
  # - flight plan heigh is divisible by 100
  # - flight plan is not 0
  if (length(x[, 10]) > 12) {
    if (all(x[18:20, 2] %% 100 == 0)) {
      if (all(x[18:20, 2] < 500)) {  
        x <- x[1:17,]
      }
    }else{
      x <- x[1:17,]
    }
  }
  
  # remove invalid pressure values (300 < p < 1200)
  if (
    (sum((x[, 10] > 0), na.rm = TRUE) == length(x[, 10])) + 
    (sum((x[, 10] < 300), na.rm = TRUE) == 0) +
    (sum((x[, 10] > 1200), na.rm = TRUE) == 0) != 3
  ) {
    x[, 10] <- NA
  }
  return(x)
})

bc_df <- do.call("rbind", bc_list)                                              # cast set of lists to data.frame

out_file <- file.path(data_out_path, paste(site, "_briefdata.txt", sep = ''))
debug(logger, sprintf("writing to file: %s", out_file))
write.table(bc_df, file = out_file)

#-------------------------------------------------------------------------------
# find the data file with errors in format
#-------------------------------------------------------------------------------
#for(i in 1:length(file.vec)){
#  print(file.vec[i])
#  .getHistoryAC(file.vec[i])
#}
#-------------------------------------------------------------------------------
# call script on a single file
#-------------------------------------------------------------------------------
# file.in <- "/home/lagee/home 2/magicc//Malas/SAN/2011 SAN/PFP_210_SAN_2011_05_24.txt"
# .getHistoryAC(file.in)
