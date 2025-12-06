
## This script is to extract 4800 simulated islands that applied to get MLE results (empirical data).
# `sim_list.rds`
## And make a data frame of all MLE results `mle_df_{repID}.rds`(100 * 6) saved in "mle_df_data" folder.

rm(list = ls())
library(purrr)
#### Deal with MLE results ####
# Need to extract the corresponding island simulated that gets the MLE (100),
# and make all MLE a data frame

# read all island data (48 lists, and each has 1000 sublists)
iw_sim_list <- readRDS("~/Downloads/temp/MLE_example/dst/iw_sim_list.rds")

# folder path "01" and list all .txt files
# 08, 13, 14, 26, 38

folder_path <- "~/Downloads/temp/MLE_example/num_cycles5_100/38"
txt_files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# read the .txt files into data frames
read_txt_file <- function(filepath) {
  read.table(filepath, header = TRUE)
}

mle_list <- map(txt_files, read_txt_file)
mle_df <- bind_rows(mle_list)
saveRDS(mle_df, "~/Downloads/phd_yang/pkgs/iwABC/script/forResults/mle_data/mle_df_38.rds")

# extract indices from file names (e.g., 0001.txt → 1, 0402.txt → 402)
extract_index <- function(filename) {
  as.integer(gsub(".txt", "", basename(filename)))
}

file_indices <- map_int(txt_files, extract_index)



# read sim_list
#sim_list <- readRDS("~/Downloads/phd_yang/pkgs/iwABC/script/forResults/mle_data/sim_list.rds")

#sim_list <- vector(mode = "list", 48)
# extract sublists from iw_sim_list[[1]]
sim_list[[38]] <- iw_sim_list[[38]][file_indices]



# saveRDS(sim_list, "~/Downloads/phd_yang/pkgs/iwABC/script/forResults/mle_data/sim_list.rds")


# Read it at one step -----------------------------------------------------

# Results are from
# 01, 02, 03, 04, 05, 06, 08, 09, 10, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26,
# 30, 32, 33, 34, 37, 38, 40, 41, 42, 43, 44, 48

input_base <- "script/forResults/mle_data/num_cycles5_100"
output_base <- "script/forResults/mle_data/mle_df_data"

dataset_ids <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                 "12", "13", "14", "15", "16", "17", "18", "19",  "20",
                 "21", "22", "23", "24", "25", "26","27", "28", "29", "30","31", "32",
                 "33", "34", "35","36", "37", "38", "39", "40", "41", "42",
                 "43", "44", "45", "46", "47", "48")

read_save_mle_data <- function(folder_id){

  folder_path <- file.path(input_base, folder_id)
  txt_files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

  # check if there are no .txt files in the folder
  if(length(txt_files) == 0){
    message("No .txt files found in folder: ", folder_path)
    return(NULL)
  }

  mle_list <- map(txt_files, ~ read.table(.x, header = TRUE))
  mle_df <- bind_rows(mle_list)

  save_path <- file.path(output_base, paste0("mle_df_", as.integer(folder_id), ".rds"))
  saveRDS(mle_df, save_path)

}

walk(dataset_ids, read_save_mle_data) # purr::walk, Use **walk()** when you’re
# saving files, printing, or doing something else where the result is not needed.





# read all island data (48 lists, and each has 1000 sublists)
iw_sim_list <- readRDS("~/Downloads/temp/MLE_example/dst/iw_sim_list.rds")

# Extract index function
extract_index <- function(filename) {
  as.integer(gsub(".txt", "", basename(filename)))
}

sim_list <- vector(mode = "list", 48)

for (id in dataset_ids) {

  folder_path <- file.path(input_base, id)
  txt_files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

  # check if there are no .txt files in the folder
  if(length(txt_files) == 0){
    message("No .txt files found in folder: ", folder_path)
    next
  }

  file_indices <- map_int(txt_files, extract_index)
  i <- as.integer(id)
  sim_list[[i]] <- iw_sim_list[[i]][file_indices]
}

saveRDS(sim_list, "script/forResults/mle_data/sim_list.rds")

