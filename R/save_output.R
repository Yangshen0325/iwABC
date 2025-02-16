
# save the output
save_output <- function(output,
                        param_set,
                        ss_set){

  output_file_name <- paste0("param_set_", param_set, "_ss_", ss_set, ".rds")
  output_folder <- file.path(
    getwd(),
    "results"
  )

  # Create the folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  output_file_path <- file.path(output_folder, output_file_name)
  message(
    paste0("Trying to save ", output_file_name, " to ", output_file_path, "\n")
  )
  saveRDS(output, file = output_file_path)

  if (file.exists(output_file_path)) {
    message(paste0("Saved ", output_file_name, " to ", output_file_path, "\n"))
  } else {
    warning(paste0(
      "File not found. Saving to ",
      output_file_path,
      " likely failed.\n")
    )
  }

}
