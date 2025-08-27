remove <- function(path, full.names = TRUE, recursive = TRUE) {
# List all files inside (recursive)
files <- list.files(path, full.names = full.names, recursive = recursive)
# Keep only files (not directories)
files <- files[!dir.exists(files)]
# Delete them
file.remove(files)
}

remove("F:/MSc Ecology & Data Science Research/3. augment data")
remove("F:/MSc Ecology & Data Science Research/3. augment data_train")
remove("F:/MSc Ecology & Data Science Research/3. test_data")
remove("F:/MSc Ecology & Data Science Research/3. train_data")
