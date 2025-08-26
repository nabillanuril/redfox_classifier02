# Path to the folder
path <- "C:/path/to/your/folder"

remove <- function(path, full.names = TRUE, recursive = TRUE) {
# List all files inside (recursive)
files <- list.files(path, full.names = full.names, recursive = recursive)

# Keep only files (not directories)
files <- files[!dir.exists(files)]

# Delete them
file.remove(files)
}