
### Code Generating Root Folders ##############################################

#Define Top-Level Directories
directories <- c("build", "analysis")

# Define the subdirectories for each top-level directory
subdirectories <- c("input", "code", "output", "temp")

# Create the directory structure
for (dir in directories) {
  # Create the top-level directory
  dir.create(dir)
  
  # Create the subdirectories within the top-level directory
  for (subdir in subdirectories) {
    dir.create(file.path(dir, subdir))
  }
}

###############################################################################


