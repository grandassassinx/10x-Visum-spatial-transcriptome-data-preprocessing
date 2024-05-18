# Install and load necessary packages
install.packages("ggplot2")
library(ggplot2)

# Define the directory containing gene expression CSV files
path <- "C:/Users/86136/Downloads/anus/"

# Load the spatial data
colData <- read.csv("C:/Users/86136/Downloads/bio/tissue_positions_list.csv", header = FALSE)
colnames(colData) <- c("spot", "in_tissue", "row", "col", "imagerow", "imagecol")
rownames(colData) <- colData$spot

# List all gene expression CSV files
file_list <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Function to read and label each file
read_gene_file <- function(file) {
  data <- read.csv(file, row.names = 1)  # Assuming first column is row names
  gene_name <- sub(".csv", "", basename(file))
  data$gene <- gene_name
  data$spot <- rownames(data)
  return(data)
}

# Load all gene expression files into a list
gene_data_list <- lapply(file_list, read_gene_file)

# Combine into a single data frame
gene_data <- do.call(rbind, gene_data_list)

# Merge gene expression data with spatial coordinates
merged_data <- merge(gene_data, colData, by.x = "spot", by.y = "spot")

# Plot gene expression for each gene
unique_genes <- unique(merged_data$gene)

for (gene in unique_genes) {
  gene_subset <- subset(merged_data, gene == gene)
  p <- ggplot(gene_subset, aes(x = imagecol, y = imagerow, color = V1)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red") +
    labs(title = paste("Expression of", gene), x = "Image Column", y = "Image Row") +
    theme_minimal()
  
  print(p)  # Display plot
}