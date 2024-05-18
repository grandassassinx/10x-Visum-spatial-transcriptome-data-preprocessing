library(Matrix)

# Load barcode data
barcodes <- read.csv("C:/Users/86136/Downloads/bio/barcodes.tsv", header = FALSE, sep = '\t')
print(paste("barcodes dimensions:", dim(barcodes)))
print(head(barcodes))

# Load spatial data and set column names
colData <- read.csv("C:/Users/86136/Downloads/bio/tissue_positions_list.csv", header = FALSE)
colnames(colData) <- c("spot", "in_tissue", "row", "col", "imagerow", "imagecol")
rownames(colData) <- colData$spot
print(paste("colData dimensions:", dim(colData)))
print(head(colData))

# Load feature data and set column names
rowData <- read.csv("C:/Users/86136/Downloads/bio/features.tsv", header = FALSE, sep = '\t')
colnames(rowData) <- c("gene_id", "gene_name", "feature_type")
rowData <- rowData[, c("gene_id", "gene_name")]
print(paste("rowData dimensions:", dim(rowData)))
print(head(rowData))

# Ensure gene names are unique
rowData$gene_name <- make.unique(rowData$gene_name)
if (length(unique(rowData$gene_name)) != nrow(rowData)) {
  stop("RowData gene names are still not unique after attempting to make unique")
}

# Load counts data and set row and column names
counts <- readMM("C:/Users/86136/Downloads/bio/matrix.mtx")
print(paste("counts dimensions before setting names:", dim(counts)))

colnames(counts) <- barcodes$V1
rownames(counts) <- rowData$gene_name

# Identify mismatches between colData row names and counts column names
missing_in_counts <- setdiff(rownames(colData), colnames(counts))
missing_in_colData <- setdiff(colnames(counts), rownames(colData))

if (length(missing_in_counts) > 0) {
  print("Identifiers in colData but not in counts:")
  print(missing_in_counts)
}

if (length(missing_in_colData) > 0) {
  print("Identifiers in counts but not in colData:")
  print(missing_in_colData)
}

# Filter colData and counts to ensure they align
common_identifiers <- intersect(rownames(colData), colnames(counts))

# Subset colData and counts to the common identifiers
colData <- colData[common_identifiers, ]
counts <- counts[, common_identifiers]

print("After subsetting, colData dimensions:")
print(dim(colData))
print("After subsetting, counts dimensions:")
print(dim(counts))

# Define path for saving filtered gene expression data
path <- "C:/Users/86136/Downloads/"

# Filter genes with expression level greater than 60 and save them
for (i in 1:nrow(counts)) {
  tem <- counts[i, ]
  if (sum(tem) >= 60) {
    gene_name <- rownames(counts)[i]
    dir <- file.path(path, paste(gene_name, ".csv", sep = ""))
    write.csv(as.matrix(tem), dir)
  }
}