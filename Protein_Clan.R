pfam = read.csv('~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/SDS_326E_Final_Project/Pfam-A.clans.tsv', 
                sep = '\t',
                header = FALSE)

df = read.csv('~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/Data/Protein_Family_Dataset.csv',
              col.names = c('Entry', 'Entry_Name', 'Protein_Name', 'Genes', 'Length', 'Sequence',
                            'Genome_Location', 'Mass', 'Features', 'Protein_Location', 'Chain',
                            'Disulfide_Bond', 'Glycosylation', 'Peptide', 'Residue', 'Lipidation',
                            'Post_Translational_Modification', 'Propeptide', 'Signal_Peptide',
                            'Transit_Peptide', 'Beta_Strand', 'Turn', 'Helix', 'Coiled_Coil',
                            'Compositional_Bias', 'Domain_Comments', 'Domain_Feature_Table',
                            'Repeat', 'Protein_Family', 'Motif'),
              na.strings = '')

# Compare V5 col in pfam with protein name in df
intersect(pfam$V5, df$Protein_Name) %>% length()


uniprot_names <- df$Protein_Name[!is.na(df$Protein_Name)]  # Extract non-NA protein names from df
pfam_names <- pfam$V5[!is.na(pfam$V5)]  # Extract non-NA protein names from pfam

library(stringdist)
library(dplyr)

# Compute pairwise string distances using Jaro-Winkler similarity
distance_matrix <- stringdistmatrix(uniprot_names, pfam_names, method = "jw") 


# Organize the result to display UniProt name, Pfam name, and distance
# Create a dataframe with each pair and their corresponding Jaro-Winkler distance
result_data <- expand.grid(UniProt_Name = uniprot_names, Pfam_Name = pfam_names)
result_data$Distance <- as.vector(distance_matrix)  # Adding the distance values

# Optionally, convert distance to similarity (1 - distance)
result_data$Similarity_Score <- 1 - result_data$Distance


# Find the best match for each UniProt protein
best_matches <- apply(similarity_matrix, 1, function(x) {
  max_score <- max(x)
  matched_pfam <- pfam_names[which.max(x)]
  return(c(matched_pfam, max_score))
})

# Create a new dataframe to store the best match and its similarity score
best_matches_df <- data.frame(
  UniProt_Name = uniprot_names,
  Best_Pfam_Name = sapply(best_matches, `[`, 1),
  Best_Similarity_Score = sapply(best_matches, `[`, 2)
)

# Merge the best matches with the full result_data
merged_data <- merge(result_data, best_matches_df, by = "UniProt_Name")



