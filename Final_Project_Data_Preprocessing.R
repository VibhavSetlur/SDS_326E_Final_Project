library(tidyverse)

df = read.csv('~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/Data/Protein_Family_Dataset.csv',
              col.names = c('Entry', 'Entry_Name', 'Protein_Name', 'Genes', 'Length', 'Sequence', 'Genome_Location', 'Mass', 'Features', 'Protein_Location', 'Chain', 'Disulfide_Bond', 'Glycosylation', 'Peptide', 'Residue', 'Lipidation', 'Post_Translational_Modification', 'Propeptide', 'Signal_Peptide', 'Transit_Peptide', 'Beta_Strand', 'Turn', 'Helix', 'Coiled_Coil', 'Compositional_Bias', 'Domain_Comments', 'Domain_Feature_Table', 'Repeat', 'Protein_Family', 'Motif'),
              na.strings = '')

clans = read.csv('~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/SDS_326E_Final_Project/protein_clan_names.csv',
                 row.names = 1,
                 col.names = c('UniProt_Name', 'Pfam_Name', 'Distance', 'Similarity', 'Protein_Clan'),
                 na.strings = '') 

df %>% 
  # Remove extra text in Genome_Location column
  mutate(Genome_Location = str_remove(Genome_Location, 'UP000005640: '))  %>% 
  
  # Remove unnecessary columns
  select(-Entry,
         -Entry_Name,
         -Genes,
         -Protein_Location,
         -Chain, 
         -Disulfide_Bond, 
         -Glycosylation, 
         -Peptide, 
         -Residue, 
         -Lipidation, 
         -Post_Translational_Modification, 
         -Propeptide, 
         -Signal_Peptide, 
         -Transit_Peptide,
         -Beta_Strand,
         -Turn,
         -Helix,
         -Coiled_Coil,
         -Compositional_Bias,
         -Domain_Comments,
         -Domain_Feature_Table) %>% 
  
  # Extract information from Features column
  {
    # Get all unique features
    features = str_split(.$Features, "; ") %>%
      unlist() %>%
      str_extract("^[A-Za-z ]+") %>%
      str_trim() %>%
      unique()
    
    # Define the extract function
    extract_count = function(text, feature) {
      if (is.na(text)) return(0)
      pattern = paste0(feature, " \\((\\d+)\\)")
      match = str_extract(text, pattern)
      if (is.na(match)) return(0)
      as.numeric(str_extract(match, "\\d+"))
    }
    
    # Apply to each feature
    for (feature in features) {
      .[[feature]] = map_dbl(.$Features, ~extract_count(.x, feature))
    }
    # Return the modified data frame with new feature columns
    .  
  } %>% 
  
  # Remove the original Features column as it's no longer needed
  select(-Features) %>% 
  
  # Replace Protein_Family with Protein_Clan
  inner_join(clans, by = c('Protein_Name' = 'UniProt_Name')) %>% 
  select(-Protein_Family,
         -Pfam_Name,
         -Distance,
         -Similarity) %>% 
  filter(!is.na(Protein_Clan)) %>% 
  
  # Remove all other NA values if low number of NAs
  filter(!is.na(Genome_Location)) -> df

# Write the cleaned data frame to a new CSV file
write.csv(df, '~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/Data/Protein_Family_Dataset_Cleaned.csv')


