library(tidyverse)

df = read.csv('~/Downloads/UT_Austin/Spring_2025/SDS_326E_Elements_of_Machine_Learning/Final_Project/Data/Protein_Family_Dataset.csv',
              col.names = c('Entry', 'Entry_Name', 'Protein_Name', 'Genes', 'Length', 'Sequence', 'Genome_Location', 'Mass', 'Features', 'Protein_Location', 'Chain', 'Disulfide_Bond', 'Glycosylation', 'Peptide', 'Residue', 'Lipidation', 'Post_Translational_Modification', 'Propeptide', 'Signal_Peptide', 'Transit_Peptide', 'Beta_Strand', 'Turn', 'Helix', 'Coiled_Coil', 'Compositional_Bias', 'Domain_Comments', 'Domain_Feature_Table', 'Repeat', 'Protein_Family', 'Motif'),
              na.strings = '')

df %>% 
  # Filter out NA values in Protein_Family (Target Variable)
  filter(!is.na(Protein_Family)) %>% 
  
  # Remove extra text in Genome_Location column
  mutate(Genome_Location = str_remove(Genome_Location, 'UP000005640: '))  %>% 
  
  # Extract information from Features column
  {
    # Get all unique features
    features <- str_split(.$Features, "; ") %>%
      unlist() %>%
      str_extract("^[A-Za-z ]+") %>%
      str_trim() %>%
      unique()
    
    # Define the extract function
    extract_count <- function(text, feature) {
      if (is.na(text)) return(0)
      pattern <- paste0(feature, " \\((\\d+)\\)")
      match <- str_extract(text, pattern)
      if (is.na(match)) return(0)
      as.numeric(str_extract(match, "\\d+"))
    }
    
    # Apply to each feature
    for (feature in features) {
      .[[feature]] <- map_dbl(.$Features, ~extract_count(.x, feature))
    }
    # Return the modified data frame with new feature columns
    .  
  } %>% 
  
  # Remove unnecessary columns
  select(-Entry, 
         -Protein_Name, 
         -Genes, 
         -Chain, 
         -Disulfide_Bond, 
         -Glycosylation, 
         -Peptide, 
         -Residue, 
         -Lipidation, 
         -Post_Translational_Modification, 
         -Propeptide, 
         -Signal_Peptide, 
         -Transit_Peptide) -> df




# ANY OF IDS CAN BE USED ALL ARE UNIQUE
# see if entry name, entry and protein name are unique
df %>% 
  # Check for duplicates in Entry, Entry_Name, and Protein_Name
  summarise(
    unique_entry = n_distinct(Entry),
    unique_entry_name = n_distinct(Entry_Name),
    unique_protein_name = n_distinct(Protein_Name),
    total_rows = n()
  ) %>%
  mutate(
    entry_duplicates = total_rows - unique_entry,
    entry_name_duplicates = total_rows - unique_entry_name,
    protein_name_duplicates = total_rows - unique_protein_name
  )













# Separate Features column into multiple columns
{
  feature_names <- .$Features %>%
    str_split("; ") %>%
    unlist() %>%
    str_extract("^[A-Za-z ]+") %>%
    unique() %>%
    str_trim()
  
  extract_count <- function(text, feature) {
    if (is.na(text)) {
      return(0)
    }
    pattern <- paste0(feature, " \\((\\d+)\\)")
    match <- str_extract(text, pattern)
    if (is.na(match)) {
      return(0)
    } else {
      return(as.numeric(str_extract(match, "\\d+")))
    }
  }
  
  feature_names %>%
    walk(function(feature) {
      assign("df", mutate(df, !!sym(feature) := sapply(df$Features, function(x) extract_count(x, feature))), envir = parent.frame())
    })
  
  .
}
    pattern = paste0(feature, " \\((\\d+)\\)")
    match = str_extract(text, pattern)
    if (is.na(match)) {
      return(0)
    } else {
      return(as.numeric(str_extract(match, "\\d+")))
    }
  }
  
  feature_names %>%
    purrr::walk(function(feature) {
      . <= mutate(., !!sym(feature) := sapply(.$Features, function(x) extract_count(x, feature)))
    })
  
  .
}
