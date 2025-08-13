library(stringr)

# The example recommendation provided
recommendation <- "153.13 Ratify the Council of Europe Convention on Preventing and Combating Violence against Women and Domestic Violence (Istanbul Convention) (North Macedonia) (Slovenia) (Belgium) (Canada) (Sweden);"

# A sample list of countries for filtering.
# For a real application, you would use a more comprehensive list.
countries <- c("North Macedonia", "Slovenia", "Belgium", "Canada", "Sweden", "Turkey", "Germany", "Japan")

# Extract all text within parentheses
parentheses_phrases <- str_extract_all(recommendation, "\\([^)]+\\)")[[1]]

# Remove the parentheses from the extracted phrases
cleaned_phrases <- str_replace_all(parentheses_phrases, "[()]", "")

# Filter the cleaned phrases to keep only the ones that are in the `countries` list
state_issuing <- cleaned_phrases[cleaned_phrases %in% countries]

# Count the number of states
number_of_states <- length(state_issuing)

# Print the results
print(paste("Extracted States:", paste(state_issuing, collapse = ", ")))
print(paste("Number of States:", number_of_states))



library(dplyr)
library(tidyr)
library(stringr)

# 1. Create a sample dataset similar to yours
recommendations_df <- tibble(
  rec_id = c("153.13", "154.21"),
  recommendation_text = c(
    "Ratify the Council of Europe Convention on Preventing and Combating Violence against Women and Domestic Violence (Istanbul Convention) (North Macedonia) (Slovenia) (Belgium) (Canada) (Sweden)",
    "Strengthen laws on human trafficking (France) (Germany);"
  )
)

# 2. Reshape the dataset
reshaped_df <- recommendations_df %>%
  # Extract all parenthetical phrases that look like a country name
  mutate(states = str_extract_all(recommendation_text, "\\((.*?)\\)")) %>%
  # Filter out non-state items by checking for a country list (optional but recommended)
  # For this example, we will assume all parenthetical phrases are states.
  # If you have a country list, you would apply a filtering step here.
  # The next step will handle the separation of states into individual rows
  unnest_longer(states, values_to = "issuing_state") %>%
  # Clean up the `issuing_state` column to remove parentheses
  mutate(issuing_state = str_remove_all(issuing_state, "[()]"))

# Print the resulting dataset
print(reshaped_df)
