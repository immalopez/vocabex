library(tidyverse)
library(here)

df <- read_csv("exercises_analysis.csv")

# 1. Drop useless columns & rename & order existent ones

clean_df <- df %>%
  select(-word_count,
         -freq_A1,
         -freq_A2,
         -freq_B1,
         -freq_B2,
         -freq_C1,
         -freq_C2,
         -post_test_freq_A1,
         -post_test_freq_A2,
         -post_test_freq_B1,
         -post_test_freq_B2,
         -post_test_freq_C1,
         -post_test_freq_C2,
         -lex_complex,
         -msynt_complex,
         -post_test_lex_complex,
         -post_test_msynt_complex,
         -(starts_with("upos-") & !ends_with(" DOC%")),
         -(starts_with("deprel-") & !ends_with(" DOC%"))
         )


# 2. Label row-position within each participant

df2 <- clean_df %>%
  group_by(participant_ID) %>%
  mutate(rn = row_number()) %>%
  ungroup()

# 3. Pull out exercise rows (1-15)

exercises <- df2 %>%
  filter(rn <= 15)

# 4. Summarize post-test metrics into a wide table

post_tests <- df2 %>%
  filter(rn %in% 16:17) %>%
  mutate(test = if_else(rn == 16, "imm", "del")) %>%
  select(participant_ID, test, Total:last_col()) %>%
  pivot_wider(
    id_cols = participant_ID,
    names_from = test,
    values_from = Total:last_col(),
    names_sep = "_"
  )

# 5. Join back onto the 15 exercise rows

final_df <- exercises %>%
  left_join(post_tests, by = "participant_ID") %>%
  select(-rn,
         -rn_imm,
         -rn_del)

# 6. Save it all

write.csv(final_df, "cleaned_data.csv", row.names = FALSE)
