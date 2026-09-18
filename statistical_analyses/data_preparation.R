library(tidyverse)
library(here)
library(stringr)

source("R/run_registry.R")

df_errors <- read_csv("annotated_exercise_data.csv")

# Use the dedicated retrieval annotation; missing values must not become zero.
stopifnot("retrieval" %in% names(df_errors))
stopifnot(
  !anyNA(df_errors$retrieval),
  all(df_errors$retrieval %in% c(0, 1))
)

df_complexity <- read_csv("cleaned_data.csv")

df_complexity <- df_complexity %>%
  rename(text_file = `Text file`) %>%
  select(-any_of(setdiff(intersect(names(.), names(df_errors)), "text_file")))

df <- df_errors %>%
  inner_join(df_complexity, by = "text_file")

# 1. ------------ Check for missing values ------------

anyNA(df)

# 2. ------------ Rename exercise types and formats ------------

df0 <- df %>% mutate(id = row_number())
df0 <- df0 %>%
  mutate(exercise_format = exercise_format %>%
           str_squish() %>%
           str_to_lower())

sep_pat <- "\\s*, \\s*"

typ_map <- c(
  # 1. Meaning and sense elaboration
  "2K" = "meaning and sense elaboration",
  "2L" = "meaning and sense elaboration",
  "3I" = "meaning and sense elaboration",
  "4B" = "meaning and sense elaboration",
  "2M" = "meaning and sense elaboration",
  "5C" = "meaning and sense elaboration",
  # 2. Contextual retrieval
  "3B" = "contextual retrieval",
  "2F" = "contextual retrieval",
  "2D" = "contextual retrieval",
  # 3. Productive use of targets
  "5E" = "productive use of targets",
  "2I" = "productive use of targets",
  "1D" = "productive use of targets",
  "4A" = "productive use of targets",
  "4D" = "productive use of targets",
  # 4. Collocational control
  "5D" = "collocational control",
  "3H" = "collocational control",
  "3A" = "collocational control",
  "3D" = "collocational control",
  "5B" = "collocational control",
  # 5. Conceptual organization
  "5A" = "conceptual organization",
  "5H" = "conceptual organization",
  "1C" = "conceptual organization",
  "1A" = "conceptual organization",
  "3F" = "conceptual organization",
  "2J" = "conceptual organization",
  "5F" = "conceptual organization",
  # 6. Paraphrastic flexibility
  "2G" = "paraphrastic flexibility",
  "2H" = "paraphrastic flexibility",
  # 7. Pragmatic appropriateness
  "2A" = "pragmatic appropriateness",
  "2N" = "pragmatic appropriateness"
)

fmt_map <- c(
  # 1. Cloze
  "fill-in-the-blanks" = "cloze",
  # 2. Discrete choice
  "binary choice" = "discrete choice",
  "multiple choice" = "discrete choice",
  "leave the odd one out" = "discrete choice",
  # 3. Matching
  "matching" = "matching",
  "picture-based matching" = "matching",
  "situation matching" = "matching",
  # 4. Prompted writing
  "writing" = "prompted writing",
  "picture-based writing" = "prompted writing",
  # 5. Sentence reconstruction
  "sentence combining" = "sentence reconstruction",
  "reorder" = "sentence reconstruction",
  # 6. Translation
  "translation" = "translation",
  "table-based translation" = "translation",
  "table based-translation" = "translation",
  # 7. Text editing
  "error correction" = "text editing",
  "rewriting" = "text editing",
  # 8. Metalinguistic reflection
  "reflection" = "metalinguistic reflection",
  "picture-based reflection" = "metalinguistic reflection",
  "definition creation" = "metalinguistic reflection",
  "context creation" = "metalinguistic reflection",
  "scale of intensity" = "metalinguistic reflection",
  # 9. Syntagmatic systematization
  "table-based collocation combining" = "syntagmatic systematization",
  "table-based word combining" = "syntagmatic systematization",
  "continue a sequence" = "syntagmatic systematization",
  # 10. Paradigmatic systematization
  "classification" = "paradigmatic systematization",
  "table completion" = "paradigmatic systematization",
  "example selection" = "paradigmatic systematization",
  "instantiation" = "paradigmatic systematization",
  "drawing" = "paradigmatic systematization",
  # 11. Vocabulary extraction and list compilation
  "vocabulary extraction" = "vocabulary extraction and list creation",
  "list creation" = "vocabulary extraction and list creation",
  # 12. Accent marking and read-aloud
  "accent placement exercise" = "accent marking and read-aloud",
  "reading out loud" = "accent marking and read-aloud"
)

recode_csl <- function(x, map, collapse = ", ") {
  map_chr(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    toks <- str_split(s, sep_pat)[[1]] |> str_trim()
    rec <- recode(toks, !!!map, .default = NA_character_)
    rec <- rec[!is.na(rec)]
    rec <- rec[!duplicated(rec)]
    if (!length(rec)) NA_character_ else paste(rec, collapse = collapse)
  })
}


df0 <- df0 %>%
  mutate(
    exercise_type = recode_csl(exercise_type, typ_map),
    exercise_format = recode_csl(str_to_lower(exercise_format), fmt_map)
  )

gold_cols <- c("participant_ID","text_file","error",
               "exercise_type", "exercise_format")

write_csv(df0[, gold_cols], run_output("inputs", "exercises_recoded_gold.csv"))

# 3. ------------ Basic distribution summaries ------------

types_long <- df0 %>%
  mutate(id = row_number()) %>%
  select(id, exercise_type) %>%
  separate_rows(exercise_type, sep = sep_pat) %>%
  mutate(exercise_type = recode(exercise_type, !!!typ_map)) %>%
  distinct(id, exercise_type)

formats_long <- df0 %>%
  mutate(id = row_number()) %>%
  select(id, exercise_format) %>%
  separate_rows(exercise_format, sep = sep_pat) %>%
  mutate(exercise_format = str_to_lower(str_squish(exercise_format)),
         exercise_format = recode(exercise_format, !!!fmt_map)) %>%
  distinct(id, exercise_format)

# 3.1. Unique codes

n_types <- dplyr::n_distinct(types_long$exercise_type)
n_formats <- dplyr::n_distinct(formats_long$exercise_format)

# 3.2. Frequencies (counts + proportions)
types_freq <- types_long %>% count(exercise_type, sort = TRUE) %>% mutate(p = n/sum(n))
formats_freq <- formats_long %>% count(exercise_format, sort = TRUE) %>% mutate(p = n/sum(n))

# 3.3. Codes per exercise
types_per_row <- types_long %>% count(id, name = "n_types")
formats_per_row <- formats_long %>% count(id, name = "n_formats")

dist_types_per_row <- types_per_row %>% count(n_types, name = "n") %>% mutate(p = n/sum(n))
dist_formats_per_row <- formats_per_row %>% count(n_formats, name = "n") %>% mutate(p = n/sum(n))

# 3.4. Type-format co-occurrence (which pairs appear together)
pairs_long <- df0 %>%
  separate_rows(exercise_type, sep = sep_pat) %>%
  separate_rows(exercise_format, sep = sep_pat) %>%
  mutate(across(c(exercise_type, exercise_format), ~str_squish(.))) %>%
  filter(exercise_type != "", exercise_format != "", !is.na(exercise_type), !is.na(exercise_format)) %>%
  distinct(id, exercise_type, exercise_format)

pairs_freq <- pairs_long %>% count(exercise_type, exercise_format, sort = TRUE)

# 3.5. Heatmap

stopifnot(!any(duplicated(types_long[,c("id","exercise_type")])))
stopifnot(!any(duplicated(formats_long[,c("id","exercise_format")])))

pairs_freq <- types_long %>%
  inner_join(formats_long, by = "id", relationship = "many-to-many") %>%
  count(exercise_type, exercise_format, name = "n", sort = TRUE)

pairs_rowprop <- pairs_freq %>%
  group_by(exercise_type) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()

pairs_rowprop %>%
  ggplot(aes(exercise_format, exercise_type, fill = p)) +
  geom_tile() +
  coord_fixed() +
  labs(x = "Format", y = "Goal type", fill = "Row %") +
  scale_fill_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 11) +
  scale_x_discrete(guide = guide_axis(angle = 90))


# 4. ------------ Join back to the original df ------------

snake <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

goal_wide <- types_long %>%
  mutate(
    goal_key = snake(exercise_type),
    goal_col = paste0("goal_", goal_key),
    val = 1L
  ) %>%
  distinct(id, goal_col, .keep_all = TRUE) %>%
  pivot_wider(id_cols = id,
              names_from = goal_col,
              values_from = val,
              values_fill = 0)

fmt_wide <- formats_long %>%
  mutate(
    fmt_key = snake(exercise_format),
    fmt_col = paste0("fmt_", fmt_key),
    val = 1L
  ) %>%
  distinct(id, fmt_col, .keep_all = TRUE) %>%
  pivot_wider(id_cols = id,
              names_from = fmt_col,
              values_from = val,
              values_fill = 0)

df_multi <- df0 %>%
  select(id, everything()) %>%
  left_join(goal_wide, by = "id") %>%
  left_join(fmt_wide, by = "id") %>%
  mutate(
    across(starts_with("goal_"), ~coalesce(.x, 0L)),
    across(starts_with("fmt_"),  ~coalesce(.x, 0L))
  )

df_multi <- df_multi %>%
  mutate(
    n_goals = rowSums(across(starts_with("goal_"))),
    n_formats = rowSums(across(starts_with("fmt_")))
  )

# 5. ------------ Recode gram_cat and IL variables ------------

slash_sep <- "\\s*/\\s*"

normalize_gram <- function(x) {
  map_chr(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    toks <- str_split(s, slash_sep)[[1]] |> str_trim()
    toks <- toks[toks != ""]
    toks_u <- unique(toks)
    if(length(toks_u) == 0) NA_character_
    else if (length(toks_u) == 1) toks_u
    else paste(toks_u, collapse = ", ")
  })
}

df_multi <- df_multi %>%
  mutate(gram_cat_clean = normalize_gram(gram_cat))

df_multi <- df_multi %>%
  mutate(
    gram_primary = if_else(
      is.na(gram_cat_clean), NA_character_,
      str_split(gram_cat_clean, sep_pat) |> map_chr(~str_trim(.x[1]))
    ),
    gram_primary = factor(gram_primary)
  )

gram_long <- df_multi %>%
  transmute(id, gram = gram_cat_clean) %>%
  separate_rows(gram, sep = sep_pat) %>%
  mutate(gram = str_trim(gram)) %>%
  filter(!is.na(gram), gram != "") %>%
  distinct()

gram_wide <- gram_long %>%
  mutate(val = 1L) %>%
  pivot_wider(names_from = gram, values_from = val,
              values_fill = 0, names_prefix = "gram_")

df_multi <- df_multi %>%
  left_join(gram_wide, by = "id") %>%
  mutate(
    across(c(starts_with("gram_"),
             -all_of(c("gram_cat","gram_cat_clean","gram_primary"))),
           ~ as.integer(.)),
    n_gramcats = rowSums(
      pick(c(starts_with("gram_"),
             -all_of(c("gram_cat","gram_cat_clean","gram_primary")))),
      na.rm = TRUE
    )
  )

aligned_1 <- c(
  "because i am aware i need to practice a lot",
  "helpful",
  "i needed the practice",
  "interesting and specific to my problems",
  "it was good practice",
  "personalized learning is a great opportunity",
  "to improve my level of spanish",
  "to improve myself",
  "to increase my error awareness and improve my spanish",
  "to learn",
  "to learn and work on recurrent mistakes",
  "to learn from my mistakes",
  "to learn in general and specifically from my mistakes",
  "to make the most out of this opportunity to learn",
  "to put my abilities to the test",
  "to reflect on my mistakes",
  "useful",
  "useful for practice"
)

aligned_0 <- c(
  "forgot about them",
  "it was easy",
  "it was required",
  "it was fun and required"
)

df_multi <- df_multi %>%
  mutate(
    why_norm = str_to_lower(str_squish(why_solve)),
    ownership = case_when(
      why_norm %in% aligned_1 ~ 1L,
      why_norm %in% aligned_0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

rx_any <- function(vec) regex(paste(vec, collapse="|"), ignore_case = TRUE)

pos <- c(
  "personalized",
  "excellent for",
  "helpful even for my l1",
  "interlinguistic compar",
  "made me think", "^it made me think$",
  "raised my error awareness", "^it raised my error awareness$",
  "^insightful$",
  "^relevant$", "^relevant and reflective$", "relevant and useful definitions",
  "^focused$", "creative, useful", "promotes idiomaticity",
  "^useful$", "very useful", "useful for", "useful example", "useful examples",
  "useful explanation", "useful rules", "useful revision", "useful for revision",
  "^helpful$", "good practice", "good drill", "challenging and very useful"
)

neg <- c(
  "unclear", "a little unclear", "confusing", "confusing rules",
  "did not understand", "did not know what to do", "did not know how to solve",
  "did not see the point", "did not understand the purpose",
  "lack of (guidance|explanation|context|depth)", "no explanation",
  "solution already given", "the solution is missing",
  "irrelevant", "not relevant", "not directly related to my errors",
  "not a frequent (error|mistake)", "already acquired", "forgot about doing it",
  "too (easy|difficult|long|much information)", "^easy$", "^difficult$", "^very difficult$",
  "boring", "boring but useful", "weird sentences", "more stylistic than essential",
  "only useful to learn new vocabulary"
)

df_multi <- df_multi %>%
  mutate(
    belief_norm = str_to_lower(str_squish(belief)),
    goal_aligned = case_when(
      str_detect(belief_norm, rx_any(pos)) ~ 1L,
      TRUE                                 ~ 0L
    ))

# Binary transforms (threshold 1 if > 2 for 1–4 scales)
df_multi <- df_multi %>%
  mutate(
    relevant_goals = as.integer(overall_usefulness_score > 2),
    interesting_content = as.integer(evaluation_score > 2),
    positive_emotion = as.integer(overall_experience_score > 2),
    
# Union of L1/L2 comparison
    l1_l2_comparison = as.integer(coalesce(l1_comparison, 0) == 1 | coalesce(l2_comparison, 0) == 1)
  ) %>%
  mutate(across(c(
    goal_aligned, ownership, understanding, focused,
    receptive_explanation, productive_metalinguistic, search,
    retrieval, productive_retrieval, recall, multiple_retrievals,
    spaced_retrievals, option_comparison, sentence_creation,
    composition_creation, examples, images, interference,
    relevant_goals, interesting_content, positive_emotion, l1_l2_comparison
  ), ~ as.integer(coalesce(.x, 0))))
  
# 6. ------------ Compute IL/TFA index ------------

il_vars <- c(
  "goal_aligned", "relevant_goals", "interesting_content", "ownership", "understanding",
  "positive_emotion", "focused", "receptive_explanation", "productive_metalinguistic",
  "search", "retrieval", "productive_retrieval", "recall",
  "multiple_retrievals", "spaced_retrievals", "option_comparison",
  "sentence_creation", "composition_creation", "l1_l2_comparison",
  "examples", "images", "interference"
)

il_need_motivation_vars <- c(
  "goal_aligned", "relevant_goals", "interesting_content", "ownership", "understanding",
  "positive_emotion"
)

il_noticing_vars <- c("focused", "receptive_explanation", "productive_metalinguistic")

il_search_vars <- "search"

il_retrieval_vars <- c(
  "retrieval", "productive_retrieval", "recall",
  "multiple_retrievals", "spaced_retrievals"
)

il_evaluation_generation_vars <- c("option_comparison", "sentence_creation", "composition_creation")

il_retention_vars <- c("l1_l2_comparison", "examples", "images", "interference")

df_multi <- df_multi %>%
  mutate(il_22 = rowSums(across(all_of(il_vars))),
         il_need_motivation = rowSums(across(all_of(il_need_motivation_vars))),
         il_noticing = rowSums(across(all_of(il_noticing_vars))),
         il_search = rowSums(across(all_of(il_search_vars))),
         il_retrieval = rowSums(across(all_of(il_retrieval_vars))),
         il_evaluation_generation = rowSums(across(all_of(il_evaluation_generation_vars))),
         il_retention = rowSums(across(all_of(il_retention_vars))))

write_csv(df_multi, run_output("inputs", "data_for_analysis.csv"))
