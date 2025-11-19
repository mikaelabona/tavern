library(tidyverse)
library(janitor)
library(stringr)
library(jsonlite)
library(httr2)

# Add OpenAI API key here
api_key <- "OPENAI_API_KEY_HERE"

raw_q <- read_csv("questions.csv") %>%
  clean_names()

# quick peek
glimpse(raw_q)

# We'll send question_id + question_text to the model
items_for_gpt <- raw_q %>%
  select(question_id, question_text)

items_json <- toJSON(items_for_gpt, pretty = TRUE, auto_unbox = TRUE)

system_prompt <- "
You are helping to build a reusable survey question bank.

For each question, you must assign:
1) question_type
2) topic

Use ONLY these values for question_type:
- single_choice          (standard single-response questions with discrete options)
- multi_choice           (Select all that apply)
- open_end               (free text, e.g., 'In your own words...')
- ranking                (rank items in order)
- scale_0_100            (0-100 feeling thermometer or rating)
- numeric_scale          (e.g., strongly agree to strongly disagree, likelihood 1-5)
- maxdiff_pair           (Which message is more persuasive... style pairwise questions)

Use ONLY these values for topic (choose the best single topic):
- economy
- healthcare
- education
- elections
- immigration
- climate
- guns
- crime
- housing
- governance
- social_security
- foreign_policy
- democracy_trust
- labor_unions
- other

Important decision rules:
- If the question explicitly mentions 'Select all that apply', classify as multi_choice.
- If it asks 'In your own words', classify as open_end.
- If it asks to 'Rank the following', classify as ranking.
- If it asks for a rating on a 0-100 scale, classify as scale_0_100.
- If it asks 'Which message is more persuasive' or compares two messages as more persuasive, classify as maxdiff_pair.
- If it uses a Likert-type agreement or likelihood scale (e.g., strongly agree/disagree, very likely/not at all likely), classify as numeric_scale.
- Otherwise, treat it as single_choice.

Return a JSON object with the following structure:

{
  \"items\": [
    {
      \"question_id\": \"Q001\",
      \"question_type\": \"single_choice\",
      \"topic\": \"education\"
    }
  ]
}
"

user_prompt <- paste0(
  "Here is an array of questions with IDs:\n\n",
  items_json,
  "\n\nFor each, assign question_type and topic as described."
)

req <- request("https://api.openai.com/v1/chat/completions") %>%
  req_method("POST") %>%
  req_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type"  = "application/json"
  ) %>%
  req_body_json(list(
    model = "gpt-4.1-mini",
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    )
  ))

resp <- req_perform(req)
resp_json <- resp_body_json(resp, simplifyVector = FALSE)
gpt_content <- resp_json$choices[[1]]$message$content

gpt_result <- fromJSON(gpt_content, simplifyVector = TRUE)
classification_tbl <- as_tibble(gpt_result$items)

# sanity check
classification_tbl %>% slice_head(n = 10)

# Build questions table
questions_tbl <- raw_q %>%
  left_join(classification_tbl, by = "question_id") %>%
  mutate(
    source_survey = "Example Survey",
    notes         = NA_character_
  ) %>%
  select(
    question_id,
    question_text,
    question_type,
    topic,
    field_date,
    source_survey,
    notes
  )

questions_tbl %>% slice_head(n = 5)

# Normalize response options
response_options_tbl <- raw_q %>%
  filter(!is.na(response_options)) %>%
  separate_rows(response_options, sep = "\\|") %>%
  group_by(question_id) %>%
  mutate(
    option_order = row_number(),
    option_value = as.character(option_order)
  ) %>%
  ungroup() %>%
  transmute(
    question_id,
    option_value,
    option_label = str_trim(response_options),
    option_order,
    is_exclusive = FALSE
  )

response_options_tbl %>% filter(question_id == "Q003")

# Here we convert the single topic into a tag row.
# If this wasn't an assessment and we had more time, we could extend this by later adding more tags per question!
question_tags_tbl <- questions_tbl %>%
  filter(!is.na(topic)) %>%
  transmute(
    question_id,
    tag = topic
  )

question_tags_tbl %>% slice_head(n = 10)


# Bundling into question_bank object
question_bank <- list(
  questions        = questions_tbl,
  response_options = response_options_tbl,
  question_tags    = question_tags_tbl
)

# Quick checks
question_bank$questions %>% slice_head(n = 5)
question_bank$response_options %>% filter(question_id == "Q003")
question_bank$question_tags %>% slice_head(n = 5)
