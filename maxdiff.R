library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(jsonlite)
library(httr2)

df <- read_csv(
  here("data", "maxdiff_dummy_data.csv")
)

api_key <- "ENTER_OPENAI_API_KEY_HERE"

maxdiff <- df %>%
  clean_names()

# Basic stats: SE, confidence intervals, ranking

maxdiff <- maxdiff %>%
  mutate(
    se       = sqrt(maxdiff_mean * (1 - maxdiff_mean) / sample_size),
    ci_low   = maxdiff_mean - 1.96 * se,
    ci_high  = maxdiff_mean + 1.96 * se
  )

maxdiff_ranked <- maxdiff %>%
  arrange(desc(maxdiff_mean)) %>%
  mutate(rank = row_number())

# Quick look at top and bottom performers
top5 <- maxdiff_ranked %>% slice_head(n = 5)
bottom5 <- maxdiff_ranked %>% slice_tail(n = 5)

print(top5 %>% select(rank, video_id, maxdiff_mean, ci_low, ci_high, text))
print(bottom5 %>% select(rank, video_id, maxdiff_mean, ci_low, ci_high, text))

# Preparing data for OpenAI thematic analysis

# We only are going to pass id, text, and score
messages_for_gpt <- maxdiff_ranked %>%
  transmute(
    video_id,
    text,
    maxdiff_mean
  )

messages_json <- toJSON(messages_for_gpt, pretty = TRUE, auto_unbox = TRUE)

# Actually calling OpenAI to extract themes + insights:

# OpenAI Goals:
#  - Define themes
#  - Assign each message to a theme
#  - Summarize performance by theme
#  - Explain what characterizes high vs low performing themes
#  - Output some parseable data

system_prompt <- "
You are an expert survey and political communications analyst.

You are given messages from a MaxDiff experiment about Donald Trump's policies.
Each message has:
- video_id
- text
- maxdiff_mean (proportion chosen as the more persuasive option)

Your tasks:
1. Read all messages and group them into 5-8 coherent themes (e.g., energy costs, worker safety, public services, etc.).
2. Give each theme a short, descriptive name (e.g., 'Energy Bills for Families', 'Worker Safety and Inspections').
3. For each message, assign:
   - a single best-fitting theme_name
   - a short 1-sentence rationale for why it fits that theme.
4. For each theme, compute:
   - average_maxdiff_mean across its assigned messages
   - number_of_messages in the theme
5. Rank themes from highest to lowest by average_maxdiff_mean.
6. Provide a short narrative:
   - What types of themes and framings tend to perform best?
   - What types tend to perform worst?
   - Any notable rhetorical patterns (e.g., rhymes, emotional appeals, concrete harms to families) that seem to help or hurt.
7. Your entire response MUST be valid JSON with this structure:

{
  \"themes\": [
    {
      \"theme_name\": \"...\",
      \"description\": \"...\",
      \"average_maxdiff_mean\": 0.0,
      \"number_of_messages\": 0,
      \"rank\": 0
    }
  ],
  \"message_assignments\": [
    {
      \"video_id\": 0,
      \"theme_name\": \"...\",
      \"rationale\": \"...\"
    }
  ],
  \"narrative_summary\": \"...\"
}
"

user_prompt <- paste0(
  "Here is the JSON array of messages:\n\n",
  messages_json,
  "\n\nPlease analyze and respond following the instructions in the system prompt."
)

# Build the request to OpenAI 
req <- request("https://api.openai.com/v1/chat/completions") %>%
  req_method("POST") %>%
  req_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type"  = "application/json"
  ) %>%
  req_body_json(list(
    model = "gpt-4.1-mini", # I'm using this because it's cheap and fast, but you can use any model.
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    )
  ))

resp <- req_perform(req)

# Extracting JSON from the response
resp_json <- resp_body_json(resp, simplifyVector = FALSE)

gpt_content <- resp_json$choices[[1]]$message$content

# Parsing model output
gpt_result <- fromJSON(gpt_content, simplifyVector = TRUE)

# Inspect structure
str(gpt_result, max.level = 2)

# Merge OpenAI theme assignments back into the data
message_assignments <- as_tibble(gpt_result$message_assignments)

# Ensure video_id type matches
message_assignments <- message_assignments %>%
  mutate(video_id = as.integer(video_id))

maxdiff_with_themes <- maxdiff_ranked %>%
  left_join(message_assignments, by = "video_id")

# Theme-level performance summary in R
theme_summary <- maxdiff_with_themes %>%
  group_by(theme_name) %>%
  summarize(
    avg_maxdiff = mean(maxdiff_mean),
    n_messages   = n(),
    avg_se       = mean(se),
    .groups      = "drop"
  ) %>%
  arrange(desc(avg_maxdiff))

print(theme_summary)

# For fun and cognitive profit: view GPT's narrative summary
cat("\n\n--- GPT Narrative Summary ---\n\n")
cat(gpt_result$narrative_summary, "\n")

# --- 8. Example: pull top messages within each theme ----

top_messages_by_theme <- maxdiff_with_themes %>%
  group_by(theme_name) %>%
  slice_max(order_by = maxdiff_mean, n = 3, with_ties = FALSE) %>%
  arrange(theme_name, desc(maxdiff_mean)) %>%
  ungroup() %>%
  select(theme_name, video_id, maxdiff_mean, ci_low, ci_high, text, rationale)

print(top_messages_by_theme)


### FIGURES ###
# These are the figures used in the README.md


fig1 <- ggplot(
  maxdiff_ranked,
  aes(x = rank, y = maxdiff_mean, color = maxdiff_mean)
) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1, alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "MaxDiff Persuasiveness Scores (Ranked)",
    x = "Message Rank (1 = Most Persuasive)",
    y = "Proportion Chosen as More Persuasive",
    color = "Score"
  ) +
  theme_minimal(base_size = 12)

fig1

fig2 <- ggplot(
  theme_summary,
  aes(x = reorder(theme_name, avg_maxdiff), y = avg_maxdiff)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average MaxDiff Score by Theme",
    x = "Theme",
    y = "Average proportion chosen as more persuasive"
  )

fig2

fig3 <- ggplot(
  maxdiff_with_themes,
  aes(x = reorder(theme_name, maxdiff_mean, FUN = median), y = maxdiff_mean)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of MaxDiff Scores Within Each Theme",
    x = "Theme (ordered by median score)",
    y = "Proportion chosen as more persuasive"
  ) +
  coord_flip()

fig3
