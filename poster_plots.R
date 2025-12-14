# poster_plots.R
# Create clean, poster-ready figures for SMS spam EDA

library(tidyverse)
library(tidytext)
library(tidylo)
library(ggplot2)

#-------------------------------------------------------------------
# 1. Load data and prepare objects (sms, sms_feats, tokens)
#-------------------------------------------------------------------

data_path <- "C:/Users/gacak/Downloads/EDA FINALS/spam.csv"

sms_raw <- readr::read_csv(
  file = data_path,
  locale = readr::locale(encoding = "latin1")
)

sms_raw <- sms_raw |>
  mutate(across(
    where(is.character),
    ~ iconv(.x, from = "latin1", to = "UTF-8", sub = "")
  ))

sms <- sms_raw |>
  janitor::clean_names() |>
  rename(
    label = v1,
    text  = v2
  ) |>
  mutate(
    label = factor(label, levels = c("ham", "spam")),
    text  = as.character(text)
  )

sms_feats <- sms |>
  mutate(
    n_chars  = nchar(text),
    n_words  = stringr::str_count(text, "\\S+"),
    n_digits = stringr::str_count(text, "\\d"),
    n_urls   = stringr::str_count(text, "http[s]?://|www\\."),
    n_caps   = stringr::str_count(text, "\\b[A-Z]{2,}\\b")
  )

data("stop_words")
tokens <- sms |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  filter(str_detect(word, "[a-z]"))

uni_counts <- tokens |>
  count(label, word, sort = TRUE)

uni_log_odds <- uni_counts |>
  bind_log_odds(label, word, n)

#-------------------------------------------------------------------
# 2. Palette and theme
#-------------------------------------------------------------------

palette_sms <- c(
  ham  = "#90AB8B",  # lighter green
  spam = "#5A7863"   # darker green
)

palette_bg_light <- "#EBF4DD"
palette_dark      <- "#3B4953"

theme_poster <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      text = element_text(color = palette_dark),
      plot.title = element_text(
        size = 22, face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 16, color = "#5A6860",
        margin = margin(b = 10)
      ),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      panel.grid.major = element_line(color = "#D7E0C8"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = palette_bg_light, color = NA),
      strip.text = element_text(size = 14, face = "bold"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

#-------------------------------------------------------------------
# 3. Plot 1 – Word count distribution
#-------------------------------------------------------------------

p_len <- ggplot(sms_feats, aes(x = n_words, fill = label)) +
  geom_histogram(
    alpha = 0.8,
    position = "identity",
    bins = 40,
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 60)) +
  scale_fill_manual(values = palette_sms, name = "Class") +
  labs(
    title = "Spam messages tend to be longer than ham",
    subtitle = "Distribution of SMS length (number of words) by class",
    x = "Number of words in message",
    y = "Number of messages"
  ) +
  theme_poster()

print(p_len)

ggsave(
  filename = "poster_word_length.png",
  plot = p_len,
  width = 10, height = 7, dpi = 300
)

#-------------------------------------------------------------------
# 4. Plot 2 – Structural feature boxplots
#-------------------------------------------------------------------

sms_feats_long <- sms_feats |>
  select(label, n_chars, n_words, n_digits, n_urls, n_caps) |>
  pivot_longer(
    cols = c(n_chars, n_words, n_digits, n_urls, n_caps),
    names_to = "feature",
    values_to = "value"
  ) |>
  mutate(
    feature = factor(
      feature,
      levels = c("n_chars", "n_words", "n_digits", "n_urls", "n_caps"),
      labels = c(
        "Characters", "Words", "Digits",
        "URLs", "All-caps tokens"
      )
    )
  )

p_feats <- ggplot(sms_feats_long, aes(x = label, y = value, fill = label)) +
  geom_boxplot(outlier.alpha = 0.15) +
  facet_wrap(~ feature, scales = "free_y") +
  scale_fill_manual(values = palette_sms, guide = "none") +
  labs(
    title = "Structural features differ between ham and spam",
    subtitle = "Spam messages contain more digits, URLs, and all-caps tokens",
    x = "",
    y = "Value"
  ) +
  theme_poster()

print(p_feats)

ggsave(
  filename = "poster_structural_features.png",
  plot = p_feats,
  width = 11, height = 7, dpi = 300
)

#-------------------------------------------------------------------
# 5. Plot 3 – Top unigrams by class
#-------------------------------------------------------------------

top_uni <- uni_counts |>
  group_by(label) |>
  slice_max(n, n = 15) |>
  ungroup()

p_uni <- ggplot(
  top_uni,
  aes(x = reorder_within(word, n, label), y = n, fill = label)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ label, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = palette_sms) +
  labs(
    title = "Frequently used words in ham and spam messages",
    subtitle = "Top unigrams by raw count in each class",
    x = "",
    y = "Count"
  ) +
  theme_poster()

print(p_uni)

ggsave(
  filename = "poster_top_unigrams.png",
  plot = p_uni,
  width = 11, height = 7, dpi = 300
)

#-------------------------------------------------------------------
# 6. Plot 4 – Class-specific words (log-odds)
#-------------------------------------------------------------------

top_log_odds <- uni_log_odds |>
  group_by(label) |>
  slice_max(log_odds_weighted, n = 15) |>
  ungroup()

p_log <- ggplot(
  top_log_odds,
  aes(
    x = reorder_within(word, log_odds_weighted, label),
    y = log_odds_weighted,
    fill = label
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ label, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = palette_sms) +
  labs(
    title = "Words especially indicative of ham or spam",
    subtitle = "Weighted log-odds scores by class",
    x = "",
    y = "Weighted log-odds"
  ) +
  theme_poster()

print(p_log)

ggsave(
  filename = "poster_log_odds_words.png",
  plot = p_log,
  width = 11, height = 7, dpi = 300
)

#-------------------------------------------------------------------
cat("Poster plots saved in working directory.\n")

