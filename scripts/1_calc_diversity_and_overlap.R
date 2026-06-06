library(dplyr)
library(tidyr)
library(purrr)
library(vegan)   # per diversity, optional ma utile

# =========================
# 0. CLEAN BASE
# =========================

df <- df_final %>%
  mutate(DM.x = as.character(DM.x)) %>%
  filter(!is.na(DM.x))

# =========================
# 1. SIMILARITÀ (JACCARD)
# =========================

jaccard_sim <- function(x, y) {
  sum(pmin(x, y)) / sum(pmax(x, y))
}

# =========================
# 2. HILL NUMBERS (FUNZIONE)
# =========================

hill_numbers <- function(p) {
  p <- p[p > 0]
  q0 <- length(p)              # richness
  q1 <- exp(-sum(p * log(p)))  # Shannon exponential
  q2 <- 1 / sum(p^2)           # inverse Simpson
  c(Hill_q0 = q0, Hill_q1 = q1, Hill_q2 = q2)
}

# =========================
# 3. EPITEMICO - DIVERSITÀ + HILL
# =========================

epist_div <- df %>%
  filter(!is.na(Tipo_Epistemico), Tipo_Epistemico != "") %>%
  count(DM.x, Tipo_Epistemico) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  group_by(DM.x) %>%
  summarise(
    epist_richness = n_distinct(Tipo_Epistemico),
    epist_shannon = -sum(prop * log(prop)),
    epist_simpson = sum(prop^2),
    Hill_q0 = epist_richness,
    Hill_q1 = exp(epist_shannon),
    Hill_q2 = 1 / epist_simpson,
    .groups = "drop"
  )

# =========================
# 4. MICRO - DIVERSITÀ + HILL
# =========================

micro_div <- df %>%
  filter(!is.na(Microfunzioni), Microfunzioni != "") %>%
  separate_rows(Microfunzioni, sep = "\\|") %>%
  count(DM.x, Microfunzioni) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  group_by(DM.x) %>%
  summarise(
    micro_richness = n_distinct(Microfunzioni),
    micro_shannon = -sum(prop * log(prop)),
    micro_simpson = sum(prop^2),
    Hill_q0 = micro_richness,
    Hill_q1 = exp(micro_shannon),
    Hill_q2 = 1 / micro_simpson,
    .groups = "drop"
  )

# =========================
# 5. MACRO - DIVERSITÀ + HILL
# =========================

macro_div <- df %>%
  filter(!is.na(Macrofunzioni), Macrofunzioni != "") %>%
  separate_rows(Macrofunzioni, sep = "\\|") %>%
  count(DM.x, Macrofunzioni) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  group_by(DM.x) %>%
  summarise(
    macro_richness = n_distinct(Macrofunzioni),
    macro_shannon = -sum(prop * log(prop)),
    macro_simpson = sum(prop^2),
    Hill_q0 = macro_richness,
    Hill_q1 = exp(macro_shannon),
    Hill_q2 = 1 / macro_simpson,
    .groups = "drop"
  )

# =========================
# 6. EPITEMICO MATRIX + PS
# =========================

epist_mat <- df %>%
  filter(!is.na(Tipo_Epistemico), Tipo_Epistemico != "") %>%
  count(DM.x, Tipo_Epistemico) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = Tipo_Epistemico,
              values_from = prop,
              values_fill = 0)

epist_dm <- epist_mat$DM.x
epist_mat <- epist_mat %>% column_to_rownames("DM.x")

epist_ps <- outer(
  epist_dm, epist_dm,
  Vectorize(function(i, j) jaccard_sim(epist_mat[i,], epist_mat[j,]))
)

# =========================
# 7. MICRO MATRIX + PS
# =========================

micro_mat <- df %>%
  filter(!is.na(Microfunzioni), Microfunzioni != "") %>%
  separate_rows(Microfunzioni, sep = "\\|") %>%
  count(DM.x, Microfunzioni) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = Microfunzioni,
              values_from = prop,
              values_fill = 0)

micro_dm <- micro_mat$DM.x
micro_mat <- micro_mat %>% column_to_rownames("DM.x")

micro_ps <- outer(
  micro_dm, micro_dm,
  Vectorize(function(i, j) jaccard_sim(micro_mat[i,], micro_mat[j,]))
)

# =========================
# 8. MACRO MATRIX + PS
# =========================

macro_mat <- df %>%
  filter(!is.na(Macrofunzioni), Macrofunzioni != "") %>%
  separate_rows(Macrofunzioni, sep = "\\|") %>%
  count(DM.x, Macrofunzioni) %>%
  group_by(DM.x) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = Macrofunzioni,
              values_from = prop,
              values_fill = 0)

macro_dm <- macro_mat$DM.x
macro_mat <- macro_mat %>% column_to_rownames("DM.x")

macro_ps <- outer(
  macro_dm, macro_dm,
  Vectorize(function(i, j) jaccard_sim(macro_mat[i,], macro_mat[j,]))
)

# =========================
# 9. TOP FREQUENCIES
# =========================

top_epist <- df %>%
  filter(!is.na(Tipo_Epistemico), Tipo_Epistemico != "") %>%
  count(DM.x, Tipo_Epistemico) %>%
  group_by(DM.x) %>%
  arrange(desc(n)) %>%
  summarise(top_epistemic = list(head(Tipo_Epistemico, 5)),
            .groups = "drop")

top_micro <- df %>%
  filter(!is.na(Microfunzioni), Microfunzioni != "") %>%
  separate_rows(Microfunzioni, sep = "\\|") %>%
  count(DM.x, Microfunzioni) %>%
  group_by(DM.x) %>%
  arrange(desc(n)) %>%
  summarise(top_micro = list(head(Microfunzioni, 5)),
            .groups = "drop")

top_macro <- df %>%
  filter(!is.na(Macrofunzioni), Macrofunzioni != "") %>%
  separate_rows(Macrofunzioni, sep = "\\|") %>%
  count(DM.x, Macrofunzioni) %>%
  group_by(DM.x) %>%
  arrange(desc(n)) %>%
  summarise(top_macro = list(head(Macrofunzioni, 5)),
            .groups = "drop")

# =========================
# 10. OUTPUT FINALE
# =========================

df_summary <- epist_div %>%
  left_join(micro_div, by = "DM.x", suffix = c("_ep", "_mic")) %>%
  left_join(macro_div, by = "DM.x") %>%
  left_join(top_epist, by = "DM.x") %>%
  left_join(top_micro, by = "DM.x") %>%
  left_join(top_macro, by = "DM.x")



# =========================
# PERCENTAGE SIMILARITY (BRAY-CURTIS / ODUM)
# =========================

ps_matrix <- function(mat) {
  
  dm <- rownames(mat)
  
  ps <- outer(
    dm, dm,
    Vectorize(function(i, j) {
      
      x <- mat[i, ]
      y <- mat[j, ]
      
      numerator <- 2 * sum(pmin(x, y))
      denominator <- sum(x + y)
      
      if (denominator == 0) return(NA_real_)
      
      numerator / denominator
    })
  )
  
  rownames(ps) <- dm
  colnames(ps) <- dm
  
  ps * 100
}

# =========================
# EPITEMICO
# =========================

epist_ps <- ps_matrix(epist_mat)

# =========================
# MICRO
# =========================

micro_ps <- ps_matrix(micro_mat)

# =========================
# MACRO
# =========================

macro_ps <- ps_matrix(macro_mat)


#### -html


library(dplyr)
library(tidyr)
library(htmltools)
library(htmlTable)

df <- df_final
dms <- unique(df$DM.x)

# =========================
# PS (Bray-Curtis similarity)
# =========================
ps_fun <- function(mat) {
  
  dm <- rownames(mat)
  
  ps <- outer(dm, dm, Vectorize(function(i, j) {
    
    x <- mat[i, ]
    y <- mat[j, ]
    
    2 * sum(pmin(x, y)) / (sum(x + y) + 1e-9)
    
  }))
  
  rownames(ps) <- dm
  colnames(ps) <- dm
  
  ps
}

# =========================
# DIVERSITY
# =========================
diversity_block <- function(data, var) {
  
  data %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(DM.x, key = .data[[var]]) %>%
    group_by(DM.x) %>%
    mutate(prop = n / sum(n)) %>%
    summarise(
      Richness = n_distinct(key),
      Shannon = -sum(prop * log(prop)),
      Simpson = sum(prop^2),
      Hill_q1 = exp(Shannon),
      Hill_q2 = 1 / Simpson,
      .groups = "drop"
    )
}

build_mat <- function(data, var) {
  
  data %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(DM.x, key = .data[[var]]) %>%
    group_by(DM.x) %>%
    mutate(prop = n / sum(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = key, values_from = prop, values_fill = 0) %>%
    tibble::column_to_rownames("DM.x")
}

# =========================
# EPITEMICO
# =========================
epist_div <- diversity_block(df, "Tipo_Epistemico")
epist_ps <- ps_fun(as.matrix(build_mat(df, "Tipo_Epistemico")))

# =========================
# MICRO
# =========================
micro_div <- df %>%
  separate_rows(Microfunzioni, sep = "\\|") %>%
  diversity_block("Microfunzioni")

micro_ps <- ps_fun(as.matrix(build_mat(
  df %>% separate_rows(Microfunzioni, sep = "\\|"),
  "Microfunzioni"
)))

# =========================
# MACRO
# =========================
macro_div <- df %>%
  separate_rows(Macrofunzioni, sep = "\\|") %>%
  diversity_block("Macrofunzioni")

macro_ps <- ps_fun(as.matrix(build_mat(
  df %>% separate_rows(Macrofunzioni, sep = "\\|"),
  "Macrofunzioni"
)))

# =========================
# SAFE ROUND FUNCTION
# =========================
round_df <- function(x, digits = 2) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, function(col) {
      if (is.numeric(col)) round(col, digits) else col
    })
  }
  if (is.matrix(x)) {
    x <- round(x, digits)
  }
  x
  return(x)
}

# =========================
# APPLY ROUNDING
# =========================
epist_div <- round_df(epist_div)
epist_ps  <- round_df(epist_ps)

micro_div <- round_df(micro_div)
micro_ps  <- round_df(micro_ps)

macro_div <- round_df(macro_div)
macro_ps  <- round_df(macro_ps)

# =========================
# HTML BUILDER
# =========================
out <- tags$html(
  tags$head(
    tags$style("
      body { font-family: Arial; margin: 30px; }
      h1 { margin-top: 40px; }
      table { margin-bottom: 25px; border-collapse: collapse; }
      td, th { padding: 4px 8px; }
    ")
  ),
  tags$body(
    
    tags$h1("Diversity + Percentage Similarity Report"),
    
    # ================= EPISTEMICO =================
    tags$h2("Epistemico - Diversity"),
    HTML(htmlTable(epist_div)),
    
    tags$h2("Epistemico - Percentage Similarity"),
    HTML(htmlTable(epist_ps)),
    
    # ================= MICRO =================
    tags$h2("Microfunzioni - Diversity"),
    HTML(htmlTable(micro_div)),
    
    tags$h2("Microfunzioni - Percentage Similarity"),
    HTML(htmlTable(micro_ps)),
    
    # ================= MACRO =================
    tags$h2("Macrofunzioni - Diversity"),
    HTML(htmlTable(macro_div)),
    
    tags$h2("Macrofunzioni - Percentage Similarity"),
    HTML(htmlTable(macro_ps))
  )
)

htmltools::save_html(out, "DM_diversity_similarity_report.html")

