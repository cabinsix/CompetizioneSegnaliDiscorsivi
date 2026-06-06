# =====================================================
# 10. PACCHETTI AGGIUNTIVI PER L'ALBERO
# =====================================================
library(partykit)
library(dplyr)

# =====================================================
# 11. FILTRO SOSTITUIBILITÀ (Entrambi i valori non-NA >= 1)
# =====================================================

df_tree_ready <- df_final %>%
  rowwise() %>%
  # Creiamo una condizione: isoliamo i valori non NA tra i tre sost_x
  filter({
    # Vettore con i tre valori della riga corrente
    valori <- c(sost_mi_sembra, sost_mi_pare, sost_mi_sa)
    # Teniamo solo quelli che NON sono NA
    valori_validi <- valori[!is.na(valori)]
    
    # Condizione: ci devono essere valori validi E TUTTI devono essere >= 1
    length(valori_validi) > 0 && all(valori_validi >= 1)
  }) %>%
  ungroup()

# =====================================================
# 11. FILTRO SOSTITUIBILITÀ + RIMOZIONE "ESTERO"
# =====================================================

df_tree_ready <- df_final %>%
  # 1. Rimuoviamo i dati dove la provenienza è 'estero'
  filter(!provenienza %in% c("estero", "===NONE===")) %>%
  
  
  
  # 2. Applichiamo il filtro sulla sostituibilità (entrambi i non-NA >= 1)
  rowwise() %>%
  filter({
    valori <- c(sost_mi_sembra, sost_mi_pare, sost_mi_sa)
    valori_validi <- valori[!is.na(valori)]
    length(valori_validi) > 0 && all(valori_validi >= 1)
  }) %>%
  ungroup()


# =====================================================
# 12. PREPARAZIONE VARIABILI (Fattori per ctree)
# =====================================================
df_tree_ready <- df_tree_ready %>%
  mutate(
    DM.x = as.factor(DM.x),
    provenienza = as.factor(provenienza),
    area_geo = as.factor(area_geo),
    Tipo_interazione = as.factor(Tipo_interazione)
  )

# =====================================================
# 13. COSTRUZIONE DEL CONDITIONAL INFERENCE TREE
# =====================================================
albero_modello <- partykit::ctree(
  DM.x ~ area_geo + Tipo_interazione, 
  data = df_tree_ready
)

# =====================================================
# 14. VISUALIZZAZIONE
# =====================================================
print(albero_modello)
plot(albero_modello, main = "Conditional Inference Tree per DM.x (Sostituibilità >= 1)")


library(dplyr)
library(nnet)

# ==========================================
# ACCORPAMENTO TIPO INTERAZIONE
# ==========================================

df_tree_ready <- df_tree_ready %>%
  mutate(
    Tipo_interazione = case_when(
      Tipo_interazione %in% c(
        "esame",
        "lezione",
        "ricevimento studenti"
      ) ~ "istituzionale",
      
      TRUE ~ as.character(Tipo_interazione)
    )
  )

df_tree_ready$Tipo_interazione <- factor(
  df_tree_ready$Tipo_interazione
)

# =====================================================
# 22. NUOVA RICODIFICA IN MACRO-FASCE D'ETÀ
# =====================================================

df_eta_clean <- df_tree_ready %>%
  mutate(
    # Creiamo la variabile categoriale basandoci sulla stringa originale 'Età'
    Fascia_Eta = case_when(
      Età %in% c("16-20", "21-25", "26-30") ~ "Giovani (16-30)",
      Età %in% c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60") ~ "Adulti (31-60)",
      Età %in% c("61-65", "66-70", "71-75", "76-80", "81-85", "over 85") ~ "Anziani (over 61)"
    )
  ) %>%
  mutate(
    Fascia_Eta = factor(Fascia_Eta, levels = c("Giovani (16-30)", "Adulti (31-60)", "Anziani (over 61)"))
  )

# =====================================================
# 23. AGGIORNAMENTO MODELLO CON LA NUOVA VARIABILE
# =====================================================
# Sostituiamo eta_ordinale con la nuova Fascia_Eta categoriale
modello_eta_macro <- multinom(
  DM.x ~ Fascia_Eta + area_geo + Tipo_interazione, 
  data = df_eta_clean
)

# =====================================================
# 24. CONTRASTI A PAIA SULL'ETÀ (PULITI E SIGNIFICATIVI)
# =====================================================
library(emmeans)

# Calcoliamo le medie marginali basate sulle probabilità
emm_eta_macro <- emmeans(modello_eta_macro, ~ Fascia_Eta | DM.x, mode = "prob")

cat("\n--- CONFRONTI DIRETTI TRA FASCE D'ETÀ (PER OGNI DM) ---\n")
# Questo ti dirà ad esempio se i Giovani usano mi_sa significativamente più degli Adulti o degli Anziani
pairs(emm_eta_macro, adjust = "tukey")

library(emmeans)
library(dplyr)

# 1. Generiamo le medie marginali incrociando le 3 variabili indipendenti
emm_totale <- emmeans(
  modello_eta_macro, 
  ~ DM.x | Fascia_Eta + Tipo_interazione + area_geo, 
  mode = "prob"
)

# 2. Generiamo tutti i confronti a paia tra i DM per ciascun incrocio
coppie_totali <- pairs(emm_totale, by = c("Fascia_Eta", "Tipo_interazione", "area_geo"))

# =====================================================
# STAMPA 1: Output completo (Attenzione, sarà molto lungo!)
# =====================================================
print(coppie_totali)

# =====================================================
# STAMPA 2: Solo i contrasti statisticamente significativi (Consigliato!)
# =====================================================
cat("\n--- INCROCI TOTALE: SOLO CONFRONTI SIGNIFICATIVI (p < 0.05) ---\n")

df_significativi <- as.data.frame(coppie_totali) %>%
  filter(p.value < 0.05) %>%
  # Arrotondiamo i numeri per renderli leggibili nella tesi
  mutate(
    estimate = round(estimate, 4),
    SE = round(SE, 4),
    p.value = round(p.value, 4)
  )

print(df_significativi)


