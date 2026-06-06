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

# controlla
table(df_tree_ready$Tipo_interazione)

# ==========================================
# MODELLO MULTINOMIALE
# ==========================================

df_tree_ready$DM.x <- relevel(
  factor(df_tree_ready$DM.x),
  ref = "mi_sembra"
)

modello_multinomiale <- multinom(
  DM.x ~ eta_ordinale +
    area_geo +
    Tipo_interazione,
  data = df_tree_ready
)

summary(modello_multinomiale)

# ==========================================
# P-VALUE (WALD)
# ==========================================

z <- summary(modello_multinomiale)$coefficients /
  summary(modello_multinomiale)$standard.errors

p_values <- 2 * (1 - pnorm(abs(z)))

round(p_values, 4)

# ==========================================
# ODDS RATIOS
# ==========================================

exp(coef(modello_multinomiale))


'''
--- CONFRONTI A PAIA TRA DM (A PARITÀ DEGLI ALTRI FATTORI) ---
  > coppie_dm <- pairs(emm_modello, by = c("area_geo", "Tipo_interazione"))
  > print(coppie_dm)
  area_geo = Centro, Tipo_interazione = conversazione libera:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare  0.11595 0.3009 14   0.385  0.9219
  mi_sembra - mi_sa    0.01410 0.3198 14   0.044  0.9989
  mi_pare - mi_sa     -0.10186 0.2536 14  -0.402  0.9155
  
  area_geo = Nord, Tipo_interazione = conversazione libera:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.09337 0.2714 14  -0.344  0.9371
  mi_sembra - mi_sa    0.07452 0.2131 14   0.350  0.9351
  mi_pare - mi_sa      0.16789 0.1992 14   0.843  0.6835
  
  area_geo = Sud e Isole, Tipo_interazione = conversazione libera:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.45241 0.1999 14  -2.263  0.0948
  mi_sembra - mi_sa   -0.25175 0.1784 14  -1.411  0.3620
  mi_pare - mi_sa      0.20066 0.3057 14   0.656  0.7918
  
  area_geo = Centro, Tipo_interazione = intervista semistrutturata:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare  0.47612 0.1920 14   2.480  0.0644
  mi_sembra - mi_sa    0.42015 0.2035 14   2.064  0.1336
  mi_pare - mi_sa     -0.05597 0.1274 14  -0.439  0.8998
  
  area_geo = Nord, Tipo_interazione = intervista semistrutturata:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare  0.31364 0.1009 14   3.108  0.0198
  mi_sembra - mi_sa    0.42324 0.0942 14   4.494  0.0014
  mi_pare - mi_sa      0.10960 0.0772 14   1.420  0.3576
  
  area_geo = Sud e Isole, Tipo_interazione = intervista semistrutturata:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.23453 0.2136 14  -1.098  0.5308
  mi_sembra - mi_sa   -0.05617 0.1884 14  -0.298  0.9523
  mi_pare - mi_sa      0.17835 0.2164 14   0.824  0.6946
  
  area_geo = Centro, Tipo_interazione = istituzionale:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.20049 0.3997 14  -0.502  0.8717
  mi_sembra - mi_sa    0.31301 0.2190 14   1.429  0.3533
  mi_pare - mi_sa      0.51350 0.2246 14   2.286  0.0911
  
  area_geo = Nord, Tipo_interazione = istituzionale:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.46691 0.2587 14  -1.805  0.2038
  mi_sembra - mi_sa    0.21947 0.1367 14   1.606  0.2757
  mi_pare - mi_sa      0.68638 0.1404 14   4.888  0.0007
  
  area_geo = Sud e Isole, Tipo_interazione = istituzionale:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.81619 0.1298 14  -6.289  0.0001
  mi_sembra - mi_sa    0.03029 0.0730 14   0.415  0.9100
  mi_pare - mi_sa      0.84648 0.1124 14   7.532  <.0001
  
  area_geo = Centro, Tipo_interazione = pasto:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare  0.21745 0.1933 14   1.125  0.5150
  mi_sembra - mi_sa   -0.08478 0.2776 14  -0.305  0.9501
  mi_pare - mi_sa     -0.30223 0.1974 14  -1.531  0.3070
  
  area_geo = Nord, Tipo_interazione = pasto:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare  0.07669 0.1708 14   0.449  0.8956
  mi_sembra - mi_sa    0.00639 0.1933 14   0.033  0.9994
  mi_pare - mi_sa     -0.07030 0.1665 14  -0.422  0.9070
  
  area_geo = Sud e Isole, Tipo_interazione = pasto:
    contrast            estimate     SE df t.ratio p.value
  mi_sembra - mi_pare -0.27097 0.1127 14  -2.403  0.0739
  mi_sembra - mi_sa   -0.39358 0.1474 14  -2.670  0.0454
  mi_pare - mi_sa     -0.12261 0.2053 14  -0.597  0.8238
  
  P value adjustment: tukey method for comparing a family of 3 estimates '''


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
