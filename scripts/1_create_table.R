library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# =====================================================
# 1. LETTURA FILE
# =====================================================

epist <- read_excel("C:/Users/fpisc/Downloads/oviedo/New folder/miV_epist_scope.xlsx", sheet = "miV_epist_scope")
disc  <- read_excel("C:/Users/fpisc/Downloads/oviedo/New folder/miV_discfunc.xlsx")
sub   <- read_excel("C:/Users/fpisc/Downloads/oviedo/New folder/miV_sub_agreement.xlsx", sheet = "Sheet1")

# =====================================================
# 2. FILTRO: SOLO TOKEN CON SOSTITUIBILITÀ ANNOTATA
# =====================================================

sub <- sub %>%
  mutate(across(starts_with("mis_") | starts_with("mip_") | starts_with("misa_"),
                as.numeric))

sub <- sub %>%
  filter(
    !(
      is.na(mis_FP) & is.na(mis_LT) &
        is.na(mip_FP) & is.na(mip_LT) &
        is.na(misa_FP) & is.na(misa_LT)
    )
  )

# =====================================================
# 3. MEDIA SOSTITUIBILITÀ
# =====================================================

sub <- sub %>%
  mutate(
    sost_mi_sembra = rowMeans(select(., mis_FP, mis_LT), na.rm = TRUE),
    sost_mi_pare   = rowMeans(select(., mip_FP, mip_LT), na.rm = TRUE),
    sost_mi_sa     = rowMeans(select(., misa_FP, misa_LT), na.rm = TRUE)
  )

# =====================================================
# 4. RICODIFICA ETA'
# =====================================================

epist <- epist %>%
  mutate(
    eta_ordinale = case_when(
      Età == "16-20" ~ 1,
      Età == "21-25" ~ 2,
      Età == "26-30" ~ 3,
      Età == "31-35" ~ 4,
      Età == "36-40" ~ 5,
      Età == "41-45" ~ 6,
      Età == "46-50" ~ 7,
      Età == "51-55" ~ 8,
      Età == "56-60" ~ 9,
      Età == "61-65" ~ 10,
      Età == "66-70" ~ 11,
      Età == "71-75" ~ 12,
      Età == "76-80" ~ 13,
      Età == "81-85" ~ 14,
      Età == "over 85" ~ 15,
      TRUE ~ NA_real_
    )
  )

# =====================================================
# 5. RICODIFICA PROVENIENZA (macro-aree)
# =====================================================

epist <- epist %>%
  mutate(
    area_geo = case_when(
      provenienza %in% c("piemonte","valle-d-aosta","lombardia","trentino-alto-adige",
                         "veneto","friuli-venezia-giulia","liguria","emilia-romagna") ~ "Nord",
      
      provenienza %in% c("toscana","umbria","marche","lazio") ~ "Centro",
      
      provenienza %in% c("abruzzo","molise","campania","puglia","basilicata","calabria",
                         "sicilia","sardegna") ~ "Sud e Isole",
      
      provenienza == "estero" ~ "Estero",
      TRUE ~ NA_character_
    )
  )

# =====================================================
# 6. MICRO + MACRO FUNZIONI (da dataset discorsivo)
# =====================================================

# mapping (come già definito da te)
ALL_MAP <- c(
  INTERAZIONALI_MAP,
  METATESTUALI_MAP,
  COGNITIVE_MAP
)

disc_fun <- disc %>%
  mutate(
    Microfunzioni = apply(disc[, names(ALL_MAP)], 1, function(x){
      active <- names(x)[!is.na(x)]
      paste(unname(ALL_MAP[active]), collapse = "|")
    }),
    
    Macrofunzioni = apply(disc[, names(ALL_MAP)], 1, function(x){
      active <- names(x)[!is.na(x)]
      
      macro <- c()
      
      if(any(active %in% names(INTERAZIONALI_MAP)))
        macro <- c(macro, "Interazionale")
      
      if(any(active %in% names(METATESTUALI_MAP)))
        macro <- c(macro, "Metatestuale")
      
      if(any(active %in% names(COGNITIVE_MAP)))
        macro <- c(macro, "Cognitiva")
      
      paste(macro, collapse = "|")
    })
  ) %>%
  filter(Microfunzioni != "")

# =====================================================
# 7. MERGE 3 DATASET SU ID
# =====================================================

df_final <- sub %>%
  left_join(epist, by = "ID") %>%
  left_join(disc_fun %>% select(ID, Microfunzioni, Macrofunzioni), by = "ID")

# =====================================================
# 8. SELEZIONE VARIABILI FINALI
# =====================================================

df_final <- df_final %>%
  transmute(
    ID,
    DM.x,
    Tipo_Epistemico,
    IsAbout,
    Scope,
    Fattualita_matrice,
    Posizione,
    
    Microfunzioni,
    Macrofunzioni,
    
    sost_mi_sembra,
    sost_mi_pare,
    sost_mi_sa,
    
    Età,
    eta_ordinale,
    provenienza,
    area_geo,
    Tipo_interazione,
    
    Left.y,
    KWIC.y,
    Right.y
  )

# =====================================================
# 9. OUTPUT
# =====================================================

df_final

df_final <- df_final %>%
  filter(Tipo_Epistemico != "-")

df_final <- df_final %>%
  mutate(
    Tipo_Epistemico = case_when(
      str_to_lower(Tipo_Epistemico) %in% c("incertezza", "certezza") ~ "grado di certezza",
      TRUE ~ Tipo_Epistemico
    )
  )

