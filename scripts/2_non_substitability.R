#################à ----------------- TOP 10 func

library(dplyr)
top10_micro <- df %>%
  filter(!is.na(Microfunzioni), Microfunzioni != "") %>%
  separate_rows(Microfunzioni, sep = "\\|") %>%
  count(DM.x, Microfunzioni, sort = TRUE) %>%
  group_by(DM.x) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    microfunzione = Microfunzioni,
    freq = n
  )

top10_micro


library(dplyr)
library(tidyr)
library(purrr)
library(htmltools)
library(htmlTable)

threshold <- 1

dms <- unique(df$DM.x)

# =====================================================
# KEYNESS TABLE
# =====================================================

keyness_table <- function(non_sub, sub, var){
  
  A <- non_sub %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(key = .data[[var]], name = "A")
  
  B <- sub %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(key = .data[[var]], name = "B")
  
  tab <- full_join(A,B,by="key") %>%
    mutate(
      A = replace_na(A,0),
      B = replace_na(B,0)
    )
  
  totalA <- sum(tab$A)
  totalB <- sum(tab$B)
  
  out <- map_dfr(seq_len(nrow(tab)), function(i){
    
    a <- tab$A[i]
    b <- tab$B[i]
    
    c <- totalA - a
    d <- totalB - b
    
    logOR <- log(
      ((a+0.5)/(c+0.5)) /
        ((b+0.5)/(d+0.5))
    )
    
    p <- tryCatch(
      fisher.test(matrix(c(a,b,c,d),2,2))$p.value,
      error = function(e) NA
    )
    
    tibble(
      key = tab$key[i],
      non_sub = a,
      sub = b,
      prop_non_sub = a/totalA,
      prop_sub = b/totalB,
      lift =
        (a/totalA) /
        ((a+b)/(totalA+totalB)+1e-9),
      logOR = logOR,
      p = p
    )
    
  })
  
  out %>%
    mutate(
      across(
        where(is.numeric),
        ~round(.x,3)
      )
    ) %>%
    arrange(desc(logOR))
}

# =====================================================
# MICRO SPLIT
# =====================================================

keyness_micro <- function(non_sub, sub){
  
  A <- non_sub %>%
    separate_rows(Microfunzioni, sep="\\|") %>%
    filter(!is.na(Microfunzioni), Microfunzioni!="") %>%
    count(key = Microfunzioni, name="A")
  
  B <- sub %>%
    separate_rows(Microfunzioni, sep="\\|") %>%
    filter(!is.na(Microfunzioni), Microfunzioni!="") %>%
    count(key = Microfunzioni, name="B")
  
  tab <- full_join(A,B,by="key") %>%
    mutate(
      A=replace_na(A,0),
      B=replace_na(B,0)
    )
  
  totalA <- sum(tab$A)
  totalB <- sum(tab$B)
  
  out <- map_dfr(seq_len(nrow(tab)), function(i){
    
    a <- tab$A[i]
    b <- tab$B[i]
    
    c <- totalA-a
    d <- totalB-b
    
    logOR <- log(
      ((a+0.5)/(c+0.5)) /
        ((b+0.5)/(d+0.5))
    )
    
    p <- tryCatch(
      fisher.test(matrix(c(a,b,c,d),2,2))$p.value,
      error=function(e) NA
    )
    
    tibble(
      key=tab$key[i],
      non_sub=a,
      sub=b,
      prop_non_sub=a/totalA,
      prop_sub=b/totalB,
      lift=
        (a/totalA) /
        ((a+b)/(totalA+totalB)+1e-9),
      logOR=logOR,
      p=p
    )
    
  })
  
  out %>%
    mutate(across(where(is.numeric), ~round(.x,3))) %>%
    arrange(desc(logOR))
}

# =====================================================
# REPORT
# =====================================================

make_report <- function(df, dm_i, dm_j){
  
  score_col <- paste0("sost_", dm_j)
  
  non_sub <- df %>%
    filter(
      DM.x == dm_i,
      !is.na(.data[[score_col]]),
      .data[[score_col]] < threshold
    )
  
  sub <- df %>%
    filter(
      DM.x == dm_i,
      !is.na(.data[[score_col]]),
      .data[[score_col]] >= threshold
    )
  
  if(nrow(non_sub)==0) return(NULL)
  
  list(
    
    meta =
      tibble(
        DM_i = dm_i,
        DM_j = dm_j,
        non_sub = nrow(non_sub),
        sub = nrow(sub)
      ),
    
    epistemico =
      keyness_table(
        non_sub,
        sub,
        "Tipo_Epistemico"
      ),
    
    scope =
      keyness_table(
        non_sub,
        sub,
        "Scope"
      ),
    
    isabout =
      keyness_table(
        non_sub,
        sub,
        "IsAbout"
      ),
    
    fattualita =
      keyness_table(
        non_sub,
        sub,
        "Fattualita_matrice"
      ),
    
    micro =
      keyness_micro(
        non_sub,
        sub
      )
    
  )
  
}

# =====================================================
# HTML REPORT
# =====================================================

all_pairs <- expand.grid(
  dm_i = dms,
  dm_j = dms,
  stringsAsFactors = FALSE
)

html <- tags$html(
  
  tags$head(
    
    tags$style("
      body {
        font-family: Arial;
        margin: 30px;
      }

      h1{
        color:#222;
      }

      h2{
        color:#444;
        margin-top:35px;
      }

      table{
        margin-bottom:25px;
      }
    ")
    
  ),
  
  tags$body(
    
    tags$h1(
      "DM Non-Substitutability Report"
    ),
    
    tags$p(
      "Threshold = "
    ),
    
    tags$h2(
      "Formulas"
    ),
    
    tags$p(
      "Lift = p(non_sub) / p(global)"
    ),
    
    tags$p(
      "Log Odds Ratio = log[(a/c)/(b/d)]"
    ),
    
    tags$p(
      "p-values computed with Fisher exact test"
    ),
    
    lapply(seq_len(nrow(all_pairs)), function(i){
      
      dm_i <- all_pairs$dm_i[i]
      dm_j <- all_pairs$dm_j[i]
      
      if(dm_i==dm_j) return(NULL)
      
      res <- make_report(df, dm_i, dm_j)
      
      if(is.null(res)) return(NULL)
      
      tagList(
        
        tags$hr(),
        
        tags$h1(
          paste(dm_i,"→",dm_j)
        ),
        
        tags$h2("Meta"),
        HTML(htmlTable(res$meta)),
        
        tags$h2("Tipo epistemico"),
        HTML(htmlTable(res$epistemico)),
        
        tags$h2("Scope"),
        HTML(htmlTable(res$scope)),
        
        tags$h2("IsAbout"),
        HTML(htmlTable(res$isabout)),
        
        tags$h2("Fattualità"),
        HTML(htmlTable(res$fattualita)),
        
        tags$h2("Microfunzioni"),
        HTML(htmlTable(res$micro))
        
      )
      
    })
    
  )
  
)

save_html(
  html,
  "DM_non_substitutability_report.html"
)

