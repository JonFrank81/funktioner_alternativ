

hamta_data_FK <- function(webbadresser = "https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-barn/TFPVabBarnAlder.xlsx)",
                          dataset_namn = c("Vab_antal_barn"),
                          region_vekt = "20" ){
  
  # Funktion som tar hem data från Försäkringskassans öppna data på läns (och kommunnivå). Kräver dels dels region, dels webbadresser och dels de namn man vill att datasetten skall ha 
  # Webbadresser finns här: https://www.dataportal.se, välj organisation Försäkringskassan.
  # När du har hittat rätt data, klicka på länk och högerklicka sedan på ladda ned data och välj kopiera länkadress.
  # För Antal barn vars föräldrar har vabbat ser länken ut som följer: https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-barn/TFPVabBarnAlder.xlsx
  
  # Funktionen returnerar en lista med dataset som sedan med fördel kan sparas till Excel. Notera att antalet webbadresser och antalet namn på dataset måste var a lika många, annars avslutas funktion
  
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(janitor,
         openxlsx,
         rio,
         tidyverse)
  
                            
  if(length(webbadresser) != length(dataset_namn)){
    print("Felaktigt val av namn! Måste vara lika många som webbadresser")
    break
  }
  
  # Adresser till data
  path = webbadresser
  
  flik_lista = lst()
  i=1
  
  # Uttag av data. Eftersom det är en väldigt stor datamängd delas den upp i två flikar (av Försäkringskassan), varför lapply används,
  while(i <= length(path)){
    
    td = tempdir()              # skapa temporär mapp
    varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
    download.file(path[[i]], destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
    
    lista = lapply(getSheetNames(varsel_fil), function(x) import(file=path[[i]],which = x) %>% 
                     filter(!row_number() %in% c(0, 1)) %>% 
                     row_to_names(1) %>% 
                     filter(substr(Län,1,2) == region_vekt))
    
    # Binder ihop data från de olika flikarna i Excelfilen
    j=1
    df=c()
    while(j<=length(lista)){
      df <- rbind(df,lista[[j]])
      j=j+1
    }
    
    flik_lista[[i]] <- df
    
    print(paste0("Dataset ",i, " klart"))
    i=i+1
  }
  
  names(flik_lista) <- dataset_namn
  
  return(flik_lista)
  
}

hamta_data_TS <- function (returnera_lista = FALSE,
                           spara_till_Excel = TRUE,
                           mapp = "G:/skript/jon/Slask/",
                           filnamn = "flygstatistik.xlsx"){
  
  # Tar hem statistik för passagerare på flygplatser från Transportstyrelsen. Åren 2006 till senaste år.
  # Uppdateras genom att lägga till ett år till längst upp i lisan år. Koden får man från:
  # https://www.dataportal.se/sv/datasets/272_2057/flygplatsstatistik#ref=?p=1&q=transportstyrelsen&s=2&t=20&f=&rt=dataset%24esterms_IndependentDataService%24esterms_ServedByDataService&c=false
  # Klicka på CSV, sedan högerklick på senaste år (exempelvis 2023.csv) och sedan spara länk. 
  
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         tidyverse)
  
  # Skapar en tom dataframe
  df <- data.frame(matrix(ncol = 6,nrow = 0))
  colnames(df) <- c("Period_år","Period_månad","Flygplats","Inrikes_utrikes","ankomst_avgang","Passagerare")
  
  # Skapar en vektor med värden, där varje värde motsvarare ett år (det sista värdet i länken till CSV-filen)
  ar <- c("179",
          "105",
          as.character(93:78))
  
  # Loop som hämtar hem data
  i=1
  while(i<=length(ar)){
    lank = paste0("https://transportstyrelsen.entryscape.net/store/7/resource/",ar[i])
    df_temp <- read.csv2(lank,encoding="latin1" ) %>% 
      filter(Rörelsesubklass...Subklass == "Passagerarflyg") %>% 
      rename("Period_år" = Period...År,
             "Period_månad" = Period...Månad,
             "Flygplats" = Rapporterande.Flygplats...Namn,
             "Inrikes_utrikes" = Grundpost.Flygplats...Inrikes.utrikes,
             "ankomst_avgang" = Ankomst.avgång...Ankomst.avgång) %>%   
      group_by(Period_år,Period_månad,Flygplats,Inrikes_utrikes,ankomst_avgang) %>% 
      summarize("Passagerare" = sum(Kabinfaktorpassagerare))
    print(unique(df_temp$Period_år))
    i=i+1
    df=rbind(df,df_temp)
  }
  # Tar bort tillfällig dataframe
  rm(df_temp)
  #Grupperar på årsnivå
  df_ar <- df %>% 
    group_by(Period_år,Flygplats,Inrikes_utrikes,ankomst_avgang) %>% 
    summarize(Passagerare=sum(Passagerare))
  
  # Grupperar för utrikes och inrikes (i Sverige)
  df_Sverige_utrikes <- df %>%
    filter(Inrikes_utrikes == "Utrikes") %>% 
    group_by(Period_år,Inrikes_utrikes,ankomst_avgang) %>% 
    summarize(Passagerare=sum(Passagerare))
  
  # Inrikes, enbart avgående för att undvika dubbelräkning
  df_Sverige_inrikes <- df %>%
    filter(Inrikes_utrikes == "Inrikes",ankomst_avgang == "Avgång") %>% 
    group_by(Period_år,Inrikes_utrikes,ankomst_avgang) %>% 
    summarize(Passagerare=sum(Passagerare))
  
  flik_lista=lst("Årsdata" = df_ar,"Månadsdata" = df,"Sverige_utrikes" = df_Sverige_utrikes,"Sverige_inrikes" = df_Sverige_inrikes)
  
  if(spara_till_Excel == TRUE) write.xlsx(flik_lista,paste0(mapp,filnamn))
  
  if(returnera_lista == TRUE) return(flik_lista)
}

hamta_data_regso <- function(region = "20",
                               url = ""){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx,
         tidyverse)
  
  source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
  
  retur <- pxvardelist(url,"Region") %>% 
    filter(substr(.$kod,1,2) == region,substr(.$kod,5,5) == "R") %>% 
      .$kod
  
  return(retur)
}
                          
                          