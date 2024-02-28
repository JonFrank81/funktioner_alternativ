skapa_kortnamn_lan_alternativ <- function(lansnamn, byt_ut_riket_mot_sverige = FALSE){
  # Skript som skapar kortnamn av län. Alternativ till Peters skript (som inte fungerar i de fall där län saknas i namnen redan från början)
  nyttnamn <- NA
  for (elem in 1:length(lansnamn)){
    if (substr(lansnamn[elem], nchar(lansnamn[elem])-3, nchar(lansnamn[elem]))==" län") nyttnamn[elem] <- substr(lansnamn[elem],1, nchar(lansnamn[elem])-4) else nyttnamn[elem] <- lansnamn[elem]
    if (substr(nyttnamn[elem], nchar(nyttnamn[elem]),nchar(nyttnamn[elem]))=="s") nyttnamn[elem] <- substr(nyttnamn[elem],1,nchar(nyttnamn[elem])-1)
    if (byt_ut_riket_mot_sverige == TRUE) if (lansnamn[elem] == "Riket") nyttnamn[elem] <- "Sverige"
  }
  return(nyttnamn)
}

byt_namn_lan_kolada <- function(lansnamn){
  
  lansnamn[lansnamn == "Region Stockholm"] = "Stockholms län"
  lansnamn[lansnamn == "Region Uppsala"] = "Uppsala län"             
  lansnamn[lansnamn == "Region Sörmland"] = "Södermanlands län"            
  lansnamn[lansnamn == "Region Östergötland"] = "Östergötlands län"
  lansnamn[lansnamn == "Region Jönköpings län"] = "Jönköpings län"
  lansnamn[lansnamn == "Region Kronoberg"] = "Kronobergs län"
  lansnamn[lansnamn == "Region Kalmar"] = "Kalmar län"
  lansnamn[lansnamn == "Region Gotland"] = "Gotlands län"
  lansnamn[lansnamn == "Region Blekinge"] = "Blekinge län"
  lansnamn[lansnamn == "Region Skåne"] = "Skåne län"
  lansnamn[lansnamn == "Region Halland"] = "Hallands län"
  lansnamn[lansnamn == "Västra Götalandsregionen"] = "Västra Götalands län"
  lansnamn[lansnamn == "Region Värmland"] = "Värmlands län"
  lansnamn[lansnamn == "Region Örebro län"] = "Örebro län"
  lansnamn[lansnamn == "Region Västmanland"] = "Västmanlands län"
  lansnamn[lansnamn == "Region Dalarna"] = "Dalarnas län"
  lansnamn[lansnamn == "Region Gävleborg"] = "Gävleborgs län"
  lansnamn[lansnamn == "Region Västernorrland"] = "Västernorrlands län"
  lansnamn[lansnamn == "Region Jämtland Härjedalen"] = "Jämtlands län"
  lansnamn[lansnamn == "Region Västerbotten"] = "Västerbottens län"
  lansnamn[lansnamn == "Region Norrbotten"] = "Norrbottens län"
  
  return(lansnamn)
  
}