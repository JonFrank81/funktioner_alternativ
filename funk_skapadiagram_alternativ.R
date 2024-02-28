
# skapa_linjediagram_ny(df <- KPI_df %>% filter(ar>"2021"),
#                       linje_typ = "solid",
#                       X_rotera_text =45,
#                       x_axel_namn = "",
#                       valt_tema = "void",
#                       titel= "Inflation",
#                       titel_justering = 0.5,
#                       plotly_diagram = TRUE)

#distinctColorPalette(25)

hej = ggplot(data=nettooms %>% filter(ar_stod == "2017",År%in%as.character(2018:2022)),
       aes(x=År, y=value, colour=`Företagsnamn`)) +
  ggtitle("Antal anställda")+
    geom_line()+ scale_color_manual(values = distinctColorPalette(25))+ guides(color=guide_legend(ncol =1))+
      labs(y = "Nettoomsättning", x = "År")+
          theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(output_mapp,filnamn))

skapa_linjediagram_ny(df <-  anstallda %>%
                        filter(ar_stod == "2017",År%in%c(2014:2022),!(is.na(value))) %>% 
                          mutate(value = as.numeric(value)),
                      linje_typ = "solid",
                      x_variabel = "År", 
                      y_variabel = "value",
                      x_variabel_grupp ="Företagsnamn",
                      X_rotera_text =45,
                      x_axel_namn = "",
                      titel= "Inflation",
                      titel_justering = 0.5,
                      linje_farg =distinctColorPalette(25))


skapa_linjediagram_ny <- function(df = data.frame(),
                                  x_variabel = "Period",
                                  y_variabel = "KPIF,.månadsförändring,.1987=100",
                                  x_variabel_grupp = 1,
                                  linje_typ = "solid", # Som standard. Övriga val: blank,dashed,dotted,dotdash,longdash,twodash,
                                  linje_farg = c("red"),
                                  bakgrund_farg = "#efefef",
                                  omgivning_farg = "#efefef",
                                  X_vertikal_justering = 0.5,
                                  X_horisontell_justering = 0.5,
                                  X_rotera_text = 90, # I grader, 0 är standard,
                                  Y_vertikal_justering = 0.5,
                                  Y_horisontell_justering = 0.5,
                                  Y_rotera_text = 0, # I grader, 0 är standard,
                                  x_axel_namn = NA, # Sätt namn på x-axel. "" om man vill ha blankt
                                  y_axel_namn = NA,  # Sätt namn på y-axel. "" om man vill ha blankt
                                  titel = "", # Sätter diagrammets titel. Standard är ingen titel
                                  titel_justering = 0.5, # Justerar diagrammets titel. 0 - 1 där 0.5 är mitt i.
                                  valt_tema = "classic", # Andra val, bw,gray, dark, light, iinedraw, minimal, void
                                  spara_figur = TRUE,
                                  plotly_diagram = FALSE,
                                  output_mapp = "G:/skript/jon/Slask/",
                                  filnamn = "figur.png"
){
  
  # Testar om det finns ett dataset
  if(is_empty(df)){
    break
    print("Dataset saknas")
  }
  
  # Sätter ett tema baserat på användarens val
  # Standardval
  tema <- theme_set(theme_classic())
  
  if(valt_tema == "bw"){
    tema <- theme_set(theme_bw())
  }
  if(valt_tema == "gray"){
    tema <- theme_set(theme_gray())
  }
  if(valt_tema == "dark"){
    tema <- theme_set(theme_dark())
  }
  if(valt_tema == "light"){
    tema <- theme_set(theme_light())
  }
  if(valt_tema == "linedraw"){
    tema <- theme_set(theme_linedraw())
  }
  if(valt_tema == "minimal"){
    tema <- theme_set(theme_minimal())
  }
  if(valt_tema == "void"){
    tema <- theme_set(theme_void())
  }
  
  # Sätter namn på y-axel
  if(is.na(y_axel_namn) == TRUE){
    y_axel_namn = y_variabel
  }else y_axel_namn = y_axel_namn
  
  # Sätter namn på x-axel
  if(is.na(x_axel_namn) == TRUE){
    x_axel_namn = x_variabel
  }else x_axel_namn = x_axel_namn
  
  x_var = as.name(x_variabel)
  y_var = as.name(y_variabel)
  
  #"#efefef"
  
  figur <- ggplot(df,aes({{x_var}},{{y_var}}))+
    geom_line(group=1,linetype=x_variabel_grupp,color = linje_farg)+
    ggtitle(titel)+
    theme_set(tema)+
    theme(plot.title = element_text(hjust = titel_justering),
          axis.text.x=element_text(angle = X_rotera_text, vjust = X_vertikal_justering,hjust = X_horisontell_justering),
          axis.text.y=element_text(angle = Y_rotera_text, vjust =Y_vertikal_justering,hjust = Y_horisontell_justering),
          panel.background = element_rect(fill = bakgrund_farg,colour="red"),
          plot.background = element_rect(fill = "#efefef"))+ # region outside plot)
    labs(y = y_axel_namn, x = x_axel_namn)
  
  
  if(plotly_diagram == TRUE) figur = ggplotly(figur,tooltip = c("x","y"))
  
  
  if (spara_figur == TRUE) ggsave(paste0(output_mapp,filnamn))
  
  return(figur)
}







