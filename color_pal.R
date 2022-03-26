# set colors manually -----------------------------------------------------
# here, i'm making a list of each "group" of species, which will be 
# colored with shades of the same color
#014701, darkgreen, #4b944b, #82c882, #af9100, #dd0000, #76ee00, #b3b3b3
pal <- c(
  # kelps
  paste(colorspace::darken("darkgreen",.3)),
  "darkgreen",
  paste(colorspace::lighten("darkgreen",.3)),
  paste(colorspace::lighten("darkgreen",.6)),
  
  # non kelp browns
  "#af9100",
  
  # reds
  "#dd0000",
  
  # green
  "#76ee00",
  
  #other
  "grey70"
)



colors <- 
  tribble(
    ~species_general,                   ~color,
    # kelps:
    "Alaria esculenta",      pal[1],
    "Rope kelps",            pal[2],
    "Laminaria digitata",    pal[3],
    "Saccharina latissima",            pal[4],
    
    # other non-kelp browns:
    "Saccorhiza dermatodea",            pal[5],
    
    # reds:
    "Mixed reds",           pal[6],
    
    # greens:
    "Codium fragile",        pal[7],
    
    # other
    "Urchin barrens",        pal[8],
    
    
  )

