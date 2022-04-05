pacman::p_load(tidyverse,ggplot2,ggrepel,bigrquery,googlesheets4,googledrive,jsonlite,janitor,tidyRSS,lubridate,anytime,rtweet,magick,gmailr)
my_secrets = read_json("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/personal_data.json")
'%!in%' <- function(x,y)!('%in%'(x,y))

# Anything with "cyber" in the names are my visuals, with colours based on cyberpunk themes.
# I will not provide commentation inside of them, but they are tailored to the data for each daily release.
# as the month went on I stretched more and more for content, and thus needed more and more visual functions,
# Though I'd hoped to keep them as few as possible bc I personally don't find them particularly enjoyable. 
cujos_cyber_lines <- function(df, 
                              y_col,
                              y_format = NULL,
                              main = NULL,
                              ylab = NULL,
                              xlab = NULL,
                              cap = "Powered By MTGBAN.com",
                              force_glow = T){
  # These options could (perhaps should) be in the function parameters, but I prefer hard setting them inside and reducing input options
  area = FALSE
  main.size = 20
  bg.col = "#222035"
  grid.col = "#242d4d"
  text.col = "Green"
  xlim = NULL
  ylim = NULL
  
  # Glow is deprecated in this particular function
  glow = TRUE
  n_lines <- 10
  diff_linewidth <- 0.65
  alpha_value <- 0.05
  #df = logic
  y_m <- df
  
  if(is.na(unique(y_m$number))){
    y_m = y_m %>% mutate(hasFoil = ifelse(hasFoil == 1, "Foil","Non Foil"),
                         identifier = paste(card,hasFoil))
  }else{
    y_m = y_m %>% mutate(hasFoil = ifelse(hasFoil == 1, "Foil","Non Foil"),
               identifier = paste(card,set,number,hasFoil))
  }
  
  if(unique(y_m$rarity)=="S"){
    y_m = y_m %>% mutate(identifier = paste(card))
  }
  
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  choices = y_m%>%select(card)%>%distinct()%>% nrow()
  
  if(choices <= 5){
    lwd = 1.75
  }else if( (choices > 5)&(choices <= 10) ){
    lwd = 1.25
  }else if( (choices > 10)&(choices <= 15) ){
    lwd = .75
  }else{
    lwd = .50
  }
  
  #Set colour palette
  col <- colorRampPalette(c("#ff184c", "#ff577d", "#ffccdc", "#0a9cf5", "#003062",
                            "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27",
                            "#7700a6", "#fe00fe", "#defe47","#00b3f3","#0016ee"))(choices)
  
  
  
  p <- ggplot(data = y_m, aes(x = Date, y = {{y_col}}, group = identifier, colour = identifier, fill = identifier)) +
    geom_line(size = lwd) +
    scale_color_manual(values = col) + 
    geom_line(size = 0.75 + (diff_linewidth), alpha = alpha_value) 
  
  # y_format will auto scale axis to Integer without instruction, otherwise dollar & percent are acceptable parameters
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = ({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
      
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = scales::dollar({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p  +
      labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = '#05d9e8', size = 10),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = '#05d9e8',size=10, face = 'bold'),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "top",
            plot.title = element_text(colour = "#05d9e8", size = 13,hjust = .5, face = 'bold'),
            plot.caption = element_text(colour = "#05d9e8", size = 6, face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(colour = '#05d9e8', size = 11, face = 'bold'),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(Date, {{y_col}},  label = scales::percent({{y_col}})),
                       data = rbind(y_m[1,],y_m[7,],y_m[14,],y_m[21,],y_m[28,]), 
                       fontface = "bold",
                       box.padding = unit(0.45, "lines"),
                       point.padding = unit(0.45, "lines"),
                       nudge_y = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }

  
  return(p)
  
}

# Bar graph mainly for meta play patterns from mtgo decklists
cujos_cyber_bars <- function(df, 
                             y_col,
                             y_format = NULL,
                             palette = 1, 
                             area = TRUE, 
                             main = NULL, 
                             main.size = 20, 
                             cap = "Powered By MTGBAN.com", 
                             xlab = NULL, 
                             ylab = NULL 
                            ){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  df = df %>% head(10) 
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(card,desc({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
  }
  
  p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
    theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
          panel.grid = element_line(colour = grid.col, size = 1),
          axis.text = element_text(colour = text.col),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.y = element_text(angle=0,vjust = 0.5),
          axis.title = element_text(colour = text.col),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
          plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
          legend.title = element_text(colour = text.col),
          legend.text = element_text(colour = text.col, size = 12, face = "bold"),
          plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
          panel.background = element_rect(fill = bg.col, colour = bg.col)) +
    geom_label_repel(aes(card, {{y_col}},  label = scales::percent({{y_col}})),
                     data = df, 
                     fontface = "bold",
                     box.padding = unit(0.25, "lines"),
                     point.padding = unit(0.25, "lines"),
                     label.size = 0.005,
                     nudge_y = 0.03,
                     #nudge_x = 1,
                     colour = "#ff184c",
                     fill = "black",
                     show.legend = FALSE)
  
  p
  
  return(p)
  
}
# Sealed Product required it's own function
# Instead of elongating this one function into thousands of lines
# Kept separate for ease of pulling.
cujos_extra_cyber_bars <- function(df, 
                             y_col,
                             y_format = NULL,
                             palette = 1, 
                             area = TRUE, 
                             main = NULL, 
                             main.size = 20, 
                             cap = "Powered By MTGBAN.com", 
                             xlab = NULL, 
                             ylab = NULL 
){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  df = df %>% head(10) 
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(card,({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = {{y_col}}),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = scales::dollar_format({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(card, {{y_col}},  label = scales::percent_format({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }
  
  
  return(p)
  
}
cujos_secret_cyber_bars <- function(df, 
                                   y_col,
                                   y_format = NULL,
                                   palette = 1, 
                                   area = TRUE, 
                                   main = NULL, 
                                   main.size = 20, 
                                   cap = "Powered By MTGBAN.com", 
                                   xlab = NULL, 
                                   ylab = NULL 
){
  #df = logic
  bg.col = "#212946"
  grid.col = "#242d4d"
  text.col = "#05d9e8" 
  lwd = 1.75
  xlim = NULL
  ylim = NULL
  #df = logic %>% head(20) 
  df = df %>% head(20)
  if(length(df$condition)!=0){
    df = df %>% mutate(product_name = paste0(product_name,"|",condition)) %>% select(-condition) %>% group_by(product_name,current_supply,current_months_sales,most_recent_sale,qty_last_4_months,average_sales_price,monthly_sell_through,quarterly_sell_through,daily_sell_through_current_month,daily_sell_through_current_quarter) %>% summarize(tcg_low = min(tcg_low)) %>% ungroup() 
  }
  # https://blog.depositphotos.com/15-cyberpunk-color-palettes-for-dystopian-designs.html
  
  if(palette == 1) {
    col <- colorRampPalette(c("#00ff9f", "#00b8ff", "#001eff", "#bd00ff", "#d600ff",
                              "#ff124f", "#ff00a0", "#fe75fe", "#7a04eb", "#ff6e27"))(nrow(df))
  }
  
  p <- ggplot(df, aes(x = reorder(product_name,desc({{y_col}})), y = {{y_col}}, label = round({{y_col}},0) ))
  
  if(area == FALSE) {
    p <- p + geom_bar(stat = "identity", fill = NA, color = col, size = 2)
  } else {
    p <- p + geom_bar(stat = "identity", fill = col)
  }
  
  if(is.null(y_format)){
    p = p + scale_y_continuous(limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = ({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
    
  }else if(tolower(y_format) == "dollars"){
    p = p +
      scale_y_continuous(labels=scales::dollar_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = scales::dollar({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }else if(tolower(y_format)=="percent"){
    p = p +
      scale_y_continuous(labels=scales:::percent_format(),limits = ylim)
    
    p <- p + labs(x = xlab, y = ylab, title = main, caption = cap) +
      theme(plot.background = element_rect(fill = bg.col, colour = bg.col),
            panel.grid = element_line(colour = grid.col, size = 1),
            axis.text = element_text(colour = text.col),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.y = element_text(angle=0,vjust = 0.5),
            axis.title = element_text(colour = text.col),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            plot.title = element_text(colour = text.col, size = main.size,hjust=.5),
            plot.caption = element_text(colour = text.col, size = 6, face = 'bold'),
            legend.title = element_text(colour = text.col),
            legend.text = element_text(colour = text.col, size = 12, face = "bold"),
            plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
            panel.background = element_rect(fill = bg.col, colour = bg.col)) +
      geom_label_repel(aes(product_name, {{y_col}},  label = scales::percent({{y_col}})),
                       data = df, 
                       fontface = "bold",
                       box.padding = unit(0.25, "lines"),
                       point.padding = unit(0.25, "lines"),
                       label.size = 0.005,
                       nudge_y = 0.03,
                       #nudge_x = 1,
                       colour = "#ff184c",
                       fill = "black",
                       show.legend = FALSE)
  }
  
  
  return(p)
  
}

# My data base is named Gaea's Cradle! I have so much imaginative prowess. Let's connect to it with this function.
gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = my_secrets$project,
    dataset = my_secrets$dataset,
    billing = my_secrets$billing
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
} 
# Incredibly important function. It is the powerhouse of the cell. This basic math will consilidate the insights into simple filtering to provide ease of understanding.
# To be fair, this should likely be dynamic to the data source being looked at to opt between median and mean, but, for now, it opts to highly skewed data, bc mtg is really skewed data.
detect_outliers <- function(x) {
  
  if (missing(x)) stop("The argument x needs a vector.")
  
  if (!is.numeric(x)) stop("The argument x must be numeric.")
  
  #Snag the numeric column and make it its own df
  data_tbl <- tibble(data = x)
  
  #Lets apply some fundamental outlier detection (like NO ONE in mtg seems to want do...)
  limits_tbl <- data_tbl %>%
    summarise(
      quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE),
      quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE),
      iqr         = IQR(data, na.rm = TRUE),
      limit_lo    = quantile_lo - 1.5 * iqr,
      limit_hi    = quantile_hi + 1.5 * iqr
    )
  
  #And right here, let's remove those evil speculators, money launderers, etc.
  #If you're not laughing stop lying to yourself, das good meme right here.
  output_tbl <- data_tbl %>%
    mutate(outlier = case_when(
      data < limits_tbl$limit_lo ~ TRUE,
      data > limits_tbl$limit_hi ~ TRUE,
      TRUE ~ FALSE
    ))
  #Return a boolean as a numeric so I can easily calculate % of outliers present
  #Helps me keep the baddies out of the populace (wait, am are we the baddies?!)
  return(as.numeric(output_tbl$outlier))
  
}

# Whether we're looking at NA or JPN data, let's prep the data from the BAN API in the same fashion for consistency
bl_pre_preparations = function(ban_bl_tbl){
  #Snag the data from the API and format evenly for `database_pull` logic to be a little easier
  ban_bl_tbl %>% 
    select(-id) %>%
    distinct() %>%
    #Needs to be numeric for the join even though it likely shouldn't be numeric
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    #Arrange for the lag below, even though we order by down there, this redundant step makes me feel better as a person. And I need that.
    arrange(tcg_id,hasFoil,Date,card,set,rarity,number) %>%
    #All we really need to group by is id and hasFoil,maybe* vendor too, the rest is for end users eyeballs to hate me less.
    group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
    summarize(
      Date = Date,
      offer = offer,
      l_offer = lag(offer,1,order_by = Date),
      #avg_offer is essential for filtering for pertinent cards/products of interest. Was a last minute addition I'll never go back on.
      avg_offer = mean(offer,na.rm=T),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0)
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    distinct()
}
# Likewise for markets, lets's be consistent. The main benefit of this approach is allows me to have very few
# visualizations, which I just love.
mkt_pre_preparations = function(ban_retail_tbl){
  #Snag the data from the API and format evenly for `database_pull` logic to be a little easier
  ban_retail_tbl %>%
    select(-id) %>%
    distinct() %>%
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    #OUTLIER DETECTION TO ENSURE (best as possible) FIDELITY OF THE DATA
    mutate(mkt_value_outlier_flag = as.numeric(detect_outliers(mkt_value)),
           mkt_value_outliers = sum(mkt_value_outlier_flag),
           mkt_value_cleansed = ifelse(mkt_value_outlier_flag == 1,NA,mkt_value),
           mkt_value_cleansed = ifelse(is.na(mkt_value_cleansed),round(mean(mkt_value,na.rm=T),0),mkt_value )) %>%
    #Arrange for the lag below, even though we order by down there, this redundant step makes me feel better as a person. And I need that.
    arrange(tcg_id,hasFoil,Date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
    summarize(
      Date = Date,
      #replace the `sell_price` with our cleansed replacement
      offer = mkt_value_cleansed,
      l_offer = lag(mkt_value_cleansed,1,order_by = Date),
      #avg_offer is essential for filtering for pertinent cards/products of interest. Was a last minute addition I'll never go back on.
      avg_offer = mean(mkt_value_cleansed,na.rm=T),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_absolute = sum(abs(boolean_offer),na.rm=T),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0)
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    distinct()
}
#Likewise for all data in regards to copies sold, for singles and sealed, we'll format everything so my post_logic function knows what it is grabbing every time.
basket_copies_pre_preparations = function(mtg_basket_all_tbl){
  #Snag the data from my DB and format evenly for `database_pull` logic to be a little easier
  mtg_basket_all_tbl %>% 
    #Ugh, converting a column named "number' from numeric. Oh the cruel irony. Yet, the deed must be done. Damn you 17 versions of cards with letters in your number. Posers.
    mutate(number = as.character(number)) %>%
    #I only care about english. Too much weird stuff happens outside of it. That said, maybe I should only focus on Russian cards aye? Hmmm, seems to hold potential...
    filter(language == "English") %>%
    #Get aggregated data by day before going forward. Weird little mini aggregation step that for some reason didn't originally occur to me
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    # OUTLIER DETECTION!!!!!!!!
    mutate(sold_quant_outlier_flag = as.numeric(detect_outliers(sold_quantity)),
           sold_quant_outliers = sum(sold_quant_outlier_flag),
           sold_quantity_cleansed = ifelse(sold_quant_outlier_flag == 1,NA,sold_quantity),
           sold_quantity_cleansed = ifelse(is.na(sold_quantity_cleansed),round(mean(sold_quantity,na.rm=T),0),sold_quantity )) %>%
    distinct() %>%
    #Arrange for the lag below, even though we order by down there, this redundant step makes me feel better as a person. And I need that.
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      #Replace the actual quantity sold with our cleansed value
      offer = sold_quantity_cleansed,
      avg_offer = mean(sold_quantity_cleansed),
      #This is here to make sure Im not going crazy when cards overlap or I make an error, even though
      #I'll not use it in anything presented, it helps me occams razor a lot of errors that may pop up.
      sell_price,
      #avg_offer is essential for filtering for pertinent cards/products of interest. Was a last minute addition I'll never go back on.
      avg_sell_price = mean(sell_price),
      l_offer = lag(sold_quantity_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0),
      #Nerdy curiosity really, but how volatile is the item by gauging the occurrence of outliers vs all data points reviewed.
      outliers_detected = sum(sold_quant_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct() 
}
basket_sale_pre_preparations = function(mtg_basket_all_tbl){
  #Snag the data from my DB and format evenly for `database_pull` logic to be a little easier
  mtg_basket_all_tbl %>% 
    #Ugh, converting a column named "number' from numeric. Oh the cruel irony. Yet, the deed must be done. Damn you 17 versions of cards with letters in your number. Posers.
    mutate(number = as.character(number)) %>%
    #I only care about english. Too much weird stuff happens outside of it. That said, maybe I should only focus on Russian cards aye? Hmmm, seems to hold potential...
    filter(language == "English") %>%
    #Get aggregated data by day before going forward. Weird little mini aggregation step that for some reason didn't originally occur to me
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    # OUTLIER DETECTION!!!!!!!!
    mutate(sell_price_outlier_flag = as.numeric(detect_outliers(sell_price)),
           sell_price_outliers = sum(sell_price_outlier_flag),
           sell_price_cleansed = ifelse(sell_price_outlier_flag == 1,NA,sell_price),
           sell_price_cleansed = ifelse(is.na(sell_price_cleansed),round(mean(sell_price,na.rm=T),0),sell_price )) %>%
    distinct() %>%
    #Arrange for the lag below, even though we order by down there, this redundant step makes me feel better as a person. And I need that.
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      #Replace the actual quantity sold with our cleansed value
      offer = sell_price_cleansed,
      avg_offer = mean(offer),
      l_offer = lag(sell_price_cleansed,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0),
      #Nerdy curiosity really, but how volatile is the item by gauging the occurrence of outliers vs all data points reviewed.
      outliers_detected = sum(sell_price_outlier_flag)
    ) %>% 
    ungroup() %>%
    distinct()
}
basket_sealed_copies_pre_preparations = function(mtg_basket_all_tbl){
  #Snag the data from my DB and format evenly for `database_pull` logic to be a little easier
  mtg_basket_all_tbl %>%
    #Ugh, converting a column named "number' from numeric. Oh the cruel irony. Yet, the deed must be done. Damn you 17 versions of cards with letters in your number. Posers.
    mutate(number = as.character(number)) %>%
    #Only want sealed product
    filter(rarity == "S") %>%
    #There's a whole lot behind the 1 being needed...it just needs to be there.
    #Fine, Jack Black Jumanjii "Class is in Session..."
    #Old school items need to take into account photo listings bc buyers are so weary at such high $ items
    #However, bc users are weird (- not silly...okay silly) and SOME sites, ahem, don't allow for foreign product to be listed
    #in other languages, just sorting for english or the like still lets outliers leak through. As we'll see below that's a huge problem
    #for these items.
    filter(listing_type ==1 ) %>%
    #Get aggregated data by day before going forward. Weird little mini aggregation step that for some reason didn't originally occur to me
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    # OUTLIER DETECTION!!!!!!!! Although it's largely ignored here for sealed. Sad day. I wanted to be clever and I got hoisted by my own petard. 
    mutate(sold_quant_outlier_flag = as.numeric(detect_outliers(sold_quantity)),
           sold_quant_outliers = sum(sold_quant_outlier_flag),
           sold_quantity_cleansed = ifelse(sold_quant_outlier_flag == 1,NA,sold_quantity),
           sold_quantity_cleansed = ifelse(is.na(sold_quantity_cleansed),round(mean(sold_quantity,na.rm=T),0),sold_quantity )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      #Replace the actual quantity sold with actual quantity sold! The ole' switcheroo on ya.
      #Sealed is too volatiles, the cleansed value just distorts what's going on in a data set
      #where every point of information just has too much information.
      offer = sold_quantity,
      avg_offer = mean(offer),
      l_offer = lag(sold_quantity,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0),
      outliers_detected = sum(sold_quant_outlier_flag),
      #Someone commented on one of my posts they'd like to see this and ya know, it is handy info to have
      #So gosh darn it! the feedback was productive, and I wasn't super smart right away! I had to improve my work! Wait, what are my values....
      avg_price = mean(sell_price,na.rm=T)
    ) %>% 
    ungroup() %>%
    # Keep booster packs and bundles and other weird stuff outta here.
    # Potential content source down the line though!
    filter(avg_offer >= 80) %>%
    distinct() 
}
basket_sealed_sale_pre_preparations = function(mtg_basket_all_tbl){
  #Snag the data from my DB and format evenly for `database_pull` logic to be a little easier
 mtg_basket_all_tbl %>% 
    #Ugh, converting a column named "number' from numeric. Oh the cruel irony. Yet, the deed must be done. Damn you 17 versions of cards with letters in your number. Posers.
    mutate(number = as.character(number)) %>%
    #Only want sealed product
    filter(rarity == "S") %>%
    #There's a whole lot behind the 1 being needed...it just needs to be there.
    #Fine, Jack Black Jumanjii "Class is in Session..."
    #Old school items need to take into account photo listings bc buyers are so weary at such high $ items
    #However, bc users are weird (- not silly...okay silly) and SOME sites, ahem, don't allow for foreign product to be listed
    #in other languages, just sorting for english or the like still lets outliers leak through. As we'll see below that's a huge problem
    #for these items.
    filter(listing_type ==1 ) %>%
    #Get aggregated data by day before going forward. Weird little mini aggregation step that for some reason didn't originally occur to me
    group_by(date,tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price,na.rm=T),1)) %>%
    ungroup() %>%
    # OUTLIER DETECTION!!!!!!!! Although it's largely ignored here for sealed. Sad day. I wanted to be clever and I got hoisted by my own petard. 
    mutate(sell_price_outlier_flag = as.numeric(detect_outliers(sell_price)),
           sell_price_outliers = sum(sell_price_outlier_flag),
           sell_price_cleansed = ifelse(sell_price_outlier_flag == 1,NA,sell_price),
           sell_price_cleansed = ifelse(is.na(sell_price_cleansed),round(mean(sell_price,na.rm=T),0),sell_price )) %>%
    distinct() %>%
    arrange(tcg_id,hasFoil,date,card,set,rarity,number) %>%
    group_by(tcg_id,hasFoil,card,set,rarity,number) %>%
    summarize(
      Date = date,
      #Replace the actual quantity sold with actual quantity sold! The ole' switcheroo on ya.
      #Sealed is too volatiles, the cleansed value just distorts what's going on in a data set
      #where every point of information just has too much information.
      offer = sell_price,
      avg_offer = mean(offer),
      l_offer = lag(offer,1,order_by = Date),
      change = offer - l_offer,
      change_sum = sum(change,na.rm=T),
      #Utilize the day over day changes for a modified boolean calculation
      # 1 for going up, 0 for static, -1 for going down. Much Math. Much Logic.
      boolean_offer = ifelse(offer > l_offer,1, ifelse(offer == l_offer,0,-1)),
      boolean_score = sum(boolean_offer,na.rm=T),
      #How many days did we receive data on this card/product. Crucial cutoff on all content generated.
      ct = round(n(),0),
      outliers_detected = sum(sell_price_outlier_flag),
      #Someone commented on one of my posts they'd like to see this and ya know, it is handy info to have
      #So gosh darn it! the feedback was productive, and I wasn't super smart right away! I had to improve my work! Wait, what are my values....
      qty_sold = sum(sold_quantity)
    ) %>% 
    ungroup() %>%
    # Keep booster packs and bundles and other weird stuff outta here.
    # Potential content source down the line though!
    filter(avg_offer >= 80) %>%
    distinct()
  
}

#I've had this logic in my scripts for 3+ years, and just felt it was such an easy to communicate finding that it deserved it's own day.
card_kingdom_buylist_review = function(){
  #Lines 829:852 are some old code from 2019 I really need to update and get fancier with... but it works still
  #Pulling roster information from all the cards. 
  options(httr_oob_default=TRUE) 
  options(gargle_oauth_email = my_secrets$og_patches)
  drive_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gs4_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gc()
  #drive_create("TCG_Review")
  ss <- drive_get("Sets")
  
  Sets <- read_sheet(ss,"Sets") %>% mutate_if(is.character,as.factor)
  #View(Sets)
  ck_conversion <- read_sheet(ss,"mtgjson_ck_sets")
  
  Exclusion <- Sets %>% select(contains("Excl"))
  
  #There was a time when the tidyverse had an update on rename and it could go in either direction depending on the VM I was working with
  #SO instead of updating everything and sorting it out intelligently, I went ratchet, and never went back.
  tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
      rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
      #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
      mutate(Semi = paste(name, Set,sep=""))})
  
  Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                          Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                          Semi = paste(name,Set,sep=""))
  
  #Result comes back as a json, I really did not clean this as well as I should have, but it's bc I know what I want. Very familiar with their endpoint.
  CK_Buylist = fromJSON(my_secrets$ck_api) %>% as.data.frame() %>% 
    mutate(data.edition = as.factor(data.edition), 
           data.price_buy = as.numeric(as.character(data.price_buy)),
           data.price_retail = as.numeric(as.character(data.price_retail)))
  
  #Everyones got to be special with their naming.... lets pull a Rick an Morty,break every bone in it's body and glue it back together. Again,
  #I'm not doing full justice to all the nuances of the data here. But it still generates cool stuff to see. So Hah! I'm not an engineer, great is good enough. Sorry Grixis.
  No_Foils =  CK_Buylist                                     %>% 
    filter(data.variation == "" & data.is_foil == "false") %>% 
    group_by(data.edition)                                 %>% 
    count(data.edition)                                    %>%
    rename("edition"="data.edition","count"="n")
  
  
  Foils =  CK_Buylist                                        %>% 
    filter(data.variation == "" & data.is_foil == "true")  %>% 
    group_by(data.edition)                                 %>% 
    count(data.edition)                                    %>%
    rename("edition"="data.edition","count"="n")      
  
  NF_Total_Offers <- sum(No_Foils$count)
  F_Total_Offers <- sum(Foils$count)
  
  No_Foils = No_Foils %>% mutate(composition = (round(count/NF_Total_Offers,4)))
  Foils = Foils %>% mutate(composition = (round(count/F_Total_Offers,4)))
  
  
  # Jenny Craig Buy List ----------------------------------------------------
  
  #Cue Doctor Who "I'm Clever!" giph here (pronounced "Jif" btb, as creamy on the tongue as the peanut butter, unless you're a monster)
  Slim_CK_Buylist = CK_Buylist                                %>%
    mutate(data.number = as.numeric(gsub(".*-","",data.sku)) ) %>%
    select(meta.created_at     ,
           data.name           ,
           data.edition        ,
           data.is_foil        ,
           data.number         ,
           data.price_retail   ,
           data.qty_retail     ,
           data.price_buy      ,
           data.qty_buying)                                 %>%
    #This segment comes from a larger script, where all set names need to get along. Not really needed here though.
    left_join(.,
              Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
              by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
    left_join(.,Exclusion, by = c("mtgjson" = "Set_Excl"))  %>% 
    #I used to, kinda still do, have an exlude list of sets which are so old, they deserve their own time and love. And I'm all out of both.
    #But I do still have bubblegum, so I've got that going for me.
    replace_na(list(Excl_Excl = "Unclear"))                 %>%
    #Low level filters to boot cheap stuff out of there
    filter(Excl_Excl != "Exclude"   & 
             data.qty_buying != 0 &
             data.price_buy > 1.50)                       %>%
    select(-mtgjson, - Excl_Excl)                           %>%
    mutate(data.is_foil = ifelse(data.is_foil == "false",
                                 0,
                                 1),
           #
           data.qty_retail = ifelse(data.qty_retail == 0,
                                    1, 
                                    data.qty_retail),
           #
           QTY_Diff = round((data.qty_buying
                             -
                               data.qty_retail)
                            /
                              data.qty_buying,2),
           #
           Price_Diff = round(data.price_buy
                              /
                                data.price_retail,2),
           # Lets buckets our cards into group based off retail qty, retail vs buy qty, and buy qty
           # Who cares if they hide their retail inventory, they can't hide their desired buy quantity
           # So by being super duper clever (where's my award) the ratio is more revealing than really anything else.
           Tier_QTY_Diff = ntile(QTY_Diff,10),
           Tier_Price_Diff = ntile(Price_Diff,10),
           Tier_data.qty_buying = ntile(data.qty_buying,10))%>%
    mutate(
      #Less smart, just useful, what's the average of all these buckets put together, equally weighted.
      Tier = round(rowMeans(
        (select(.,Tier_QTY_Diff,
                Tier_Price_Diff,
                Tier_data.qty_buying))),2))          %>%
    
    arrange(desc(Tier))                                     %>%
    # Drop the components so no one can see my super complex logic later
    select(-Tier_QTY_Diff,
           -Tier_Price_Diff,
           -Tier_data.qty_buying)                           %>%
    
    `row.names<-` (seq(nrow(.)))                            %>%
    #Im very lazy here and just use a pre-existing column to make my key for later stuff
    mutate(meta.created_at = paste(data.name,
                                   data.edition,
                                   data.is_foil,
                                   sep="")) %>%
    distinct()
  
  #Only show the top tier cards that they show as having full inventory of
  #Oh, thanks to mtjson for giving me the rdate column, that helps me get rid of
  # new standard sets that just make everything wonky.
  Eternal_Growers = Slim_CK_Buylist                           %>% 
    filter(Tier >= 9.5 & data.qty_retail >= 8)              %>%
    left_join(.,
              Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
              by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
    left_join(.,Updated_Tracking_Keys      %>% 
                select(rdate,Set)        %>% 
                distinct(), 
              by = c("mtgjson" = "Set"))                    %>% 
    mutate(data.edition = mtgjson)                          %>% 
    distinct()                                              %>%
    filter(is.na(rdate)==F                                   & 
             rdate <= (Sys.Date() %m-% months(13))) %>%
    arrange(desc(Tier),desc(Price_Diff)) %>%
    mutate(Tier = seq(nrow(.)))
  
  return(Eternal_Growers)
  
}

# Thank you hidden agenda, mod over at mtgban.com, for allowing me to utilize his google sheet to provide content for half the month XD
secret_lair_data = function(){
  #Hidden Agenda gave me access to his Secret Lair Spreadsheet. Be Jealous Data Nerds. I assume you have to be one if you're reading this. Sweet victory.
  options(httr_oob_default=TRUE) 
  options(gargle_oauth_email = my_secrets$og_patches)
  drive_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gs4_auth(email = my_secrets$og_patches,use_oob=TRUE)
  gc()
  #drive_create("TCG_Review")
  ss <- drive_get("TCGPlayer Pricing Sheet 2020")
  
  sl_sealed_data <- read_sheet(ss,"Sealed") %>% clean_names()
  
  sl_singles_data = read_sheet(ss,"Singles") %>% clean_names()
  
  return(list(sl_sealed_data,sl_singles_data))
}

# If you want to dissect my work, this is the place, the database_pull function is my using object oriented programming to establish content off the above functions
# for every day of the month. Mostly, it pulls on my personal database, but occasionally other resources as well to create content.
database_pull = function(){
  # This entire thing ticks off day_in_month, it decides the data base pulls as well as the content to send twitters ways.
  day_in_month = day(Sys.Date())
  #day_in_month = 13
  post_logic = function(data){
    
    day_in_month = day(Sys.Date())
    #day_in_month = 13
    
    #Buy List Analysis Dates 1:8
    
    #test on my end to ensure functionality
    
    #data = bl_review_tbl
    if(day_in_month == 1){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the worst change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum))
      
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      #Name our logic and also create our chart title
      logic_chosen = "Worst US Buylist Performance By Value"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you...
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                            " from ",
                            gsub("Cards","",unique(logic$set)),
                            "-",
                            nf_f,
                            " has been declining this month on Card Kingdoms Buy List, down ",
                            min(logic$change),
                            "(",
                            scales::percent(min(logic$change)/logic$offer[1]),
                            "). Spotlight on higher value item to gauge broader movement, 
                            may be an opportunity or heads up it's going down atm.",
                            " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      #Return all 4 elements for the day for ease of twitter post
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 2){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the best change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      #Name our logic and also create our chart title
      logic_chosen = "Best US Buylist Performance By Value"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you..
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increasing this month on Card Kingdoms Buy List, up ",
                             max(logic$change),
                             "(",
                             scales::percent(max(logic$change)/logic$offer[1]),
                             "). Spotlight on higher value item to gauge broader movement, 
                            may be an opportunity for arb or time to buy.",
                             " #mtgban #mtgfinance")
      
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 3){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Filter for the worst change
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 20) %>% filter(boolean_score == min(boolean_score))
      
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      #Name our logic and also create our chart title
      logic_chosen = "Worst US Buylist Performance By Rate of Decline"
      #Provide key details for graph axis and formatting. It was really quite the hassle to account for dollars, percents, and integers all at once, let me tell you..
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      #Ensure that if my desired message is too large, for some reason, the content will post without it, to ensure something is generate.
      # Likely will alter this to ensure it retain the hashtags for BAN.
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been declining this past month on Card Kingdoms Buy List, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             "). Spotlight on rate of change, rapid change often ties with related items. Usually a reprint, or some other cause..?",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 4){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 15) %>% filter(boolean_score == max(boolean_score))
      
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      
      logic_chosen = "Best US Buylist Performance By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="CK Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increasing this past month on Card Kingdoms Buy List, up ",
                             scales::dollar(max(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             "). Spotlight on rate of change, a rapid increase tends to show increased demand, but limited/throttled supply.",
                             " #mtgban #mtgfinance")
      
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
    #test on my end to ensure functionality
    #data = jpn_bl_review_tbl
    #JPN Buy List Data
    #Honestly I want EU MKT data evenutally but I screwed up tracking my own API for data from BAN APi. Palm + Face = Me.
    if(day_in_month == 5){
      data = data %>% group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct, max_offer = max(offer)) %>%
        ungroup() %>% 
        filter(change <= 150 & change_sum >= -150) %>% 
        filter(max_offer <= 1000) %>%  
        filter(!grepl("(Alpha|Beta|Portal)",set)) %>% 
        group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct = n(), max_offer) %>%
        ungroup()
        
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum))
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      logic_chosen = "Worst JPN Buylist Performance By Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has declined this past month on Hareruya's BL, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(min(logic$change_sum)/logic$offer[1]),
                             ").Demand from around the globe causes copies to ebb and flow! JPN is usually slower to move, but when they do...",
                             " #mtgban #mtgfinance")
      
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))

    }
    if(day_in_month == 6){
      data = data %>% group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct, max_offer = max(offer)) %>%
        ungroup() %>% 
        filter(change <= 150 & change_sum >= -150) %>% 
        filter(max_offer <= 1000) %>%  
        filter(!grepl("(Alpha|Beta|Portal)",set)) %>% 
        group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct = n(), max_offer) %>%
        ungroup()
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      logic_chosen = "Best JPN Buylist Performance by Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " has been increased this past month on Hareruya's BL, up ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             ").Always worth comparing these items to 'anomalies' in the NA Market. Just saying, can explain a lot.",
                             " #mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 7){
      data = data %>% group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct, max_offer = max(offer)) %>%
        ungroup() %>% 
        filter(change <= 150 & change_sum >= -150) %>% 
        filter(max_offer <= 1000) %>%  
        filter(!grepl("(Alpha|Beta|Portal)",set)) %>% 
        group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct = n(), max_offer) %>%
        ungroup()
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 15) %>% filter(boolean_score == min(boolean_score))
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      logic_chosen = "Worst JPN Buylist Performance By Rate of Decline"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " is declining this past month on Hareruya's BL, down ",
                             scales::dollar(min(logic$change_sum)),
                             "(",
                             scales::percent(min(logic$change_sum)/logic$offer[1]),
                             ").JPN buylist tends to be a lot more calm, than NA, but if something is moving, always worth noting.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 8){
      data = data %>% group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct, max_offer = max(offer)) %>%
        ungroup() %>% 
        filter(change <= 150 & change_sum >= -150) %>% 
        filter(max_offer <= 1000) %>%  
        filter(!grepl("(Alpha|Beta|Portal)",set)) %>% 
        group_by(tcg_id,hasFoil,card,set,rarity,number,vendor) %>%
        summarize(Date,offer,l_offer,avg_offer,change,change_sum,boolean_offer,boolean_score,ct = n(), max_offer) %>%
        ungroup()
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 15) %>% filter(boolean_score == max(boolean_score))
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      logic_chosen = "Best JPN Buylist Performance By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="JPN BL Offer",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " is increasing this past month on Hareruya's BL, up ",
                             scales::dollar(max(logic$change_sum)),
                             "(",
                             scales::percent(max(logic$change_sum)/logic$offer[1]),
                             ").JPN buylist increasing can lead a lot of bigger sellers to ship overseas, not uncommon to move our market prices.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Basket Analysis Dates 9:17
    #test on my end to ensure functionality
    #data = basket_copies_sold_tbl
    if(day_in_month == 9){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(rarity != 'S')  %>% filter(avg_sell_price >= 15) %>%filter(change_sum == min(change_sum))
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      logic_chosen = "Worst Card By Actual Copies Sold"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate declined the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".Market value tends to declines with low demand, but not always. In general, Caveat Emptor on these ones.The 'why' is crucial.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 10){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(rarity != 'S') %>% filter(avg_sell_price >= 15) %>% filter(change_sum == max(change_sum))
      
      if(length(unique(logic$card))> 1){
        the_chosen_one = logic %>% filter(offer == max(offer)) %>% select(tcg_id,hasFoil) %>% distinct()
        
        logic = logic %>% filter(tcg_id == the_chosen_one$tcg_id & hasFoil == the_chosen_one$hasFoil)
      }
      
      logic_chosen = "Best Card Performance By Actual Copies Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate increased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".Market value tends to increase with increasing demand. In my experience these are the cards folks 'don't notice' go up.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 11){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(rarity != 'S') %>% filter(avg_sell_price > 5) %>% filter(boolean_score == min(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Worst Card By Declining Sales Rate"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate decreased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".This card is experiencing a lot of gaps in demand, bringing into question what might be occuring here recently.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 12){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(rarity != 'S')  %>% filter(avg_sell_price > 15) %>% filter(boolean_score == max(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card By Increasing Sales Rate"
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Copies Sold",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sales rate increased the prior month, w/ mkt value starting at ",
                             scales::dollar((logic$sell_price[1])),
                             " ending at ",
                             scales::dollar(logic$sell_price[nrow(logic)]),
                             ".This card is experiencing a lot of increasing demand, interesting to watch how the overall value moves here.",
                             "#mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #DAY 13 NEEDS REWORK //// NO - We made it PODCAST DAY!
    if(day_in_month == 13){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      logic_chosen = "Podcast Day!"
      
      rss_feed = tidyfeed(
        feed = "https://media.rss.com/bandwidth/feed.xml",
        config = list(),
        clean_tags = TRUE,
        list = FALSE,
        parse_dates = TRUE
      )
      
      latest_rss = rss_feed%>% filter(item_pub_date == max(item_pub_date))
      
      tweet_content = paste0("Check out the latest Podcast from ",
                             latest_rss$feed_title,
                             " where we discuss ",
                             latest_rss$item_title,
                             " over on https://open.spotify.com/show/755ha9x9YrO5zsBHdBmQjK?si=b1c0699d9f7747cc ",
                             "#mtgban #mtgfinance")
      
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      media_component <- magick::image_read('/home/cujo253/mines_of_moria/Essential_Referential_CSVS/ban_logo.png')
      media_component = image_flatten(media_component)

      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality
    #data = basket_sale_price_tbl
    if(day_in_month == 14){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      #Remember those exclusion sets from the cardkingdom function? Yeah. Here's but a taste. I'm next to positive this will say something crazy eventually off a nuts set that doesn't really matter.
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(!grepl("(Ice Age|Alpha|Beta|Arabian)",set)) %>% filter(change_sum == min(change_sum)) %>% filter(outliers_detected == max(outliers_detected))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Volatile Card By Value Fluctuation"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " sale price is fluctating, w/ a mkt value base of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".This card is experiencing a greater trend downwards, but it raises the question, Why is it fighting against greater value? ",
                             "#mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 15){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 5) %>%filter(!grepl("(Ice Age|Alpha|Beta|Arabian)",set))  %>% filter(change_sum == max(change_sum))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card Performance By Value"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("Cards","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is fluctating, w/ a mkt base of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".This card trending upwards. Keep in mind, if the uptick is around $5, it may show shipping costs affecting price points, before others notice",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 16){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(avg_offer > 5) %>% filter(boolean_score == min(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == min(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Worst Card By Rate of Decline"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("(\\s*Cards|\\s*Promos)","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is declining, starting the month w/ a mkt value of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and down to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".The buyer spotlight, or phase, may be moving away. This is creating either a warning sign or an opportunity",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 17){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)-1)
      logic = data %>% filter(ct >= cutoff$cutoff)  %>% filter(avg_offer > 5) %>% filter(boolean_score == max(boolean_score))
      if( length(unique(logic$card)) > 1 ){
        logic_id = logic %>% filter(offer == max(offer,na.rm=T)) %>% select(tcg_id) %>% unique()
        logic = logic %>% filter(tcg_id %in% logic_id$tcg_id)
      }
      logic_chosen = "Best Card By Rate of Increase"
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      if(unique(logic$hasFoil)==1){
        nf_f = "(F)"
      }else{
        nf_f = "(NF)"
      }
      
      tweet_content = paste0(unique(logic$card),
                             " from ",
                             gsub("(\\s*Cards|\\s*Promos)","",unique(logic$set)),
                             "-",
                             nf_f,
                             " value is increasing, starting the month w/ a mkt value of ",
                             scales::dollar(min(logic$offer,na.rm=T)),
                             " and up to ",
                             scales::dollar(max(logic$offer,na.rm=T)),
                             ".Buyers, for whatever reason, have focused on it. Will it last, and grow, or will this be just another phase?",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
    #Play Dates 18:21
    #I truly believe these have so little value, but mtg is a game not a business!
    #Always forget that. SO! I've been running this gambit a long time, and there's one lesson to know:
    
    #Give the people what they want.
    
    #test on my end to ensure functionality
    #data = decklist_data
    if(day_in_month == 18){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("pioneer",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("pioneer",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2 ) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.))) %>%
        arrange(desc(percent_of_meta))
      
      logic_chosen = "Pioneer Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen,cap="Powered By MTGBAN.com")
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Pioneer format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 19){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("modern",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("modern",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.))) %>%
        arrange(desc(percent_of_meta))
      
      logic_chosen = "Modern Meta Data"
      
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Modern format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 20){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("legacy",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("legacy",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.))) %>%
        arrange(desc(percent_of_meta))
      
      logic_chosen = "Legacy Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Legacy format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 21){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% 
        filter(grepl("pauper",format)) %>% 
        select(date,format,style) %>%
        distinct() %>%
        group_by(format) %>%
        summarize(cutoff = n()) %>%
        ungroup()
      
      logic = data %>% 
        filter(grepl("pauper",format)) %>% 
        filter(!grepl("(Forest|Mountain|Plains|Swamp|Island)$",card)) %>%
        arrange(card,desc(date)) %>%
        group_by(card,rarity) %>%
        summarize(
          meta_rank = round(mean(meta_rank),1),
          percent_of_meta = round(sum(potential_decks)/sum(decks_in_tournament),3),
          copies_entered = sum(qty),
          avg_copies_in_deck = round(mean(avg_copies),0),
          tournaments_entered = n()
        ) %>%
        ungroup() %>%
        filter(tournaments_entered >= cutoff$cutoff/2) %>%
        arrange(meta_rank) %>%
        mutate(meta_rank = seq(nrow(.))) %>%
        arrange(desc(percent_of_meta))
      
      logic_chosen = "Pauper Meta Data"
      
      media_component = logic %>% cujos_cyber_bars(.,percent_of_meta,y_format = "percent",main=logic_chosen)
      
      tweet_content = paste0(logic$card[1],", ",
                             logic$card[2],", ",
                             logic$card[3],", ",
                             logic$card[4]," & ",
                             logic$card[5],
                             " are the defining cards for the Pauper format this prior month via mtgo. Of all decks submitted, X% contain these cards, and define the current meta",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Sealed Only 22:25
    #test on my end to ensure functionality
    #data = basket_sealed_sale_price_tbl
    if(day_in_month == 22){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == min(change_sum)) 
      
      logic_chosen = "Worst Sealed by Value Lost this Month"
      
      #mtg_basket_all_tbl %>% filter(grepl("Core Set 2021 - Collector Booster",card)) %>% view()
      
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " has experienced a drop from ",
                             scales::dollar(max(logic$offer)),
                             " to ",
                             scales::dollar(logic$offer[nrow(logic)]),", a ", scales::percent(1 - logic$offer[nrow(logic)]/max(logic$offer)),
                             " decline this prior month.",
                             "Sealed product moves slowly, prone to outliers... even still, this items movement is at least worth noting",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 23){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(ct >= cutoff$cutoff) %>% filter(change_sum == max(change_sum))
      logic_chosen = "Best Sealed Performer by Value Gained this Month"
      
      media_component = logic %>% cujos_cyber_lines(.,offer,y_format="dollars",ylab="Sold For",main=logic_chosen)
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " has gone up this prior month.",
                             "Sealed product tends to be slow, and yet this is up ", scales::percent((logic$offer[nrow(logic)] - logic$offer[1])/logic$offer[1])," (",
                             scales::dollar((logic$offer[nrow(logic)] - logic$offer[1])),") having sold ",unique(logic$qty_sold), " Editions. ",
                             "#mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality
    #data = basket_sealed_copies_sold
    if(day_in_month == 24){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(!grepl("Secret Lair",card)) %>%filter(ct >= cutoff$cutoff) %>% filter(boolean_score == min(boolean_score))
      logic_chosen = "Worst Sealed by Rate of Decline"
      
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Sold For",main=logic_chosen)
      
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " is experiencing a high rate of loss this prior month.",
                             "These rapid losers tend to be standard products falling out of fashion...buying opp or indications of things to come for the set? ",
                             "#mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 25){
      #Create a cutoff to ensure that we stick with data that has represented itself a great deal in the prior month
      cutoff = data %>% select(ct) %>% summarize(cutoff = max(ct)/2)
      logic = data %>% filter(!grepl("Secret Lair",card)) %>%filter(ct >= cutoff$cutoff)%>%filter(!grepl("Double Masters",card)) %>% filter(boolean_score == max(boolean_score))
      logic_chosen = "Best Sealed Performer by Rate of Increase"
      
      
      media_component = logic %>% cujos_cyber_lines(.,offer,ylab="Sold For",main=logic_chosen)
      
      
      tweet_content = paste0("The ",
                             unique(logic$card),
                             " is experiencing a high rate of growth this prior month.",
                             "Sealed growth is rare, and if the item is rising, are the cards inside of greater value than the sealed itself? Or is the reverse happening. ",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Parlour Trick With CK Buy List 
    #test on my end to ensure functionality
    #data = card_kingdom_buylist_review()
    if(day_in_month == 26){
      logic = data
      logic_chosen = "Card Kingdom's REAL Hotlist"
      
      media_component = logic %>%rename(card=data.name)%>% cujos_extra_cyber_bars(.,ylab = "Rank", Tier,main=logic_chosen)
      
      tweet_content = paste0("CK's hot buylist has always been a curiosity to me. The data showing their wants has never been in line with the list itself. I've had this in my scripts for years as a `parlour trick`, so weird(:P) how it coincides with tcgplayer sales... ",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #Secret Lair Data 27:30
    #test on my end to ensure functionality
    data = secret_lair_data()
    if(day_in_month == 27){
      logic = data[[1]] %>% filter(!is.na(tc_gplayer_id)) %>% arrange(desc(monthly_sell_through)) %>% head(10)
      logic_chosen = "Best Secret Lair by Monthly Sales Rate"
      
      media_component = logic %>% cujos_secret_cyber_bars(.,monthly_sell_through,y_format="percent",ylab="% Inventory Sold",main=logic_chosen)
      
      tweet_content = paste0("Secret Lair's are the future aye? Alrighty then. Let's looks at the best sealed releases are performing. We're looking at the amount of copies sold vs current inventory to discover which are the best selling SL products ",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
    }
    if(day_in_month == 28){
      logic = data[[1]]  %>% filter(!is.na(tc_gplayer_id)) %>% arrange(desc(last_4_months_qty)) %>% head(10) 
      logic_chosen = "Best Secret Lair by Quarterly Editions Sold"
      
      
      media_component = logic %>% arrange(last_4_months_qty) %>% cujos_secret_cyber_bars(.,last_4_months_qty,y_format = NULL,ylab="Products Sold",main=logic_chosen)
      
      tweet_content = paste0("Okay, I'm sassy, I also think Secret Lairs are the future. As such, let's throw that window wider into the past and look at all sealed copies sold in the last 3 months. Remember, % only matter with solid sales amts beneath them.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
      
      
    }
    #test on my end to ensure functionality
    #data = data_base[[2]]
    if(day_in_month == 29){
      logic = data[[2]] %>% 
        clean_names() %>% 
        select(-condition_2) %>% 
        filter(!is.na(tc_gplayer_id)) %>% 
        filter(current_supply >= 15) %>% 
        arrange(desc(monthly_sell_through)) %>% 
        mutate(monthly_sell_through = round(monthly_sell_through,2)) %>%
        head(10)
      logic_chosen = "Best Secret Lair Singles by Monthly Sales Rate"
      
      media_component = logic %>% cujos_secret_cyber_bars(.,monthly_sell_through,y_format="percent",ylab="% Inventory Sold",main=logic_chosen)
      
      
      tweet_content = paste0("Percentages aren't everything... But churn is very important. Reviewing the average card, the expected sell through rate. Meanwhile secret lair cards... Not hard to see why vendors love these products",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    if(day_in_month == 30){
      logic = data[[2]] %>% clean_names() %>% filter(!is.na(tc_gplayer_id)) %>% filter(current_supply >= 15)%>% arrange(desc(qty_last_4_months)) %>% head(20)
      logic_chosen = "Best Secret Lair Singles by Quarterly Copies Sold"
      
      media_component = logic %>% arrange(qty_last_4_months) %>% cujos_secret_cyber_bars(.,qty_last_4_months,y_format = NULL,ylab="Copies Sold",main=logic_chosen)
      
      tweet_content = paste0("Please be mindful, cards that are cheaper will sell more copies. Cards of higher value being compared to lower value cards irg to copies sold is very important elemtn to keep in mind. A 20$ card is moving as many copies as a $1 card? That is of interest.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    #test on my end to ensure functionality - Shilling still needs testing now
    #SHILL BABY SHILL
    #100% going to ask Charly T. to write this tweet template.
    #Despite the clearly earned bravado within, I have no idea how to shill.
    if(day_in_month == 31){
      logic = "I don't need logic to shill. MTGBAN IS THE BEST. I like money, please support me."
      logic_chosen  = "I don't need logic to shill. MTGBAN IS THE BEST. I like money, please support me."
      media_component = ""
      
      tweet_content = paste0("Please support me over at: https://www.patreon.com/ban_community . We're focused on open source contributions, attempting to move mtg finance out of the 90's from a tech, data, and community understanding, as fast as we can.",
                             " #mtgban #mtgfinance")
      #If this tweet for some reason is above threshold, just post the media. 
      #I worked too hard on the visuals to not at least hope for discussion 
      #around them if I don't have guidelines on it in tweet form. Just a fools hope. Thanks Gandalf.
      if(nchar(tweet_content)> 280){
        tweet_content = ""
      }
      
      
      return(list(logic,logic_chosen,media_component,tweet_content))
    }
    
  }
  
  #Pull appropriate data from BQ, GS, or my own general scripts depending on dates for content generation
  if(day_in_month <=4){
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
 ',sep = "")
  table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
  
  
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor,'Card Kingdom') AND card is not null ",sep = "")
  ban_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-uuid,-tcg_id) %>% rename(tcg_id = tcg_id_1) %>% distinct()
  
  
  con <- gaeas_cradle(my_secrets$patches)
  statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid  
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor, 'TCG Low') ",sep = "")
  ban_retail_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id=tcg_id_1) %>% distinct()
  
  

  pacman::p_load(janitor)
  
  #max(bl_review_tbl$Date)
  bl_review_tbl = bl_pre_preparations(ban_bl_tbl)
  
  performant = "Unknown"
  while(performant == "Unknown"){
    
    all_content = post_logic(bl_review_tbl)
    
    performant = all_content[[1]]
    
    if(length(unique(performant$tcg_id)) > 1){
      performant_prio = performant %>% 
        filter(Date == min(Date)) %>% 
        arrange(desc(offer)) %>%
        .[1,];
      performant = performant %>% filter( (tcg_id %in% performant_prio$tcg_id) & (hasFoil %in% performant_prio$hasFoil) )
    }
    
    ggplot_data = performant %>%
      select(-l_offer,-contains("change"),-contains("boolean")) %>%
      full_join(
        ban_retail_tbl %>% 
          mutate(tcg_id = as.numeric(tcg_id)) %>%
          filter(tcg_id == unique(performant$tcg_id) & hasFoil == unique(performant$hasFoil)) %>% 
          arrange(Date) %>%
          rename(mkt_vendor = vendor) %>%
          select(-id)
      )
    
    con <- gaeas_cradle(my_secrets$patches)
    statement <- paste('SELECT * FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"',unique(ggplot_data %>% filter(!is.na(vendor)) %>% select(vendor) ),'") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
    performant_alt_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1)
    
    ggplot_aux_bl_data = performant_alt_bl_tbl %>% 
      mutate(tcg_id = as.numeric(tcg_id)) %>%
      group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
      summarize(all_others_offer = round(mean(offer,na.rm=T),-1)) %>%
      ungroup() %>%
      arrange(Date)
    
    
    
    con <- gaeas_cradle(my_secrets$patches)
    statement <- paste('SELECT * FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"(',unique(ggplot_data %>% filter(!is.na(mkt_vendor)) %>% select(mkt_vendor) ),'|Trend)") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
    performant_alt_mkt_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id = tcg_id_1)
    
    if(nrow(performant_alt_mkt_tbl) == 0){bl_review_tbl = bl_review_tbl %>% filter( (tcg_id != unique(performant$tcg_id)) & (hasFoil != unique(performant$hasFoil)));performant = "Unknown"}
    
    if(length(performant)>1){print("All Error Handling Completed");break}
    
  }
  
  ggplot_aux_mkt_data = performant_alt_mkt_tbl %>% 
    mutate(tcg_id = as.numeric(tcg_id)) %>%
    group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
    summarize(all_others_mkt = round(mean(mkt_value,na.rm=T),-1)) %>%
    ungroup() %>%
    arrange(Date)
  
  con <- gaeas_cradle(my_secrets$patches)
  
  statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'"  
                   AND a.number = ',unique(ggplot_data$number),' 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
  mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
  
  if(nrow(mtg_basket_tbl)==0){
    statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',(30 + table_limitations$days_behind_today ) ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'" 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
    mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
  }
  
  cleansed_basket_tbl = mtg_basket_tbl %>% 
    mutate(listing_type = ifelse(listing_type == 1, "Photo","")) %>%
    group_by(date,tcg_id,version) %>%
    summarize(conditions = paste(condition, collapse = ", "),
              languages = paste(language, collapse = ", "),
              listings_sold = paste(listing_type, collapse = ", "),
              sold_quantity = sum(sold_quantity),
              sell_price = round(mean(sell_price),0)) %>%
    ungroup()
  
  
  
  gg_data = ggplot_data %>%
    full_join(ggplot_aux_bl_data) %>%
    full_join(ggplot_aux_mkt_data) %>%
    full_join(cleansed_basket_tbl %>% select(tcg_id,version,date,sold_quantity,sell_price) %>% rename(hasFoil=version), by=c("tcg_id"="tcg_id","Date"="date","hasFoil"="hasFoil")) %>%
    rename(ck_offer = offer, tcg_low = mkt_value) %>%
    clean_names() %>%
    select(-ct) %>%
    filter(!is.na(vendor)) %>%
    arrange(date) %>%
    distinct() %>%
    filter(!is.na(mkt_vendor)) %>%
    replace(is.na(.),0)
  
  return(list(gg_data,all_content[[2]],all_content[[3]],all_content[[4]]))
  }else 
    if( (day_in_month > 4)&(day_in_month<=8) ){
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )',sep = "")
      table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
      
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )',sep = "")
      table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
      

      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                    regexp_contains(vendor,'Hare') AND card is not null ",sep = "")
      jpn_ban_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-uuid,-tcg_id) %>% rename(tcg_id = tcg_id_1) %>% distinct()
      
      
      con <- gaeas_cradle(my_secrets$patches)
      statement <- paste("SELECT * ","FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid  
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",30 + table_limitations$days_behind_today ," DAY)) AND
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL ",table_limitations$days_behind_today ," DAY)) AND
                   regexp_contains(vendor, 'TCG Low') ",sep = "")
      ban_retail_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id=tcg_id_1) %>% distinct()
      
      
      jpn_bl_review_tbl = bl_pre_preparations(jpn_ban_bl_tbl) %>% filter(!grepl("The Tabernacle at Pendrell Vale",card))
      
      performant = "Unknown"
      while(performant == "Unknown"){
        
        all_content = post_logic(jpn_bl_review_tbl)
        
        performant = all_content[[1]]
        
        if(length(unique(performant$tcg_id)) > 1){
          performant_prio = performant %>% 
            filter(Date == min(Date)) %>% 
            arrange(desc(offer)) %>%
            .[1,];
          performant = performant %>% filter( (tcg_id %in% performant_prio$tcg_id) & (hasFoil %in% performant_prio$hasFoil) )
        }
        
        ggplot_data = performant %>%
          select(-l_offer,-contains("change"),-contains("boolean")) %>%
          full_join(
            ban_retail_tbl %>% 
              mutate(tcg_id = as.numeric(tcg_id)) %>%
              filter(tcg_id == unique(performant$tcg_id) & hasFoil == unique(performant$hasFoil)) %>% 
              arrange(Date) %>%
              rename(mkt_vendor = vendor) %>%
              select(-id)
          )
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT * FROM `gaeas-cradle.ban_buylist.*` a 
  LEFT JOIN (SELECT tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.tcg_id
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"',unique(ggplot_data %>% filter(!is.na(vendor)) %>% select(vendor) ),'") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
        performant_alt_bl_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id_1)
        
        ggplot_aux_bl_data = performant_alt_bl_tbl %>% 
          mutate(tcg_id = as.numeric(tcg_id)) %>%
          group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
          summarize(all_others_offer = round(mean(offer,na.rm=T),-1)) %>%
          ungroup() %>%
          arrange(Date)
        
        
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT * FROM `gaeas-cradle.ban_retail.*` a 
  LEFT JOIN (SELECT uuid,tcg_id,card,c.set,c.rarity,c.number FROM `gaeas-cradle.roster.mtgjson_ban` c) b on a.tcg_id = b.uuid
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  not regexp_contains(vendor,"(',unique(ggplot_data %>% filter(!is.na(mkt_vendor)) %>% select(mkt_vendor) ),'|Trend)") AND card like "',unique(ggplot_data$card),'"  
                   AND b.set like "',unique(ggplot_data$set),'"  
                   AND b.number like "',unique(ggplot_data$number),'" 
                   AND a.hasFoil = ',unique(ggplot_data$hasFoil),' 
  ORDER BY id, Date',sep = "")
        performant_alt_mkt_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(-tcg_id,-uuid) %>% rename(tcg_id = tcg_id_1)
        
        if(nrow(performant_alt_mkt_tbl) == 0){bl_review_tbl = bl_review_tbl %>% filter( (tcg_id != unique(performant$tcg_id)) & (hasFoil != unique(performant$hasFoil)));performant = "Unknown"}
        
        if(length(performant)>1){print("All Error Handling Completed");break}
        
      }
      
      ggplot_aux_mkt_data = performant_alt_mkt_tbl %>% 
        mutate(tcg_id = as.numeric(tcg_id)) %>%
        group_by(Date,tcg_id,card,set,rarity,number,hasFoil) %>%
        summarize(all_others_mkt = round(mean(mkt_value,na.rm=T),-1)) %>%
        ungroup() %>%
        arrange(Date)
      
      con <- gaeas_cradle(my_secrets$patches)
      
      statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'"  
                   AND a.number = ',unique(ggplot_data$number),' 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
      mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
      
      if(nrow(mtg_basket_tbl)==0){
        statement <- paste('SELECT date,tcg_id,version,condition,language,listing_type,sold_quantity,sell_price FROM `gaeas-cradle.mtg_basket.*` a 
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',(30 + table_limitations$days_behind_today ) ,' DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY)) AND
                  card_name like "',unique(ggplot_data$card),'"  
                   AND a.set like "',unique(ggplot_data$set),'" 
                   AND a.version = ',unique(ggplot_data$hasFoil),' 
  ORDER BY Date',sep = "")
        mtg_basket_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
      }
      
      cleansed_basket_tbl = mtg_basket_tbl %>% 
        filter(language == "English") %>%
        mutate(listing_type = ifelse(listing_type == 1, "Photo","")) %>%
        group_by(date,tcg_id,version) %>%
        summarize(conditions = paste(condition, collapse = ", "),
                  languages = paste(language, collapse = ", "),
                  listings_sold = paste(listing_type, collapse = ", "),
                  sold_quantity = sum(sold_quantity),
                  sell_price = round(mean(sell_price),0)) %>%
        ungroup()
      
      
      
      gg_data = ggplot_data %>%
        full_join(ggplot_aux_bl_data) %>%
        full_join(ggplot_aux_mkt_data) %>%
        full_join(cleansed_basket_tbl %>% select(tcg_id,version,date,sold_quantity,sell_price) %>% rename(hasFoil=version), by=c("tcg_id"="tcg_id","Date"="date","hasFoil"="hasFoil")) %>%
        rename(har_offer = offer, tcg_low = mkt_value) %>%
        clean_names() %>%
        select(-ct) %>%
        filter(!is.na(vendor)) %>%
        arrange(date) %>%
        distinct() %>%
        fill(mkt_vendor) %>%
        replace(is.na(.),0)
      
      return(list(gg_data,all_content[[2]],all_content[[3]],all_content[[4]]))
      
    }else
      if((day_in_month > 8)&(day_in_month<13)){
        check = 1
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                    FROM (
                        SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                        FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                    )
                    ',sep = "")
        table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
        
        
        con <- gaeas_cradle(my_secrets$patches)
        statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                            FROM `gaeas-cradle.mtg_basket.*` a 
                            WHERE _TABLE_SUFFIX BETWEEN
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                            FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                            ORDER BY Date',sep = "")
        mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
        
        basket_copies_sold_tbl = basket_copies_pre_preparations(mtg_basket_all_tbl)
        
        gg_data = post_logic(basket_copies_sold_tbl)
        
        while( (max(gg_data[[1]]$change,na.rm=T)/unique(gg_data[[1]]$change_sum,na.rm=T) >= .65) | (unique(gg_data[[1]]$avg_offer)<=5) ){
          if(check == 1){
            refined_basket_sale_price = basket_sale_price_tbl %>% 
              filter( tcg_id != unique(gg_data[[1]]$tcg_id)  )
            
            gg_data = post_logic(refined_basket_sale_price)
            
            check = check + 1
          }else{
            refined_basket_sale_price = refined_basket_sale_price %>% filter( (tcg_id !=  unique(gg_data[[1]]$tcg_id)) )
            
            gg_data = post_logic(refined_basket_sale_price)
            
            check = check + 1
          }
        }
        
        if(unique(gg_data[[1]]$card)>1){
          
          card_selection = gg_data[[1]] %>% filter(avg_sell_price == max(avg_sell_price))
          
          gg_data[[1]] = gg_data[[1]] %>% filter(card %in% card_selection$card)
        }
        
        return(gg_data)
        
      }else
        if((day_in_month == 13)){
          post_logic()
        }else
          if((day_in_month > 13)&(day_in_month<=17)){
            check = 1
            con <- gaeas_cradle(my_secrets$patches)
            statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                      FROM (
                          SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                          FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                      )
            ',sep = "")
            table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
            
            
            con <- gaeas_cradle(my_secrets$patches)
            statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                              FROM `gaeas-cradle.mtg_basket.*` a 
                              WHERE _TABLE_SUFFIX BETWEEN
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                              ORDER BY Date',sep = "")
            mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
            
            basket_sale_price_tbl = basket_sale_pre_preparations(mtg_basket_all_tbl)
            #data = basket_sale_price_tbl
            gg_data = post_logic(basket_sale_price_tbl)
            
            
            if(unique(gg_data[[1]]$card)>1){
              
              card_selection = gg_data[[1]] %>% filter(avg_offer == max(avg_offer))
              
              gg_data[[1]] = gg_data[[1]] %>% filter(card %in% card_selection$card)
            }
            
            
            return(gg_data)
          }else
            if((day_in_month > 17)&(day_in_month<=21)){
              gs4_auth(email=my_secrets$og_patches, use_oob = T)
              drive_auth(email=my_secrets$og_patches, use_oob = T)
              ss <- drive_get("Decklists For Ban")
              decklist_data = read_sheet(ss =ss,
                                         sheet = 'all')
              
              gg_data = post_logic(decklist_data)
              
            }else
              if((day_in_month > 21)&(day_in_month<=23)){
                con <- gaeas_cradle(my_secrets$patches)
                statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                      FROM (
                          SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                          FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                      )
                            ',sep = "")
                table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
                
                
                con <- gaeas_cradle(my_secrets$patches)
                statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                              FROM `gaeas-cradle.mtg_basket.*` a 
                              WHERE _TABLE_SUFFIX BETWEEN
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                              ORDER BY Date',sep = "")
                mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
                
                basket_sealed_sale_price_tbl = basket_sealed_sale_pre_preparations(mtg_basket_all_tbl)
                
                gg_data = post_logic(basket_sealed_sale_price_tbl)
                
                return(gg_data)
              }else
                if((day_in_month > 23)&(day_in_month<=25)){
                  con <- gaeas_cradle(my_secrets$patches)
                  statement <- paste('SELECT most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                      FROM (
                          SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table
                          FROM `gaeas-cradle`.mtg_basket.INFORMATION_SCHEMA.TABLES
                      )
                      ',sep = "")
                  table_limitations <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble()
                  
                  con <- gaeas_cradle(my_secrets$patches)
                  statement <- paste('SELECT date, tcg_id, Card_name card,a.set,rarity,number,version hasFoil,condition,a.language,listing_type, sold_quantity,dop,sell_price
                              FROM `gaeas-cradle.mtg_basket.*` a 
                              WHERE _TABLE_SUFFIX BETWEEN
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',30 + table_limitations$days_behind_today ,' DAY)) AND
                              FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',table_limitations$days_behind_today ,' DAY))  
                              ORDER BY Date',sep = "")
                  mtg_basket_all_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()
                  
                  basket_sealed_copies_sold = basket_sealed_sale_pre_preparations(mtg_basket_all_tbl)
                  
                  gg_data = post_logic(basket_sealed_copies_sold)
                  
                  return(gg_data)
                }else
                  if((day_in_month > 25)&(day_in_month<=26)){
                    
                    gg_data = post_logic(card_kingdom_buylist_review())
                    
                    return(gg_data)
                  } else 
                    if((day_in_month > 26)&(day_in_month<=31)){
                    
                    secret_lair = secret_lair_data()
                    gg_data = post_logic(secret_lair)
                    
                    return(gg_data)
                  }
  

}

#Pulls based off the day of the month from 30 content templates
todays_content = database_pull()

tryCatch({ggsave(filename = "/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_media.png",dpi = 320, width=3600,height=2400,units="px", device = "png", plot = todays_content[[3]])},
         error = function(e){ggsave(filename = "/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_media.png",dpi = 320, width=3600,height=2400,units="px", device = "png", plot = todays_content[[3]] %>% image_ggplot())})


# Let's Make a Post (finally) after all that selection work ---------------
# Send an email to my personal gmail to confirm that I do find value in todays content generation, and approve the twitter_bot_poster to post it on my behalf.
gm_auth_configure(path = "/home/cujo253/mines_of_moria/Essential_Referential_CSVS/gmail_ids.json",use_oob=T)

gm_auth(email = patches$patches, use_oob = T)

my_email = gm_mime() %>%
  gm_to(patches$patches) %>%
  gm_from(patches$patches) %>%
  gm_subject(paste0(Sys.Date(),": Twitter Content")) %>%
  gm_text_body(paste0(todays_content[[4]]," 

Shall I post this for you, wolf?" )) %>% 
  gm_attach_file("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/twitter_media.png")

gm_send_message(my_email)




