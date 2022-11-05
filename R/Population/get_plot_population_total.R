get_plot_population_total <- function(){
  #-----------------------------#
  # >> helper << #
  #-----------------------------#
  get_population_table <- function(filename) {
    filename %>% 
      str_c(., ".csv") %>% 
      str_c(
        source_url_main, 
        "Population", 
        "Table", 
        .,
        sep = "/"
      )
  }
  
  linetypes <- c(
    `低位` = "dot", 
    `中位` = "solid", 
    `高位` = "dash"
  )
  colors <- c(
    `低位` = red_main, 
    `中位` = blue_dark, 
    `高位` = green_main
  )
  
  
  #-----------------------------#
  # >> data << #
  #-----------------------------#
  dt_total_00 <- 
    get_population_table("total") %>% 
    read_csv(
      col_types = cols(
        id = "d", 
        value = "d",
        .default = "c"
      )
    )
  
  dt_total_01 <- 
    dt_total_00 %>% 
    mutate(
      year = `時間軸(調査年)` %>% 
        str_remove_all("年") %>% 
        as.numeric(),
      value_million = value / 100000000
    ) %>% 
    select(year, matches("value"))
  
  
  dt_projection_00 <- 
    get_population_table("total_projection") %>% 
    read_csv(
      col_types = cols(
        year = "d", 
        value = "d",
        .default = "c"
      )
    )
  
  dt_projection_01 <- 
    dt_projection_00 %>% 
    mutate(
      `死亡` = `死亡` %>% 
        fct_inorder(),
      `出生` = `出生` %>% 
        fct_inorder(),
      value_million = value / 100000000,
    )
  
  
  #-----------------------------#
  # >> plot << #
  #-----------------------------#
  #:[ base plot ]:#
  p0 <- 
    dt_total_01 %>% 
    plot_ly(
      type = "bar",
      x = ~ year,
      y = ~ value_million,
      marker = list(
        color = blue_main
      ),
      name = "国勢調査",
      showlegend = F
    )
  
  #:[ add traces ]:#
  add_projection <- function(ob_plotly, death = "低位", birth = "低位", ...){
    dt <- 
      dt_projection_01 %>% 
      filter(
        `死亡` == death, 
        `出生` == birth
      )
    
    label <- str_c("出生", birth, "&", "死亡", death)
    
    pl <- 
      ob_plotly %>% 
      add_lines(
        data = dt,
        x = ~ year,
        y = ~ value_million,
        mode = "lines",
        name = label,
        line = list(
          width = 3,
          color = colors[birth],
          dash = linetypes[death]
        ),
        inherit = F,
        ...
      )
    
    pl
  }
  
  p1 <- p0
  
  for (x in c("低位", "中位", "高位")) {
    for (y in c("低位", "中位", "高位")) {
      p1 <- 
        p1 %>% 
        add_projection(
          death = y, birth = x,
          legendgroup = x
        )
    }
  }
  
  #:[ layout ]:#
  p2 <- 
    p1 %>% 
    layout(
      paper_bgcolor = graph_background_color,
      plot_bgcolor = graph_background_color,
      font = list(
        family = "Arial", 
        size = 16, 
        color = graph_text_color
      ),
      legend = list(
        title = list(
          side = "top",
          text = "人口推計の仮定",
          font = list(
            size = 16
          )
        ),
        groupclick = "toggleitem",
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 100
      ),
      xaxis = list(
        title = list(
          text = NULL
        ),
        tick0 = 1920, 
        dtick = 20,
        ticklen = 10,
        tickcolor = "white"
      ),
      yaxis = list(
        title = list(
          text = "億人",
          font = list(
            size = 14
          )
        ),
        tickfont = list(
          size = 14
        ),
        ticklen = 5,
        tickcolor = "white",
        tickformat = ".2f",
        hoverformat = ".3f",
        gridcolor = graph_axis_color,
        gridwidth = 1.2,
        zerolinecolor = graph_text_color,
        zerolinewidth = 2
      ),
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = graph_background_color,
        bordercolor = graph_axis_color,
        font = list(
          color = graph_text_color,
          size = 12
        )
      )
    )
  
  #:[ config ]:#
  p3 <- 
    p2 %>% 
    config(
      scrollZoom = F,
      displayModeBar = T,
      displaylogo = F,
      toImageButtonOptions = list(
        filename = "image",
        format = "png",
        width = 1.5*600,
        height = 0.75*600,
        scale = 5
      )
    )
  
  #:[ return ]:#
  p3
}
