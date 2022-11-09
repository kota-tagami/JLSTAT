get_plot_pyramid_total <- function(){
  
  ## 出生中位(死亡中位)推計
  
  ## 2000年までは85歳以上でまとめられているが
  ## 85-89歳のカテゴリーにしている。
  
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
  
  ## 男性と女性で同じ色の場合
  # "年少人口" = "#03af7a",
  # "生産年齢人口" = "#4dc4ff",
  # "老年人口" = "#f6aa00"
  
  ## 男性用
  # "年少人口" = "#039669",
  # "生産年齢人口" = "#34bcff",
  # "老年人口" = "#dd9800"
  
  ## 女性用
  # "年少人口" = "#03c88b",
  # "生産年齢人口" = "#67ccff",
  # "老年人口" = "#ffb511"
  
  colors <- list(
    "男性" = list(
      "年少人口" = "#03af7a",
      "生産年齢人口" = "#4dc4ff",
      "老年人口" = "#f6aa00"
    ),
    "女性" = list(
      "年少人口" = "#03af7a",
      "生産年齢人口" = "#4dc4ff",
      "老年人口" = "#f6aa00"
    )
  )
  
  #-----------------------------#
  # >> data << #
  #-----------------------------#
  
  #:[ 国勢調査 ]:#
  dt_total_00 <-
    get_population_table("pyramid_total") %>%
    read_csv(
      col_types = cols(
        year = "d", value = "d",
        .default = "c"
      )
    ) %>%
    mutate(
      type = "国勢調査"
    )
  
  #:[ 人口推計 ]:#
  dt_projection_00 <-
    get_population_table("pyramid_total_projection") %>%
    read_csv(
      col_types = cols(
        year = "d", value = "d",
        .default = "c"
      )
    ) %>%
    mutate(
      type = "人口推計"
    )
  
  #:[ 結合 ]:#
  dt_all_00 <-
    bind_rows(dt_total_00, dt_projection_00)
  
  dt_all_01 <-
    dt_all_00 %>%
    filter(
      !(type == "人口推計" &
          (year >= 2015 & year <= 2020)),
    )
  
  year_age_sex <-
    expand_grid(
      year = unique(dt_all_01$year),
      age = unique(dt_all_01$age),
      sex = unique(dt_all_01$sex),
    )
  
  dt_all_02 <-
    year_age_sex %>%
    left_join(dt_all_01, by = c("year", "age", "sex")) %>%
    mutate(
      across(where(is.numeric), ~ if_else(is.na(.), 0, .)),
      across(where(is.character), fct_inorder),
    ) %>%
    arrange(year, sex, age)
  
  dt_all_03 <-
    dt_all_02 %>%
    mutate(
      age_group = age %>%
        fct_collapse(
          "年少人口" = c(
            "0~4歳", "5~9歳", "10~14歳"
          ),
          "老年人口" = c(
            "65~69歳", "70~74歳", "75~79歳",
            "80~84歳", "85~89歳", "90~94歳",
            "95~99歳", "100歳以上"
          ),
          other_level = "生産年齢人口"
        ),
      age_group = age_group %>%
        fct_relevel("年少人口", "生産年齢人口", "老年人口"),
      value = value / 1000000,
      value_plot = if_else(
        sex == "男性", value * -1, value
      )
    ) %>% 
    select(-value)
  
  
  #-----------------------------#
  # >> plot << #
  #-----------------------------#
  #:[ base plot ]:#
  p0 <-
    plot_ly()
  
  #:[ add traces ]:#
  add_bars_colored <- function(ob_plotly,
                               Sex = "男性",
                               Age_group = "年少人口") {
    clrs <- colors[[Sex]][[Age_group]]
    
    dt <-
      dt_all_03 %>%
      filter(
        `sex` == Sex,
        `age_group` == Age_group
      ) %>% 
      mutate(
        text = str_c(age_group, sex, sep = " ")
      )
    
    if(Sex == "男性") {
      lgnd <- str_c("（左側）")
    } else {
      lgnd <- str_c("（右側）")
    }
    
    pl <-
      ob_plotly %>%
      add_bars(
        data = dt,
        x = ~value_plot,
        y = ~age,
        frame = ~year,
        base = 0,
        name = str_c(Sex, Age_group, sep = " "),
        legendgroup = Sex,
        legendgrouptitle = list(
          text = lgnd,
          font = pl_layout_font()
        ),
        marker = list(
          color = clrs
        ),
        hovertext = NULL,
        hovertemplate = paste(
          "%{y}: %{x:.2f}百万人"
        ),
        inherit = F
      )
    
    pl
  }
  
  p1 <- p0
  
  for (x in c("男性", "女性")) {
    for (y in c("年少人口", "生産年齢人口", "老年人口")) {
      p1 <-
        p1 %>%
        add_bars_colored(Sex = x, Age_group = y)
    }
  }
  
  #:[ layout ]:#
  p2 <-
    p1 %>%
    layout(
      pl_layout_base,
      bargap = 0.15,
      barmode = "overlay",
      xaxis = list(
        range = list(-5.5, 5.5),
        tickvals = seq(-5, 5, 1),
        ticktext = c(
          "5", "4", "3", "2", "1", "0", 
          "1", "2", "3", "4", "5"
        ),
        tickmode = "array",
        tickfont = pl_layout_font(size = 14),
        title = list(
          text = "人口（百万人）",
          standoff = 1,
          font = pl_layout_font(size = 14)
        ),
        zeroline = T,
        zerolinecolor = graph_text_color,
        gridcolor = graph_axis_color,
        gridwidth = 1.2
      ),
      yaxis = list(
        tickfont = pl_layout_font(size = 14),
        title = list(
          text = NULL
        ),
        linewidth = 0
      ),
      legend = list(
        groupclick = "toggleitem",
        orientation = "h",
        font = pl_layout_font(),
        x = 0.5,
        xanchor = "center",
        y = 1,
        yanchor = "bottom"
      ),
      hoverlabel = list(
        bgcolor = graph_background_color,
        bordercolor = graph_axis_color,
        font = pl_layout_font(size = 12)
      )
    )
  
  #:[ animation ]:#
  p3 <-
    p2 %>%
    animation_opts(
      frame = 800,
      transition = 500,
      easing = "linear"
    ) %>% 
    animation_slider(
      currentvalue = list(
        prefix = NULL,
        suffix = " 年",
        font = pl_layout_font(size = 20),
        xanchor = "center"
      ),
      font = pl_layout_font(size = 14),
      bgcolor = graph_axis_color,
      bordercolor = graph_axis_color,
      tickcolor = graph_text_color
    ) %>% 
    animation_button(
      font = pl_layout_font(),
      bgcolor = graph_axis_color,
      bordercolor = graph_axis_color
    )
  
  #:[ config ]:#
  p4 <- 
    p3 %>% 
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
  p4
}