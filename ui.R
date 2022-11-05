#-----------------------------#
# >> UI headers << #
#-----------------------------#
ui_headers <- 
  dashboardHeader(
    title = h4("JLSTAT"),
    titleWidth = "thin",
    show_menu_button = F,
    color = "blue",
    inverted = T,
    center = h2(
      "Japan Labor Statistics", 
      `style` = "color: white;"
    )
  )


#-----------------------------#
# >> UI sidebars << #
#-----------------------------# 

#:[ functions ]:#
make_subitems <- function(items, category) {
  dt <- 
    items %>% 
    enframe(
      name = "tab_name", 
      value = "tab_text"
    ) %>% 
    mutate(
      tab_name = str_c(category, tab_name, sep = "_")
    )
  
  subitems <- 
    dt %>% 
    pmap(function(tab_name, tab_text) {
      menuSubItem(
        tabName = tab_name,
        text = tab_text
      )
    })
  
  subitems
}


#:[ 人口タブ ]:#
items_population <- c(
  total = "総人口",
  pyramid = "人口ピラミッド",
  birth = "出生",
  death = "死亡"
)

tabs_population <- 
  make_subitems(items_population, "population")


#:[ 労働力タブ ]:#
items_workers <- c(
  total = "総労働力",
  employment = "雇用"
)

tabs_workers <- 
  make_subitems(items_workers, "workers")


#:[ all sidebars ]:#
ui_sidebars <- 
  dashboardSidebar(
    size = "thin",
    color = "blue",
    inverted = T,
    sidebarMenu(
      menuItem(
        tabName = "home", 
        text = "ホーム", 
        icon = icon("home")
      ),
      menuItem(
        tabName = "population",
        text = "人口",
        tabs_population
      ),
      menuItem(
        tabName = "workers", 
        text = "労働力",
        tabs_workers
      )
    )
  )


#-----------------------------#
# >> UI bodies << #
#-----------------------------#

#:[ ホームタブ ]:#
make_box_desc <- function(...) {
  box(
    collapsible = F,
    title_side = "top left",
    color = "grey",
    ...
  )
}

bd_tab_home <- function() {
  box_desc_total <- make_box_desc(
    title = "JLSTATとは",
    ribbon = F,
    width = 16,
    paste0(
      "日本の労働市場の統計をダッシュボード化する。"
    )
  )
  
  box_desc_population <- make_box_desc(
    title = "人口",
    width = 8,
    paste0(
      "人口タブでは・・・・。"
    )
  )
  
  box_desc_workers <- make_box_desc(
    title = "労働力",
    width = 8,
    paste0(
      "労働力タブでは・・・・。"
    )
  )
  
  bd_tab_home <- tabItem(
    tabName = "home",
    fluidRow(box_desc_total),
    fluidRow(
      box_desc_population,
      box_desc_workers
    )
  )
  
  bd_tab_home
}


#:[ 人口タブ ]:#
bd_tab_population <- function() {
  tab_total <- tabItem(
    tabName = "population_total",
    column(
      width = 16,
      
      header(
        title = "総人口のトレンド", 
        description = "1920年～2065年",
        icon = "chart bar"
      ),

      box(
        title = "解説",
        color = "blue",
        div(
          p(str_c(read_lines("R/Population/desc_population_total.txt"), collapse = "")),
          `style` = "font-size: 18px;"
        )
      ),
      box(
        title = "図",
        color = "blue",
        segment(
          class = "raised segment",
          plotlyOutput(
            outputId = "population_total",
            width = "100%",
            height = "550px"
          )
        ),
        segment(
          class = "raised segment",
          div(
            p(read_lines("R/Population/cap_population_total.txt")),
            p("注：棒は観測値，線は推計値．"),
            `style` = "font-size: 10px;"
          )
        )
      )
    )
  )
  
  tab_pyramid <- tabItem(
    tabName = "population_pyramid",
    column(
      width = 16,
      box(title = "解説"),
      box(plotOutput("plot2"), title = "図")
    )
  )
  
  bd_tab_population <- list(
    tab_total,
    tab_pyramid
  )
  
  bd_tab_population
}


#:[ all bodies ]:#
ui_bodies <- 
  dashboardBody(
    color = "blue",
    inverted = T,
    tabItems(
      bd_tab_home(),
      bd_tab_population(),
      tabItem(
        tabName = "workers",
      )
    )
  )


#-----------------------------#
# >> UI combinded << #
#-----------------------------#
ui <- dashboardPage(
  header = ui_headers,
  sidebar = ui_sidebars,
  body = ui_bodies,
  theme = "readable"
)

