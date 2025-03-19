library(data.table)
library(plotly)
library(shiny)
library(reshape2)
library(shinyWidgets)
library(DT)
library(bslib)
library(bsplus)
library(shinytreeview)

cvi_colours = list(
  cvi_purples = c("#381532", "#4b1b42", "#5d2252", "#702963",
                  "#833074", "#953784", "#a83e95"),
  my_favourite_colours = c("#702963", "#637029",    "#296370"),
  ul = c("#E03127", "#e1e1e2", "#c4c4c5", 
         "#a8a8a9", "#8c8d8e", "#717274", 
         "#58595b", "#494a4c", "#3b3c3d",
         "#2d2e2f", "#202121", "#141414")
)

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

scale_fill_cvi_d = function(name) {
  ggplot2::scale_fill_manual(values = cvi_palettes(name,
                                                   type = "discrete"))
}

si <- list(
  "sEmptyTable" = "Ni podatkov",
  "sInfo" = "_START_-_END_ od _TOTAL_ zapisov",
  "sInfoEmpty" = "", # mogoče raj ni zapisov
  "sInfoFiltered" = "(filtrirano od _MAX_ vseh zapisov)",
  "sInfoPostFix" = "",
  "sInfoThousands" = ",",
  "sLengthMenu" = "Prikaži _MENU_",
  "sLoadingRecords" = "Nalagam...",
  "sProcessing" = "Obdelujem...",
  "sSearch" = '<i class="fas fa-search" aria-hidden="true"></i>',
  "sZeroRecords" = "Noben zapis ne ustreza",
  "oPaginate" = list(
    "sFirst" = "Prvi", 
    "sLast" = "Zadnji",
    "sNext" = "Nasl.",
    "sPrevious" = "Prej."
  ),
  "oAria" = list(
    "sSortAscending" = ": vključite za naraščujoči sort",
    "sSortDescending" = ": vključite za padajoči sort"
  )
)

dat <- read.csv2("data_final.csv", stringsAsFactors = TRUE)


setDT(dat)


plotVec <- c("Članica" = "Kratica", 
             "Spol" = "Spol", 
             "Stopnja" = "Stopnja", 
             "Način" = "Način", 
             "Letnik" = "Letnik")

tabVec <- c("Članica" = "Članica",
             "Spol" = "Spol", 
            "Stopnja" = "Stopnja", 
            "Način" = "Način", 
            "Letnik" = "Letnik", 
             "Državljanstvo" = "Državljanstvo")

letnikVec <- c("Prvi" = "01", "Drugi" = "02", 
               "Tretji" = "03", "Četrti" = "04",
               "Peti" = "05", "Šesti"= "06", 
               "Absolventi" = "0A")

ui <- navbarPage(#includeScript("code.js"),
                 theme = bs_theme(version = 4, bootswatch = 'minty',
                                  primary = "#58595b", secondary = "#E03127",
                                  info = "#E03127"),
                 title = "Vpisani študentje 23/24",
                 
                 tags$head(tags$style(HTML("
                            a {
                              color: #E03127;
                            }
                            
                            .pagination {
                              --bs-pagination-disabled-bg: #babbbc;
                              --bs-pagination-disabled-border-color: #babbbc;
                            }
                            
                            .page-item.disabled .page-link {
                              background-color: #babbbc;
                              border-color: #babbbc;
                            }

                                                        "))),
                 #title = "Podatki o vpisanih študentih 2023/2024",
                 nav_panel("Tabela",
                          fluidRow(
                            column(4,
                          wellPanel(
                            "Pri vsaki spremenljivki izberi kategorije, za katere te zanima število vpisanih študentov. Če ne izbereš nobene
                            kategorije se število vpisanih ne bo razčlenilo glede na kategorije te spremenljivke.",
                            div(style = "height:20px"),
                            #tags$head(tags$script(js)),
                            bs_accordion(
                              id = "accordion"
                            )  |> 
                              bs_set_opts(
                                use_heading_link = TRUE
                              ) |> 
                              bs_append(
                                title = "Članica",
                                content = pickerInput(
                                  inputId = "Članica", 
                                  label = "Izberi članico/e:",
                                  choices = as.vector(unique(dat$Članica)[order(unique(dat$Članica))]),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = "Akademija za glasbo",
                                  multiple = TRUE
                                )
                              ) |> 
                              bs_append(
                                title = "Program in členitev",
                                content = checkboxGroupInput(
                                  inputId = "PČ",
                                  label = "Razčleni po:",
                                  choices = c("Program", "Členitev")
                                  )
                              # bs_append(
                              #   title = "Članica, program, členitev",
                              #   content = treeInput(
                              #     inputId = "PČ",
                              #     label = "Izberi:",
                              #     closeDepth = 0,
                              #     choices = create_tree(
                              #       dat, c("Članica", "Program", "Členitev")
                              #     ),
                              #     width = "100%"
                              #   )
                              ) |> 
                              bs_append(
                                title = "Način",
                                content = pickerInput(
                                  inputId = "Način", label = "Izberi način  študija:",
                                  choices = as.vector(unique(dat$`Način`)),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = "REDNI",
                                  multiple = TRUE
                                )
                              ) |> 
                              bs_append(
                                title = "Stopnja",
                                content = pickerInput(
                                  inputId = "Stopnja", label = "Izberi stopnjo študijskega programa:",
                                  choices = as.vector(unique(dat$Stopnja)),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = "prva stopnja",
                                  multiple = TRUE
                                )
                              ) |> 
                              bs_append(
                                title = "Letnik",
                                content = pickerInput(
                                  inputId = "Letnik", 
                                  label = "Izberi Letnik študentov_k:",
                                  choices = unique(dat$Letnik)[c(1, 3, 2, 5, 6, 7, 4)],
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = NULL,
                                  multiple = TRUE
                                )
                              ) |> 
                              bs_append(
                                title = "Spol",
                                content = pickerInput(
                                  inputId = "Spol", label = "Izberi spol študentov_k:",
                                  choices = unique(dat$Spol),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = "Ž",
                                  multiple = TRUE
                                )
                              ) |> 
                              bs_append(
                                title = "Državljanstvo",
                                content = pickerInput(
                                  inputId = "Državljanstvo", label = "Izberi državljanstvo študentov_k:",
                                  choices = as.vector(unique(dat$Državljanstvo)[order(unique(dat$Državljanstvo))]),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Ne izberi nobene možnosti",
                                                 `select-all-text` = "Izberi Vse",
                                                 `none-selected-text` = "Skupaj",
                                                 `all-selected-text` = "Vse"
                                                 ),
                                  selected = "SLOVENIJA",
                                  multiple = TRUE
                                )
                              ) 
                            
                          )),
                          column(8, mainPanel(DT::dataTableOutput("mytable1"))),
                          verbatimTextOutput("lala")
                 )),
                 
                 nav_panel("Graf",
                          wellPanel(
                            includeScript("js4checkbox.js"),
                            fluidRow(
                              column(6, radioButtons("var1", label="Izberi osnovno kategorično spremenljivko za izris grafa (x os).",
                                               choices = plotVec,
                                               selected = "Kratica"
                            )),
                            column(6, checkboxGroupInput("var2", label="Opcijsko: Izberi dodatno kategorično spremenljivko za izris grafa (legenda - barve).",
                                               choices = setdiff(plotVec, "Kratica"),
                                               selected = NULL
                            ))
                          )),
                          (plotly::plotlyOutput("mygraph1"))
                 ), fluid = F)


server <- function(input, output){
  
  output$mytable1 <- DT::renderDataTable({
    
    by_vars <- vector()
    fltrs <- setNames(vector("list", length(tabVec)),
                       names(tabVec))

    for (var_nm in names(tabVec)){

      var_val <- input[[var_nm]]

      if (!is.null(var_val) && any(var_val == "Vse")){
        
        by_vars <- c(by_vars, tabVec[[var_nm]])
        
      } else if ((!is.null(var_val) && any(var_val == "Skupaj")) || (is.null(var_val))){

        fltrs[[var_nm]] <- as.vector(unique(dat[[tabVec[[var_nm]]]]))
        
      } else {
        
        by_vars <- c(by_vars, tabVec[[var_nm]])
        fltrs[[var_nm]] <- var_val
        
      }

    }
    

    if (!is.null(input$PČ)){
      by_vars <- c(by_vars, input$PČ)
    } 

    DT::datatable(dat[(Članica %in% c(fltrs$Članica, fltrs$PČ) |
                        Program %in% fltrs$Program |
                        Členitev %in% fltrs$Členitev) &
                        Spol %in% fltrs$Spol &
                        Stopnja %in% fltrs$Stopnja &
                        Način %in% fltrs$Način &
                        Letnik %in% fltrs$Letnik &
                        Državljanstvo %in% fltrs$Državljanstvo, 
                      list("Št. študentov" = sum(UTEZ)), 
                      by=by_vars],
                  options = list(
                    language = si
                  ))
  })
  
  output$mygraph1 <- renderPlotly({
    
    dat_plot <- copy(dat[, sum(UTEZ), by=c(input$var1, input$var2)])
    #dat_plot <- copy(dat)
    
    x <- sym(input$var1)
    x_lab <- names(plotVec)[plotVec == input$var1]
    if (!is.null(input$var2)){
      gr <- sym(input$var2)
      gr_lab <- names(plotVec)[plotVec == input$var2]
    } else {
      gr <- input$var2
      gr_lab <- NULL
    }
    
    
    setDF(dat_plot)
    
    if ("Stopnja" %in% colnames(dat_plot)){
      
      dat_plot$Stopnja <- factor(dat_plot$Stopnja,
                                         unique(dat$Stopnja))
      
    } 
    
    if ("Letnik" %in% colnames(dat_plot)){
      
      dat_plot$Letnik <- factor(dat_plot$Letnik,
                                names(letnikVec))
      
      
    }
    
    #lapply(input$spremenljivke, function(x) as.factor(dat_plot[x]))
    p <- ggplot(dat_plot, aes(x=!!x, y=V1, fill=!!gr)) + 
      geom_bar(stat="identity", position=position_dodge(), colour="black") + 
      theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
      ylab("število vpisanih študentov") +
      xlab(x_lab) + 
      labs(fill=gr_lab) +
      scale_fill_cvi_d("ul")
      
    ggplotly(p) 
    
  }) 
  
  output$lala <- renderText(input$PČ)
  
  
  
}


shinyApp(ui = ui, server = server)
  
