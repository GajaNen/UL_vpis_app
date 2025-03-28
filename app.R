library(data.table)
library(plotly)
library(shiny)
library(reshape2)
library(shinyWidgets)
library(DT)
library(bslib)
library(bsplus)
library(shinytreeview)

js <- HTML("
$(function() {
  let observer = new MutationObserver(callback);

  function clickHandler(evt) {
    Shiny.setInputValue('group_select', $(this).children('span').text());
  }

  function callback(mutations) {
    for (let mutation of mutations) {
      if (mutation.type === 'childList') {
        $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');
        
      }
    }
  }

  let options = {
    childList: true,
  };

  observer.observe($('.inner')[0], options);
})
")

cvi_colours = list(
  ul = c("#E03127", "#e1e1e2", "#c4c4c5", 
         "#a8a8a9", "#8c8d8e", "#717274", 
         "#58595b", "#494a4c", "#3b3c3d",
         "#2d2e2f", "#202121", "#141414")
)

cvi_palettes = function(name, 
                        n, 
                        all_palettes = cvi_colours, 
                        type = c("discrete", "continuous")
                        ) {
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
  "sLengthMenu" = "Prikaži _MENU_ zapisov",
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

tabVec <- c("Članica", "Program", "Členitev", "Spol", 
            "Stopnja", "Način", "Letnik", "Državljanstvo")

letnikVec <- c("Prvi" = "01", "Drugi" = "02", 
               "Tretji" = "03", "Četrti" = "04",
               "Peti" = "05", "Šesti"= "06", 
               "Absolventi" = "0A")

# a to dejansko prekodira ravni glede na podan vrstni red???


###########################

ui <- navbarPage(
                 theme = bs_theme(version = 4, bootswatch = 'minty',
                                  primary = "#58595b", secondary = "#E03127",
                                  info = "#E03127"),
                 
                 title = div(img(src="UL_logo-RGB_barv.png", 
                                 height="100px"), 
                             "Vpisani študentje 23/24"),
                 
                 tags$head(tags$style(HTML("
                 
                            body {
                              font-family: Arial;
                           }
                            
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
                            
                                                        "
                                           )
                                      )
                           ),

                 nav_panel("Tabela",
                           #includeScript("jsSelectAllSubcat.js"),
                          fixedRow(
                            column(12,
                          wellPanel(
                            fluidRow(
                              column(
                                6,
                                bs_accordion(
                                  id = "accordion_left"
                                )  |> 
                                  bs_set_opts(
                                    use_heading_link = TRUE
                                  ) |> 
                                  bs_append(
                                    title = "Članica",
                                    content = pickerInput(
                                      inputId = "Članica", 
                                      label = "Izberi članico/e:",
                                      choices = sort(unique(dat$Članica)),
                                      options = list(`actions-box` = TRUE,
                                                     liveSearch = TRUE,
                                                     liveSearchStyle = "contains",
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
                                    tags$script(js),
                                    title = "Program",
                                    content = conditionalPanel(
                                      condition = "output.Prog == true",
                                      pickerInput(
                                        inputId = "Program", 
                                        label = "Izberi program/e:",
                                        choices = list(),
                                        options = list(`actions-box` = TRUE,
                                                       `liveSearch` = TRUE,
                                                       `deselect-all-text` = "Ne izberi nobene možnosti",
                                                       `select-all-text` = "Izberi Vse",
                                                       `none-selected-text` = "Skupaj",
                                                       `all-selected-text` = "Vse"
                                        ),
                                        selected = NULL,
                                        multiple = TRUE
                                      )
                                    )
                                  ) |>
                                  bs_append(
                                    title = "Členitev",
                                    content = conditionalPanel(
                                      condition = "output.Clen == true",
                                      pickerInput(
                                        inputId = "Členitev", 
                                        label = "Izberi členitev/e:",
                                        choices = list(),
                                        options = list(`actions-box` = TRUE,
                                                       `deselect-all-text` = "Ne izberi nobene možnosti",
                                                       `select-all-text` = "Izberi Vse",
                                                       `none-selected-text` = "Skupaj",
                                                       `all-selected-text` = "Vse"
                                        ),
                                        selected = NULL,
                                        multiple = TRUE
                                      )
                                    )
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
                                  )
                                ),
                                column(
                                  6,
                                  bs_accordion(
                                    id = "accordion_right"
                                  )  |> 
                                    bs_set_opts(
                                      use_heading_link = TRUE
                                    ) |> 
                                  bs_append(
                                    title = "Stopnja",
                                    content = pickerInput(
                                      inputId = "Stopnja", label = "Izberi stopnjo študijskega programa:",
                                      choices = unique(sort(dat$Stopnja)),
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
                                      choices = unique(sort(dat$Letnik)),
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
                                )
                              )
                            
                          ))),
                          div(style = "height:50px"),
                          fluidRow(
                          column(12, 
                                 div(DT::dataTableOutput("mytable1"),
                          style = 'horizontal-align: middle;')
                 ))),
                 
                 nav_panel("Graf",
                          wellPanel(
                            includeScript("js4checkbox.js"),
                            fluidRow(
                              column(6, radioButtons("var1", 
                                                     label = div("Izberi osnovno kategorično spremenljivko za izris grafa (x os).", br(), br()),
                                                     choices = plotVec,
                                                     selected = "Kratica"
                            )),
                            column(6, checkboxGroupInput("var2", 
                                                         label=div(strong("Opcijsko:"), "Izberi dodatno spremenljivko (legenda - barve).", br(), br()),
                                                         choices = setdiff(plotVec, "Kratica"),
                                                         selected = NULL
                            ))
                          )),
                          (plotly::plotlyOutput("mygraph1"))
                 ), fluid = F)


server <- function(input, output, session){
  
  
  output$Prog <- reactive({
    ProgNeeded <- !is.null(input$Članica)
    return(ProgNeeded)
  })
  
  output$Clen <- reactive({
    ClenNeeded <- !is.null(input$Program)
    return(ClenNeeded)
  })
  
  lapply(c("Prog", "Clen"),
         function(x) outputOptions(output, x, suspendWhenHidden = FALSE)
  )
  
  observe({updatePickerInput(session, 
                             inputId="Program", 
                             choices = setNames(
                               lapply(
                                 input$Članica, 
                                 function(x) lapply(sort(unique(dat[Članica == x, Program])), identity)
                                ), input$Članica
                               ),
                             
                             options = list(`actions-box` = TRUE,
                                            `deselect-all-text` = "Ne izberi nobene možnosti",
                                            `select-all-text` = "Izberi Vse",
                                            `none-selected-text` = "Skupaj",
                                            `all-selected-text` = "Vse"
                             ),
                             selected = NULL
                             )
    })
  
  
  observe({updatePickerInput(session, 
                             inputId="Členitev", 
                             choices = setNames(
                               lapply(
                                 input$Program, 
                                 function(x) lapply(sort(unique(dat[Program == x, Členitev])), identity)
                               ), input$Program
                             ),
                             
                             options = list(`actions-box` = TRUE,
                                            `deselect-all-text` = "Ne izberi nobene možnosti",
                                            `select-all-text` = "Izberi Vse",
                                            `none-selected-text` = "Skupaj",
                                            `all-selected-text` = "Vse"
                             ),
                             selected = NULL
                             )
  })
  
  output$mytable1 <- DT::renderDataTable({
    
    by_vars <- vector()
    fltrs <- setNames(vector("list", length(tabVec)),
                       tabVec)

    for (var_nm in tabVec){

      var_val <- input[[var_nm]]

      if (!is.null(var_val) && any(var_val == "Vse")){
        
        by_vars <- c(by_vars, var_nm)
        
      } else if ((!is.null(var_val) && any(var_val == "Skupaj")) || (is.null(var_val))){

        fltrs[[var_nm]] <- as.vector(unique(dat[[var_nm]]))
        
      } else {
        
        by_vars <- c(by_vars, var_nm)
        fltrs[[var_nm]] <- var_val
        
      }

    }

    DT::datatable(dat[Članica %in% fltrs$Članica &
                        Program %in% fltrs$Program &
                        Členitev %in% fltrs$Členitev &
                        Spol %in% fltrs$Spol &
                        Stopnja %in% fltrs$Stopnja &
                        Način %in% fltrs$Način &
                        Letnik %in% fltrs$Letnik &
                        Državljanstvo %in% fltrs$Državljanstvo, 
                      list("Število študentov" = sum(UTEZ)), 
                      by=by_vars],
                    options = list(
                      language = si
                    )
    )
    
                  
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
  
  
  
}


shinyApp(ui = ui, server = server)
  
