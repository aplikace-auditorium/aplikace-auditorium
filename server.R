###############################################################################
###############################################################################
###############################################################################

for(my_package in c(
    
    "shiny",
    "shinyjs",
    "RColorBrewer",
    "shinydashboard",
    "shinyBS",
    "randomForest"
    
)){
    
    if(
        !(
            my_package %in% rownames(installed.packages())
        )
    ){
        
        install.packages(
            my_package,
            dependencies = TRUE,
            repos = "http://cran.us.r-project.org"
        )
        
    }
    
    library(
        my_package,
        character.only = TRUE
    )
    
}


## loaduji skript se všemi uživatelsky definovanými funkcemi ------------------

source(
    file = "my_functions.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## loaduji globální nastavení -------------------------------------------------

source(
    file = "global.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## tvořím jádro aplikace ------------------------------------------------------

my_server <- function(input, output, session){
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## zavádím counter --------------------------------------------------------
    
    output$my_counter <- renderText({
        
        # if(
            # !file.exists("my_counter.Rdata")
        # ){
            # my_counter <- 0
        # }else{
            # load(file = "my_counter.Rdata")
        # }
        
        if(
            ! file.exists("my_counter.txt")
        ){
            my_counter <- 0
        }else{
            
            my_counter <- as.numeric(
                readLines(
                    con = "my_counter.txt",
                    encoding = "UTF-8"
                )
            )
            
        }
        
        my_counter <- my_counter + 1
        
        # save(
            # my_counter,
            # file = "my_counter.Rdata"
        # )
        
        writeLines(
            text = as.character(my_counter),
            con = "my_counter.txt"
        )
        
        paste(
            "Počet návštěv: ",
            as.character(
                my_counter
            ),
            sep = ""
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## loaduju data -----------------------------------------------------------
    
    #### ukládám aktuální pracovní (serverovou) složku jako mateřskou ---------
    
    mother_working_directory <- reactive({
        
        getwd()
        
    })
    
    
    #### nahrávám vestavěná data do proměnné "my_data()" ----------------------
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data
        ){
            
            output$my_transaction_inbuilt_data_selection <- renderUI({
                
                selectInput(
                    inputId = "my_transaction_inbuilt_data_file",
                    label = "Vyberte vestavěná data",
                    choices = c(
                        "novofest" = "my_transaction_inbuilt_data_novofest",
                        "hradec" = "my_transaction_inbuilt_data_hradec"
                    ),
                    selected = "novofest",
                    width = "160px"
                )
                
            })
            
        }else{
            
            output$my_transaction_inbuilt_data_selection <- renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    #### nahrávám codebook ----------------------------------------------------
    
    my_transaction_codebook <- reactive({
        
        setwd(
            paste(
                mother_working_directory(),
                "data",
                sep = "/"
            )
        )
        
        my_data <- read.table(
            file = "my_transaction_inbuilt_data_codebook.txt",
            header = TRUE,
            sep = ";",
            colClasses = "character",
            encoding = "UTF-8"
        )
        
        setwd(
            mother_working_directory()
        )
        
        return(
            my_data
        )
        
    })
    
    
    #### vytvářím objekt s použitými daty -------------------------------------
    
    my_transaction_data <- reactive({
        
        if(
            input$to_use_my_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_data <- read.table(
                file = paste(
                    input$my_transaction_inbuilt_data_file,
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }else{
            
            if(
                is.null(input$my_transaction_file)
            ){
                return(NULL)
            }
            
            my_data <- read.table(
                file = input$my_transaction_file$datapath,
                header = input$my_transaction_header_option,
                sep = input$my_transaction_separator_option,
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
        }
        
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_transaction_inbuilt_data
        ){
            
            for(my_variable in colnames(my_data)){
                
                if(
                    my_transaction_codebook()[
                        my_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ] == "as.Date"
                ){
                    
                    my_data[, my_variable] <- as.Date(
                        my_data[, my_variable],
                        format = "%Y-%m-%d"
                    )
                    
                }else{
                    
                    class(my_data[, my_variable]) <-
                    
                    my_transaction_codebook()[
                        my_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ]    
                    
                }
                
            }
            
        }else{
            
            if(
                nchar(
                    input$my_transaction_data_types_string
                ) == dim(
                    my_data
                )[2]
            ){
                
                my_data_types <- strsplit(
                    input$my_transaction_data_types_string,
                    split = ""
                )[[1]]
                
                if(
                    length(
                        setdiff(my_data_types, c("N", "D", "S", "L"))
                    ) == 0
                ){
                    
                    my_dictionary <- list(
                        "N" = "numeric",
                        "D" = "as.Date",
                        "S" = "character",
                        "L" = "logical"
                    )
                    
                    for(i in 1:length(my_data_types)){
                        
                        if(
                            my_data_types[i] == "D"
                        ){
                            
                            my_data[, i] <- as.Date(
                                my_data[, i],
                                format = "%Y-%m-%d"
                            )
                            
                        }else{
                            
                            class(my_data[, i]) <- my_dictionary[[
                                my_data_types[i]
                            ]]
                            
                        }
                        
                    }
                    
                }
                
            }
            
        }
        
        return(
            my_data
        )
        
    })
    
    
    ## nahrávám data modelů ---------------------------------------------------
    
    my_transaction_model_data <- reactive({
        
        if(
            input$to_use_my_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_model_data <- read.table(
                file = paste(
                    input$my_transaction_inbuilt_data_file,
                    "_modely",
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_transaction_inbuilt_data
        ){
            
            for(my_variable in c(
                
                "predikce_celkem",
                "predikce_na_klienta"
                
            )){
                
                
                my_model_data[, my_variable] <- as.numeric(
                    
                    my_model_data[, my_variable]
                    
                )
            }
            
        }
        
        return(
            my_model_data
        )
        
    })
    
    
    ## zobrazuji data ---------------------------------------------------------
    
    output$my_transaction_data_table <- renderDataTable(
        
        {
            
            if(
                !input$to_use_my_transaction_inbuilt_data
            ){
                
                if(
                    is.null(input$my_transaction_file)
                ){
                    
                    return(NULL)
                    
                }
                
            }
            
            my_transaction_data()
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            )
            
        )
        
    )
    
    output$my_transaction_data_positive_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_transaction_inbuilt_data
            ) & all(
                colnames(my_transaction_data()) == c(
                    
                    "ID",
                    "Date",
                    "Price",
                    "Amount"
                    
                )
            ) & all(
                
                unlist(
                    lapply(
                        1:dim(my_transaction_data())[2],
                        function(i){
                            class(my_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "character",
                    "Date",
                    "numeric",
                    "numeric"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data byla úspěšně nahrána a mají správný datový formát. ",
                "Pokračujte do sekce RFM analýza nebo CLV analýza.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_transaction_data_negative_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_transaction_inbuilt_data
            ) & !all(
                colnames(my_transaction_data()) == c(
                    
                    "ID",
                    "Date",
                    "Price",
                    "Amount"
                    
                )
            ) | !all(
                
                unlist(
                    lapply(
                        1:dim(my_transaction_data())[2],
                        function(i){
                            class(my_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "character",
                    "Date",
                    "numeric",
                    "numeric"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data neobsahují odpovídající proměnné nebo nemají správný datový formát. ",
                "Zkontrolujte formát podle vzoru a nahrání opakujte.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_transaction_data_message_text_gap <- renderUI({
        
        if(
            ! input$to_use_my_transaction_inbuilt_data
        ){
            
            HTML("<br>")
            HTML("<br>")
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_transaction_data_table_info_handler <- renderUI({
        
        if(
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_transaction_data_table_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_transaction_data_table_info",
                title = "Info",
                content = "Vyhledávání řetězce ve všech sloupcích.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    # output$my_transaction_data_table <- renderTable(
        
        # {
            
            # if(
                # !input$to_use_my_transaction_inbuilt_data
            # ){
                
                # if(
                    # is.null(input$my_transaction_file)
                # ){
                    
                    # return(NULL)
                    
                # }
                
            # }
            
            # my_transaction_data()
            
        # }
        
    # )
    
    
    #### zobrazuji datové typy proměnných -------------------------------------
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            output$my_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            output$my_transaction_variable_types_table_label <-
            
            renderUI({
                
                h4("Datové typy proměnných")
                
            })
            
        }else{
            
            output$my_transaction_variable_types_table_label <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            output$my_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    output$my_transaction_variable_types_table <- renderTable({
        
        if(
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        variable_types_dictionary <- list(
            "numeric" = "numerická proměnná",
            "Date" = "proměnná typu datum",
            "character" = "textová proměnná",
            "logical" = "logická proměnná"
        )
        
        my_data_types <- NULL
        
        for(i in 1:dim(my_transaction_data())[2]){
            
            my_data_types <- c(
                my_data_types,
                variable_types_dictionary[[
                    class(my_transaction_data()[, i])]
                ]
            )
            
        }
        
        cbind(
            "proměnná" = colnames(my_transaction_data()),
            "datový typ proměnné" = my_data_types
        )
        
    }, include.rownames = FALSE)
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            output$my_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## loaduju netransakční data ----------------------------------------------
    
    #### nahrávám vestavěná data do proměnné "my_data()" ----------------------
    
    observe(
        
        if(
            input$to_use_my_non_transaction_inbuilt_data
        ){
            
            output$my_non_transaction_inbuilt_data_selection <- renderUI({
                
                selectInput(
                    inputId = "my_non_transaction_inbuilt_data_file",
                    label = "Vyberte vestavěná data",
                    choices = c(
                        "novofest" =
                            "my_non_transaction_inbuilt_data_novofest"
                    ),
                    selected = "novofest",
                    width = "160px"
                )
                
            })
            
        }else{
            
            output$my_non_transaction_inbuilt_data_selection <- renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    #### nahrávám codebook ----------------------------------------------------
    
    my_non_transaction_codebook <- reactive({
        
        setwd(
            paste(
                mother_working_directory(),
                "data",
                sep = "/"
            )
        )
        
        my_data <- read.table(
            file = "my_non_transaction_inbuilt_data_codebook.txt",
            header = TRUE,
            sep = ";",
            colClasses = "character",
            encoding = "UTF-8"
        )
        
        setwd(
            mother_working_directory()
        )
        
        return(
            my_data
        )
        
    })
    
    
    #### vytvářím objekt s použitými daty -------------------------------------
    
    my_non_transaction_data <- reactive({
        
        if(
            input$to_use_my_non_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_data <- read.table(
                file = paste(
                    input$my_non_transaction_inbuilt_data_file,
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }else{
            
            if(
                is.null(input$my_non_transaction_file)
            ){
                return(NULL)
            }
            
            my_data <- read.table(
                file = input$my_non_transaction_file$datapath,
                header = input$my_non_transaction_header_option,
                sep = input$my_non_transaction_separator_option,
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
        }
        
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_non_transaction_inbuilt_data
        ){
            
            for(my_variable in colnames(my_data)){
                
                if(
                    my_non_transaction_codebook()[
                        my_non_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ] == "as.Date"
                ){
                    
                    my_data[, my_variable] <- as.Date(
                        my_data[, my_variable],
                        format = "%Y-%m-%d"
                    )
                    
                }else{
                    
                    class(my_data[, my_variable]) <-
                    
                    my_non_transaction_codebook()[
                        my_non_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ]    
                    
                }
                
            }
            
        }else{
            
            if(
                nchar(
                    input$my_non_transaction_data_types_string
                ) == dim(
                    my_data
                )[2]
            ){
                
                my_data_types <- strsplit(
                    input$my_non_transaction_data_types_string,
                    split = ""
                )[[1]]
                
                if(
                    length(
                        setdiff(my_data_types, c("N", "D", "S", "L"))
                    ) == 0
                ){
                    
                    my_dictionary <- list(
                        "N" = "numeric",
                        "D" = "as.Date",
                        "S" = "character",
                        "L" = "logical"
                    )
                    
                    for(i in 1:length(my_data_types)){
                        
                        if(
                            my_data_types[i] == "D"
                        ){
                            
                            my_data[, i] <- as.Date(
                                my_data[, i],
                                format = "%Y-%m-%d"
                            )
                            
                        }else{
                            
                            class(my_data[, i]) <- my_dictionary[[
                                my_data_types[i]
                            ]]
                            
                        }
                        
                    }
                    
                }
                
            }
            
        }
        
        return(
            my_data
        )
        
    })
    
    
    ## nahrávám data modelů ---------------------------------------------------
    
    my_non_transaction_model_data <- reactive({
        
        if(
            input$to_use_my_non_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_model_data <- read.table(
                file = paste(
                    input$my_non_transaction_inbuilt_data_file,
                    "_modely",
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_non_transaction_inbuilt_data
        ){
            
            for(my_variable in c(
                
                "predikce_celkem",
                "predikce_na_klienta"
                
            )){
                
                
                my_model_data[, my_variable] <- as.numeric(
                    
                    my_model_data[, my_variable]
                    
                )
            }
            
        }
        
        return(
            my_model_data
        )
        
    })
    
    
    ## zobrazuji data ---------------------------------------------------------
    
    output$my_non_transaction_data_table <- renderDataTable(
        
        {
            
            if(
                !input$to_use_my_non_transaction_inbuilt_data
            ){
                
                if(
                    is.null(input$my_non_transaction_file)
                ){
                    
                    return(NULL)
                    
                }
                
            }
            
            my_non_transaction_data()
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            )
            
        )
        
    )
    
    output$my_non_transaction_data_positive_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_non_transaction_inbuilt_data
            ) & all(
                colnames(my_non_transaction_data()) == c(
                    
                    "Date",
                    "WeekDay",
                    "KnownArtist",
                    "MajorEra",
                    "MinorEra",
                    "ConcertType"
                    
                )
            ) & all(
                
                unlist(
                    lapply(
                        1:dim(my_non_transaction_data())[2],
                        function(i){
                            class(my_non_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "Date",
                    "character",
                    "character",
                    "character",
                    "character",
                    "character"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data byla úspěšně nahrána a mají správný datový formát. ",
                "Pokračujte do sekce RFM analýza nebo CLV analýza.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_non_transaction_data_negative_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_non_transaction_inbuilt_data
            ) & !all(
                colnames(my_non_transaction_data()) == c(
                    
                    "Date",
                    "WeekDay",
                    "KnownArtist",
                    "MajorEra",
                    "MinorEra",
                    "ConcertType"
                    
                )
            ) | !all(
                
                unlist(
                    lapply(
                        1:dim(my_non_transaction_data())[2],
                        function(i){
                            class(my_non_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "Date",
                    "character",
                    "character",
                    "character",
                    "character",
                    "character"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data neobsahují odpovídající proměnné nebo nemají správný datový formát. ",
                "Zkontrolujte formát podle vzoru a nahrání opakujte.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_non_transaction_data_message_text_gap <- renderUI({
        
        if(
            ! input$to_use_my_non_transaction_inbuilt_data
        ){
            
            HTML("<br>")
            HTML("<br>")
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_non_transaction_data_table_info_handler <- renderUI({
        
        if(
            !input$to_use_my_non_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_non_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_non_transaction_data_table_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_non_transaction_data_table_info",
                title = "Info",
                content = "Vyhledávání řetězce ve všech sloupcích.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    # output$my_non_transaction_data_table <- renderTable(
        
        # {
            
            # if(
                # !input$to_use_my_non_transaction_inbuilt_data
            # ){
                
                # if(
                    # is.null(input$my_non_transaction_file)
                # ){
                    
                    # return(NULL)
                    
                # }
                
            # }
            
            # my_non_transaction_data()
            
        # }
        
    # )
    
    
    #### zobrazuji datové typy proměnných -------------------------------------
    
    observe(
        
        if(
            input$to_use_my_non_transaction_inbuilt_data |
            ! is.null(input$my_non_transaction_file)
        ){
            
            output$my_non_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_non_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_non_transaction_inbuilt_data |
            ! is.null(input$my_non_transaction_file)
        ){
            
            output$my_non_transaction_variable_types_table_label <-
            
            renderUI({
                
                h4("Datové typy proměnných")
                
            })
            
        }else{
            
            output$my_non_transaction_variable_types_table_label <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_non_transaction_inbuilt_data |
            ! is.null(input$my_non_transaction_file)
        ){
            
            output$my_non_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_non_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    output$my_non_transaction_variable_types_table <- renderTable({
        
        if(
            !input$to_use_my_non_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_non_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        variable_types_dictionary <- list(
            "numeric" = "numerická proměnná",
            "Date" = "proměnná typu datum",
            "character" = "textová proměnná",
            "logical" = "logická proměnná"
        )
        
        my_data_types <- NULL
        
        for(i in 1:dim(my_non_transaction_data())[2]){
            
            my_data_types <- c(
                my_data_types,
                variable_types_dictionary[[
                    class(my_non_transaction_data()[, i])]
                ]
            )
            
        }
        
        cbind(
            "proměnná" = colnames(my_non_transaction_data()),
            "datový typ proměnné" = my_data_types
        )
        
    }, include.rownames = FALSE)
    
    observe(
        
        if(
            input$to_use_my_non_transaction_inbuilt_data |
            ! is.null(input$my_non_transaction_file)
        ){
            
            output$my_non_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_non_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## loaduju data plánované budoucí sezóny ----------------------------------
    
    #### nahrávám vestavěná data do proměnné "my_data()" ----------------------
    
    observe(
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            output$my_fictive_non_transaction_inbuilt_data_selection <-
            renderUI({
                
                selectInput(
                    inputId = "my_fictive_non_transaction_inbuilt_data_file",
                    label = "Vyberte vestavěná data",
                    choices = c(
                        "novofest" =
                            "my_fictive_non_transaction_inbuilt_data_novofest"#,
                        # "novofest_unreal" = 
                            # "my_fictive_non_transaction_inbuilt_data_novofest_unreal"
                    ),
                    selected = "novofest",
                    width = "160px"
                )
                
            })
            
        }else{
            
            output$my_fictive_non_transaction_inbuilt_data_selection <-
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    #### nahrávám codebook ----------------------------------------------------
    
    my_fictive_non_transaction_codebook <- reactive({
        
        setwd(
            paste(
                mother_working_directory(),
                "data",
                sep = "/"
            )
        )
        
        my_data <- read.table(
            file = "my_fictive_non_transaction_inbuilt_data_codebook.txt",
            header = TRUE,
            sep = ";",
            colClasses = "character",
            encoding = "UTF-8"
        )
        
        setwd(
            mother_working_directory()
        )
        
        return(
            my_data
        )
        
    })
    
    
    #### vytvářím objekt s použitými daty -------------------------------------
    
    my_fictive_non_transaction_data <- reactive({
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_data <- read.table(
                file = paste(
                    input$my_fictive_non_transaction_inbuilt_data_file,
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }else{
            
            if(
                is.null(input$my_fictive_non_transaction_file)
            ){
                return(NULL)
            }
            
            my_data <- read.table(
                file = input$my_fictive_non_transaction_file$datapath,
                header = input$my_fictive_non_transaction_header_option,
                sep = input$my_fictive_non_transaction_separator_option,
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
        }
        
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            for(my_variable in colnames(my_data)){
                
                if(
                    my_fictive_non_transaction_codebook()[
                        my_fictive_non_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ] == "as.Date"
                ){
                    
                    my_data[, my_variable] <- as.Date(
                        my_data[, my_variable],
                        format = "%Y-%m-%d"
                    )
                    
                }else{
                    
                    class(my_data[, my_variable]) <-
                    
                    my_fictive_non_transaction_codebook()[
                        my_fictive_non_transaction_codebook()[, "variable_name"] ==
                        my_variable,
                        "variable_type"
                    ]    
                    
                }
                
            }
            
        }else{
            
            if(
                nchar(
                    input$my_fictive_non_transaction_data_types_string
                ) == dim(
                    my_data
                )[2]
            ){
                
                my_data_types <- strsplit(
                    input$my_fictive_non_transaction_data_types_string,
                    split = ""
                )[[1]]
                
                if(
                    length(
                        setdiff(my_data_types, c("N", "D", "S", "L"))
                    ) == 0
                ){
                    
                    my_dictionary <- list(
                        "N" = "numeric",
                        "D" = "as.Date",
                        "S" = "character",
                        "L" = "logical"
                    )
                    
                    for(i in 1:length(my_data_types)){
                        
                        if(
                            my_data_types[i] == "D"
                        ){
                            
                            my_data[, i] <- as.Date(
                                my_data[, i],
                                format = "%Y-%m-%d"
                            )
                            
                        }else{
                            
                            class(my_data[, i]) <- my_dictionary[[
                                my_data_types[i]
                            ]]
                            
                        }
                        
                    }
                    
                }
                
            }
            
        }
        
        return(
            my_data
        )
        
    })
    
    
    ## nahrávám data modelů ---------------------------------------------------
    
    my_fictive_non_transaction_model_data <- reactive({
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            setwd(
                paste(
                    mother_working_directory(),
                    "data",
                    sep = "/"
                )
            )
            
            my_model_data <- read.table(
                file = paste(
                    input$my_fictive_non_transaction_inbuilt_data_file,
                    "_modely",
                    ".csv",
                    sep = ""
                ),
                header = TRUE,
                sep = ";",
                colClasses = "character",
                encoding = "UTF-8",
                check.names = FALSE
            )
            
            setwd(
                mother_working_directory()
            )
            
        }
        
        #### nyní překódovávám datové typy proměnných -------------------------
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            for(my_variable in c(
                
                "predikce_celkem",
                "predikce_na_klienta"
                
            )){
                
                
                my_model_data[, my_variable] <- as.numeric(
                    
                    my_model_data[, my_variable]
                    
                )
            }
            
        }
        
        return(
            my_model_data
        )
        
    })
    
    
    ## zobrazuji data ---------------------------------------------------------
    
    output$my_fictive_non_transaction_data_table <- renderDataTable(
        
        {
            
            if(
                !input$to_use_my_fictive_non_transaction_inbuilt_data
            ){
                
                if(
                    is.null(input$my_fictive_non_transaction_file)
                ){
                    
                    return(NULL)
                    
                }
                
            }
            
            my_fictive_non_transaction_data()
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            )
            
        )
        
    )
    
    output$my_fictive_non_transaction_data_positive_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_fictive_non_transaction_inbuilt_data
            ) & all(
                colnames(my_fictive_non_transaction_data()) == c(
                    
                    "Date",
                    "WeekDay",
                    "KnownArtist",
                    "MajorEra",
                    "MinorEra",
                    "ConcertType"
                    
                )
            ) & all(
                
                unlist(
                    lapply(
                        1:dim(my_fictive_non_transaction_data())[2],
                        function(i){
                            class(my_fictive_non_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "Date",
                    "character",
                    "character",
                    "character",
                    "character",
                    "character"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data byla úspěšně nahrána a mají správný datový formát. ",
                "Pokračujte do sekce RFM analýza nebo CLV analýza.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_fictive_non_transaction_data_negative_message_text <- renderText({
        
        if(
            
            (
                ! input$to_use_my_fictive_non_transaction_inbuilt_data
            ) & !all(
                colnames(my_fictive_non_transaction_data()) == c(
                    
                    "Date",
                    "WeekDay",
                    "KnownArtist",
                    "MajorEra",
                    "MinorEra",
                    "ConcertType"
                    
                )
            ) | !all(
                
                unlist(
                    lapply(
                        1:dim(my_fictive_non_transaction_data())[2],
                        function(i){
                            class(my_fictive_non_transaction_data()[, i])
                        }
                    )
                ) == c(
                    
                    "Date",
                    "character",
                    "character",
                    "character",
                    "character",
                    "character"
                    
                )
                
            )
            
        ){
            
            paste(
                "Data neobsahují odpovídající proměnné nebo nemají správný datový formát. ",
                "Zkontrolujte formát podle vzoru a nahrání opakujte.",
                sep = ""
            )
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_fictive_non_transaction_data_message_text_gap <- renderUI({
        
        if(
            ! input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            HTML("<br>")
            HTML("<br>")
            
        }else{
            
            return(NULL)
            
        }
        
    })
    
    output$my_fictive_non_transaction_data_table_info_handler <- renderUI({
        
        if(
            !input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_fictive_non_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_fictive_non_transaction_data_table_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_fictive_non_transaction_data_table_info",
                title = "Info",
                content = "Vyhledávání řetězce ve všech sloupcích.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    # output$my_non_transaction_data_table <- renderTable(
        
        # {
            
            # if(
                # !input$to_use_my_non_transaction_inbuilt_data
            # ){
                
                # if(
                    # is.null(input$my_non_transaction_file)
                # ){
                    
                    # return(NULL)
                    
                # }
                
            # }
            
            # my_non_transaction_data()
            
        # }
        
    # )
    
    
    #### zobrazuji datové typy proměnných -------------------------------------
    
    observe(
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data |
            ! is.null(input$my_fictive_non_transaction_file)
        ){
            
            output$my_fictive_non_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_fictive_non_transaction_variable_types_table_upper_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data |
            ! is.null(input$my_fictive_non_transaction_file)
        ){
            
            output$my_fictive_non_transaction_variable_types_table_label <-
            
            renderUI({
                
                h4("Datové typy proměnných")
                
            })
            
        }else{
            
            output$my_fictive_non_transaction_variable_types_table_label <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    observe(
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data |
            ! is.null(input$my_fictive_non_transaction_file)
        ){
            
            output$my_fictive_non_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_fictive_non_transaction_variable_types_table_mid_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    output$my_fictive_non_transaction_variable_types_table <- renderTable({
        
        if(
            !input$to_use_my_fictive_non_transaction_inbuilt_data
        ){
            
            if(
                is.null(input$my_fictive_non_transaction_file)
            ){
                
                return(NULL)
                
            }
            
        }
        
        variable_types_dictionary <- list(
            "numeric" = "numerická proměnná",
            "Date" = "proměnná typu datum",
            "character" = "textová proměnná",
            "logical" = "logická proměnná"
        )
        
        my_data_types <- NULL
        
        for(i in 1:dim(my_fictive_non_transaction_data())[2]){
            
            my_data_types <- c(
                my_data_types,
                variable_types_dictionary[[
                    class(my_fictive_non_transaction_data()[, i])]
                ]
            )
            
        }
        
        cbind(
            "proměnná" = colnames(my_fictive_non_transaction_data()),
            "datový typ proměnné" = my_data_types
        )
        
    }, include.rownames = FALSE)
    
    observe(
        
        if(
            input$to_use_my_fictive_non_transaction_inbuilt_data |
            ! is.null(input$my_fictive_non_transaction_file)
        ){
            
            output$my_fictive_non_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                tags$hr()
                
            })
            
        }else{
            
            output$my_fictive_non_transaction_variable_types_table_lower_hline <-
            
            renderUI({
                
                NULL
                
            })
            
        }
        
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## zavádím některé reactive proměnné --------------------------------------
    
    left_censoring <- reactive({
        
        return(
            input$my_transaction_recency_left_censoring
        )
        
    })
    
    right_censoring <- reactive({
        
        return(
            input$my_transaction_recency_right_censoring
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## processing dat ---------------------------------------------------------
    
    my_transaction_tidy_data <- reactive({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        my_table <- NULL
        
        for(my_id in unique(my_transaction_data()[, "ID"])){
            
            if(
                any(
                    my_transaction_data()[
                        my_transaction_data()[, "ID"] == my_id,
                        "Date"
                    ] <= right_censoring() &
                    my_transaction_data()[
                        my_transaction_data()[, "ID"] == my_id,
                        "Date"
                    ] >= left_censoring()
                )
            ){
                
                my_table <- rbind(
                    
                    my_table,
                    c(
                        "ID" = my_id,
                        "recency" = min(
                            as.numeric(
                                right_censoring() - my_transaction_data()[
                                    my_transaction_data()[, "ID"] ==
                                    my_id,
                                    "Date"
                                ][
                                    my_transaction_data()[
                                        my_transaction_data()[, "ID"] ==
                                        my_id,
                                        "Date"
                                    ] <= right_censoring() &
                                    my_transaction_data()[
                                        my_transaction_data()[, "ID"] ==
                                        my_id,
                                        "Date"
                                    ] >= left_censoring()
                                ]
                            ),
                            na.rm = TRUE
                        ),
                        "frequency" = length(
                            my_transaction_data()[
                                my_transaction_data()[, "ID"] == my_id,
                                "Date"
                            ][
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] <= right_censoring() &
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] >= left_censoring()
                            ]
                        ),
                        "monetary" = sum(
                            my_transaction_data()[
                                my_transaction_data()[, "ID"] == my_id,
                                "Price"
                            ][
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] <= right_censoring() &
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] >= left_censoring()
                            ] * my_transaction_data()[
                                my_transaction_data()[, "ID"] == my_id,
                                "Amount"
                            ][
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] <= right_censoring() &
                                my_transaction_data()[
                                    my_transaction_data()[, "ID"] == my_id,
                                    "Date"
                                ] >= left_censoring()
                            ]
                        )
                        # / sum(
                            # my_transaction_data()[
                                # my_transaction_data()[, "ID"] == my_id,
                                # "Amount"
                            # ][
                                # my_transaction_data()[
                                    # my_transaction_data()[, "ID"] == my_id,
                                    # "Date"
                                # ] <= right_censoring() &
                                # my_transaction_data()[
                                    # my_transaction_data()[, "ID"] == my_id,
                                    # "Date"
                                # ] >= left_censoring()
                            # ]
                        # )
                    )
                    
                )
                
            }
            
        }
        
        my_table <- data.frame(
            
            my_table,
            stringsAsFactors = FALSE
            
        )
        
        for(my_variable in c(
            
            "recency",
            "frequency",
            "monetary"
            
        )){
            
            my_table[, my_variable] <- as.numeric(
                
                my_table[, my_variable]
                
            )
            
        }
        
        return(
            my_table
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## váhy pro recency, frequency, monetary ----------------------------------
    
    output$my_transaction_weights_text_display <- renderUI({
        
        HTML(
            paste(
                "Aktuální váhy pro ",
                "(recency, frequency, monetary) ",
                "jsou<br><b>(",
                format(
                    round(
                        input$my_transaction_weights_slider[1],
                        digits = 3
                    ),
                    nsmall = 3
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_weights_slider[2] -
                        input$my_transaction_weights_slider[1],
                        digits = 3
                    ),
                    nsmall = 3
                ),
                ", ",
                format(
                    round(
                        1 - input$my_transaction_weights_slider[2],
                        digits = 3
                    ),
                    nsmall = 3
                ),
                ")</b>.",
                sep = ""
            )
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## hranice mezi segmenty pro recency, frequency, monetary -----------------
    
    output$my_transaction_recency_borders_slider_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        sliderInput(
            inputId = "my_transaction_recency_borders_slider",
            label = HTML(
                "Recency [hranice mezi segmenty]"
            ),
            min = min(
                my_transaction_tidy_data()[
                    ,
                    "recency"
                ],
                na.rm = TRUE
            ),
            max = max(
                my_transaction_tidy_data()[
                    ,
                    "recency"
                ],
                na.rm = TRUE
            ),
            value = c(
                floor(
                    min(
                        my_transaction_tidy_data()[
                            ,
                            "recency"
                        ],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        ) - min(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        )
                    ) * 1 / 3
                ),
                floor(
                    min(
                        my_transaction_tidy_data()[
                            ,
                            "recency"
                        ],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        ) - min(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        )
                    ) * 2 / 3
                )
            ),
            step = 1,
            width = "100%"
        )
        
    })
    
    output$my_transaction_frequency_borders_slider_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        sliderInput(
            inputId = "my_transaction_frequency_borders_slider",
            label = HTML(
                "Frequency [hranice mezi segmenty]"
            ),
            min = min(
                my_transaction_tidy_data()[
                    ,
                    "frequency"
                ],
                na.rm = TRUE
            ),
            max = max(
                my_transaction_tidy_data()[
                    ,
                    "frequency"
                ],
                na.rm = TRUE
            ),
            value = c(
                min(
                    my_transaction_tidy_data()[
                        ,
                        "frequency"
                    ],
                    na.rm = TRUE
                ) + (
                    max(
                        my_transaction_tidy_data()[
                            ,
                            "frequency"
                        ],
                        na.rm = TRUE
                    ) - min(
                        my_transaction_tidy_data()[
                            ,
                            "frequency"
                        ],
                        na.rm = TRUE
                    )
                ) * 1 / 3,
                min(
                    my_transaction_tidy_data()[
                        ,
                        "frequency"
                    ],
                    na.rm = TRUE
                ) + (
                    max(
                        my_transaction_tidy_data()[
                            ,
                            "frequency"
                        ],
                        na.rm = TRUE
                    ) - min(
                        my_transaction_tidy_data()[
                            ,
                            "frequency"
                        ],
                        na.rm = TRUE
                    )
                ) * 2 / 3
            ),
            step = 1,
            width = "100%"
        )
        
    })
    
    output$my_transaction_monetary_borders_slider_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        sliderInput(
            inputId = "my_transaction_monetary_borders_slider",
            label = HTML(
                "Monetary [hranice mezi segmenty]"
            ),
            min = min(
                my_transaction_tidy_data()[
                    ,
                    "monetary"
                ],
                na.rm = TRUE
            ),
            max = max(
                my_transaction_tidy_data()[
                    ,
                    "monetary"
                ],
                na.rm = TRUE
            ),
            value = c(
                min(
                    my_transaction_tidy_data()[
                        ,
                        "monetary"
                    ],
                    na.rm = TRUE
                ) + (
                    max(
                        my_transaction_tidy_data()[
                            ,
                            "monetary"
                        ],
                        na.rm = TRUE
                    ) - min(
                        my_transaction_tidy_data()[
                            ,
                            "monetary"
                        ],
                        na.rm = TRUE
                    )
                ) * 1 / 3,
                min(
                    my_transaction_tidy_data()[
                        ,
                        "monetary"
                    ],
                    na.rm = TRUE
                ) + (
                    max(
                        my_transaction_tidy_data()[
                            ,
                            "monetary"
                        ],
                        na.rm = TRUE
                    ) - min(
                        my_transaction_tidy_data()[
                            ,
                            "monetary"
                        ],
                        na.rm = TRUE
                    )
                ) * 2 / 3
            ),
            step = 1,
            width = "100%"
        )
        
    })
    
    output$my_transaction_recency_borders_text_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        HTML(
            paste(
                "Aktuální hranice pro ",
                "recency ",
                "jsou<br><b>(",
                format(
                    round(
                        min(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_recency_borders_slider[1],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_recency_borders_slider[2],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        max(
                            my_transaction_tidy_data()[
                                ,
                                "recency"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ")</b>.",
                sep = ""
            )
        )
        
    })
    
    output$my_transaction_frequency_borders_text_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        HTML(
            paste(
                "Aktuální hranice pro ",
                "frequency ",
                "jsou<br><b>(",
                format(
                    round(
                        min(
                            my_transaction_tidy_data()[
                                ,
                                "frequency"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_frequency_borders_slider[1],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_frequency_borders_slider[2],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        max(
                            my_transaction_tidy_data()[
                                ,
                                "frequency"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ")</b>.",
                sep = ""
            )
        )
        
    })
    
    output$my_transaction_monetary_borders_text_display <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        HTML(
            paste(
                "Aktuální hranice pro ",
                "monetary ",
                "jsou<br><b>(",
                format(
                    round(
                        min(
                            my_transaction_tidy_data()[
                                ,
                                "monetary"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_monetary_borders_slider[1],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        input$my_transaction_monetary_borders_slider[2],
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ", ",
                format(
                    round(
                        max(
                            my_transaction_tidy_data()[
                                ,
                                "monetary"
                            ],
                            na.rm = TRUE
                        ),
                        digits = 0
                    ),
                    nsmall = 0
                ),
                ")</b>.",
                sep = ""
            )
        )
        
    })
    
    output$my_transaction_hr_definition_middle <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        HTML(
            "<hr>"
        )
        
    })
    
    output$my_transaction_hr_definition_bottom <- renderUI({
        
        if(
            is.null(input$my_transaction_file) &
            !input$to_use_my_transaction_inbuilt_data
        ){
            
            return(NULL)
            
        }
        
        HTML(
            "<hr>"
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    
    ## odkrávání a skrývání ovládacích prvků v RFM analýze --------------------
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(id = "my_transaction_weights_slider")
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(id = "my_transaction_weights_text_display")
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(id = "my_transaction_recency_left_censoring")
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(id = "my_transaction_recency_right_censoring")
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_recency_borders_slider_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_recency_borders_text_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_frequency_borders_slider_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_frequency_borders_text_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_monetary_borders_slider_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_monetary_borders_text_display"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_hr_definition_middle"
            )
        }
    )
    
    observeEvent(
        input$my_transaction_settings_toggle_button,
        {
            shinyjs::toggle(
                id = "my_transaction_hr_definition_bottom"
            )
        }
    )
    
    
    ## zajišťuji, aby byly objekty inicializovány, přepočítávány a updatovány
    ## i tehdy, když jsou v režimu hidden -------------------------------------
    
    # outputOptions(
        # x = input,
        # name = "my_transaction_weights_slider",
        # suspendWhenHidden = FALSE
    # )
    
    outputOptions(
        x = output,
        name = "my_transaction_weights_text_display",
        suspendWhenHidden = FALSE
    )
    
    # outputOptions(
        # x = output,
        # name = "my_transaction_recency_left_censoring",
        # suspendWhenHidden = FALSE
    # )
    
    # outputOptions(
        # x = output,
        # name = "my_transaction_recency_right_censoring",
        # suspendWhenHidden = FALSE
    # )
    
    outputOptions(
        x = output,
        name = "my_transaction_recency_borders_slider_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_recency_borders_text_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_frequency_borders_slider_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_frequency_borders_text_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_monetary_borders_slider_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_monetary_borders_text_display",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_hr_definition_middle",
        suspendWhenHidden = FALSE
    )
    
    outputOptions(
        x = output,
        name = "my_transaction_hr_definition_bottom",
        suspendWhenHidden = FALSE
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## definuji cut-pointy pro recenty, frequency a monetary ------------------
    
    my_transaction_recency_cut_points <- reactive({
        
        return(
            
            c(
                input$my_transaction_recency_borders_slider[1],
                input$my_transaction_recency_borders_slider[2]
            )
            
        )
        
    })
    
    my_transaction_frequency_cut_points <- reactive({
        
        return(
            
            c(
                input$my_transaction_frequency_borders_slider[1],
                input$my_transaction_frequency_borders_slider[2]
            )
            
        )
        
    })
    
    my_transaction_monetary_cut_points <- reactive({
        
        return(
            
            c(
                input$my_transaction_monetary_borders_slider[1],
                input$my_transaction_monetary_borders_slider[2]
            )
            
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## připravuji data na finální zobrazení -----------------------------------
    
    my_transaction_final_data <- reactive({
        
        recency_score <- unlist(
            lapply(
                1:dim(my_transaction_tidy_data())[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_transaction_tidy_data()[i, "recency"],
                        categories_cut_points =
                            my_transaction_recency_cut_points(),
                        categories_labels = as.character(
                            3:1
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        frequency_score <- unlist(
            lapply(
                1:dim(my_transaction_tidy_data())[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_transaction_tidy_data()[i, "frequency"],
                        categories_cut_points =
                            my_transaction_frequency_cut_points(),
                        categories_labels = as.character(
                            1:3
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        monetary_score <- unlist(
            lapply(
                1:dim(my_transaction_tidy_data())[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_transaction_tidy_data()[i, "monetary"],
                        categories_cut_points =
                            my_transaction_monetary_cut_points(),
                        categories_labels = as.character(
                            1:3
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        
        temp_data <- data.frame(
            
            my_transaction_tidy_data(),
            "recency segment" = recency_score,
            "frequency segment" = frequency_score,
            "monetary segment" = monetary_score,
            "celkove vazene skore" = (
                
                as.numeric(recency_score) * (
                    input$my_transaction_weights_slider[1]
                ) +
                as.numeric(frequency_score) * (
                    input$my_transaction_weights_slider[2] -
                    input$my_transaction_weights_slider[1]
                ) +
                as.numeric(monetary_score) * (
                    1 - input$my_transaction_weights_slider[2]
                )
                
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
            
        )
        
        for(my_variable in colnames(temp_data)){
            
            temp_data[, my_variable] <- as.character(
                
                temp_data[, my_variable]
                
            )
            
        }
        
        for(my_variable in colnames(temp_data)){
            
            if(my_variable != "ID"){
                
                temp_data[, my_variable] <- as.numeric(
                    
                    temp_data[, my_variable]
                    
                )
                
            }
            
        }
        
        return(
            temp_data
        )
        
    })
    
    observe(
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            output$my_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    downloadButton(
                        outputId = "my_transaction_final_data_download",
                        label = "Stáhnout finální dataset s RFM skóre"
                    )
                    
                }
                
            )
            
        }else{
            
            output$my_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    NULL
                    
                }
                
            )
            
        }
        
    )
    
    output$my_transaction_final_data_download <- downloadHandler(
        
        filename = function(){
            
            paste(
                if(
                    input$to_use_my_transaction_inbuilt_data
                ){
                    input$my_transaction_inbuilt_data_file
                }else{
                    input$my_transaction_file
                },
                "_with_RFM_score",
                ".csv",
                sep = ""
            )
            
        },
        
        content = function(file){
            
            write.csv2(
                my_transaction_final_data(),
                file,
                row.names = FALSE
            )
            
        }
        
    )
    
    output$my_transaction_final_data_table <- renderDataTable(
        
        {
            
            my_transaction_final_data()
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            )
            
        )
        
    )
    
    # output$my_transaction_final_data_table <- renderTable(
        
        # {
            
            # my_transaction_final_data()
            
        # }
        
    # )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## histogramy -------------------------------------------------------------
    
    # output$my_transaction_recency_histogram <- renderPlot({
        
        # # getMyHistogram(
            
            # # my_data = tidy_data(),
            # # my_variable = "recency",
            # # x_lab = "recency [dny]",
            # # my_main = "histogram proměnné recency",
            # # my_colour = "lightgrey"
            
        # # )
        
        # hist(
            # right_censoring() - my_transaction_tidy_data()[, "recency"],
            # breaks = "months",
            # xlab = "recency [měsíce]",
            # ylab = "absolutní počet klientů",
            # main = "histogram proměnné recency",
            # col = "lightgrey",
            # freq = TRUE
        # )
        
    # })
    
    # output$my_transaction_frequency_histogram <- renderPlot({
        
        # getMyHistogram(
            
            # my_data = my_transaction_tidy_data(),
            # my_variable = "frequency",
            # x_lab = "frequency [počet nákupů za sledované období]",
            # my_main = "histogram proměnné frequency",
            # my_colour = "lightgrey"
            
        # )
        
    # })
    
    # output$my_transaction_monetary_histogram <- renderPlot({
        
        # getMyHistogram(
            
            # my_data = my_transaction_tidy_data(),
            # my_variable = "monetary",
            # x_lab = "monetary [Kč / nákup]",
            # my_main = "histogram proměnné monetary",
            # my_colour = "lightgrey"
            
        # )
        
    # })
    
    
    ## barploty ---------------------------------------------------------------
    
    # output$my_transaction_recency_barplot <- renderPlot(
        
        # {
            
            # getMyBarplot(
                
                # my_data = my_transaction_final_data(),
                # my_variable = "recency segment",
                # x_lab = "segment proměnné recency",
                # my_main =
                    # "počty klientů v segmentech \nproměnné recency"
                
            # )
            
        # },
        # width = function(){
            # 3 * 120
        # }
        
    # )
    
    # output$my_transaction_frequency_barplot <- renderPlot(
        
        # {
            
            # getMyBarplot(
                
                # my_data = my_transaction_final_data(),
                # my_variable = "frequency segment",
                # x_lab = "segment proměnné frequency",
                # my_main =
                    # "počty klientů v segmentech \nproměnné frequency"
                
            # )
            
        # },
        # width = function(){
            # 3 * 120
        # }
        
    # )
    
    # output$my_transaction_monetary_barplot <- renderPlot(
        
        # {
            
            # getMyBarplot(
                
                # my_data = my_transaction_final_data(),
                # my_variable = "monetary segment",
                # x_lab = "segment proměnné monetary",
                # my_main =
                    # "počty klientů v segmentech \nproměnné monetary"
                
            # )
            
        # },
        # width = function(){
            # 3 * 120
        # }
        
    # )
    
    
    ## scatterploty -----------------------------------------------------------
    
    # output$my_transaction_monetary_vs_recency <- renderPlot(
        
        # {
            
            # getMyScatterplot(
                
                # my_data = my_transaction_tidy_data(),
                # my_first_variable = "recency",
                # my_second_variable = "monetary",
                # x_lab = "recency [dny]",
                # y_lab = "monetary [Kč / nákup]",
                # my_main = "monetary vs. recency scatterplot"
                
            # )
            
        # }
        
    # )
    
    # output$my_transaction_monetary_vs_frequency <- renderPlot(
        
        # {
            
            # getMyScatterplot(
                
                # my_data = my_transaction_tidy_data(),
                # my_first_variable = "frequency",
                # my_second_variable = "monetary",
                # x_lab = "frequency [počet nákupů za sledované období]",
                # y_lab = "monetary [Kč / nákup]",
                # my_main = "monetary vs. frequency scatterplot"
                
            # )
            
        # }
        
    # )
    
    # output$my_transaction_recency_vs_frequency <- renderPlot(
        
        # {
            
            # getMyScatterplot(
                
                # my_data = my_transaction_tidy_data(),
                # my_first_variable = "frequency",
                # my_second_variable = "recency",
                # x_lab = "frequency [počet nákupů za sledované období]",
                # y_lab = "recency [dny]",
                # my_main = "recency vs. frequency scatterplot"
                
            # )
            
        # }
        
    # )
    
    
    ## histogram celkového skóre ----------------------------------------------
    
    # output$my_transaction_total_score_histogram <- renderPlot({
        
        # getMyHistogram(
            
            # my_data = my_transaction_final_data(),
            # my_variable = "celkové vážené skóre",
            # x_lab = "celkové vážené RFM skóre",
            # my_main = "histogram celkového váženého RFM skóre",
            # my_colour = "salmon2",
            # xlim = c(
                # 0,
                # max(
                    # 3
                # )
            # )
            
        # )
        
    # })
    
    
    ## heatmapy ---------------------------------------------------------------
    
    # output$my_transaction_monetary_vs_recency_heatmap <- renderPlot(
        
        # {
            
            # getMyHeatmap(
                
                # my_data = my_transaction_final_data(),
                # my_first_variable = "recency segment",
                # my_second_variable = "monetary segment",
                # x_lab = "segment proměnné recency",
                # y_lab = "segment proměnné monetary",
                # my_main = "monetary vs. recency heatmapa",
                # my_colours = brewer.pal(
                    # 9,
                    # "Blues"
                # )
                
            # )
            
        # }
        
    # )
    
    # output$my_transaction_monetary_vs_frequency_heatmap <- renderPlot(
        
        # {
            
            # getMyHeatmap(
                
                # my_data = my_transaction_final_data(),
                # my_first_variable = "frequency segment",
                # my_second_variable = "monetary segment",
                # x_lab = "segment proměnné frequency",
                # y_lab = "segment proměnné monetary",
                # my_main = "monetary vs. frequency heatmapa",
                # my_colours = brewer.pal(
                    # 9,
                    # "Blues"
                # )
                
            # )
            
        # }
        
    # )
    
    # output$my_transaction_recency_vs_frequency_heatmap <- renderPlot(
        
        # {
            
            # getMyHeatmap(
                
                # my_data = my_transaction_final_data(),
                # my_first_variable = "frequency segment",
                # my_second_variable = "recency segment",
                # x_lab = "segment proměnné frequency",
                # y_lab = "segment proměnné recency",
                # my_main = "recency vs. frequency heatmapa",
                # my_colours = brewer.pal(
                    # 9,
                    # "Blues"
                # )
                
            # )
            
        # }
        
    # )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    # output$my_model_table <- renderDataTable(
        
        # {
            
            # if(
                # !input$to_use_inbuilt_data
            # ){
                
                # return(NULL)
                
            # }
            
            # setNames(
                # my_model_data()[
                    # ,
                    # c(
                        # "model",
                        # input$my_model_variable
                    # )
                # ],
                # c(
                    # "model",
                    # if(
                        # input$my_model_variable == "predikce_celkem"
                    # ){
                        # "predikce celkem (Kč)"
                    # }else{
                        
                        # if(
                            # input$my_model_variable == "predikce_na_klienta"
                        # ){
                            # "predikce na klienta (Kč)"
                        # }
                        
                    # }
                # )
            # )
            
        # },
        # options = list(
            
            # scrollX = TRUE,
            # #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            # paging = FALSE,  # paging = TRUE vede na stránkování tabulky
            # language = list(
                # url = "czech_for_data_table.json"
            # )
            
        # )
        
    # )
    
    
    # output$my_model_plot <- renderPlot(
        
        # {
            
            # getMyModelPlot(
                
                # my_data = my_model_data(),
                # my_variable = input$my_model_variable,
                # x_lab = "modely",
                # y_lab = if(
                    # input$my_model_variable == "predikce_celkem"
                # ){
                    # "predikce celkem (Kč)"
                # }else{
                    
                    # if(
                        # input$my_model_variable == "predikce_na_klienta"
                    # ){
                        # "predikce na klienta (Kč)"
                    # }
                    
                # },
                # my_main = "predikce daná modely",
                # x_names = my_model_data()[, "model"]
                
            # )
            
        # }
        
    # )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## vytvářím dataset mergovaných transakčních a netransakčních dat
    ## vhodný pro zobrazování -------------------------------------------------
    
    my_non_transaction_display_data <- reactive({
        
        ## merguji transakční data s netransakčními daty ----------------------
        
        my_data <- with(
            
            temp_data <- merge(
                x = my_transaction_data(),
                y = my_non_transaction_data(),
                by = "Date"
            ),
            
            temp_data <- temp_data[
                order(as.numeric(temp_data[, "ID"]))
                ,
            ]
            
        )
        
        
        # return(my_data)
        
        ## upravuji nyní "my_data" ve smyslu nových proměnných
        ## recency, frequency, monetary a annual_amount -----------------------
        
        my_table <- NULL
        
        for(my_id in unique(my_data[, "ID"])){
            
            # my_row <- c(
                
                # "ratio_of_visited_events_to_all_events" = length(
                    # unique(
                        # my_data[
                            # my_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # )
                # ) / length(
                    # unique(
                        # my_data[, "Date"]
                    # )
                # )
                
            # )
            
            # for(my_variable in c(
                
                # "WeekDay",
                # "KnownArtist",
                # "MajorEra",
                # "MinorEra",
                # "ConcertType"
                
            # )){
                
                # for(
                    # my_level in unique(
                        # my_data[, my_variable]
                    # )
                # ){
                    
                    # my_row <- c(
                        
                        # my_row,
                        # length(
                            # which(
                                # my_data[
                                    # my_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # my_data[
                                # my_data[, "ID"] == my_id,
                                # my_variable
                            # ]
                        # ),
                        # length(
                            # which(
                                # my_data[
                                    # my_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # which(
                                # my_data[
                                    # ,
                                    # my_variable
                                # ] == my_level
                            # )
                        # )
                        
                    # )
                    
                    # names(my_row)[
                        # c(
                            # length(my_row) - 1,
                            # length(my_row)
                        # )
                    # ] <- gsub(
                        
                        # "[ |-]",
                        # "_",
                        # c(
                            
                            # paste(
                                # "ratio_of_",
                                # my_level,
                                # "_to_all_",
                                # my_variable,
                                # "_levels_within_ID",
                                # sep = ""
                            # ),
                            # paste(
                                # "ratio_to_all_",
                                # my_level,
                                # "_of_",
                                # my_variable,
                                # "_within_dataset",
                                # sep = ""
                            # )
                            
                        # )
                        
                    # )
                    
                # }
                
            # }
            
            
            ## doplňuji k datům sloupec s RFM skóre ---------------------------
            
            my_table <- rbind(
                
                my_table,
                c(
                    "ID" = my_id,
                    "annual_amount" = sum(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Price"
                        ] * my_data[
                            my_data[, "ID"] == my_id,
                            "Amount"
                        ]
                    ) / (
                        as.numeric(
                            max(
                                my_data[
                                    ,
                                    "Date"
                                ],
                                na.rm = TRUE
                            ) - min(
                                my_data[
                                    ,
                                    "Date"
                                ],
                                na.rm = TRUE
                            )
                        ) / 365.25
                    ),
                    # my_row,
                    "recency" = min(
                        as.numeric(
                            right_censoring() -
                            my_data[
                                my_data[, "ID"] == my_id,
                                "Date"
                            ]
                        ),
                        na.rm = TRUE
                    ),
                    "frequency" = length(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Date"
                        ]
                    ),
                    "monetary" = sum(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Price"
                        ] * my_data[
                            my_data[, "ID"] == my_id,
                            "Amount"
                        ]
                    )
                    # / sum(
                        # my_data[
                            # my_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                )
                
            )
            
        }
        
        my_table <- data.frame(
            
            my_table,
            stringsAsFactors = FALSE
            
        )
        
        for(my_variable in colnames(my_table)){
            
            if(my_variable != "ID"){
                
                my_table[, my_variable] <- as.numeric(
                    
                    my_table[, my_variable]
                    
                )
                
            }
            
        }
        
        
        ## vytvářím kategorickou závisle proměnnou ----------------------------
        
        # my_table <- data.frame(
            
            # my_table,
            # "output_label" = ifelse(
                # my_table[, "annual_amount"] <= quantile(
                    # my_table[, "annual_amount"],
                    # probs = 1 / 3,
                    # names = FALSE,
                    # na.rm = TRUE
                # ),
                # "low",
                # ifelse(
                    # my_table[, "annual_amount"] <= quantile(
                        # my_table[, "annual_amount"],
                        # probs = 2 / 3,
                        # names = FALSE,
                        # na.rm = TRUE
                    # ),
                    # "middle",
                    # "high"
                # )
            # ),
            # stringsAsFactors = FALSE
            
        # )
        
        
        ## vynechávámm proměnné, které jsou kompletně populované chybějícími
        ## hodnotami ----------------------------------------------------------
        
        # to_remove <- NULL
        
        # for(my_variable in colnames(my_table)){
            
            # if(
                # all(
                    # is.na(my_table[, my_variable])
                # )
            # ){
                
                # to_remove <- c(to_remove, my_variable)
                
            # }
            
        # }
        
        # my_table <- my_table[
            # ,
            # setdiff(
                # colnames(my_table),
                # to_remove
            # )
        # ]
        
        
        ## počítám recency, frequency a monetary skóre ------------------------
        
        recency_score <- unlist(
            lapply(
                1:dim(my_table)[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_table[i, "recency"],
                        categories_cut_points =
                            my_transaction_recency_cut_points(),
                        categories_labels = as.character(
                            3:1
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        frequency_score <- unlist(
            lapply(
                1:dim(my_table)[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_table[i, "frequency"],
                        categories_cut_points =
                            my_transaction_frequency_cut_points(),
                        categories_labels = as.character(
                            1:3
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        monetary_score <- unlist(
            lapply(
                1:dim(my_table)[1],
                function(i){
                    
                    getMyCategory(
                        
                        x = my_table[i, "monetary"],
                        categories_cut_points =
                            my_transaction_monetary_cut_points(),
                        categories_labels = as.character(
                            1:3
                        ),
                        return_label = TRUE
                        
                    )
                    
                }
                
            )
        )
        
        
        my_table <- data.frame(
            
            my_table,
            "recency segment" = recency_score,
            "frequency segment" = frequency_score,
            "monetary segment" = monetary_score,
            "celkove vazene skore" = (
                
                as.numeric(recency_score) * (
                    input$my_transaction_weights_slider[1]
                ) +
                as.numeric(frequency_score) * (
                    input$my_transaction_weights_slider[2] -
                    input$my_transaction_weights_slider[1]
                ) +
                as.numeric(monetary_score) * (
                    1 - input$my_transaction_weights_slider[2]
                )
                
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
            
        )
        
        for(my_variable in colnames(my_table)){
            
            my_table[, my_variable] <- as.character(
                
                my_table[, my_variable]
                
            )
            
        }
        
        for(my_variable in colnames(my_table)){
            
            if(my_variable != "ID"){
                
                my_table[, my_variable] <- as.numeric(
                    
                    my_table[, my_variable]
                    
                )
                
            }
            
        }
        
        
        ## vytvářím výstup ----------------------------------------------------
        
        return(
            with(
                
                temp_data <- merge(
                    x = my_data,
                    y = my_table,
                    by = "ID"
                ),
                
                temp_data <- temp_data[
                    order(as.numeric(temp_data[, "ID"]))
                    ,
                ]
                
            )
            
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## diagramy ---------------------------------------------------------------
    
    #### diagramy pro záložku "Segmentace zákazníků" --------------------------
    
    ###### malé piecharty -----------------------------------------------------
    
    output$my_transaction_top_k_percent_piechart <- renderPlot({
        
        par(mar = c(4.1, 4.1, 4.1, 4.1))
        
        pie(
            c(
                input$my_transaction_top_k_percent_slider,
                100 - input$my_transaction_top_k_percent_slider
            ),
            labels = paste(
                c(
                    input$my_transaction_top_k_percent_slider,
                    100 - input$my_transaction_top_k_percent_slider
                ),
                " %",
                sep = ""
            ),
            col = c(
                "#f4646c",
                "lightgrey"
            ),
            main = "Zvolené procento klíčových zákazníků",
            cex.main = 0.90#0.75
        )
        
    })
    
    output$my_transaction_top_k_percent_amount_piechart <- renderPlot({
        
        par(mar = c(4.1, 4.1, 4.1, 4.1))
        
        pie(
            c(
                sum(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ][
                        order(
                            my_transaction_final_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_transaction_top_k_percent_slider /
                                    100
                                )
                            )
                        ]
                    ]
                ) / sum(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ]
                ),
                1 - sum(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ][
                    order(
                            my_transaction_final_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_transaction_top_k_percent_slider /
                                    100
                                )
                            )
                        ]
                    ]
                ) / sum(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ]
                )
            ),
            labels = paste(
                round(
                    c(
                        sum(
                            my_transaction_final_data()[
                                ,
                                "monetary"
                            ][
                                order(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_transaction_final_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_transaction_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ]
                            ]
                        ) / sum(
                            my_transaction_final_data()[
                                ,
                                "monetary"
                            ]
                        ) * 100,
                        100 - sum(
                            my_transaction_final_data()[
                                ,
                                "monetary"
                            ][
                                order(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_transaction_final_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_transaction_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ]
                            ]
                        ) / sum(
                            my_transaction_final_data()[
                                ,
                                "monetary"
                            ]
                        ) * 100
                    ),
                    digits = 0
                ),
                " %",
                sep = ""
            ),
            col = c(
                "#f4646c",
                "lightgrey"
            ),
            main = "generuje uvedené procento příjmů.",
            cex.main = 0.90#0.75
        )
        
    })
    
    
    ###### histogram celkového skóre ------------------------------------------
    
    # output$pokus <- renderPrint({
        
        # str(my_transaction_final_data())
        
    # })
    
    output$my_transaction_recency_introduction_text <- renderText({
        
        paste(
            "Vaši zákazníci v průměru nakoupili naposledy před ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "recency"
                    ]
                ),
                digits = 0
            ),
            " dny. ",
            "Nejlepších ",
            input$my_transaction_top_k_percent_slider,
            " % zákazníků dle RFM skóre v průměru nakoupilo naposledy před ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "recency"
                    ][
                        order(
                            my_transaction_final_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_transaction_top_k_percent_slider /
                                    100
                                )
                            )
                        ]
                    ]
                ),
                digits = 0
            ),
            " dny.",
            sep = ""
        )
        
    })
    
    output$my_transaction_recency_introduction_text_handler <- renderUI({
        
        if(
            !input$to_use_my_transaction_inbuilt_data &
            is.null(input$my_transaction_file)
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_transaction_recency_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_transaction_recency_introduction_text_info",
                title = "Info",
                content = "Sloupce šedé barvy značí celek, sloupce červené barvy značí vybrané procento nejlepších zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_transaction_recency_quantitative_barplot <- renderPlot({
        
        getMyQuantitativeBarplot(
            
            my_data = my_transaction_final_data(),
            my_variable = "recency",
            n_of_bars = 8,
            which_to_add = order(
                my_transaction_final_data()[
                    ,
                    "celkove vazene skore"
                ],
                decreasing = TRUE
            )[
                1:floor(
                    length(
                        my_transaction_final_data()[
                            ,
                            "celkove vazene skore"
                        ]
                    ) * (
                        input$my_transaction_top_k_percent_slider /
                        100
                    )
                )
            ],
            my_main = "Sloupcový diagram proměnné recency",
            my_first_colour = "lightgrey",
            my_second_colour = "#f4646c",
            my_xlab = "počet dní od poslední transakce",
            my_ylab = "počet zákazníků"
            
        )
        
    })
    
    # output$my_transaction_recency_double_histogram <- renderPlot({
        
        # getMyDoubleHistogram(
            
            # my_data = my_transaction_final_data(),
            # my_variable = "recency",
            # which_to_add = which(
                # my_transaction_final_data()[
                    # ,
                    # "celkove vazene skore"
                # ] >= quantile(
                    # x = my_transaction_final_data()[
                        # ,
                        # "celkove vazene skore"
                    # ],
                    # probs = (
                        # 100 - input$my_transaction_top_k_percent_slider
                    # ) / 100,
                    # na.rm = TRUE,
                    # names = FALSE
                # )
            # ),
            # x_lab = "recency [dny]",
            # my_main = "histogram proměnné recency",
            # my_first_colour = "lightgrey",
            # my_second_colour = "#f4646c"#,
            # # my_breaks = log2(
                # # length(
                    # # my_data[, my_variable]
                # # )
            # # )
            
        # )
        
    # })
    
    output$my_transaction_frequency_introduction_text <- renderText({
        
        paste(
            "Vaši zákazníci v průměru nakoupili za dané období ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "frequency"
                    ]
                ),
                digits = 1
            ),
            "-krát. ",
            "Nejlepších ",
            input$my_transaction_top_k_percent_slider,
            " % zákazníků dle RFM skóre v průměru nakoupilo za dané období ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "frequency"
                    ][
                        order(
                            my_transaction_final_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_transaction_top_k_percent_slider /
                                    100
                                )
                            )
                        ]
                    ]
                ),
                digits = 1
            ),
            "-krát.",
            sep = ""
        )
        
    })
    
    output$my_transaction_frequency_introduction_text_handler <- renderUI({
        
        if(
            !input$to_use_my_transaction_inbuilt_data &
            is.null(input$my_transaction_file)
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_transaction_frequency_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_transaction_frequency_introduction_text_info",
                title = "Info",
                content = "Sloupce šedé barvy značí celek, sloupce červené barvy značí vybrané procento nejlepších zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_transaction_frequency_quantitative_barplot <- renderPlot({
        
        getMyQuantitativeBarplot(
            
            my_data = my_transaction_final_data(),
            my_variable = "frequency",
            n_of_bars = 8,
            which_to_add = order(
                my_transaction_final_data()[
                    ,
                    "celkove vazene skore"
                ],
                decreasing = TRUE
            )[
                1:floor(
                    length(
                        my_transaction_final_data()[
                            ,
                            "celkove vazene skore"
                        ]
                    ) * (
                        input$my_transaction_top_k_percent_slider /
                        100
                    )
                )
            ],
            my_main = "Sloupcový diagram proměnné frequency",
            my_first_colour = "lightgrey",
            my_second_colour = "#f4646c",
            my_xlab = "počet transakcí za dané období",
            my_ylab = "počet zákazníků"
            
        )
        
    })
    
    # output$my_transaction_frequency_double_histogram <- renderPlot({
        
        # getMyDoubleHistogram(
            
            # my_data = my_transaction_final_data(),
            # my_variable = "frequency",
            # which_to_add = which(
                # my_transaction_final_data()[
                    # ,
                    # "celkove vazene skore"
                # ] >= quantile(
                    # x = my_transaction_final_data()[
                        # ,
                        # "celkove vazene skore"
                    # ],
                    # probs = (
                        # 100 - input$my_transaction_top_k_percent_slider
                    # ) / 100,
                    # na.rm = TRUE,
                    # names = FALSE
                # )
            # ),
            # x_lab = "počet nákupů za sledované období",
            # my_main = "histogram proměnné frequency",
            # my_first_colour = "lightgrey",
            # my_second_colour = "#f4646c"#,
            # # my_breaks = log2(
                # # length(
                    # # my_data[, my_variable]
                # # )
            # # )
            
        # )
        
    # })
    
    output$my_transaction_monetary_introduction_text <- renderText({
        
        paste(
            "Vaši zákazníci v průměru nakoupili za dané období za ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ]
                ),
                digits = 0
            ),
            " Kč. ",
            "Nejlepších ",
            input$my_transaction_top_k_percent_slider,
            " % zákazníků dle RFM skóre v průměru nakoupilo za dané období za ",
            round(
                mean(
                    my_transaction_final_data()[
                        ,
                        "monetary"
                    ][
                        order(
                            my_transaction_final_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_transaction_final_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_transaction_top_k_percent_slider /
                                    100
                                )
                            )
                        ]
                    ]
                ),
                digits = 0
            ),
            " Kč.",
            sep = ""
        )
        
    })
    
    output$my_transaction_monetary_introduction_text_handler <- renderUI({
        
        if(
            !input$to_use_my_transaction_inbuilt_data &
            is.null(input$my_transaction_file)
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_transaction_monetary_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_transaction_monetary_introduction_text_info",
                title = "Info",
                content = "Sloupce šedé barvy značí celek, sloupce červené barvy značí vybrané procento nejlepších zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_transaction_monetary_quantitative_barplot <- renderPlot({
        
        getMyQuantitativeBarplot(
            
            my_data = my_transaction_final_data(),
            my_variable = "monetary",
            n_of_bars = 8,
            which_to_add = order(
                my_transaction_final_data()[
                    ,
                    "celkove vazene skore"
                ],
                decreasing = TRUE
            )[
                1:floor(
                    length(
                        my_transaction_final_data()[
                            ,
                            "celkove vazene skore"
                        ]
                    ) * (
                        input$my_transaction_top_k_percent_slider /
                        100
                    )
                )
            ],
            my_main = "Sloupcový diagram proměnné monetary",
            my_first_colour = "lightgrey",
            my_second_colour = "#f4646c",
            my_xlab = "úhrná hodnota transakcí za dané období",
            my_ylab = "počet zákazníků"
            
        )
        
    })
    
    # output$my_transaction_monetary_double_histogram <- renderPlot({
        
        # getMyDoubleHistogram(
            
            # my_data = my_transaction_final_data(),
            # my_variable = "monetary",
            # which_to_add = which(
                # my_transaction_final_data()[
                    # ,
                    # "celkove vazene skore"
                # ] >= quantile(
                    # x = my_transaction_final_data()[
                        # ,
                        # "celkove vazene skore"
                    # ],
                    # probs = (
                        # 100 - input$my_transaction_top_k_percent_slider
                    # ) / 100,
                    # na.rm = TRUE,
                    # names = FALSE
                # )
            # ),
            # x_lab = "nakoupená roční částka [Kč]",
            # my_main = "histogram proměnné monetary",
            # my_first_colour = "lightgrey",
            # my_second_colour = "#f4646c"#,
            # # my_breaks = log2(
                # # length(
                    # # my_data[, my_variable]
                # # )
            # # )
            
        # )
        
    # })
    
    output$my_transaction_total_RFM_score_histogram <- renderPlot({
        
        getMyHistogram(
            
            my_data = my_transaction_final_data(),
            my_variable = "celkove vazene skore",
            x_lab = "celkové vážené RFM skóre",
            my_main = "Rozložení celkového váženého RFM skóre",
            my_colour = "lightgrey",#"#f4646c",
            xlim = c(
                0,
                max(
                    3
                )
            )
            
        )
        
    })
    
    
    #### diagramy pro záložku "Preference klíčového zákazníka" ----------------
    
    # output$pokus <- renderPrint({head(my_non_transaction_display_data(), 20)})
    
    output$my_preferences_weekday_introduction_text <- renderText({
        
        paste(
            "Nejvyšší podíl vstupenek zakoupených ",
            input$my_preferences_top_k_percent_slider,
            " % nejlepších (klíčových) zákazníků dle RFM skóre je ",
            "v kategorii '",
            names(
                table(
                    my_non_transaction_display_data()[
                        ,
                        "WeekDay"
                    ]
                )
            )[which.max(
                table(
                    my_non_transaction_display_data()[
                        order(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_preferences_top_k_percent_slider /
                                    100
                                )
                            )
                        ],
                        "WeekDay"
                    ]
                ) / table(
                    my_non_transaction_display_data()[
                        ,
                        "WeekDay"
                    ]
                )
            )],
            "' a tvoří ",
            format(
                round(
                    max(
                        table(
                            my_non_transaction_display_data()[
                                order(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_non_transaction_display_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_preferences_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ],
                                "WeekDay"
                            ]
                        ) / table(
                            my_non_transaction_display_data()[
                                ,
                                "WeekDay"
                            ]
                        ) * 100
                    ),
                    digits = 1
                ),
                nsmall = 1
            ),
            " % z celkového počtu prodaných vstupenek v dané kategorii.",
            sep = ""
        )
        
    })
    
    output$my_preferences_weekday_introduction_text_info_handler <- renderUI({
        
        if(
            (
                !input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                !input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_preferences_weekday_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_preferences_weekday_introduction_text_info",
                title = "Info",
                content = "Sloupce značí, kolik procent ze všech prodaných vstupenek dané kategorie zakoupilo vybrané procento nejlepších (klíčových) zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_preferences_weekday_qualitative_barplot <- renderPlot({
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            getMyQualitativeBarplot(
                
                my_data = my_non_transaction_display_data(),
                my_variable = "WeekDay",
                which_to_add = order(
                    my_non_transaction_display_data()[
                        ,
                        "celkove vazene skore"
                    ],
                    decreasing = TRUE
                )[
                    1:floor(
                        length(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ]
                        ) * (
                            input$my_preferences_top_k_percent_slider /
                            100
                        )
                    )
                ],
                my_main = "Sloupcový diagram proměnné WeekDay",
                # my_first_colour = "lightgrey",
                # my_second_colour = "#f4646c",
                my_colour = "#f4646c",
                my_sorting_vector = c(
                    "pondělí",
                    "úterý",
                    "středa",
                    "čtvrtek",
                    "pátek",
                    "sobota",
                    "neděle"
                ),
                my_xlab = "den v týdnu, kdy se akce konala",
                my_ylab = "počet transakcí"
                
            )
            
        }else{
            
            NULL
            
        }
        
    })
    
    
    output$my_preferences_knownartist_introduction_text <- renderText({
        
        paste(
            "Nejvyšší podíl vstupenek zakoupených ",
            input$my_preferences_top_k_percent_slider,
            " % nejlepších (klíčových) zákazníků dle RFM skóre je ",
            "v kategorii '",
            names(
                table(
                    my_non_transaction_display_data()[
                        ,
                        "KnownArtist"
                    ]
                )
            )[which.max(
                table(
                    my_non_transaction_display_data()[
                        order(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_preferences_top_k_percent_slider /
                                    100
                                )
                            )
                        ],
                        "KnownArtist"
                    ]
                ) / table(
                    my_non_transaction_display_data()[
                        ,
                        "KnownArtist"
                    ]
                )
            )],
            "' a tvoří ",
            format(
                round(
                    max(
                        table(
                            my_non_transaction_display_data()[
                                order(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_non_transaction_display_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_preferences_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ],
                                "KnownArtist"
                            ]
                        ) / table(
                            my_non_transaction_display_data()[
                                ,
                                "KnownArtist"
                            ]
                        ) * 100
                    ),
                    digits = 1
                ),
                nsmall = 1
            ),
            " % z celkového počtu prodaných vstupenek v dané kategorii.",
            sep = ""
        )
        
    })
    
    output$my_preferences_knownartist_introduction_text_info_handler <- renderUI({
        
        if(
            (
                !input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                !input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_preferences_knownartist_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_preferences_knownartist_introduction_text_info",
                title = "Info",
                content = "Sloupce značí, kolik procent ze všech prodaných vstupenek dané kategorie zakoupilo vybrané procento nejlepších (klíčových) zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_preferences_knownartist_qualitative_barplot <- renderPlot({
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            getMyQualitativeBarplot(
                
                my_data = my_non_transaction_display_data(),
                my_variable = "KnownArtist",
                which_to_add = order(
                    my_non_transaction_display_data()[
                        ,
                        "celkove vazene skore"
                    ],
                    decreasing = TRUE
                )[
                    1:floor(
                        length(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ]
                        ) * (
                            input$my_preferences_top_k_percent_slider /
                            100
                        )
                    )
                ],
                my_main = "Sloupcový diagram proměnné KnownArtist",
                # my_first_colour = "lightgrey",
                # my_second_colour = "#f4646c",
                my_colour = "#f4646c",
                my_sorting_vector = c(
                    "známý",
                    "méně známý",
                    "neznámý"
                ),
                my_xlab = "odhadnutá známost interpreta",
                my_ylab = "počet transakcí"
                
            )
            
        }else{
            
            NULL
            
        }
        
    })
    
    
    output$my_preferences_majorera_introduction_text <- renderText({
        
        paste(
            "Nejvyšší podíl vstupenek zakoupených ",
            input$my_preferences_top_k_percent_slider,
            " % nejlepších (klíčových) zákazníků dle RFM skóre je ",
            "v kategorii '",
            names(
                table(
                    my_non_transaction_display_data()[
                        ,
                        "MajorEra"
                    ]
                )
            )[which.max(
                table(
                    my_non_transaction_display_data()[
                        order(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_preferences_top_k_percent_slider /
                                    100
                                )
                            )
                        ],
                        "MajorEra"
                    ]
                ) / table(
                    my_non_transaction_display_data()[
                        ,
                        "MajorEra"
                    ]
                )
            )],
            "' a tvoří ",
            format(
                round(
                    max(
                        table(
                            my_non_transaction_display_data()[
                                order(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_non_transaction_display_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_preferences_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ],
                                "MajorEra"
                            ]
                        ) / table(
                            my_non_transaction_display_data()[
                                ,
                                "MajorEra"
                            ]
                        ) * 100
                    ),
                    digits = 1
                ),
                nsmall = 1
            ),
            " % z celkového počtu prodaných vstupenek v dané kategorii.",
            sep = ""
        )
        
    })
    
    output$my_preferences_majorera_introduction_text_info_handler <- renderUI({
        
        if(
            (
                !input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                !input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_preferences_majorera_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_preferences_majorera_introduction_text_info",
                title = "Info",
                content = "Sloupce značí, kolik procent ze všech prodaných vstupenek dané kategorie zakoupilo vybrané procento nejlepších (klíčových) zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_preferences_majorera_qualitative_barplot <- renderPlot({
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            getMyQualitativeBarplot(
                
                my_data = my_non_transaction_display_data(),
                my_variable = "MajorEra",
                which_to_add = order(
                    my_non_transaction_display_data()[
                        ,
                        "celkove vazene skore"
                    ],
                    decreasing = TRUE
                )[
                    1:floor(
                        length(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ]
                        ) * (
                            input$my_preferences_top_k_percent_slider /
                            100
                        )
                    )
                ],
                my_main = "Sloupcový diagram proměnné MajorEra",
                # my_first_colour = "lightgrey",
                # my_second_colour = "#f4646c",
                my_colour = "#f4646c",
                my_xlab = "hlavní žánr akce",
                my_ylab = "počet transakcí"
                
            )
            
        }else{
            
            NULL
            
        }
        
    })
    
    
    output$my_preferences_minorera_introduction_text <- renderText({
        
        paste(
            "Nejvyšší podíl vstupenek zakoupených ",
            input$my_preferences_top_k_percent_slider,
            " % nejlepších (klíčových) zákazníků dle RFM skóre je ",
            "v kategorii '",
            names(
                table(
                    my_non_transaction_display_data()[
                        ,
                        "MinorEra"
                    ]
                )
            )[which.max(
                table(
                    my_non_transaction_display_data()[
                        order(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_preferences_top_k_percent_slider /
                                    100
                                )
                            )
                        ],
                        "MinorEra"
                    ]
                ) / table(
                    my_non_transaction_display_data()[
                        ,
                        "MinorEra"
                    ]
                )
            )],
            "' a tvoří ",
            format(
                round(
                    max(
                        table(
                            my_non_transaction_display_data()[
                                order(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_non_transaction_display_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_preferences_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ],
                                "MinorEra"
                            ]
                        ) / table(
                            my_non_transaction_display_data()[
                                ,
                                "MinorEra"
                            ]
                        ) * 100
                    ),
                    digits = 1
                ),
                nsmall = 1
            ),
            " % z celkového počtu prodaných vstupenek v dané kategorii.",
            sep = ""
        )
        
    })
    
    output$my_preferences_minorera_introduction_text_info_handler <- renderUI({
        
        if(
            (
                !input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                !input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_preferences_minorera_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_preferences_minorera_introduction_text_info",
                title = "Info",
                content = "Sloupce značí, kolik procent ze všech prodaných vstupenek dané kategorie zakoupilo vybrané procento nejlepších (klíčových) zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_preferences_minorera_qualitative_barplot <- renderPlot({
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            getMyQualitativeBarplot(
                
                my_data = my_non_transaction_display_data(),
                my_variable = "MinorEra",
                which_to_add = order(
                    my_non_transaction_display_data()[
                        ,
                        "celkove vazene skore"
                    ],
                    decreasing = TRUE
                )[
                    1:floor(
                        length(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ]
                        ) * (
                            input$my_preferences_top_k_percent_slider /
                            100
                        )
                    )
                ],
                my_main = "Sloupcový diagram proměnné MinorEra",
                # my_first_colour = "lightgrey",
                # my_second_colour = "#f4646c",
                my_colour = "#f4646c",
                my_xlab = "vedlejší žánr akce",
                my_ylab = "počet transakcí",
                
            )
            
        }else{
            
            NULL
            
        }
        
    })
    
    
    output$my_preferences_concerttype_introduction_text <- renderText({
        
        paste(
            "Nejvyšší podíl vstupenek zakoupených ",
            input$my_preferences_top_k_percent_slider,
            " % nejlepších (klíčových) zákazníků dle RFM skóre je ",
            "v kategorii '",
            names(
                table(
                    my_non_transaction_display_data()[
                        ,
                        "ConcertType"
                    ]
                )
            )[which.max(
                table(
                    my_non_transaction_display_data()[
                        order(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ],
                            decreasing = TRUE
                        )[
                            1:floor(
                                length(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ]
                                ) * (
                                    input$my_preferences_top_k_percent_slider /
                                    100
                                )
                            )
                        ],
                        "ConcertType"
                    ]
                ) / table(
                    my_non_transaction_display_data()[
                        ,
                        "ConcertType"
                    ]
                )
            )],
            "' a tvoří ",
            format(
                round(
                    max(
                        table(
                            my_non_transaction_display_data()[
                                order(
                                    my_non_transaction_display_data()[
                                        ,
                                        "celkove vazene skore"
                                    ],
                                    decreasing = TRUE
                                )[
                                    1:floor(
                                        length(
                                            my_non_transaction_display_data()[
                                                ,
                                                "celkove vazene skore"
                                            ]
                                        ) * (
                                            input$my_preferences_top_k_percent_slider /
                                            100
                                        )
                                    )
                                ],
                                "ConcertType"
                            ]
                        ) / table(
                            my_non_transaction_display_data()[
                                ,
                                "ConcertType"
                            ]
                        ) * 100
                    ),
                    digits = 1
                ),
                nsmall = 1
            ),
            " % z celkového počtu prodaných vstupenek v dané kategorii.",
            sep = ""
        )
        
    })
    
    output$my_preferences_concerttype_introduction_text_info_handler <- renderUI({
        
        if(
            (
                !input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                !input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        list(
            bsButton(
                inputId =
                    "my_preferences_concerttype_introduction_text_info",
                label = "",
                icon = icon("info"),
                style = "info",
                size = "extra-small"
            ),
            bsPopover(
                id = "my_preferences_concerttype_introduction_text_info",
                title = "Info",
                content = "Sloupce značí, kolik procent ze všech prodaných vstupenek dané kategorie zakoupilo vybrané procento nejlepších (klíčových) zákazníků dle RFM skóre.",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
            )
        )
        
    })
    
    output$my_preferences_concerttype_qualitative_barplot <- renderPlot({
        
        if(
            input$to_use_my_transaction_inbuilt_data |
            ! is.null(input$my_transaction_file)
        ){
            
            getMyQualitativeBarplot(
                
                my_data = my_non_transaction_display_data(),
                my_variable = "ConcertType",
                which_to_add = order(
                    my_non_transaction_display_data()[
                        ,
                        "celkove vazene skore"
                    ],
                    decreasing = TRUE
                )[
                    1:floor(
                        length(
                            my_non_transaction_display_data()[
                                ,
                                "celkove vazene skore"
                            ]
                        ) * (
                            input$my_preferences_top_k_percent_slider /
                            100
                        )
                    )
                ],
                my_main = "Sloupcový diagram proměnné ConcertType",
                # my_first_colour = "lightgrey",
                # my_second_colour = "#f4646c",
                my_colour = "#f4646c",
                my_xlab = "koncertní typ akce",
                my_ylab = "počet transakcí"
                
            )
            
        }else{
            
            NULL
            
        }
        
    })
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## CLV analýza pouze transakčních dat -------------------------------------
    
    #### vytvářím slider pro nastavení training a testing set -----------------
    
    # observe(
        
        # if(
            # input$to_use_my_transaction_inbuilt_data |
            # ! is.null(input$my_transaction_file)
        # ){
            
            # output$my_transaction_train_vs_test_set_slider <- renderUI(
                
                # {
                    
                    # sliderInput(
                        # inputId = "my_transaction_train_vs_test_set_cutoff",
                        # label = "Rozsah trénovací množiny",
                        # min = min(
                            # my_transaction_data()[, "Date"],
                            # na.rm = TRUE
                        # ),
                        # max = max(
                            # my_transaction_data()[, "Date"],
                            # na.rm = TRUE
                        # ),
                        # value = (
                            # min(
                                # my_transaction_data()[, "Date"],
                                # na.rm = TRUE
                            # ) + 0.8 * as.numeric(
                                # max(
                                    # my_transaction_data()[, "Date"],
                                    # na.rm = TRUE
                                # ) - min(
                                    # my_transaction_data()[, "Date"],
                                    # na.rm = TRUE
                                # )
                            # )
                        # ),
                        # width = "50%",
                        # timeFormat = "%d. %m. %Y"
                    # )
                    
                # }
                
            # )
            
        # }else{
            
            # output$my_transaction_train_vs_test_set_slider <- renderUI(
                
                # {
                    
                    # NULL
                    
                # }
                
            # )
            
        # }
        
    # )
    
    
    ## tvořím trénovací a testovací množinu -----------------------------------
    
    # my_transaction_predictions <- reactive({
        
        # ## vytvářím pouze podmnožinu trénovacích a testovacích dat ------------
        
        # my_train_data <- my_transaction_data()[
            
            # my_transaction_data()[, "Date"] <
                # input$my_transaction_train_vs_test_set_cutoff
            # ,
            
        # ]
        
        # my_test_data <- my_transaction_data()[
            
            # my_transaction_data()[, "Date"] >=
                # input$my_transaction_train_vs_test_set_cutoff
            # ,
            
        # ]
        
        
        # ## upravuji nyní "my_train_data" ve smyslu nových proměnných
        # ## recency, frequency, monetary a annual_amount -----------------------
        
        # my_train_table <- NULL
        
        # for(my_id in unique(my_train_data[, "ID"])){
            
            # my_train_table <- rbind(
                
                # my_train_table,
                # c(
                    # "ID" = my_id,
                    # "recency" = min(
                        # as.numeric(
                            # input$my_transaction_train_vs_test_set_cutoff -
                            # my_train_data[
                                # my_train_data[, "ID"] == my_id,
                                # "Date"
                            # ]
                        # ),
                        # na.rm = TRUE
                    # ),
                    # "frequency" = length(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # ),
                    # "monetary" = sum(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                    # # / sum(
                        # # my_train_data[
                            # # my_train_data[, "ID"] == my_id,
                            # # "Amount"
                        # # ]
                    # # )
                    # ,
                    # "annual_amount" = sum(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # ) / (
                        # as.numeric(
                            # max(
                                # my_train_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - min(
                                # my_train_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # )
                        # ) / 365.25
                    # )
                # )
                
            # )
            
        # }
        
        # my_train_table <- data.frame(
            
            # my_train_table,
            # stringsAsFactors = FALSE
            
        # )
        
        # for(my_variable in c(
            
            # "recency",
            # "frequency",
            # "monetary",
            # "annual_amount"
            
        # )){
            
            # my_train_table[, my_variable] <- as.numeric(
                
                # my_train_table[, my_variable]
                
            # )
            
        # }
        
        
        # ## vytvářím kategorickou závisle proměnnou ----------------------------
        
        # my_train_table <- data.frame(
            
            # my_train_table,
            # "output_label" = ifelse(
                # my_train_table[, "annual_amount"] <= quantile(
                    # my_train_table[, "annual_amount"],
                    # probs = 1 / 3,
                    # names = FALSE,
                    # na.rm = TRUE
                # ),
                # "low",
                # ifelse(
                    # my_train_table[, "annual_amount"] <= quantile(
                        # my_train_table[, "annual_amount"],
                        # probs = 2 / 3,
                        # names = FALSE,
                        # na.rm = TRUE
                    # ),
                    # "middle",
                    # "high"
                # )
            # ),
            # stringsAsFactors = FALSE
            
        # )
        
        
        # ## upravuji nyní "my_test_data" ve smyslu nových proměnných
        # ## recency, frequency, monetary a annual_amount -----------------------
        
        # my_test_table <- NULL
        
        # for(my_id in unique(my_test_data[, "ID"])){
            
            # my_test_table <- rbind(
                
                # my_test_table,
                # c(
                    # "ID" = my_id,
                    # "recency" = min(
                        # as.numeric(
                            # max(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - my_test_data[
                                # my_test_data[, "ID"] == my_id,
                                # "Date"
                            # ]
                        # ),
                        # na.rm = TRUE
                    # ),
                    # "frequency" = length(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # ),
                    # "monetary" = sum(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                    # # / sum(
                        # # my_test_data[
                            # # my_test_data[, "ID"] == my_id,
                            # # "Amount"
                        # # ]
                    # # )
                    # ,
                    # "annual_amount" = sum(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # ) / (
                        # as.numeric(
                            # max(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - min(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # )
                        # ) / 365.25
                    # )
                # )
                
            # )
            
        # }
        
        # my_test_table <- data.frame(
            
            # my_test_table,
            # stringsAsFactors = FALSE
            
        # )
        
        # for(my_variable in c(
            
            # "recency",
            # "frequency",
            # "monetary",
            # "annual_amount"
            
        # )){
            
            # my_test_table[, my_variable] <- as.numeric(
                
                # my_test_table[, my_variable]
                
            # )
            
        # }
        
        
        # ## vytvářím kategorickou závisle proměnnou ----------------------------
        
        # my_test_table <- data.frame(
            
            # my_test_table,
            # "output_label" = ifelse(
                # my_test_table[, "annual_amount"] <= quantile(
                    # my_test_table[, "annual_amount"],
                    # probs = 1 / 3,
                    # names = FALSE,
                    # na.rm = TRUE
                # ),
                # "low",
                # ifelse(
                    # my_test_table[, "annual_amount"] <= quantile(
                        # my_test_table[, "annual_amount"],
                        # probs = 2 / 3,
                        # names = FALSE,
                        # na.rm = TRUE
                    # ),
                    # "middle",
                    # "high"
                # )
            # ),
            # stringsAsFactors = FALSE
            
        # )
        
        
        # ## vytvářím model náhodných lesů --------------------------------------
        
        # my_model <- randomForest::randomForest(
            
            # formula = output_label ~
                # recency +
                # frequency +
                # monetary
            # ,
            # data = with(
                # temp_data <- my_train_table,
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                    # ]
                # }
            # ),
            # importance = TRUE,
            # ntree = 180
            
        # )
        
        
        # ## vytvářím predikce --------------------------------------------------
        
        # my_predictions <- predict(
            # my_model,
            # with(
                # temp_data <- my_test_table,
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                    # ]
                # }
            # ),
            # type = "class"
        # )
        
        
        # ## vytvářím matici záměn ----------------------------------------------
        
        # my_confusion_table <- table(
            # with(
                # temp_data <- my_test_table,
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                        # "output_label"
                    # ]
                # }
            # ),
            # my_predictions,
            # dnn = list(
                # "skutečné hodnoty",
                # "predikované hodnoty"
            # )
        # )
        
        
        # ## počítám predikční přesnost -----------------------------------------
        
        # my_accuracy <- getMyAccuracy(
            
            # my_confusion_table
            
        # )
        
        
        # ## vytvářím výstup ----------------------------------------------------
        
        # return(
            # list(
                # "my_final_table" = data.frame(
                    # my_test_table[
                        # complete.cases(
                            # my_test_table
                        # )
                        # ,
                    # ],
                    # "output_label_prediction" = unname(
                        # my_predictions
                    # )
                # ),
                # "my_predictions" = my_predictions,
                # "my_confusion_table" = my_confusion_table,
                # "my_accuracy" = my_accuracy
            # )
        # )
        
    # })
    
    
    # output$CLV_transaction_console <- renderPrint({
        
        # my_transaction_predictions()
        
    # })
    
    # output$CLV_transaction_table <- renderTable({
        
        # my_transaction_predictions()[["my_final_table"]]
        
    # })
    
    # output$CLV_transaction_table <- renderDataTable(
        
        # {
            
            # my_transaction_predictions()[["my_final_table"]]
            
        # },
        # options = list(
            
            # scrollX = TRUE,
            # #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            # paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            # language = list(
                # url = "czech_for_data_table.json"
            # ),
            # scroller = TRUE
            
        # )
        
    # )
    
    # output$CLV_transaction_confusion <- renderPrint({
        
        # my_transaction_predictions()[["my_confusion_table"]]
        
    # })
    
    # output$CLV_transaction_accuracy <- renderUI({
        
        # HTML(
            # paste(
                # "Predikční přesnost je ",
                # format(
                    # round(
                        # my_transaction_predictions()[[
                            # "my_accuracy"
                        # ]] * 100,
                        # digits = 1
                    # ),
                    # nsmall = 1
                # ),
                # " %.",
                # sep = ""
            # )
        # )
        
    # })
    
    
    ## download handler pro stažení analýzy pouze transakčních dat ------------
    
    # observe(
        
        # if(
            # input$to_use_my_transaction_inbuilt_data |
            # ! is.null(input$my_transaction_file)
        # ){
            
            # output$CLV_transaction_final_data_download_button <- renderUI(
                
                # {
                    
                    # downloadButton(
                        # outputId = "CLV_transaction_final_data_download",
                        # label = "Stáhnout finální dataset s CLV predikcemi"
                    # )
                    
                # }
                
            # )
            
        # }else{
            
            # output$CLV_transaction_final_data_download_button <- renderUI(
                
                # {
                    
                    # NULL
                    
                # }
                
            # )
            
        # }
        
    # )
    
    # output$CLV_transaction_final_data_download <- downloadHandler(
        
        # filename = function(){
            
            # paste(
                # if(
                    # input$to_use_my_transaction_inbuilt_data
                # ){
                    
                    # input$my_transaction_inbuilt_data_file
                    
                # }else{
                    
                    # input$my_transaction_file
                    
                # },
                # "_with_CLV_predictions",
                # ".csv",
                # sep = ""
            # )
            
        # },
        
        # content = function(file){
            
            # write.csv2(
                # my_transaction_predictions()[["my_final_table"]],
                # file,
                # row.names = FALSE
            # )
            
        # }
        
    # )
    
    
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    
    my_transaction_predictions <- reactive({
        
        if(
            (
                ! input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        
        ## pojmenovávám my_transaction_data() na pohodlnější název ------------
        
        my_data <- my_transaction_data()
        
        
        ## určuji poslední rok v rámci celého datasetu ------------------------
        
        # return(my_data)
        
        my_last_year <- max(
            as.numeric(
                format(
                    my_data[, "Date"],
                    "%Y"
                )
            ),
            na.rm = TRUE
        )
        
        # return(my_last_year)
        
        
        ## pro každé ID se dívám, jaký byl annual amount pro každý rok
        ## od prvního, kdy dané ID provedlo nějaký nákup, až po poslední rok
        ## v datasetu, tedy "my_last_year" ------------------------------------
        
        my_table <- NULL
        
        for(my_id in unique(my_data[, "ID"])){
            
            ## vytvářím řadu let od prvního rok, kdy byl proveden nákup,
            ## po poslední rok v datasetu -------------------------------------
            
            my_first_year <- min(
                as.numeric(
                    format(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Date"
                        ],
                        "%Y"
                    )
                ),
                na.rm = TRUE
            )
            
            my_years <- c(my_first_year:my_last_year)
            
            
            ## nyní počítám řadu annual amounts k příslušným rokům ------------
            
            my_annual_amounts <- NULL
            
            for(
                my_year in my_years
            ){
                
                if(
                    my_year %in% as.numeric(
                        format(
                            my_data[
                                my_data[, "ID"] == my_id,
                                "Date"
                            ],
                            "%Y"
                        )
                    )
                ){
                    
                    my_annual_amounts <- c(
                        
                        my_annual_amounts,
                        sum(
                            my_data[
                                my_data[, "ID"] == my_id,
                                "Price"
                            ][
                                as.numeric(
                                    format(
                                        my_data[
                                            my_data[, "ID"] == my_id,
                                            "Date"
                                        ],
                                        "%Y"
                                    )
                                ) == my_year
                            ] * my_data[
                                my_data[, "ID"] == my_id,
                                "Amount"
                            ][
                                as.numeric(
                                    format(
                                        my_data[
                                            my_data[, "ID"] == my_id,
                                            "Date"
                                        ],
                                        "%Y"
                                    )
                                ) == my_year
                            ]
                        )
                        
                    )
                    
                }else{
                    
                    my_annual_amounts <- c(
                        
                        my_annual_amounts,
                        0
                        
                    )
                    
                }
                
            }
            
            
            ## vytvářím predikci annual amount pro rok následující
            ## po posledním roce obsaženém v datasetu, tedy my_last_year ------
            
            my_predicted_annual_amount <- if(
                
                length(my_years) == 1
                
            ){
                
                my_annual_amounts
                
            }else{
                
                max(
                    0,
                    predict(
                        lm(
                            formula = my_annual_amounts ~ my_years
                        ),
                        newdata = data.frame(
                            "my_years" = my_last_year + 1
                        )
                    )
                )
                
            }
            
            if(
                my_predicted_annual_amount > 1.3 * my_annual_amounts[
                    length(my_annual_amounts)
                ]
            ){
                
                my_predicted_annual_amount <-
                   1.3 * my_predicted_annual_amount
                
            }else{
                
                if(
                    my_predicted_annual_amount < 0.7 * my_annual_amounts[
                        length(my_annual_amounts)
                    ]
                ){
                    
                    my_predicted_annual_amount <-
                        0.7 * my_predicted_annual_amount
                    
                }
                
            }
            
            
            ## sestavuji výslednou tabulku ------------------------------------
            
            my_table <- rbind(
                
                my_table,
                c(
                    "ID" = my_id,
                    "historicke_annual_amounts" = paste(
                        "{",
                        paste(
                            paste(
                                paste(
                                    "'",
                                    my_years,
                                    "'",
                                    sep = ""
                                ),
                                my_annual_amounts,
                                sep = ": "
                            ),
                            collapse = ", "
                        ),
                        "}",
                        sep = ""
                    ),
                    "annual_amount_v_nove_sezone" = my_predicted_annual_amount
                )
                
            )
            
        }
        
        my_table <- data.frame(
            
            my_table,
            stringsAsFactors = FALSE
            
        )
        
        for(my_variable in colnames(my_table)){
            
            if(
                my_variable %in% c(
                    
                    "annual_amount_v_nove_sezone"
                    
                )
            ){
                
                my_table[, my_variable] <- as.numeric(
                    
                    my_table[, my_variable]
                    
                )
                
            }
            
        }
        
        my_table[, "annual_amount_v_nove_sezone"] <- ifelse(
            
            my_table[, "annual_amount_v_nove_sezone"] < 50,
            "< 50",
            format(
                round(
                    my_table[, "annual_amount_v_nove_sezone"],
                    digits = 0
                ),
                nsmall = 0
            )
            
        )
        
        
        ## vytvářím výstup ----------------------------------------------------
        
        return(
            my_table
            # [
                # order(
                    # as.numeric(
                        # my_table[, "ID"]
                    # )
                # )
                # ,
            # ]
        )
        
    })
    
    
    # output$CLV_transaction_console <- renderPrint({
        
        # my_transaction_predictions()
        
    # })
    
    
    output$CLV_transaction_table <- renderDataTable(
        
        {
            
            my_transaction_predictions()[
                ,
                c(
                    "ID",
                    "historicke_annual_amounts",
                    "annual_amount_v_nove_sezone"
                )
            ]
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            ),
            scroller = TRUE
            
        )
        
    )
    
    # output$CLV_non_transaction_confusion <- renderPrint({
        
        # my_non_transaction_predictions()[["my_confusion_table"]]
        
    # })
    
    # output$CLV_non_transaction_accuracy <- renderUI({
        
        # HTML(
            # paste(
                # "Predikční přesnost je ",
                # format(
                    # round(
                        # my_non_transaction_predictions()[[
                            # "my_accuracy"
                        # ]] * 100,
                        # digits = 1
                    # ),
                    # nsmall = 1
                # ),
                # " %.",
                # sep = ""
            # )
        # )
        
    # })
    
    
    ## download handler pro stažení analýzy pouze transakčních dat ------------
    
    observe(
        
        if(
            (
                input$to_use_my_transaction_inbuilt_data |
                ! is.null(input$my_transaction_file)
            )
        ){
            
            output$CLV_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    downloadButton(
                        outputId = "CLV_transaction_final_data_download",
                        label = "Stáhnout finální dataset s naivními predikcemi"
                    )
                    
                }
                
            )
            
        }else{
            
            output$CLV_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    NULL
                    
                }
                
            )
            
        }
        
    )
    
    output$CLV_transaction_final_data_download <- downloadHandler(
        
        filename = function(){
            
            paste(
                if(
                    input$to_use_my_transaction_inbuilt_data
                ){
                    
                    input$my_transaction_inbuilt_data_file
                    
                }else{
                    
                    input$my_transaction_file
                    
                },
                "_with_naive_predictions",
                ".csv",
                sep = ""
            )
            
        },
        
        content = function(file){
            
            write.csv2(
                my_transaction_predictions()[
                    ,
                    c(
                        "ID",
                        "historicke_annual_amounts",
                        "annual_amount_v_nove_sezone"
                    )
                ],
                file,
                row.names = FALSE
            )
            
        }
        
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    ## CLV analýza transakčních a netransakčních dat --------------------------
    
    #### vytvářím slider pro nastavení training a testing set -----------------
    
    # observe(
        
        # if(
            # (
                # input$to_use_my_transaction_inbuilt_data |
                # ! is.null(input$my_transaction_file)
            # ) & (
                # input$to_use_my_non_transaction_inbuilt_data |
                # ! is.null(input$my_non_transaction_file)
            # )
        # ){
            
            # output$my_non_transaction_train_vs_test_set_slider <- renderUI(
                
                # {
                    
                    # sliderInput(
                        # inputId =
                            # "my_non_transaction_train_vs_test_set_cutoff",
                        # label = "Rozsah trénovací množiny",
                        # min = min(
                            # my_transaction_data()[, "Date"],
                            # na.rm = TRUE
                        # ),
                        # max = max(
                            # my_transaction_data()[, "Date"],
                            # na.rm = TRUE
                        # ),
                        # value = (
                            # min(
                                # my_transaction_data()[, "Date"],
                                # na.rm = TRUE
                            # ) + 0.8 * as.numeric(
                                # max(
                                    # my_transaction_data()[, "Date"],
                                    # na.rm = TRUE
                                # ) - min(
                                    # my_transaction_data()[, "Date"],
                                    # na.rm = TRUE
                                # )
                            # )
                        # ),
                        # width = "50%",
                        # timeFormat = "%d. %m. %Y"
                    # )
                    
                # }
                
            # )
            
        # }else{
            
            # output$my_non_transaction_train_vs_test_set_slider <- renderUI(
                
                # {
                    
                    # NULL
                    
                # }
                
            # )
            
        # }
        
    # )
    
    
    ## tvořím trénovací a testovací množinu -----------------------------------
    
    # my_non_transaction_predictions <- reactive({
        
        # ## merguji transakční data s netransakčními daty ----------------------
        
        # my_data <- with(
            
            # temp_data <- merge(
                # x = my_transaction_data(),
                # y = my_non_transaction_data(),
                # by = "Date"
            # ),
            
            # temp_data <- temp_data[
                # order(as.numeric(temp_data[, "ID"]))
                # ,
            # ]
            
        # )
        
        
        # ## vytvářím pouze podmnožinu trénovacích a testovacích dat ------------
        
        # my_train_data <- my_data[
            
            # my_data[, "Date"] <
                # input$my_non_transaction_train_vs_test_set_cutoff
            # ,
            
        # ]
        
        # my_test_data <- my_data[
            
            # my_data[, "Date"] >=
                # input$my_non_transaction_train_vs_test_set_cutoff
            # ,
            
        # ]
        
        
        # ## upravuji nyní "my_train_data" ve smyslu nových proměnných
        # ## recency, frequency, monetary a annual_amount -----------------------
        
        # my_train_table <- NULL
        
        # for(my_id in unique(my_train_data[, "ID"])){
            
            # my_row <- c(
                
                # "ratio_of_visited_events_to_all_events" = length(
                    # unique(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # )
                # ) / length(
                    # unique(
                        # my_train_data[, "Date"]
                    # )
                # )
                
            # )
            
            # for(my_variable in c(
                
                # "WeekDay",
                # "KnownArtist",
                # "MajorEra",
                # "MinorEra",
                # "ConcertType"
                
            # )){
                
                # for(
                    # my_level in unique(
                        # my_train_data[, my_variable]
                    # )
                # ){
                    
                    # my_row <- c(
                        
                        # my_row,
                        # length(
                            # which(
                                # my_train_data[
                                    # my_train_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # my_train_data[
                                # my_train_data[, "ID"] == my_id,
                                # my_variable
                            # ]
                        # ),
                        # length(
                            # which(
                                # my_train_data[
                                    # my_train_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # which(
                                # my_train_data[
                                    # ,
                                    # my_variable
                                # ] == my_level
                            # )
                        # )
                        
                    # )
                    
                    # names(my_row)[
                        # c(
                            # length(my_row) - 1,
                            # length(my_row)
                        # )
                    # ] <- gsub(
                        
                        # "[ |-]",
                        # "_",
                        # c(
                            
                            # paste(
                                # "ratio_of_",
                                # my_level,
                                # "_to_all_",
                                # my_variable,
                                # "_levels_within_ID",
                                # sep = ""
                            # ),
                            # paste(
                                # "ratio_to_all_",
                                # my_level,
                                # "_of_",
                                # my_variable,
                                # "_within_dataset",
                                # sep = ""
                            # )
                            
                        # )
                        
                    # )
                    
                # }
                
            # }
            
            
            # ## doplňuji k datům sloupec s RFM skóre ---------------------------
            
            # my_train_table <- rbind(
                
                # my_train_table,
                # c(
                    # "ID" = my_id,
                    # "annual_amount" = sum(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # ) / (
                        # as.numeric(
                            # max(
                                # my_train_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - min(
                                # my_train_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # )
                        # ) / 365.25
                    # ),
                    # my_row,
                    # "recency" = min(
                        # as.numeric(
                            # input$my_non_transaction_train_vs_test_set_cutoff -
                            # my_train_data[
                                # my_train_data[, "ID"] == my_id,
                                # "Date"
                            # ]
                        # ),
                        # na.rm = TRUE
                    # ),
                    # "frequency" = length(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # ),
                    # "monetary" = sum(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                    # # / sum(
                        # # my_train_data[
                            # # my_train_data[, "ID"] == my_id,
                            # # "Amount"
                        # # ]
                    # # )
                # )
                
            # )
            
        # }
        
        # my_train_table <- data.frame(
            
            # my_train_table,
            # stringsAsFactors = FALSE
            
        # )
        
        # for(my_variable in colnames(my_train_table)){
            
            # if(my_variable != "ID"){
                
                # my_train_table[, my_variable] <- as.numeric(
                    
                    # my_train_table[, my_variable]
                    
                # )
                
            # }
            
        # }
        
        
        # ## vytvářím kategorickou závisle proměnnou ----------------------------
        
        # my_train_table <- data.frame(
            
            # my_train_table,
            # "output_label" = ifelse(
                # my_train_table[, "annual_amount"] <= quantile(
                    # my_train_table[, "annual_amount"],
                    # probs = 1 / 3,
                    # names = FALSE,
                    # na.rm = TRUE
                # ),
                # "low",
                # ifelse(
                    # my_train_table[, "annual_amount"] <= quantile(
                        # my_train_table[, "annual_amount"],
                        # probs = 2 / 3,
                        # names = FALSE,
                        # na.rm = TRUE
                    # ),
                    # "middle",
                    # "high"
                # )
            # ),
            # stringsAsFactors = FALSE
            
        # )
        
        
        # ## --------------------------------------------------------------------
        
        # ## upravuji nyní "my_test_data" ve smyslu nových proměnných
        # ## recency, frequency, monetary a annual_amount -----------------------
        
        # my_test_table <- NULL
        
        # for(my_id in unique(my_test_data[, "ID"])){
            
            # my_row <- c(
                
                # "ratio_of_visited_events_to_all_events" = length(
                    # unique(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # )
                # ) / length(
                    # unique(
                        # my_test_data[, "Date"]
                    # )
                # )
                
            # )
            
            # for(my_variable in c(
                
                # "WeekDay",
                # "KnownArtist",
                # "MajorEra",
                # "MinorEra",
                # "ConcertType"
                
            # )){
                
                # for(
                    # my_level in unique(
                        # my_test_data[, my_variable]
                    # )
                # ){
                    
                    # my_row <- c(
                        
                        # my_row,
                        # length(
                            # which(
                                # my_test_data[
                                    # my_test_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # my_test_data[
                                # my_test_data[, "ID"] == my_id,
                                # my_variable
                            # ]
                        # ),
                        # length(
                            # which(
                                # my_test_data[
                                    # my_test_data[, "ID"] == my_id,
                                    # my_variable
                                # ] == my_level
                            # )
                        # ) / length(
                            # which(
                                # my_test_data[
                                    # ,
                                    # my_variable
                                # ] == my_level
                            # )
                        # )
                        
                    # )
                    
                    # names(my_row)[
                        # c(
                            # length(my_row) - 1,
                            # length(my_row)
                        # )
                    # ] <- gsub(
                        
                        # "[ |-]",
                        # "_",
                        # c(
                            
                            # paste(
                                # "ratio_of_",
                                # my_level,
                                # "_to_all_",
                                # my_variable,
                                # "_levels_within_ID",
                                # sep = ""
                            # ),
                            # paste(
                                # "ratio_to_all_",
                                # my_level,
                                # "_of_",
                                # my_variable,
                                # "_within_dataset",
                                # sep = ""
                            # )
                            
                        # )
                        
                    # )
                    
                # }
                
            # }
            
            
            # ## doplňuji k datům sloupec s RFM skóre ---------------------------
            
            # my_test_table <- rbind(
                
                # my_test_table,
                # c(
                    # "ID" = my_id,
                    # "annual_amount" = sum(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # ) / (
                        # as.numeric(
                            # max(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - min(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # )
                        # ) / 365.25
                    # ),
                    # my_row,
                    # "recency" = min(
                        # as.numeric(
                            # max(
                                # my_test_data[
                                    # ,
                                    # "Date"
                                # ],
                                # na.rm = TRUE
                            # ) - my_test_data[
                                # my_test_data[, "ID"] == my_id,
                                # "Date"
                            # ]
                        # ),
                        # na.rm = TRUE
                    # ),
                    # "frequency" = length(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Date"
                        # ]
                    # ),
                    # "monetary" = sum(
                        # my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Price"
                        # ] * my_test_data[
                            # my_test_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                    # # / sum(
                        # # my_test_data[
                            # # my_test_data[, "ID"] == my_id,
                            # # "Amount"
                        # # ]
                    # # )
                # )
                
            # )
            
        # }
        
        # my_test_table <- data.frame(
            
            # my_test_table,
            # stringsAsFactors = FALSE
            
        # )
        
        # for(my_variable in colnames(my_test_table)){
            
            # if(my_variable != "ID"){
                
                # my_test_table[, my_variable] <- as.numeric(
                    
                    # my_test_table[, my_variable]
                    
                # )
                
            # }
            
        # }
        
        
        # ## vytvářím kategorickou závisle proměnnou ----------------------------
        
        # my_test_table <- data.frame(
            
            # my_test_table,
            # "output_label" = ifelse(
                # my_test_table[, "annual_amount"] <= quantile(
                    # my_test_table[, "annual_amount"],
                    # probs = 1 / 3,
                    # names = FALSE,
                    # na.rm = TRUE
                # ),
                # "low",
                # ifelse(
                    # my_test_table[, "annual_amount"] <= quantile(
                        # my_test_table[, "annual_amount"],
                        # probs = 2 / 3,
                        # names = FALSE,
                        # na.rm = TRUE
                    # ),
                    # "middle",
                    # "high"
                # )
            # ),
            # stringsAsFactors = FALSE
            
        # )
        
        
        # ## vynechávámm proměnné, které jsou kompletně populované chybějícími
        # ## hodnotami ----------------------------------------------------------
        
        # for(my_data_name in c(
            
            # "my_train_table",
            # "my_test_table"
            
        # )){
            
            # my_data <- get(my_data_name)
            
            # to_remove <- NULL
            
            # for(my_variable in colnames(my_data)){
                
                # if(
                    # all(
                        # is.na(my_data[, my_variable])
                    # )
                # ){
                    
                    # to_remove <- c(to_remove, my_variable)
                    
                # }
                
            # }
            
            # my_data <- my_data[
                # ,
                # setdiff(
                    # colnames(my_data),
                    # to_remove
                # )
            # ]
            
            # assign(
                # my_data_name,
                # my_data
            # )
            
        # }
        
        
        # ## omezuju se na proměnné testovacího datasetu ------------------------
        
        # for(my_data_name in c(
            
            # "my_train_table",
            # "my_test_table"
            
        # )){
            
            # my_data <- get(my_data_name)
            
            # my_data <- my_data[
                # ,
                # intersect(
                    # colnames(my_train_table),
                    # colnames(my_test_table)
                # )
            # ]
            
            # assign(
                # my_data_name,
                # my_data
            # )
            
        # }
        
        
        # # return(
            # # list(
                # # str(my_train_table),
                # # str(my_test_table)
            # # )
        # # )
        
        
        # ## vytvářím model náhodných lesů --------------------------------------
        
        # my_model <- randomForest::randomForest(
            
            # formula = output_label ~ .
            # ,
            # data = with(
                # temp_data <- my_train_table[
                    # ,
                    # setdiff(
                        # colnames(my_train_table),
                        # c(
                            # "ID",
                            # "annual_amount"
                        # )
                    # )
                # ],
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                    # ]
                # }
            # ),
            # importance = TRUE,
            # ntree = 180
            
        # )
        
        
        # ## vytvářím predikce --------------------------------------------------
        
        # my_predictions <- predict(
            # my_model,
            # with(
                # temp_data <- my_test_table[
                    # ,
                    # setdiff(
                        # colnames(my_test_table),
                        # c(
                            # "ID",
                            # "annual_amount"
                        # )
                    # )
                # ],
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                    # ]
                # }
            # ),
            # type = "class"
        # )
        
        
        # ## vytvářím matici záměn ----------------------------------------------
        
        # my_confusion_table <- table(
            # with(
                # temp_data <- my_test_table[
                    # ,
                    # setdiff(
                        # colnames(my_test_table),
                        # c(
                            # "ID",
                            # "annual_amount"
                        # )
                    # )
                # ],
                # {
                    # temp_data[, "output_label"] <- factor(
                        # temp_data[, "output_label"],
                        # levels = c(
                            # "low",
                            # "middle",
                            # "high"
                        # )
                    # )
                    
                    # temp_data[
                        # complete.cases(
                            # temp_data
                        # )
                        # ,
                        # "output_label"
                    # ]
                # }
            # ),
            # my_predictions,
            # dnn = list(
                # "skutečné hodnoty",
                # "predikované hodnoty"
            # )
        # )
        
        
        # ## počítám predikční přesnost -----------------------------------------
        
        # my_accuracy <- getMyAccuracy(
            
            # my_confusion_table
            
        # )
        
        
        # ## vytvářím výstup ----------------------------------------------------
        
        # return(
            # list(
                # "my_final_table" = data.frame(
                    # my_test_table[
                        # complete.cases(
                            # my_test_table
                        # )
                        # ,
                    # ],
                    # "output_label_prediction" = unname(
                        # my_predictions
                    # )
                # ),
                # "my_predictions" = my_predictions,
                # "my_confusion_table" = my_confusion_table,
                # "my_accuracy" = my_accuracy
            # )
        # )
        
    # })
    
    
    # output$CLV_non_transaction_console <- renderPrint({
        
        # my_non_transaction_predictions()
        
    # })
    
    # output$CLV_non_transaction_table <- renderTable({
        
        # my_non_transaction_predictions()[["my_final_table"]]
        
    # })
    
    # output$CLV_non_transaction_table <- renderDataTable(
        
        # {
            
            # my_non_transaction_predictions()[["my_final_table"]][
                # ,
                # !grepl(
                    # "^ratio_",
                    # colnames(
                        # my_non_transaction_predictions()[["my_final_table"]]
                    # )
                # )
            # ]
            
        # },
        # options = list(
            
            # scrollX = TRUE,
            # #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            # paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            # language = list(
                # url = "czech_for_data_table.json"
            # ),
            # scroller = TRUE
            
        # )
        
    # )
    
    # output$CLV_non_transaction_confusion <- renderPrint({
        
        # my_non_transaction_predictions()[["my_confusion_table"]]
        
    # })
    
    # output$CLV_non_transaction_accuracy <- renderUI({
        
        # HTML(
            # paste(
                # "Predikční přesnost je ",
                # format(
                    # round(
                        # my_non_transaction_predictions()[[
                            # "my_accuracy"
                        # ]] * 100,
                        # digits = 1
                    # ),
                    # nsmall = 1
                # ),
                # " %.",
                # sep = ""
            # )
        # )
        
    # })
    
    
    ## download handler pro stažení analýzy pouze transakčních dat ------------
    
    # observe(
        
        # if(
            # input$to_use_my_non_transaction_inbuilt_data |
            # ! is.null(input$my_non_transaction_file)
        # ){
            
            # output$CLV_non_transaction_final_data_download_button <- renderUI(
                
                # {
                    
                    # downloadButton(
                        # outputId = "CLV_non_transaction_final_data_download",
                        # label = "Stáhnout finální dataset s CLV predikcemi"
                    # )
                    
                # }
                
            # )
            
        # }else{
            
            # output$CLV_non_transaction_final_data_download_button <- renderUI(
                
                # {
                    
                    # NULL
                    
                # }
                
            # )
            
        # }
        
    # )
    
    # output$CLV_non_transaction_final_data_download <- downloadHandler(
        
        # filename = function(){
            
            # paste(
                # if(
                    # input$to_use_my_non_transaction_inbuilt_data
                # ){
                    
                    # input$my_non_transaction_inbuilt_data_file
                    
                # }else{
                    
                    # input$my_non_transaction_file
                    
                # },
                # "_with_CLV_predictions",
                # ".csv",
                # sep = ""
            # )
            
        # },
        
        # content = function(file){
            
            # write.csv2(
                # my_non_transaction_predictions()[["my_final_table"]],
                # file,
                # row.names = FALSE
            # )
            
        # }
        
    # )
    
    
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    
    ## CLV analýza transakčních a netransakčních dat --------------------------
    
    ## tvořím trénovací a testovací množinu -----------------------------------
    
    output$pokus <- renderPrint({
        str(my_fictive_non_transaction_data())
    })
    
    my_non_transaction_predictions <- reactive({
        
        if(
            (
                ! input$to_use_my_transaction_inbuilt_data &
                is.null(input$my_transaction_file)
            ) | (
                ! input$to_use_my_non_transaction_inbuilt_data &
                is.null(input$my_non_transaction_file)
            ) | (
                ! input$to_use_my_fictive_non_transaction_inbuilt_data &
                is.null(input$my_fictive_non_transaction_file)
            )
        ){
            
            return(NULL)
            
        }
        
        
        ## merguji transakční data s netransakčními daty ----------------------
        
        my_data <- with(
            
            temp_data <- merge(
                x = my_transaction_data(),
                y = my_non_transaction_data(),
                by = "Date"
            ),
            
            temp_data <- temp_data[
                order(as.numeric(temp_data[, "ID"]))
                ,
            ]
            
        )
        
        
        ## upravuji nyní "my_train_data" ve smyslu nových proměnných
        ## recency, frequency, monetary a annual_amount -----------------------
        
        my_table <- NULL
        
        for(my_id in unique(my_data[, "ID"])){
            
            my_row <- c(
                
                "proporce_navstivenych_koncertu" = length(
                    unique(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Date"
                        ]
                    )
                ) / length(
                    unique(
                        my_data[, "Date"]
                    )
                )
                
            )
            
            for(my_variable in c(
                
                "WeekDay",
                "KnownArtist",
                "MajorEra",
                "MinorEra",
                "ConcertType"
                
            )){
                
                for(
                    my_level in unique(
                        my_data[, my_variable]
                    )
                ){
                    
                    my_row <- c(
                        
                        my_row,
                        length(
                            which(
                                my_data[
                                    my_data[, "ID"] == my_id,
                                    my_variable
                                ] == my_level
                            )
                        ) / length(
                            my_data[
                                my_data[, "ID"] == my_id,
                                my_variable
                            ]
                        )
                        
                    )
                    
                    names(my_row)[
                        c(
                            length(my_row)
                        )
                    ] <- gsub(
                        
                        "[ |-]",
                        "_",
                        c(
                            
                            paste(
                                my_variable,
                                "_",
                                my_level,
                                sep = ""
                            )
                            
                        )
                        
                    )
                    
                }
                
            }
            
            
            ## doplňuji k datům sloupec s RFM skóre ---------------------------
            
            my_table <- rbind(
                
                my_table,
                c(
                    "ID" = my_id,
                    "annual_amount" = sum(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Price"
                        ] * my_data[
                            my_data[, "ID"] == my_id,
                            "Amount"
                        ]
                    ) / (
                        as.numeric(
                            max(
                                my_data[
                                    ,
                                    "Date"
                                ],
                                na.rm = TRUE
                            ) - min(
                                my_data[
                                    ,
                                    "Date"
                                ],
                                na.rm = TRUE
                            )
                        ) / 365.25
                    ),
                    my_row,
                    "recency" = min(
                        as.numeric(
                            right_censoring() - my_data[
                                my_data[, "ID"] == my_id,
                                "Date"
                            ]
                        ),
                        na.rm = TRUE
                    ),
                    "frequency" = length(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Date"
                        ]
                    ),
                    "monetary" = sum(
                        my_data[
                            my_data[, "ID"] == my_id,
                            "Price"
                        ] * my_data[
                            my_data[, "ID"] == my_id,
                            "Amount"
                        ]
                    )
                    # / sum(
                        # my_train_data[
                            # my_train_data[, "ID"] == my_id,
                            # "Amount"
                        # ]
                    # )
                )
                
            )
            
        }
        
        my_table <- data.frame(
            
            my_table,
            stringsAsFactors = FALSE
            
        )
        
        for(my_variable in colnames(my_table)){
            
            if(my_variable != "ID"){
                
                my_table[, my_variable] <- as.numeric(
                    
                    my_table[, my_variable]
                    
                )
                
            }
            
        }
        
        
        # return(my_table)
        
        
        ## --------------------------------------------------------------------
        
        ## vynechávámm proměnné, které jsou kompletně populované chybějícími
        ## hodnotami ----------------------------------------------------------
        
        # for(my_data_name in c(
            
            # "my_table"
            
        # )){
            
            # my_data <- get(my_data_name)
            
            # to_remove <- NULL
            
            # for(my_variable in colnames(my_data)){
                
                # if(
                    # all(
                        # is.na(my_data[, my_variable])
                    # )
                # ){
                    
                    # to_remove <- c(to_remove, my_variable)
                    
                # }
                
            # }
            
            # my_data <- my_data[
                # ,
                # setdiff(
                    # colnames(my_data),
                    # to_remove
                # )
            # ]
            
            # assign(
                # my_data_name,
                # my_data
            # )
            
        # }
        
        
        ## upravuji ještě dataset s plánovanou budoucí sezónou ----------------
        
        my_fictive_table <- NULL
        
        for(my_variable in c(
            
            "WeekDay",
            "KnownArtist",
            "MajorEra",
            "MinorEra",
            "ConcertType"
            
        )){
            
            for(
                my_level in unique(
                    my_fictive_non_transaction_data()[, my_variable]
                )
            ){
                
                my_fictive_table <- c(
                    
                    my_fictive_table,
                    length(
                        which(
                            my_fictive_non_transaction_data()[
                                ,
                                my_variable
                            ] == my_level
                        )
                    ) / length(
                        my_fictive_non_transaction_data()[
                            ,
                            my_variable
                        ]
                    )
                    
                )
                
                names(my_fictive_table)[
                    c(
                        length(my_fictive_table)
                    )
                ] <- gsub(
                    
                    "[ |-]",
                    "_",
                    c(
                        
                        paste(
                            my_variable,
                            "_",
                            my_level,
                            sep = ""
                        )
                        
                    )
                    
                )
                
            }
            
        }
        
        my_fictive_table <- data.frame(
            
            t(my_fictive_table),
            stringsAsFactors = FALSE
            
        )
        
        for(my_variable in colnames(my_fictive_table)){
            
            if(my_variable != "ID"){
                
                my_fictive_table[, my_variable] <- as.numeric(
                    
                    my_fictive_table[, my_variable]
                    
                )
                
            }
            
        }
        
        # return(my_fictive_table)
        
        
        ## vytvářím prediktivní model -----------------------------------------
        
        my_final_table <- NULL
        
        for(my_id in unique(my_table[, "ID"])){
            
            my_row <- c(
                
                "ID" = my_id,
                "proporce_navstivenych_koncertu" = my_table[
                    my_table[, "ID"] == my_id,
                    "proporce_navstivenych_koncertu"
                ],
                "annual_amount" = my_table[
                    my_table[, "ID"] == my_id,
                    "annual_amount"
                ],
                "recency" = my_table[
                    my_table[, "ID"] == my_id,
                    "recency"
                ],
                "frequency" = my_table[
                    my_table[, "ID"] == my_id,
                    "frequency"
                ],
                "monetary" = my_table[
                    my_table[, "ID"] == my_id,
                    "monetary"
                ]
                
            )
            
            for(my_variable in c(
                
                "WeekDay",
                "KnownArtist",
                "MajorEra",
                "MinorEra",
                "ConcertType"
                
            )){
                
                my_factors <- NULL
                
                for(
                    my_level in unique(
                        gsub(
                            paste(
                                my_variable,
                                "_",
                                sep = ""
                            ),
                            "",
                            colnames(my_table)[
                                grepl(
                                    my_variable,
                                    colnames(my_table)
                                )
                            ]
                        )
                    )
                ){
                    
                    if(
                        any(
                            grepl(
                                paste(
                                    my_variable,
                                    "_",
                                    my_level,
                                    sep = ""
                                ),
                                colnames(my_fictive_table)
                            )
                        )
                    ){
                        
                        if(
                            my_table[
                                my_table[, "ID"] == my_id,
                                paste(
                                    my_variable,
                                    "_",
                                    my_level,
                                    sep = ""
                                )
                            ] != 0 &
                            my_fictive_table[
                                1,
                                paste(
                                    my_variable,
                                    "_",
                                    my_level,
                                    sep = ""
                                )
                            ] != 0
                        ){
                            
                            my_factors <- c(
                                
                                my_factors,
                                (
                                    my_fictive_table[
                                        1,
                                        paste(
                                            my_variable,
                                            "_",
                                            my_level,
                                            sep = ""
                                        )
                                    ] / my_table[
                                        my_table[, "ID"] == my_id,
                                        paste(
                                            my_variable,
                                            "_",
                                            my_level,
                                            sep = ""
                                        )
                                    ]
                                ) #^ 2
                                
                            )
                            
                        }
                        
                    }
                    
                }
                
                if(
                    length(my_factors) == 0
                ){
                    
                    my_row <- c(
                        
                        my_row,
                        1
                        
                    )
                    
                }else{
                    
                    my_row <- c(
                        
                        my_row,
                        prod(
                            my_factors
                        )
                        
                    )
                    
                }
                
                names(my_row)[length(my_row)] <- my_variable
                
            }
            
            my_final_table <- rbind(
                
                my_final_table,
                my_row
                
            )
            
        }
        
        my_final_table <- data.frame(
            
            my_final_table,
            stringsAsFactors = FALSE
            
        )
        
        rownames(my_final_table) <- as.character(
            
            c(1:dim(my_final_table)[1])
            
        )
        
        for(my_variable in colnames(my_final_table)){
            
            if(my_variable != "ID"){
                
                my_final_table[, my_variable] <- as.numeric(
                    
                    my_final_table[, my_variable]
                    
                )
                
            }
            
        }
        
        
        ## vytvářím predikce --------------------------------------------------
        
        my_final_table <- data.frame(
            
            my_final_table,
            "faktor" = apply(
                my_final_table[
                    ,
                    c(
                        "WeekDay",
                        "KnownArtist",
                        "MajorEra",
                        "MinorEra",
                        "ConcertType"
                    )
                ],
                1,
                "prod"
            ),
            "skalovany_faktor" = apply(
                my_final_table[
                    ,
                    c(
                        "WeekDay",
                        "KnownArtist",
                        "MajorEra",
                        "MinorEra",
                        "ConcertType"
                    )
                ],
                1,
                "prod"
            ) / (
                1 + apply(
                    my_final_table[
                        ,
                        c(
                            "WeekDay",
                            "KnownArtist",
                            "MajorEra",
                            "MinorEra",
                            "ConcertType"
                        )
                    ],
                    1,
                    "prod"
                )
            ),
            "kolikrat_vic_navstivi_koncertu" = (
                (
                    apply(
                        my_final_table[
                            ,
                            c(
                                "WeekDay",
                                "KnownArtist",
                                "MajorEra",
                                "MinorEra",
                                "ConcertType"
                            )
                        ],
                        1,
                        "prod"
                    ) / (
                        1 + apply(
                            my_final_table[
                                ,
                                c(
                                    "WeekDay",
                                    "KnownArtist",
                                    "MajorEra",
                                    "MinorEra",
                                    "ConcertType"
                                )
                            ],
                            1,
                            "prod"
                        )
                    )
                ) / sqrt(
                    my_final_table[
                        ,
                        "proporce_navstivenych_koncertu"
                    ]
                )
            ) ^ (1 / 4),
            stringsAsFactors = FALSE
            
        )
        
        my_final_table <- data.frame(
            
            my_final_table,
            "kolikrat_vic_navstivi_koncertu_v_nove_sezone" = unlist(
                
                lapply(
                    1:dim(my_final_table)[1],
                    function(i){
                        if(
                            my_final_table[
                                i,
                                "kolikrat_vic_navstivi_koncertu"
                            ] < 0.7
                        ){
                            return(0.7)
                        }else{
                            if(
                                my_final_table[
                                    i,
                                    "kolikrat_vic_navstivi_koncertu"
                                ] > 1.3
                            ){
                                return(1.3)
                            }else{
                                return(
                                    my_final_table[
                                        i,
                                        "kolikrat_vic_navstivi_koncertu"
                                    ]
                                )
                            }
                        }
                    }
                )
                
            ),
            stringsAsFactors = FALSE
            
        )
        
        
        my_final_table <- data.frame(
            
            my_final_table,
            "annual_amount_v_nove_sezone" = unlist(
                lapply(
                    1:dim(my_final_table)[1],
                    function(i){
                        if(
                            my_final_table[
                                i,
                                "annual_amount"
                            ] == 0
                        ){
                            0.05 * mean(
                                my_final_table[
                                    ,
                                    "annual_amount"
                                ]
                            ) * my_final_table[
                                i,
                                "kolikrat_vic_navstivi_koncertu_v_nove_sezone"
                            ]
                        }else{
                            my_final_table[
                                i,
                                "annual_amount"
                            ] * my_final_table[
                                i,
                                "kolikrat_vic_navstivi_koncertu_v_nove_sezone"
                            ]
                        }
                    }
                )
            ),
            # my_final_table[
                # ,
                # "annual_amount"
            # ] * my_final_table[
                # ,
                # "kolikrat_vic_navstivi_koncertu_v_nove_sezone"
            # ],
            stringsAsFactors = FALSE
            
        )
        
        my_final_table[, "annual_amount_v_nove_sezone"] <- ifelse(
            
            my_final_table[, "annual_amount_v_nove_sezone"] < 50,
            "< 50",
            format(
                round(
                    my_final_table[, "annual_amount_v_nove_sezone"],
                    digits = 0
                ),
                nsmall = 0
            )
            
        )
        
        # return(my_final_table)
        
        
        ## vytvářím výstup ----------------------------------------------------
        
        return(
            my_final_table
            # [
                # order(
                    # as.numeric(
                        # my_final_table[, "ID"]
                    # )
                # )
                # ,
            # ]
        )
        
    })
    
    
    # output$pokus <- renderPrint({
        
        # #my_non_transaction_predictions()
        
        # mean(
            # my_non_transaction_predictions()[
                # ,
                # "annual_amount_v_nove_sezone"
            # ]
        # ) / mean(
            # my_non_transaction_predictions()[
                # ,
                # "annual_amount"
            # ]
        # )
        
    # })
    
    
    output$CLV_non_transaction_table <- renderDataTable(
        
        {
            
            my_non_transaction_predictions()[
                ,
                c(
                    "ID",
                    "proporce_navstivenych_koncertu",
                    "annual_amount",
                    "kolikrat_vic_navstivi_koncertu_v_nove_sezone",
                    "annual_amount_v_nove_sezone"
                )
            ]
            
        },
        options = list(
            
            scrollX = TRUE,
            #dom = "t",      # odkomentování zruší možnost prohledávat tabulku
            paging = TRUE,   # paging = TRUE vede na stránkování tabulky
            language = list(
                url = "czech_for_data_table.json"
            ),
            scroller = TRUE
            
        )
        
    )
    
    # output$CLV_non_transaction_confusion <- renderPrint({
        
        # my_non_transaction_predictions()[["my_confusion_table"]]
        
    # })
    
    # output$CLV_non_transaction_accuracy <- renderUI({
        
        # HTML(
            # paste(
                # "Predikční přesnost je ",
                # format(
                    # round(
                        # my_non_transaction_predictions()[[
                            # "my_accuracy"
                        # ]] * 100,
                        # digits = 1
                    # ),
                    # nsmall = 1
                # ),
                # " %.",
                # sep = ""
            # )
        # )
        
    # })
    
    
    ## download handler pro stažení analýzy pouze transakčních dat ------------
    
    observe(
        
        if(
            (
                input$to_use_my_non_transaction_inbuilt_data |
                ! is.null(input$my_non_transaction_file)
            ) & (
                input$to_use_my_fictive_non_transaction_inbuilt_data |
                ! is.null(input$my_fictive_non_transaction_file)
            )
        ){
            
            output$CLV_non_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    downloadButton(
                        outputId = "CLV_non_transaction_final_data_download",
                        label = "Stáhnout finální dataset s CLV predikcemi"
                    )
                    
                }
                
            )
            
        }else{
            
            output$CLV_non_transaction_final_data_download_button <- renderUI(
                
                {
                    
                    NULL
                    
                }
                
            )
            
        }
        
    )
    
    output$CLV_non_transaction_final_data_download <- downloadHandler(
        
        filename = function(){
            
            paste(
                if(
                    input$to_use_my_fictive_non_transaction_inbuilt_data
                ){
                    
                    input$my_fictive_non_transaction_inbuilt_data_file
                    
                }else{
                    
                    input$my_fictive_non_transaction_file
                    
                },
                "_with_CLV_predictions",
                ".csv",
                sep = ""
            )
            
        },
        
        content = function(file){
            
            write.csv2(
                my_non_transaction_predictions()[
                    ,
                    c(
                        "ID",
                        "proporce_navstivenych_koncertu",
                        "annual_amount",
                        "kolikrat_vic_navstivi_koncertu_v_nove_sezone",
                        "annual_amount_v_nove_sezone"
                    )
                ],
                file,
                row.names = FALSE
            )
            
        }
        
    )
    
    
    ## ------------------------------------------------------------------------
    
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





