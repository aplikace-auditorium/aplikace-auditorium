###############################################################################
###############################################################################
###############################################################################

## tvořím tělo user interface aplikace ----------------------------------------

my_body <- dashboardBody(
    
    ## zavádím javascriptové a CSS funkcionality ------------------------------
    
    tags$head(
        
        ## CSS formátování aplikace ------------------------------------------
        
        tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "style.css"
        ),
        
        # tags$link(
            # rel = "stylesheet",
            # type = "text/css",
            # href = "pagination_colour.css"
        # ),
        
        
        ## javascriptová funkce pro busy indikátor ----------------------------
        
        tags$script(
            type = "text/javascript",
            src = "busy_indicator.js"
        ),
        
        # tags$script(
            # type = "text/javascript",
            # src = "pagination_function.js"
        # ),
        
        
        ## kolapsibilní okraje v rámci bootstrap třídy ------------------------
        
        # tags$link(
            # rel = "stylesheet",
            # type = "text/css",
            # href = "margins_and_paddings.css"
        # ),
        
        
        ## logo aplikace ------------------------------------------------------
        
        tags$link(
            rel = "shortcut icon",
            href = "Rlogo.png"
        ),
        
        
        ## pozice footeru -----------------------------------------------------
        
        tags$style(
            type = "text/css",
            paste(
                ".panel-footer {position:",
                "fixed; right: 0;",
                "bottom: 0;",
                "left: 0;",
                "height: 38px;",
                "padding-top: 2px;",
                "padding-left: 10px;",
                "padding-right: 10px;",
                "padding-bottom: 5px;",
                "}",
                sep = ""
            )
        ),
        
        
        ## formátování upozornění ---------------------------------------------
        
        tags$style(
            type = "text/css",
            paste(
                "#my_transaction_data_positive_message_text{",
                "color: blue;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        tags$style(
            type = "text/css",
            paste(
                "#my_transaction_data_negative_message_text{",
                "color: red;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        tags$style(
            type = "text/css",
            paste(
                "#my_non_transaction_data_positive_message_text{",
                "color: blue;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        tags$style(
            type = "text/css",
            paste(
                "#my_non_transaction_data_negative_message_text{",
                "color: red;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        tags$style(
            type = "text/css",
            paste(
                "#my_fictive_non_transaction_data_positive_message_text{",
                "color: blue;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        tags$style(
            type = "text/css",
            paste(
                "#my_fictive_non_transaction_data_negative_message_text{",
                "color: red;",
                "font-style: bold;",
                "}",
                sep = ""
            )
        ),
        
        
        ## dva obrázky vedle sebe ---------------------------------------------
        
        # tags$style(
            # type = "text/css",
            # paste(
                # ".column {",
                # "float: left;",
                # "width: 50.00%;",
                # "padding: 5px;",
                # "}",
                # sep = ""
            # )
        # ),
        
        # tags$style(
            # type = "text/css",
            # paste(
                # ".row::after {",
                # "content: '';",
                # "clear: both;",
                # "display: table;",
                # "}",
                # sep = ""
            # )
        # ),
        
        
        ## vypínám benigní warningové hlášky aplikace vypisované
        ## do konzole ---------------------------------------------------------
        
        tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        # tags$style(
            # "#my_file_info{sanitize:false;}"
        # )#,
        
        
        ## přidávám odkaz na jQuery funkci ------------------------------------
        
        # tags$script(
            # src = paste(
                # "http://cdn.datatables.net/plug-ins/1.10.19/",
                # "filtering/type-based/accent-neutralise.js",
                # sep = ""
            # )
        # )
        
        # tags$script(
            # type = "text/javascript",
            # src = "accent_neutralization.js"
        # )
        
        
        ## měním barevné schéma aplikace --------------------------------------
        
        tags$style(
            "
                .btn-info {
                    background-color: #f4646c !important;
                    border-color: #ee3343 !important;
                }
                
                .btn:active {
                    background-color: #f4646c !important;
                    border-color: #ee3343 !important;
                }
                
                .btn, .btn:hover{
                    background-color: #f4646c !important;
                    border-color: #ee3343 !important;
                }
            "
        ),
        
        tags$style(
            "
                .irs-bar,
                .irs-bar-edge,
                .irs-single,
                .irs-grid-pol {
                    background: #f4646c;
                    border-color: #f4646c;
                }
            "
        ),
        
        tags$style(
            "
                .irs-from,
                .irs-to,
                .irs-single {
                    background: #f4646c;
                }
            "
        ),
        
        tags$style(
            HTML(
                "
                    /* body */
                    .content-wrapper, .right-side {
                        background-color: #ffffff;
                    }
                    
                    /* logo */
                    .skin-blue .main-header .logo {
                        background-color: #000000;
                    }
                    
                    /* logo when hovered */
                    .skin-blue .main-header .logo:hover {
                        background-color: #000000;
                    }
                    
                    /* navbar (rest of the header) */
                    .skin-blue .main-header .navbar {
                        background-color: #000000;
                    }
                    
                    /* main sidebar */
                    .skin-blue .main-sidebar {
                        background-color: #f4646c;
                        border-left-color: #f4646c;
                    }
                    
                    /* hovered tab in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        background-color: #000000;
                        color: #ffffff;
                        border-left-color: #000000;
                    }
                    
                    /* active selected in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                        background-color: #ee3343;
                        color: #ffffff;
                        border-left-color: #000000;
                    }
                    
                    /* active selected tab in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .treeview-menu .active a{
                        background-color: #000000;
                        color: #ffffff;
                        border-left-color: #000000;
                    }
                    
                    /* other links in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                        background-color: #f4646c;
                        color: #ffffff;
                        border-left-color: #f4646c;
                    }
                    
                    /* other links in the sidebarmenu when hovered */
                     .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        background-color: #000000;
                        color: #ffffff;
                        border-left-color: #f4646c;
                    }
                    
                    /* toggle button when hovered  */                    
                     .skin-blue .main-header .navbar .sidebar-toggle:hover{
                        background-color: #f4646c;
                        color: #ffffff;
                    }
                "
            )
        )#,
        
        # tags$style(
            # HTML(
                # "   
                    # /* logo */
                    # .skin-blue .main-header .logo {
                                          # background-color: #f4b943;
                                          # }
                    
                    # /* logo when hovered */
                    # .skin-blue .main-header .logo:hover {
                                          # background-color: #f4b943;
                                          # }
                    
                    # /* navbar (rest of the header) */
                    # .skin-blue .main-header .navbar {
                                          # background-color: #f4b943;
                                          # }
                    
                    # /* main sidebar */
                    # .skin-blue .main-sidebar {
                                          # background-color: #f4b943;
                                          # }
                    
                    # /* active selected tab in the sidebarmenu */
                    # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                          # background-color: #ff0000;
                                          # }
                    
                    # /* other links in the sidebarmenu */
                    # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                          # background-color: #00ff00;
                                          # color: #000000;
                                          # }
                    
                    # /* other links in the sidebarmenu when hovered */
                     # .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                          # background-color: #ff69b4;
                                          # }
                    
                    # /* toggle button when hovered  */                    
                     # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                          # background-color: #ff69b4;
                                          # }
                # "
            # )
        # )
        
    ),
    
    
    ## text, který aplikace zobrazuje při svém zaneprázdnění ------------------
    
    div(
        class = "busy",
        p("Aplikace je zaneprázdněná..."),
        img(
            src = "busy_indicator_purple.gif",
            height = 100,
            width = 100
        )
    ),
    
    
    ## volám shinyjs funkcionality --------------------------------------------
    
    shinyjs::useShinyjs(),
    
    
    ## footer -----------------------------------------------------------------
    
    # tags$style(
        # type = "text/css",
        # paste(
            # ".panel-footer {position:",
            # "fixed; right: 0;",
            # "bottom: 0;",
            # "left: 0;}",
            # sep = ""
        # )
    # ),
    
    div(
        class = "panel-footer",
        style = "opacity: 1.00; z-index: 1000;",
        HTML(
            '
                <!-- název aplikace -->
                <font size = "4">
                    RFM a CLV analýza
                </font>
                
                <!-- verze aplikace -->
                <font size = "2">
                    &nbsp; | &nbsp;
                    verze 0.0.1
                    &nbsp; | &nbsp;
                </font>
                
                <!-- licence a její logo -->
                <a
                    href =
                    "https://creativecommons.org/licenses/by-nc-nd/3.0/cz/"
                    id = "tooltip_cc"
                    target = "_blank"
                >
                    <img
                        src = "cc_by_nc_nd.png",
                        style = "height: 20px;"
                    >
                </a>
                
                <!-- zkratka licence -->
                &nbsp;
                | &nbsp; CC BY-NC-ND 3.0 CZ &nbsp;
                | &nbsp; 2019&ndash;2021 &nbsp;
                | &nbsp;
                
                <!-- technická podpora -->
                <a
                    href =
                    "mailto:lubomir.stepanek@vse.cz"
                >
                    technická podpora
                </a>
            ',
            '
            <!-- počítadlo návštěv -->
            <span style = "float:right">
            ',
            paste(
                "<font size = '2'><b>",
                textOutput(
                    "my_counter",
                    inline = TRUE
                ),
                "&emsp; </b></font>",
                sep = ""
            ),
            '
            <!-- logo VŠE -->
            <a
                href = "https://www.vse.cz/"
                id = "tooltip_vse"
                target = "_blank"
            >
                <img
                    src = "vse_logo.png",
                    style = "height: 36px;"
                >
            </a>',
            '<!-- logo TAČR -->
            <a
                href = "https://www.tacr.cz/"
                id = "tooltip_tacr"
                target = "_blank"
            >
                <img
                    src = "tacr_logo.png",
                    style = "height: 27px;"
                >
            </a>',
            '</span>',
            '
            <!-- javascriptové funkcionality v HTML -->
            <script>
                $("#tooltip_cc").attr(
                    "title",
                    "CC BY-NC-ND 3.0 CZ"
                );
                $("#tooltip_vse").attr(
                    "title",
                    "VŠE"
                );
                $("#tooltip_tacr").attr(
                    "title",
                    "TAČR"
                );
            </script>'
        )
    ),
    
    
    ## inicializuji bootstrapovou CSS třídu -----------------------------------
    
    theme = "bootstrap.css",
    
    
    ## obsah záložek ----------------------------------------------------------
    
    tabItems(
        
        ## obsah záložky "Informace o aplikaci" -------------------------------
        
        tabItem(
            
            tabName = "info_tab",
            
            # "# obsah záložky s informacemi o aplikaci",
            
            h4(
                HTML(
                    "<b>Vítejte v aplikaci Auditorium pro analýzu nákupního
                    chování vašeho publika!</b>"
                )
            ),
            
            tags$br(),
            
            "Aplikace Auditorium je určeno zejména pro hudební instituce ",
            "a s její pomocí můžete:",
            
            HTML(
                "<ul>
                  <li> roztřídit vaše publikum do třech skupin podle významnosti</li>
                  <li> zjistit přínos vašich klíčových návštěvníků a jejich preference</li>
                  <li> zjistit, jak často a v jakém množství vaši návštěvníci nakupují</li>
                  <li> odhadnout celoživotní hodnotu vašich zákazníků</li>
                </ul>"
            ),
            
            tags$br(),
            #tags$br(),
            
            # "Jejím cílem je poskytnout kulturním institucím nástroj pro ",
            # "modelování a výpočet tzv. celoživotní hodnoty zákazníka (CLV). ",
            # "Aplikace kromě toho z nahraných dat dokáže provést RFM analýzu ",
            # "(R-recency, F-frequency, M-monetary), což je v byznysové praxi ",
            # "velmi populární analýza a její výstupy pak přehledně prezentuje ",
            # "ve formě grafů včetně základní interpretace.",
            
            # tags$br(),
            # tags$br(),
            
            # "Aplikace obsahuje několik částí, viz levý panel: Načtení dat, ",
            # "RFM analýza, CLV analýza. Všechny tyto části jsou v příslušné ",
            # "sekci detailněji popsané. Výstupy modelů je možné exportovat ",
            # "(*.csv formát) a využívat pro další zpracování nebo jiné ",
            # "praktické využití. Celá aplikace a její použití je znázorněno ",
            # "na následujícím schématu.",
            
            "Aplikace Auditorium funguje podle následujícího schématu:",
            
            tags$br(),
            tags$br(),
            
            HTML(
                "<center>
                <figure>
                    <img src = '_proces_.png' width = '750px'>
                </figure>
                </center>"
            ),
            
            tags$br(),
            tags$br(),
            
            # HTML(
                # "<b>CLV (Customer Lifetime Value = celoživotní hodnota zákazníka)</b> ",
                # "obecně představuje odhad příjmu z každého návštěvníka koncertů ",
                # "v následujícím roce. Součtem CLV všech návštěvníků získáme odhad ",
                # "celkových příjmů ze vstupenek. V naší aplikaci na základě analýzy ",
                # "Vámi nahraných dat rozdělíme návštěvníky koncertů podle jejich ",
                # "CLV do tří skupin (vysoká, střední a nízká celoživotní hodnota)."
            # ),
            
            # tags$br(),
            # tags$br(),
            
            # HTML(
                # "<b>RFM analýza</b> přiděluje každému návštěvníkovi číselné skóre, ",
                # "které vychází z následujících parametrů:",
                # "<ul>
                  # <li> počet dní od posledního nákupu vstupenek jednotlivými 
                  # návštěvníky (Recency)</li>
                  # <li> počet nákupů vstupenek u jednotlivých návštěvníků ve 
                  # sledovaném období (Frequency)</li>
                  # <li> průměrná útrata jednotlivých návštěvníků za jeden 
                  # nákup ve sledovaném období (Monetary)</li>
                # </ul>",
                # "Vysoké skóre mají návštěvníci, kteří nakoupili nedávno, nakupují ",
                # "často (pravidelně) a mají vysokou útratu.",
                # "Analýzou lze rozdělit návštěvníky do tří skupin (klíčoví, průměrní ",
                # "a&nbsp;nevýznamní zákazníci)."
            # ),
            
            # tags$br(),
            # tags$br(),
            
            HTML(
                "<b>Vzorový průchod aplikací:</b>",
                "<ol>
                  <li> <b>Nahrání dat</b> -- nahrajte data z vašeho počítače ve formátu *.csv
                  nebo *.txt (podrobný postup v technické dokumentaci).
                  </li>
                  <li> <b>Výstup 1: RFM analýza</b> -- po kliknutí na položku RFM analýza v levém menu
                  proběhne výpočet na základě dat načtených v předchozím kroku a aplikace
                  nabídne ke stažení tabulku obsahující výpočet RFM pro každého návštěvníka.
                  </li>
                  <li> <b>Výstup 2: CLV analýza</b> -- po kliknutí na položku CLV 
                  analýza v levém menu proběhne výpočet na základě dat načtených v předchozím
                  kroku a aplikace nabídne ke stažení tabulku obsahující výpočet CLV pro
                  každého návštěvníka.
                  </li>
                </ol>"
            ),
            
            # HTML(
                # "<b>Vzorový průchod aplikací:</b>",
                # "<ol>
                  # <li> Nahrání dat -- nahrajte data z vašeho počítače ve formátu .csv
                  # (textový soubor, v němž řádky odpovídají jednotlivým záznamům a 
                  # jednotlivé položky v řádcích jsou odděleny středníkem -- takový 
                  # soubor lze snadno získat vyexportováním z MS Excel nebo 
                  # Google Sheets). 
                  # Transakční data je zapotřebí nahrát vždy. Transakční data se skládají 
                  # z těchto údajů: identifikátor návštěvníka, datum transakce (tzn. datum 
                  # prodeje), cenu za vstupenku a počet zakoupených vstupenek. Všechny 
                  # položky jsou povinné a je nutné dodržet uvedené pořadí položek (sloupců). 
                  # Ukázkový záznam: IF;Date;Price;Amount. 
                  # Můžete nahrát také netransakční data. Ty obsahují údaje o
                  # jednotlivých akcích, zejména dnu v&nbsp;týdnu, kdy se konají,
                  # o jejich žánrovém zařazení, a o relativní míře známosti
                  # interpreta.
                  # </li>
                  # <li> RFM analýza -- po kliknutí na položku RFM analýza v levém menu
                  # proběhne výpočet na základě dat načtených v předchozím kroku a aplikace
                  # nabídne ke stažení dataset obsahující výpočet RFM pro každého návštěvníka.
                  # </li>
                  # <li> CLV analýza transakční/netransakční -- po kliknutí na položku CLV 
                  # analýza v levém menu proběhne výpočet na základě dat načtených v předchozím
                  # kroku a aplikace nabídne ke stažení dataset obsahující výpočet CLV pro
                  # každého návštěvníka/ a jejich rozdělení do tří skupin.
                  # </li>
                # </ol>"
            # ),
            
            tags$br(),
            #tags$br(),
            
            HTML(
                "<b>Podle typu nahraných dat aplikace poskytuje různé výstupy:</b>"
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "<center>
                <figure>
                    <img src = '_tabulka_dat_a_vystupu_.png' width = '600px'>
                </figure>
                </center>"
            ),
            
            tags$br(),
            tags$br(),
            
            "Tato aplikace vznikla za podpory Technologické agentury ČR (TAČR).",
            
            tags$br(),
            tags$br(),
            
            HTML(
                "<center>",
                "<a
                    href = https://www.vse.cz/'
                    target = '_blank'
                >
                <img
                    src = 'vse_logo.png',
                    style = 'height: 140px;'
                >
                </a>",
                "<a
                    href = 'https://www.tacr.cz/'
                    target = '_blank'
                >
                <img
                    src = 'tacr_logo.png',
                    style = 'height: 105px;'
                >
                </a>",
                #"<img src = 'vse_logo.png' width = '160px'>",
                #"<img src = 'tacr_logo.png' width = '120px'>",
                "</center>"
            ),
            
            # HTML(
                # "
                # <div class='row'>
                  # <div class='column'>
                    # <right>
                    # <img src = 'vse_logo.png' width = '200px'>
                    # </right>
                  # </div>
                  # <div class='column'>
                    # <left>
                    # <img src = 'tacr_logo.png' width = '200px'>
                    # </left>
                  # </div>
                # </div> 
                # "
            # ),
            
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
            
        ),
        
        
        ## obsah záložky "Načtení transakčních dat" ---------------------------
        
        tabItem(
            
            tabName = "upload_transaction_data_tab",
            
            # HTML(
                # "Sekce nahrání dat slouží k nahrání vašich vlastních dat ",
                # "do aplikace, která vám po jejich načtení zobrazí základní ",
                # "informace o nahraném data setu. Je doporučeno zkontrolovat, ",
                # "zda informace odpovídají očekávání. ",
                # "Do aplikace můžete nahrát dva typy dat:",
                # "<ol>
                  # <li> Transakční data</li>
                  # <li> Netransakční data</li>
                # </ol>",
                # "Transakční data reprezentují transakční toky -- typicky jsou ",
                # "takovými informacemi údaje o objednávkách, údaje o fakturách ",
                # "či jiných finančních dokladech. Naproti tomu netransakční ",
                # "data reprezentují informace, které se finančních toků ",
                # "netýkají. <b>Pro použití hlavních funkcí aplikace je nezbytné ",
                # "nahrát alespoň transakční data.</b>"
            # ),
            
            HTML(
                "Pro použití hlavních funkcí aplikace ",
                "(Segmentace zákazníků, Odhad budoucí útraty) je nezbytné ",
                "nahrát alespoň transakční data."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "Transakční data pro import do aplikace musí splňovat ",
                "následující podmínky:",
                "<ul>
                  <li> vstupní soubor ve formátu *.csv nebo *.txt.</li>
                  <li> soubor musí obsahovat tyto čtyři sloupce
                    <ul>
                      <li> <b>ID</b> -- jedná se o jednoznačný identifikátor 
                      zákazníka. Pokud zákazník objednal vícekrát,
                      toto ID musí být ve všech případech stejné.</li>
                      <li> <b>Date</b> -- je datum uskutečnění
                      objednávky/nákupu ve formátu RRRR-MM-DD.</li>
                      <li> <b>Price</b> -- jednotková cena za kus (např.
                      za jednu vstupenku nebo za jedno předplatné).</li>
                      <li> <b>Amount</b> -- počet
                      kusů vstupenek v objednávce.</li>
                    </ul>
                  </li>
                </ul>",
                "Detailnější informace jsou dostupné v ",
                "<a
                    href = 'http://aplikace-auditorium.online/dokumentace.html'
                    target = '_blank'
                >
                    uživatelské dokumentaci
                </a>",
                ". ",
                "Vzor vstupních dat ve formátu .csv je ke stažení ",
                "<a
                    href = 'http://aplikace-auditorium.online/data.html'
                    target = '_blank'
                >
                    zde
                </a>",
                "."
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## volené parametry analýzy ---------------------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### parametry použitých dat ------------------------------
                
                    h4("Parametry použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            
            ## jednotlivé ovládací prvky volitelných parametrů načtení dat ----
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    fileInput(
                        inputId = "my_transaction_file",
                        label = list(
                            "Nahrání vlastních dat",
                            bsButton(
                                inputId = "my_transaction_file_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_transaction_file_info",
                                title = "Info",
                                content = "Vložte .csv nebo .txt dokument (u&nbsp;vkládaného datasetu je vhodné dodržet názvy sloupců, tedy ID, Date, Price, Amount; nikoliv však nutně jejich pořadí).",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        accept = c(
                            ".csv",
                            ".txt"
                        ),
                        buttonLabel = "Vybrat soubor...",
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    radioButtons(
                        inputId = "my_transaction_separator_option",
                        #label = "Oddělovač",
                        label = list(
                            "Oddělovač",
                            bsButton(
                                inputId =
                                    "my_transaction_separator_option_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_transaction_separator_option_info",
                                title = "Info",
                                content = 
                                    "Znak oddělující v řádku položky jednotlivých sloupců.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        choices = c(
                            "středník" = ";",
                            "čárka" = ",",
                            "tabulátor" = "\t"
                        ),
                        selected = ";"
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    list(
                        HTML("<b>Názvy sloupců</b>"),
                        bsButton(
                            inputId = "my_transaction_header_option_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_transaction_header_option_info",
                            title = "Info",
                            content =
                                "Názvy jednotlivých sloupců uvedené v prvním řádku souboru.",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "my_transaction_header_option",
                        label = "Jsou přítomny jmenovky sloupců?",
                        value = TRUE
                    )
                    
                ),
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    list(
                        HTML("<b>Řetězec datových typů</b>"),
                        bsButton(
                            inputId = "my_transaction_data_types_string_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_transaction_data_types_string_info",
                            title = "Info",
                            # content = "Základní informace zde.",
                            content = paste(
                                "Pokud Vámi nahraný soubor bude zcela splňovat výše uvedené podmínky, zadejte 'SDNN'. Stejný řetězec zadejte i v případě, že si nejste jisti. Uvedená sekvence definuje datové typy proměnných v načteném datasetu. Dodržte přitom pořadí proměnných.",
                                " ",
                                "N = numerická hodnota",
                                "D = hodnota typu datum (dle schématu RRRR-MM-DD)",
                                "S = textová hodnota",
                                "L = logická hodnota",
                                sep = "<br>"
                            ),
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    tags$br(), 
                    
                    HTML(
                        paste(
                            '<textarea style="height:30px;width:120px;"',
                            'id="my_transaction_data_types_string"',
                            'placeholder="SDNN..."></textarea>',
                            sep = ""
                        )
                    )#,
                    
                    # tags$br(),
                    
                    # "N = numerická hodnota",
                    
                    # tags$br(),
                    
                    # "D = hodnota typu datum RRRR-MM-DD",
                    
                    # tags$br(),
                    
                    # "S = textová hodnota",
                    
                    # tags$br(),
                    
                    # "L = logická hodnota",
                    
                    # tags$br()
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    #### použití vestavěných dat ------------------------------
                    
                    list(
                        HTML("<b>Vestavěná data</b>"),
                        bsButton(
                            inputId =
                                "to_use_my_transaction_inbuilt_data_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "to_use_my_transaction_inbuilt_data_info",
                            title = "Info",
                            content = "Chcete využít vestavěná, správně formátovaná vzorová data?",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "to_use_my_transaction_inbuilt_data",
                        label = "Použít vestavěná data?",
                        value = FALSE
                    ),
                    
                    uiOutput(
                        outputId = "my_transaction_inbuilt_data_selection"
                    )
                    
                )
                
                
                ## ------------------------------------------------------------
                
            ),
            
            
            ## dolní panel se zobrazením načtených dat ------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### zobrazení použitých dat ------------------------------
                    
                    h4("Zobrazení použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            fluidRow(
                
                ## dolní (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 9,
                    offset = 0,
                    
                    # dataTableOutput(
                        # outputId = "my_transaction_data_table"
                    # ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_transaction_data_positive_message_text"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_transaction_data_negative_message_text"
                        )
                    ),
                    
                    uiOutput(
                        outputId = "my_transaction_data_message_text_gap"
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        dataTableOutput(
                            outputId = "my_transaction_data_table"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_transaction_data_table_info_handler"
                        )
                    ),
                    
                    # tableOutput(
                        # outputId = "my_transaction_data_table"
                    # ),
                    
                    uiOutput(
                        outputId =
                            "my_transaction_variable_types_table_upper_hline"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_transaction_variable_types_table_label"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_transaction_variable_types_table_mid_hline"
                    ),
                    
                    tableOutput(
                        outputId = "my_transaction_variable_types_table"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_transaction_variable_types_table_lower_hline"
                    ),
                    
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br()
                    
                )
                
            )
            
        ),
        
        
        ## obsah záložky "Načtení netransakčních dat" -------------------------
        
        tabItem(
            
            tabName = "upload_non_transaction_data_tab",
            
            # HTML(
                # "Sekce nahrání dat slouží k nahrání vašich vlastních dat ",
                # "do aplikace, která vám po jejich načtení zobrazí základní ",
                # "informace o nahraném data setu. Je doporučeno zkontrolovat, ",
                # "zda informace odpovídají očekávání. ",
                # "Do aplikace můžete nahrát dva typy dat:",
                # "<ol>
                  # <li> Transakční data</li>
                  # <li> Netransakční data</li>
                # </ol>",
                # "Transakční data reprezentují transakční toky -- typicky jsou ",
                # "takovými informacemi údaje o objednávkách, údaje o fakturách ",
                # "či jiných finančních dokladech. Naproti tomu netransakční ",
                # "data reprezentují informace, které se finančních toků ",
                # "netýkají. <b>Pro použití hlavních funkcí aplikace je nezbytné ",
                # "nahrát alespoň transakční data.</b>"
            # ),
            
            # tags$br(),
            # tags$br(),
            
            HTML(
                "Aplikace funguje i bez netransakčních dat, nicméně netransakční data ",
                "obohatí analýzy o další výstupy (Preference klíčového zákazníka)."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "Netransakční data pro import do aplikace musí splňovat ",
                "následující podmínky:",
                "<ul>
                  <li> vstupní soubor ve formátu *.csv nebo *.txt</li>
                  <li> soubor musí obsahovat tyto čtyři sloupce
                    <ul>
                      <li> <b>Date</b> -- kalendářní datum konání akce.</li>
                      <li> <b>WeekDate</b> -- den v týdnu, kdy se
                      akce konala.</li>
                      <li> <b>KnownArtist</b> -- relativní známost
                      interpretera (známý, méně známý, neznámý).</li>
                      <li> <b>MajorEra</b> -- hlavní žánrové
                      zaměření akce.</li>
                      <li> <b>MinorEra</b> -- vedlejší žánrové
                      zaměření akce.</li>
                      <li> <b>ConcertType</b> -- typ koncertní akce.</li>
                    </ul>
                  </li>
                </ul>",
                "Detailnější informace jsou dostupné v ",
                "<a
                    href = 'http://aplikace-auditorium.online/dokumentace.html'
                    target = '_blank'
                >
                    uživatelské dokumentaci
                </a>",
                ". ",
                "Vzor vstupních dat ve formátu .csv je ke stažení ",
                "<a
                    href = 'http://aplikace-auditorium.online/data.html'
                    target = '_blank'
                >
                    zde
                </a>",
                "."
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## volené parametry analýzy ---------------------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### parametry použitých dat ------------------------------
                
                    h4("Parametry použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            
            ## jednotlivé ovládací prvky volitelných parametrů načtení dat ----
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    fileInput(
                        inputId = "my_non_transaction_file",
                        label = list(
                            "Nahrání vlastních dat",
                            bsButton(
                                inputId = "my_non_transaction_file_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_non_transaction_file_info",
                                title = "Info",
                                content = "Vložte .csv nebo .txt dokument.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        accept = c(
                            ".csv",
                            ".txt"
                        ),
                        buttonLabel = "Vybrat soubor...",
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    radioButtons(
                        inputId = "my_non_transaction_separator_option",
                        #label = "Oddělovač",
                        label = list(
                            "Oddělovač",
                            bsButton(
                                inputId =
                                    "my_non_transaction_separator_option_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id =
                                    "my_non_transaction_separator_option_info",
                                title = "Info",
                                content =
                                    "Znak oddělující v řádku položky jednotlivých sloupců.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        choices = c(
                            "středník" = ";",
                            "čárka" = ",",
                            "tabulátor" = "\t"
                        ),
                        selected = ";"
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    list(
                        HTML("<b>Názvy sloupců</b>"),
                        bsButton(
                            inputId = "my_non_transaction_header_option_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_non_transaction_header_option_info",
                            title = "Info",
                            content =
                                "Názvy jednotlivých sloupců uvedené v prvním řádku souboru.",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "my_non_transaction_header_option",
                        label = "Jsou přítomny jmenovky sloupců?",
                        value = TRUE
                    )
                    
                ),
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    list(
                        HTML("<b>Řetězec datových typů</b>"),
                        bsButton(
                            inputId =
                                "my_non_transaction_data_types_string_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_non_transaction_data_types_string_info",
                            title = "Info",
                            # content = "Základní informace zde.",
                            content = paste(
                                "Pokud Vámi nahraný soubor bude zcela splňovat výše uvedené podmínky, zadejte 'DSSSSS'. Stejný řetězec zadejte i v případě, že si nejste jisti. Uvedená sekvence definuje datové typy proměnných v načteném datasetu. Dodržte přitom pořadí proměnných.",
                                " ",
                                "N = numerická hodnota",
                                "D = hodnota typu datum (dle schématu RRRR-MM-DD)",
                                "S = textová hodnota",
                                "L = logická hodnota",
                                sep = "<br>"
                            ),
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    tags$br(), 
                    
                    HTML(
                        paste(
                            '<textarea style="height:30px;width:120px;"',
                            'id="my_non_transaction_data_types_string"',
                            'placeholder="DSSSSS..."></textarea>',
                            sep = ""
                        )
                    )#,
                    
                    # tags$br(),
                    
                    # "N = numerická hodnota",
                    
                    # tags$br(),
                    
                    # "D = hodnota typu datum RRRR-MM-DD",
                    
                    # tags$br(),
                    
                    # "S = textová hodnota",
                    
                    # tags$br(),
                    
                    # "L = logická hodnota",
                    
                    # tags$br()
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    #### použití vestavěných dat ------------------------------
                    
                    list(
                        HTML("<b>Vestavěná data</b>"),
                        bsButton(
                            inputId =
                                "to_use_my_non_transaction_inbuilt_data_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "to_use_my_non_transaction_inbuilt_data_info",
                            title = "Info",
                            content = "Chcete využít vestavěná, správně formátovaná vzorová data?",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "to_use_my_non_transaction_inbuilt_data",
                        label = "Použít vestavěná data?",
                        value = FALSE
                    ),
                    
                    uiOutput(
                        outputId = "my_non_transaction_inbuilt_data_selection"
                    )
                    
                )
                
                
                ## ------------------------------------------------------------
                
            ),
            
            
            ## dolní panel se zobrazením načtených dat ------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### zobrazení použitých dat ------------------------------
                    
                    h4("Zobrazení použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            fluidRow(
                
                ## dolní (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 9,
                    offset = 0,
                    
                    # dataTableOutput(
                        # outputId = "my_non_transaction_data_table"
                    # ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_non_transaction_data_positive_message_text"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_non_transaction_data_negative_message_text"
                        )
                    ),
                    
                    uiOutput(
                        outputId = "my_non_transaction_data_message_text_gap"
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        dataTableOutput(
                            outputId = "my_non_transaction_data_table"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_non_transaction_data_table_info_handler"
                        )
                    ),
                    
                    # div(
                        # style = "display: inline-block;vertical-align:top;",
                        # dataTableOutput(
                            # outputId = "my_non_transaction_data_table"
                        # )
                    # ),
                    # div(
                        # style = "display: inline-block;vertical-align:top;",
                        # list(
                            # bsButton(
                                # inputId =
                                    # "my_non_transaction_data_table_info",
                                # label = "",
                                # icon = icon("info"),
                                # style = "info",
                                # size = "extra-small"
                            # ),
                            # bsPopover(
                                # id = "my_non_transaction_data_table_info",
                                # title = "Info",
                                # content = "Vyhledávání řetězce ve všech sloupcích.",
                                # placement = "right",
                                # trigger = "hover",
                                # options = list(container = "body")
                            # )
                        # )
                    # ),
                    
                    # tableOutput(
                        # outputId = "my_non_transaction_data_table"
                    # ),
                    
                    uiOutput(
                        outputId =
                        "my_non_transaction_variable_types_table_upper_hline"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_non_transaction_variable_types_table_label"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_non_transaction_variable_types_table_mid_hline"
                    ),
                    
                    tableOutput(
                        outputId = "my_non_transaction_variable_types_table"
                    ),
                    
                    uiOutput(
                        outputId =
                        "my_non_transaction_variable_types_table_lower_hline"
                    ),
                    
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br()
                    
                )
                
            )
            
        ),
        
        
        ## obsah záložky "Nahrání plánované sezóny" ---------------------------
        
        tabItem(
            
            tabName = "upload_fictive_non_transaction_data_tab",
            
            HTML(
                "Pro získání výstupu v záložce 'CLV analýza' a podzáložce ",
                "'Odhad budoucí útraty vzhledem k plánované sezóně' je nezbytné ",
                "nahrát kromě transakčních a netransakčních dat i data o budoucí ",
                "plánované sezóně."
            ),
            
            tags$br(),
            tags$br(),
            
            # HTML(
                # "Aplikace funguje i bez netransakčních dat nebo dat plánované ",
                # "budoucí sezóny, nicméně netransakční data a data plánované budoucí sezóny",
                # "obohatí analýzy o další výstupy."
            # ),
            
            # tags$br(),
            # tags$br(),
            
            HTML(
                "Data plánované budoucí sezóny pro import do aplikace musí splňovat ",
                "následující podmínky:",
                "<ul>
                  <li> vstupní soubor ve formátu *.csv nebo *.txt</li>
                  <li> soubor musí obsahovat tyto čtyři sloupce
                    <ul>
                      <li> <b>Date</b> -- kalendářní datum konání akce.</li>
                      <li> <b>WeekDate</b> -- den v týdnu, kdy se
                      akce konala.</li>
                      <li> <b>KnownArtist</b> -- relativní známost
                      interpretera (známý, méně známý, neznámý).</li>
                      <li> <b>MajorEra</b> -- hlavní žánrové
                      zaměření akce.</li>
                      <li> <b>MinorEra</b> -- vedlejší žánrové
                      zaměření akce.</li>
                      <li> <b>ConcertType</b> -- typ koncertní akce.</li>
                    </ul>
                  </li>
                </ul>",
                "Detailnější informace jsou dostupné v ",
                "<a
                    href = 'http://aplikace-auditorium.online/dokumentace.html'
                    target = '_blank'
                >
                    uživatelské dokumentaci
                </a>",
                ". ",
                "Vzor vstupních dat ve formátu .csv je ke stažení ",
                "<a
                    href = 'http://aplikace-auditorium.online/data.html'
                    target = '_blank'
                >
                    zde
                </a>",
                ". ",
                "Data plánované budoucí sezóny mají tedy stejný formát jako ",
                "netranskační data."
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## volené parametry analýzy ---------------------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### parametry použitých dat ------------------------------
                
                    h4("Parametry použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            
            ## jednotlivé ovládací prvky volitelných parametrů načtení dat ----
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    fileInput(
                        inputId = "my_fictive_non_transaction_file",
                        label = list(
                            "Nahrání vlastních dat",
                            bsButton(
                                inputId = "my_fictive_non_transaction_file_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_fictive_non_transaction_file_info",
                                title = "Info",
                                content = "Vložte .csv nebo .txt dokument.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        accept = c(
                            ".csv",
                            ".txt"
                        ),
                        buttonLabel = "Vybrat soubor...",
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    radioButtons(
                        inputId = "my_fictive_non_transaction_separator_option",
                        #label = "Oddělovač",
                        label = list(
                            "Oddělovač",
                            bsButton(
                                inputId =
                                    "my_fictive_non_transaction_separator_option_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id =
                                    "my_fictive_non_transaction_separator_option_info",
                                title = "Info",
                                content =
                                    "Znak oddělující v řádku položky jednotlivých sloupců.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        choices = c(
                            "středník" = ";",
                            "čárka" = ",",
                            "tabulátor" = "\t"
                        ),
                        selected = ";"
                    )
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    list(
                        HTML("<b>Názvy sloupců</b>"),
                        bsButton(
                            inputId = "my_fictive_non_transaction_header_option_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_fictive_non_transaction_header_option_info",
                            title = "Info",
                            content =
                                "Názvy jednotlivých sloupců uvedené v prvním řádku souboru.",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "my_fictive_non_transaction_header_option",
                        label = "Jsou přítomny jmenovky sloupců?",
                        value = TRUE
                    )
                    
                ),
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    list(
                        HTML("<b>Řetězec datových typů</b>"),
                        bsButton(
                            inputId =
                                "my_fictive_non_transaction_data_types_string_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "my_fictive_non_transaction_data_types_string_info",
                            title = "Info",
                            # content = "Základní informace zde.",
                            content = paste(
                                "Pokud Vámi nahraný soubor bude zcela splňovat výše uvedené podmínky, zadejte 'DSSSSS'. Stejný řetězec zadejte i v případě, že si nejste jisti. Uvedená sekvence definuje datové typy proměnných v načteném datasetu. Dodržte přitom pořadí proměnných.",
                                " ",
                                "N = numerická hodnota",
                                "D = hodnota typu datum (dle schématu RRRR-MM-DD)",
                                "S = textová hodnota",
                                "L = logická hodnota",
                                sep = "<br>"
                            ),
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    tags$br(), 
                    
                    HTML(
                        paste(
                            '<textarea style="height:30px;width:120px;"',
                            'id="my_fictive_non_transaction_data_types_string"',
                            'placeholder="DSSSSS..."></textarea>',
                            sep = ""
                        )
                    )#,
                    
                    # tags$br(),
                    
                    # "N = numerická hodnota",
                    
                    # tags$br(),
                    
                    # "D = hodnota typu datum RRRR-MM-DD",
                    
                    # tags$br(),
                    
                    # "S = textová hodnota",
                    
                    # tags$br(),
                    
                    # "L = logická hodnota",
                    
                    # tags$br()
                    
                ),
                
                column(
                    
                    width = 2,
                    offset = 0,
                    
                    #### použití vestavěných dat ------------------------------
                    
                    list(
                        HTML("<b>Vestavěná data</b>"),
                        bsButton(
                            inputId =
                                "to_use_my_fictive_non_transaction_inbuilt_data_info",
                            label = "",
                            icon = icon("info"),
                            style = "info",
                            size = "extra-small"
                        ),
                        bsPopover(
                            id = "to_use_my_fictive_non_transaction_inbuilt_data_info",
                            title = "Info",
                            content = "Chcete využít vestavěná, správně formátovaná vzorová data?",
                            placement = "right",
                            trigger = "hover",
                            options = list(container = "body")
                        )
                    ),
                    
                    checkboxInput(
                        inputId = "to_use_my_fictive_non_transaction_inbuilt_data",
                        label = "Použít vestavěná data?",
                        value = FALSE
                    ),
                    
                    uiOutput(
                        outputId = "my_fictive_non_transaction_inbuilt_data_selection"
                    )
                    
                )
                
                
                ## ------------------------------------------------------------
                
            ),
            
            
            ## dolní panel se zobrazením načtených dat ------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    #### zobrazení použitých dat ------------------------------
                    
                    h4("Zobrazení použitých dat"),
                    
                    HTML("<br>")
                    
                )
                
            ),
            
            fluidRow(
                
                ## dolní (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 9,
                    offset = 0,
                    
                    # dataTableOutput(
                        # outputId = "my_fictive_non_transaction_data_table"
                    # ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_fictive_non_transaction_data_positive_message_text"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_fictive_non_transaction_data_negative_message_text"
                        )
                    ),
                    
                    uiOutput(
                        outputId = "my_fictive_non_transaction_data_message_text_gap"
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        dataTableOutput(
                            outputId = "my_fictive_non_transaction_data_table"
                        )
                    ),
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput(
                            outputId = "my_fictive_non_transaction_data_table_info_handler"
                        )
                    ),
                    
                    uiOutput(
                        outputId =
                        "my_fictive_non_transaction_variable_types_table_upper_hline"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_fictive_non_transaction_variable_types_table_label"
                    ),
                    
                    uiOutput(
                        outputId =
                            "my_fictive_non_transaction_variable_types_table_mid_hline"
                    ),
                    
                    tableOutput(
                        outputId = "my_fictive_non_transaction_variable_types_table"
                    ),
                    
                    uiOutput(
                        outputId =
                        "my_fictive_non_transaction_variable_types_table_lower_hline"
                    ),
                    
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br()
                    
                )
                
            )
            
        ),
        
        
        ## obsah záložky "Segmentace návštěvníků" -----------------------------
        
        tabItem(
            
            tabName = "analysis_RFM_clients_segmentation_tab",
            
            #"# obsah záložky s RFM analýzou",
            
            HTML(
                "<b>RFM analýza</b> přiděluje každému návštěvníkovi číselné skóre, ",
                "které vychází z následujících parametrů:",
                "<ul>
                  <li> počet dní od posledního nákupu vstupenek jednotlivými 
                  návštěvníky (Recency)</li>
                  <li> počet nákupů vstupenek u jednotlivých návštěvníků ve 
                  sledovaném období (Frequency)</li>
                  <li> průměrná útrata jednotlivých návštěvníků za jeden 
                  nákup ve sledovaném období (Monetary)</li>
                </ul>",
                "Vysoké skóre mají návštěvníci, kteří nakoupili nedávno, nakupují ",
                "často (pravidelně) a mají vysokou útratu.",
                "Analýzou návštěvníky rozdělíte do dvou skupin: klíčoví návštěvníci ",
                "a&nbsp;ostatní. Procentuální poměr klíčových zákazníků lze nastavit."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "<ul>
                  <li> <b>R = recency</b> -- kdy zákazník naposledy nakoupil
                  (k dnešnímu datu nebo k datu analýzy)</li>
                  <li> <b>F = frequency</b> -- kolik nákupů/objednávek 
                  zákazník udělal</li>
                  <li> <b>M = monetary</b> -- hodnota zákazníkových
                  objednávek (suma nebo průměr)</li>
                </ul>"
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## posuvník pro určení horních $k$ procent ------------------------
            
            fluidRow(
                
                column(
                    
                    width = 4,
                    offset = 4,
                    
                    sliderInput(
                        inputId = "my_transaction_top_k_percent_slider",
                        # label =
                           # "Váhy pro recency, frequency, monetary",
                        label = list(
                            "Nastavení podílu klíčových zákazníků (dle RFM skóre)",
                            bsButton(
                                inputId =
                                    "my_transaction_top_k_percent_slider_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_transaction_top_k_percent_slider_info",
                                title = "Info",
                                content = "Posouváním kolečka na stupnici můžete měnit podíl klíčových zákazníků.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        min = 5.00,
                        max = 50.00,
                        value = 20.00,
                        step = 1.00,
                        width = "100%"
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 3,
                    
                    plotOutput(
                        outputId =
                            "my_transaction_top_k_percent_piechart",
                        width = "250px",
                        height = "250px",
                    )
                    
                ),
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    plotOutput(
                        outputId =
                            "my_transaction_top_k_percent_amount_piechart",
                        width = "250px",
                        height = "250px",
                    )
                    
                )
                
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## RFM analýza ----------------------------------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    h4("Volené parametry analýzy")
                    
                ),
                
                column(
                    
                    width = 4,
                    offset = 1,
                    
                    actionButton(
                        inputId = "my_transaction_settings_toggle_button",
                        label = "Odkrýt / skrýt ovládací prvky"
                    )
                    
                )
                
            ),
            
            
            fluidRow(
                
                #### váhy -----------------------------------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    sliderInput(
                        inputId = "my_transaction_weights_slider",
                        # label =
                           # "Váhy pro recency, frequency, monetary",
                        label = list(
                            "Váhy pro recency, frequency, monetary",
                            bsButton(
                                inputId =
                                    "my_transaction_weights_slider_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_transaction_weights_slider_info",
                                title = "Info",
                                content = "Váhy vyjadřují poměrný význam proměných recency, frequency a monetary vůči sobě.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        min = 0.00,
                        max = 1.00,
                        value = c(
                            0.333,
                            0.666
                        ),
                        step = 0.001,
                        width = "100%"
                    )
                    ),
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId = "my_transaction_weights_text_display"
                    )
                    )
                    
                ),
                
                
                ###### datum cenzorování zleva --------------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    dateInput(
                        inputId = "my_transaction_recency_left_censoring",
                        # label =
                            # "Datum cenzorování zleva [DD. MM. RRRR]",
                        label = list(
                            "Datum cenzorování zleva",
                            bsButton(
                                inputId =
                                "my_transaction_recency_left_censoring_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id =
                                "my_transaction_recency_left_censoring_info",
                                title = "Info",
                                content = "Datum je ve formátu DD. MM. RRRR.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        value = as.Date(
                            paste(
                                as.character(
                                    as.numeric(
                                        format(
                                            Sys.Date(),
                                            "%Y"
                                        )
                                    ) - 20
                                ),
                                format(
                                    Sys.Date(),
                                    "%m"
                                ),
                                format(
                                    Sys.Date(),
                                    "%d"
                                ),
                                sep = "-"
                            )
                        ),
                        min = NULL,
                        max = NULL,
                        format = "dd. mm. yyyy",
                        startview = "month",
                        weekstart = 1,
                        language = "cs",
                        width = NULL
                    )
                    )
                    
                ),
                
                
                ###### datum cenzorování zprava -------------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    dateInput(
                        inputId = "my_transaction_recency_right_censoring",
                        # label =
                            # "Datum cenzorování zprava [DD. MM. RRRR]",
                        label = list(
                            "Datum cenzorování zprava",
                            bsButton(
                                inputId =
                                "my_transaction_recency_right_censoring_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id =
                                "my_transaction_recency_right_censoring_info",
                                title = "Info",
                                content = "Datum je ve formátu DD. MM. RRRR.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        value = Sys.Date(),
                        min = NULL,
                        max = NULL,
                        format = "dd. mm. yyyy",
                        startview = "month",
                        weekstart = 1,
                        language = "cs",
                        width = NULL
                    )
                    )
                    
                )
                
            ),
            
            shinyjs::hidden(
            uiOutput(
                outputId = "my_transaction_hr_definition_middle"
            )
            ),
            #tags$hr(),
            #tags$br(),
            #HTML('<hr style="purple: black; size: 10;">'),
            
            fluidRow(
                
                ###### posuvník pro hranice recency ---------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_recency_borders_slider_display"
                    )
                    ),
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_recency_borders_text_display"
                    )
                    )
                    
                ),
                
                
                ###### posuvník pro hranice frequency -------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_frequency_borders_slider_display"
                    )
                    ),
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_frequency_borders_text_display"
                    )
                    )
                    
                ),
                
                
                ###### posuvník pro hranice monetary --------------------------
                
                column(
                    
                    width = 4,
                    offset = 0,
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_monetary_borders_slider_display"
                    )
                    ),
                    
                    shinyjs::hidden(
                    uiOutput(
                        outputId =
                            "my_transaction_monetary_borders_text_display"
                    )
                    )
                    
                )
                
            ),
            
            shinyjs::hidden(
            uiOutput(
                outputId = "my_transaction_hr_definition_bottom"
            )
            ),
            
            # tags$hr(),
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    h4("Výstupy analýzy")
                    
                )
                
            ),
            
            # tags$hr(),
            
            tags$hr(),
            
            fluidRow(
                
                ###############################################################
                
                ## pravý (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 12,
                    offset = 0,
                    
                    fluidRow(
                        
                        column(
                            
                            width = 12,
                            #offset = 0,
                            aling = "center",
                            
                            #verbatimTextOutput("pokus"),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_transaction_recency_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId = "my_transaction_recency_introduction_text_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_transaction_recency_quantitative_barplot"
                            ),
                            
                            # plotOutput(
                                # outputId =
                                    # "my_transaction_recency_double_histogram"
                            # ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_transaction_frequency_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId = "my_transaction_frequency_introduction_text_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_transaction_frequency_quantitative_barplot"
                            ),
                            
                            # plotOutput(
                                # outputId =
                                    # "my_transaction_frequency_double_histogram"
                            # ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_transaction_monetary_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId = "my_transaction_monetary_introduction_text_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_transaction_monetary_quantitative_barplot"
                            ),
                            
                            # plotOutput(
                                # outputId =
                                    # "my_transaction_monetary_double_histogram"
                            # ),
                            
                            tags$br(),
                            tags$br(),
                            
                            plotOutput(
                                outputId =
                                    "my_transaction_total_RFM_score_histogram"
                            ),
                            
                            tags$br()
                            
                        )
                        
                    )
                    
                )
                
            ),
            
            tags$hr(),
            
            fluidRow(
                
                column(
                    
                    width = 4,
                    offset = 4,
                    
                    uiOutput(
                        outputId =
                        "my_transaction_final_data_download_button"
                    )
                    
                )
                
            ),
            
            fluidRow(
                
                ###############################################################
                
                ## pravý (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 12,
                    offset = 0,
                    
                    fluidRow(
                        
                        column(
                            
                            width = 12,
                            #offset = 0,
                            aling = "center",
                            
                            # uiOutput(
                                # outputId =
                                # "my_transaction_final_data_download_button"
                            # ),
                            
                            dataTableOutput(
                                outputId = "my_transaction_final_data_table"
                            ),
                            
                            # tableOutput(
                                # outputId = "my_transaction_final_data_table"
                            # ),
                            
                            tags$hr(),
                            
                            tags$br(),
                            
                            # plotOutput(
                                # outputId = "my_transaction_recency_histogram"
                            # ),
                            
                            # tags$br(),
                            
                            # plotOutput(
                                # outputId = "my_transaction_frequency_histogram"
                            # ),
                            
                            # tags$br(),
                            
                            # plotOutput(
                                # outputId = "my_transaction_monetary_histogram"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId = "my_transaction_recency_barplot"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId = 
                                        # "my_transaction_frequency_barplot"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                        # "my_transaction_monetary_barplot"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                        # "my_transaction_monetary_vs_recency"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                        # "my_transaction_monetary_vs_frequency"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                        # "my_transaction_recency_vs_frequency"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                        # "my_transaction_total_score_histogram"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                    # "my_transaction_monetary_vs_recency_heatmap"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                    # "my_transaction_monetary_vs_frequency_heatmap"
                                # ),
                                # align = "center"
                            # ),
                            
                            # tags$br(),
                            
                            # div(
                                # plotOutput(
                                    # outputId =
                                    # "my_transaction_recency_vs_frequency_heatmap"
                                # ),
                                # align = "center"
                            # ),
                            
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br()
                            
                        )
                        
                    )
                    
                )
                
            )
            
        ),
        
        
        ## obsah záložky "Preference klíčového zákazníka" ---------------------
        
        tabItem(
            
            tabName = "analysis_RFM_clients_preferences_tab",
            
            #"# obsah záložky s RFM analýzou",
            
            HTML(
                "<b>RFM analýza</b> přiděluje každému návštěvníkovi číselné skóre, ",
                "které vychází z následujících parametrů:",
                "<ul>
                  <li> počet dní od posledního nákupu vstupenek jednotlivými 
                  návštěvníky (Recency)</li>
                  <li> počet nákupů vstupenek u jednotlivých návštěvníků ve 
                  sledovaném období (Frequency)</li>
                  <li> průměrná útrata jednotlivých návštěvníků za jeden 
                  nákup ve sledovaném období (Monetary)</li>
                </ul>",
                "Vysoké skóre mají návštěvníci, kteří nakoupili nedávno, nakupují ",
                "často (pravidelně) a mají vysokou útratu.",
                "Analýzou návštěvníky rozdělíte do dvou skupin: klíčoví návštěvníci ",
                "a&nbsp;ostatní. Procentuální poměr klíčových zákazníků lze nastavit."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "<ul>
                  <li> <b>R = recency</b> -- kdy zákazník naposledy nakoupil
                  (k dnešnímu datu nebo k datu analýzy)</li>
                  <li> <b>F = frequency</b> -- kolik nákupů/objednávek 
                  zákazník udělal</li>
                  <li> <b>M = monetary</b> -- hodnota zákazníkových
                  objednávek (suma nebo průměr)</li>
                </ul>"
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## posuvník pro určení horních $k$ procent ------------------------
            
            fluidRow(
                
                column(
                    
                    width = 4,
                    offset = 4,
                    
                    sliderInput(
                        inputId = "my_preferences_top_k_percent_slider",
                        # label =
                           # "Váhy pro recency, frequency, monetary",
                        label = list(
                            "Nastavení podílu klíčových zákazníků (dle RFM skóre)",
                            bsButton(
                                inputId =
                                    "my_preferences_top_k_percent_slider_info",
                                label = "",
                                icon = icon("info"),
                                style = "info",
                                size = "extra-small"
                            ),
                            bsPopover(
                                id = "my_preferences_top_k_percent_slider_info",
                                title = "Info",
                                content = "Posouváním kolečka na stupnici můžete měnit podíl klíčových zákazníků.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                            )
                        ),
                        min = 5.00,
                        max = 50.00,
                        value = 20.00,
                        step = 1.00,
                        width = "100%"
                    )
                    
                )
                
            ),
            
            tags$br(),
            tags$br(),
            
            
            ## výstupy analýzy ------------------------------------------------
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    h4("Výstupy analýzy")
                    
                )
                
            ),
            
            
            tags$hr(),
            
            fluidRow(
                
                ###############################################################
                
                ## pravý (hlavní) panel ---------------------------------------
                
                column(
                    
                    width = 12,
                    offset = 0,
                    
                    fluidRow(
                        
                        column(
                            
                            width = 12,
                            #offset = 0,
                            aling = "center",
                            
                            # verbatimTextOutput("pokus"),
                            
                            # Nejlepších 20 % zákazníků (dle RFM skóre) preferuje
                            # oproti průměrnému zákazníkovi nejčastěji kategorii
                            # "Hudba a tanec".
                            
                            div(
                                style = "display: inline-block;vertical-align: top;",
                                textOutput(
                                    outputId = 
                                        "my_preferences_weekday_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align: top;",
                                uiOutput(
                                    outputId =
                                        "my_preferences_weekday_introduction_text_info_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_preferences_weekday_qualitative_barplot"
                            ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_preferences_knownartist_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId =
                                        "my_preferences_knownartist_introduction_text_info_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_preferences_knownartist_qualitative_barplot"
                            ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_preferences_majorera_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId =
                                        "my_preferences_majorera_introduction_text_info_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_preferences_majorera_qualitative_barplot"
                            ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_preferences_minorera_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId =
                                        "my_preferences_minorera_introduction_text_info_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_preferences_minorera_qualitative_barplot"
                            ),
                            
                            tags$br(),
                            tags$br(),
                            
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                textOutput(
                                    outputId = 
                                        "my_preferences_concerttype_introduction_text"
                                )
                            ),
                            div(
                                style = "display: inline-block;vertical-align:top;",
                                uiOutput(
                                    outputId =
                                        "my_preferences_concerttype_introduction_text_info_handler"
                                )
                            ),
                            
                            plotOutput(
                                outputId =
                                    "my_preferences_concerttype_qualitative_barplot"
                            ),
                            
                            tags$br(),
                            
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br()
                            
                        )
                        
                    )
                    
                )
                
            )
            
        ),
        
        
        ## obsah záložky "CLV analýza" ----------------------------------------
        
        tabItem(
            
            tabName = "analysis_CLV_transaction_data_tab",
            
            # "# obsah záložky s CLV analýzou transakčních dat",
            
            HTML(
                "<b>CLV (Customer Lifetime Value = celoživotní hodnota ",
                "zákazníka)</b> obecně představuje odhad budoucí útraty každého ",
                "zákazníka. V naší aplikaci na základě analýzy Vámi nahraných ",
                "transakčních dat odhadujeme útratu jednotlivých návštěvníků ",
                "v následujícím roce. Součtem CLV všech návštěvníků získáme ",
                "odhad celkových útrat za vstupenky v následujícím roce."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "TIP: Ukázku práce s výstupem analýzy najdete v ",
                "<a
                    href = 'http://aplikace-auditorium.online/dokumentace.html'
                    target = '_blank'
                >
                    uživatelské dokumentaci
                </a>",
                "."
            ),
            
            tags$br(),
            tags$br(),
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    h4("Výstupy analýzy")
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 4,
                    offset = 4,
                    
                    uiOutput(
                        outputId =
                        "CLV_transaction_final_data_download_button"
                    )
                    
                )
                
            ),
            
            tags$hr(),
            
            #verbatimTextOutput("CLV_transaction_console"),
            
            dataTableOutput(outputId = "CLV_transaction_table"),
            
            tags$hr(),
            
            # verbatimTextOutput(outputId = "CLV_transaction_confusion"),
            
            # tags$br(),
            
            # uiOutput(outputId = "CLV_transaction_accuracy"),
            
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
            
        ),
        
        tabItem(
            
            tabName = "analysis_CLV_non_transaction_data_tab",
            
            # "# obsah záložky s CLV analýzou netransakčních dat",
            
            HTML(
                "<b>CLV (Customer Lifetime Value = celoživotní hodnota ",
                "zákazníka)</b> obecně představuje odhad budoucí útraty každého ",
                "zákazníka. V naší aplikaci na základě analýzy Vámi nahraných ",
                "transakčních dat odhadujeme útratu jednotlivých návštěvníků ",
                "v následujícím roce. Součtem CLV všech návštěvníků získáme ",
                "odhad celkových útrat za vstupenky v následujícím roce."
            ),
            
            tags$br(),
            tags$br(),
            
            HTML(
                "TIP: Ukázku práce s výstupem analýzy najdete v ",
                "<a
                    href = 'http://aplikace-auditorium.online/dokumentace.html'
                    target = '_blank'
                >
                    uživatelské dokumentaci
                </a>",
                "."
            ),
            
            tags$br(),
            tags$br(),
            
            # fluidRow(
                
                # column(
                    
                    # width = 3,
                    # offset = 0,
                    
                    # h4("Volené parametry analýzy")
                    
                # )
                
            # ),
            
            # fluidRow(
                
                # column(
                    
                    # width = 12,
                    # offset = 0,
                    
                    # uiOutput(  
                        # outputId =
                            # "my_non_transaction_train_vs_test_set_slider"
                    # )
                    
                # )
                
            # ),
            
            # tags$hr(),
            
            fluidRow(
                
                column(
                    
                    width = 3,
                    offset = 0,
                    
                    h4("Výstupy analýzy")
                    
                )
                
            ),
            
            fluidRow(
                
                column(
                    
                    width = 4,
                    offset = 4,
                    
                    uiOutput(
                        outputId =
                        "CLV_non_transaction_final_data_download_button"
                    )
                    
                )
                
            ),
            
            tags$hr(),
            
            #verbatimTextOutput("CLV_non_transaction_console"),
            
            #verbatimTextOutput("pokus"),
            
            dataTableOutput(outputId = "CLV_non_transaction_table"),
            
            tags$hr(),
            
            # verbatimTextOutput(outputId = "CLV_non_transaction_confusion"),
            
            # tags$br(),
            
            # uiOutput(outputId = "CLV_non_transaction_accuracy"),
            
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
            
            
        )
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





