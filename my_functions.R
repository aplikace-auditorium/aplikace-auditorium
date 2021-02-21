###############################################################################
###############################################################################
###############################################################################

## definuji uživatelské funkce ------------------------------------------------

getMyCategory <- function(
    
    x,
    categories_cut_points,
    categories_labels,
    return_label = TRUE
    
){
    
    # '''
    # Vrátí pro numerickou hodnotu "x" index (když return_label = FALSE)
    # anebo popisek (když return_label = TRUE) kategorie, do které tato
    # patří. Vektor "categories_cut_points" obsahuje navzájem různé
    # a vzestupně uspořádané hranice mezi intervaly; levý a pravý krajní
    # interval se považují za zleva a zprava neohraničené. Délka vektoru
    # "categories_labels" je o jedna větší než délka vektoru
    # "categories_cut_points".
    # '''
    
    if(
        return_label
    ){
        
        return(
            categories_labels[
                c(
                    TRUE,
                    x >= categories_cut_points
                )
            ][
                sum(
                    c(
                        TRUE,
                        x >= categories_cut_points
                    )
                )
            ]
        )
        
    }else{
        
        return(
            sum(
                c(
                    TRUE,
                    x >= categories_cut_points
                )
            )
        )
        
    }
    
}


## ----------------------------------------------------------------------------

getMyHistogram <- function(
    
    my_data,
    my_variable,
    x_lab = "",
    my_main = "",
    my_colour = "",
    my_breaks = log2(
        length(
            my_data[, my_variable]
        )
    ),
    ...
    
){
    
    # '''
    # Zobrazuje histogram pro danou proměnnou "my_variable"
    # z dataframeu "my_data". Lze custmizovat grafické prvky
    # jako "x_lab", "my_main" a "my_colour".
    # '''
    
    par(mar = c(4.1, 4.1, 4.1, 0.1))
    
    hist(
        my_data[, my_variable],
        xlab = x_lab,
        ylab = "počet zákazníků",
        main = my_main,
        col = my_colour,
        xlim = c(
            max(
                min(
                    my_data[, my_variable],
                    na.rm = TRUE
                ) * 0.8,
                0
            ),
            max(
                my_data[, my_variable],
                na.rm = TRUE
            ) * 1.2
        ),
        breaks = my_breaks
    )
    
}


## ----------------------------------------------------------------------------

# getMyPiechart <- function(
    
    # my_data,
    # my_variable,
    # my_main = "",
    # my_colours = "",
    # ...
    
# ){
    
    # # '''
    # # Zobrazuje koláčový diagram pro danou proměnnou "my_variable"
    # # z dataframeu "my_data". Lze custmizovat grafické prvky
    # # jako "my_main" a "my_colour".
    # # '''
    
    # par(mar = c(4.1, 4.1, 4.1, 0.1))
    
    # pie(
        # table(
            # my_data[, my_variable]
        # )
    # )
    
# }


## ----------------------------------------------------------------------------

getMyDoubleHistogram <- function(
    
    my_data,
    my_variable,
    which_to_add,
    x_lab = "",
    my_main = "",
    my_first_colour = "",
    my_second_colour = "",
    ...
    
){
    
    # '''
    # Zobrazuje histogram pro danou proměnnou "my_variable"
    # z dataframeu "my_data" v barvě "my_first_colour".
    # Argument "which_to_add" slouří k přidání hodnot proměnné "my_variable"
    # z dataframeu "my_data" do původního histogramu, podhistogram
    # bude mít barvu "my_second_colour".
    # Lze custmizovat grafické prvky jako "x_lab", "my_main" a další.
    # '''
    
    par(mar = c(4.1, 4.1, 4.1, 0.1))
    
    hist(
        my_data[, my_variable],
        xlab = x_lab,
        ylab = "absolutní počet klientů",
        main = my_main,
        col = my_first_colour,
        xlim = c(
            max(
                min(
                    my_data[, my_variable],
                    na.rm = TRUE
                ) * 0.8,
                0
            ),
            max(
                my_data[, my_variable],
                na.rm = TRUE
            ) * 1.2
        ),
        breaks = log2(
            length(
                my_data[, my_variable]
            )
        )
    )
    
    hist(
        my_data[, my_variable][
            which_to_add
        ],
        col = my_second_colour,
        breaks = log2(
            length(
                my_data[, my_variable]
            )
        ),
        add = TRUE
    )
    
}


## ----------------------------------------------------------------------------

getMyBarplot <- function(
    
    my_data,
    my_variable,
    x_lab = "",
    my_main = ""
    
){
    
    # '''
    # Zobrazuje barplot pro danou proměnnou "my_variable"
    # z dataframeu "my_data", tedy počet klientů pro každý segment
    # proměnné. Počet sloupců je tak dán počtem segmentů.
    # Lze custmizovat grafické prvky jako "x_lab" a "my_main".
    # '''
    
    par(mar = c(4.1, 4.1, 4.1, 2.1))
    
    barplot(
        table(my_data[, my_variable]),
        xlab = x_lab,
        ylab = "absolutní počet klientů na segment",
        main = my_main,
        col = "deepskyblue",
        ylim = c(
            0,
            max(table(my_data[, my_variable])) * 1.2
        )
    )
    
}


## ----------------------------------------------------------------------------

getMyScatterplot <- function(
    
    my_data,
    my_first_variable,
    my_second_variable,
    x_lab = "",
    y_lab = "",
    my_main = ""
    
){
    
    # '''
    # Vrací bodový diagram, tedy závislost proměnné "my_second_variable"
    # na proměnné "my_first_variable" dataframeu "my_data".
    # Lze custmizovat grafické prvky jako "x_lab", "y_lab" a "my_main".
    # '''
    
    par(mar = c(4.1, 4.1, 4.1, 2.1))
    
    plot(
        x = my_data[, my_first_variable],
        y = my_data[, my_second_variable],
        xlab = x_lab,
        ylab = y_lab,
        main = my_main
    )
    
}


## ----------------------------------------------------------------------------

getMyHeatmap <- function(
    
    my_data,
    my_first_variable,
    my_second_variable,
    x_lab = "",
    y_lab = "",
    my_main = "",
    my_colours
    
){
    
    # '''
    # Vrací heatmapu nad kontingenční tabulkou "my_table".
    # Lze custmizovat grafické prvky jako "x_lab", "y_lab" a "my_main",
    # barevný profil určuje vektor barev "my_colours".
    # '''
    
    par(mar = c(4.1, 4.1, 0.1, 0.1))
    
    heatmap(
        table(
            my_data[, my_second_variable],
            my_data[, my_first_variable]
        ),
        margins = c(5, 5),
        col = my_colours, #brewer.pal(9, "Blues"),
        revC = FALSE,
        xlab = x_lab,
        ylab = y_lab,
        main = my_main,
        keep.dendro = TRUE,
        cex.main = 0.7
    )
    
}


## ----------------------------------------------------------------------------

getMyModelPlot <- function(
    
    my_data,
    my_variable,
    x_lab = "",
    y_lab = "",
    my_main = "",
    x_names = ""
    
){
    
    # '''
    # Vrací barplot modelů pro dataset "my_model_data".
    # Lze custmizovat grafické prvky jako "x_lab", "y_lab", "my_main",
    # "x_names".
    # '''
    
    par(mar = c(4.1, 4.1, 0.9, 0.1))
    
    barplot(
        my_data[, my_variable],
        xlab = x_lab,
        ylab = y_lab,
        main = my_main,
        names.arg = x_names,
        space = 2.0,
        ylim = c(0, max(my_data[, my_variable]) * 1.2),
        yaxt = "n",
        col = c(
            "grey",
            rep("lightgrey", 5)
        )
    )
    
    axis(
        2,
        at = axTicks(2),
        labels = formatC(axTicks(2), format = "d")
    )
    
    abline(
        h = my_data[
            my_data[, "model"] == "Skutečnost",
            my_variable
        ],
        col = "blue",
        lty = "dashed"
    )
    
}


## ----------------------------------------------------------------------------

getMyAccuracy <- function(
    
    my_table
    
){
    
    # '''
    # Vrací přesnost pro konfuzní matici "my_table".
    # '''
    
    return(
        sum(diag(my_table)) / sum(my_table)
    )
    
}


## ----------------------------------------------------------------------------

getMyQuantitativeBarplot <- function(
    
    my_data,
    my_variable,
    n_of_bars,
    which_to_add,
    my_main = "",
    my_first_colour = "",
    my_second_colour = "",
    my_xlab = "",
    my_ylab = "",
    ...
    
){
    
    # '''
    # 
    # 
    # '''
    
    my_first_row <- NULL
    my_second_row <- NULL
    
    for(i in 1:n_of_bars){
        
        my_first_row <- c(
            
            my_first_row,
            length(
                which(
                    my_data[, my_variable] >= min(
                        my_data[, my_variable],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_data[, my_variable],
                            na.rm = TRUE
                        ) - min(
                            my_data[, my_variable],
                            na.rm = TRUE
                        )
                    ) / n_of_bars * (i - 1) &
                    my_data[, my_variable] < min(
                        my_data[, my_variable],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_data[, my_variable],
                            na.rm = TRUE
                        ) - min(
                            my_data[, my_variable],
                            na.rm = TRUE
                        )
                    ) / n_of_bars * i
                )
            )
        )
        
        
        my_second_row <- c(
            
            my_second_row,
            length(
                which(
                    my_data[
                        which_to_add,
                        my_variable
                    ] >= min(
                        my_data[
                            ,
                            my_variable
                        ],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_data[
                                ,
                                my_variable
                            ],
                            na.rm = TRUE
                        ) - min(
                            my_data[
                                ,
                                my_variable
                            ],
                            na.rm = TRUE
                        )
                    ) / n_of_bars * (i - 1) &
                    my_data[
                        which_to_add,
                        my_variable
                    ] < min(
                        my_data[
                            ,
                            my_variable
                        ],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_data[
                                ,
                                my_variable
                            ],
                            na.rm = TRUE
                        ) - min(
                            my_data[
                                ,
                                my_variable
                            ],
                            na.rm = TRUE
                        )
                    ) / n_of_bars * i
                )
            )
        )
        
    }
    
    my_first_row[n_of_bars] <- sum(
        my_first_row[n_of_bars],
        length(
            which(
                my_data[, my_variable] == max(
                    my_data[, my_variable],
                    na.rm = TRUE
                )
            )
        )
    )
    
    my_second_row[n_of_bars] <- sum(
        my_second_row[n_of_bars],
        length(
            which(
                my_data[which_to_add, my_variable] == max(
                    my_data[, my_variable],
                    na.rm = TRUE
                )
            )
        )
    )
    
    # return(
        # list(
            # my_data[, my_variable],
            # my_data[which_to_add, my_variable],
            # matrix(
                # c(
                    # my_first_row,
                    # my_second_row
                # ),
                # nrow = 2,
                # byrow = TRUE
            # )
        # )
    # )
    
    par(mar = c(4.1, 4.1, 4.1, 0.1))
    
    barplot(
        height = matrix(
            c(
                my_first_row,
                my_second_row
            ),
            nrow = 2,
            byrow = TRUE
        ),
        beside = TRUE,
        ylim = c(
            0,
            1.2 * max(
                c(
                    my_first_row,
                    my_second_row
                ),
                na.rm = TRUE
            )
        ),
        col = c(
            my_first_colour,
            my_second_colour
        ),
        names.arg = paste(
            "[",#"\u3008",
            round(
                min(
                    my_data[, my_variable],
                    na.rm = TRUE
                ) + (
                    max(
                        my_data[, my_variable],
                        na.rm = TRUE
                    ) - min(
                        my_data[, my_variable],
                        na.rm = TRUE
                    )
                ) / n_of_bars * c(0:(n_of_bars - 1)),
                digits = 0
            ),
            ", ",
            format(
                round(
                    min(
                        my_data[, my_variable],
                        na.rm = TRUE
                    ) + (
                        max(
                            my_data[, my_variable],
                            na.rm = TRUE
                        ) - min(
                            my_data[, my_variable],
                            na.rm = TRUE
                        )
                    ) / n_of_bars * c(1:(n_of_bars)),
                    digits = 0
                ),
                nsmall = 0
            ),
            ")",
            sep = ""
        ),
        main = my_main,
        xlab = my_xlab,
        ylab = my_ylab
    )
    
    text(
        x = seq(1, 1 + 3 * (n_of_bars - 1), by = 3) + 0.5,
        y = my_first_row + (
            1 + 0.065 * (
                max(
                    my_first_row
                ) - min(
                    my_first_row
                )
            )
        ),
        labels = my_first_row,
        col = my_first_colour
    )
    
    text(
        x = seq(1, 1 + 3 * (n_of_bars - 1), by = 3) + 1.5,
        y = my_second_row + (
            1 + 0.065 * (
                max(
                    my_first_row
                ) - min(
                    my_first_row
                )
            )
        ),
        labels = my_second_row,
        col = my_second_colour
    )
    
}


## ----------------------------------------------------------------------------

getMyQualitativeBarplot <- function(
    
    my_data,
    my_variable,
    which_to_add,
    my_main = "",
    # my_first_colour = "",
    # my_second_colour = "",
    my_colour = "",
    my_sorting_vector = NULL,
    my_xlab = "",
    my_ylab = "",
    ...
    
){
    
    # '''
    # 
    # 
    # '''
    
    my_first_row <- table(my_data[, my_variable])
    my_second_row <- table(my_data[which_to_add, my_variable])
    
    if(
        !is.null(my_sorting_vector)
    ){
        
        my_first_row <- my_first_row[
            order(
                match(
                    names(my_first_row),
                    my_sorting_vector
                )
            )
        ]
        
        my_second_row <- my_second_row[
            order(
                match(
                    names(my_second_row),
                    my_sorting_vector
                )
            )
        ]
        
    }
    
    par(mar = c(4.1, 4.1, 4.1, 0.1))
    
    barplot(
        # height = matrix(
            # c(
                # my_first_row,
                # my_second_row
            # ),
            # nrow = 2,
            # byrow = TRUE
        # ),
        height = c(
            my_second_row / my_first_row * 100
        ),
        # beside = TRUE,
        # ylim = c(
            # 0,
            # 1.2 * max(
                # c(
                    # my_first_row,
                    # my_second_row
                # ),
                # na.rm = TRUE
            # )
        # ),
        ylim = c(0, 100),
        # col = c(
            # my_first_colour,
            # my_second_colour
        # ),
        col = my_colour,
        names.arg = names(
            my_first_row
        ),
        main = my_main,
        xlab = my_xlab,
        ylab = my_ylab
    )
    
    # text(
        # x = seq(1, 1 + 3 * (length(my_first_row) - 1), by = 3) + 0.5,
        # y = my_first_row + (
            # 1 + 0.065 * (
                # max(
                    # my_first_row
                # ) - min(
                    # my_first_row
                # )
            # )
        # ),
        # labels = my_first_row,
        # col = my_first_colour
    # )
    
    # text(
        # x = seq(1, 1 + 3 * (length(my_first_row) - 1), by = 3) + 1.5,
        # y = my_second_row + (
            # 1 + 0.065 * (
                # max(
                    # my_first_row
                # ) - min(
                    # my_first_row
                # )
            # )
        # ),
        # labels = my_second_row,
        # col = my_second_colour
    # )
    
    text(
        x = seq(0.7, 0.7 + 1.2 * (length(my_first_row) - 1), by = 1.2),
        y = c(
            my_second_row / my_first_row * 100
        ) + 6.0,
        labels = paste(
            format(
                round(
                     my_second_row / my_first_row * 100,
                     digits = 1
                ),
                nsmall = 1
            ),
            " %",
            sep = ""
        ),
        col = my_colour
    )
    
}


## ----------------------------------------------------------------------------

# barplot(
    # height = matrix(
        # c(6, 7, 8,
        # 1, 2, 1),
        # nrow = 2,
        # byrow = TRUE
    # ),
    # beside = TRUE,
    # col = c("red", "blue"),
    # names.arg = paste(
        # "\u3008",
        # c("a", "b", "c"),
        # sep = ""
    # ),
    # ylim = c(0, 10)
# )

# text(
    # x = c(1, 4, 7) + 0.5,
    # y = c(6, 7, 8),
    # pos = 3,
    # labels = c(
        # 10, 20, 30
    # )
# )

# text(
    # x = c(1, 4, 7) + 1.5,
    # y = c(6, 7, 8),
    # pos = 3,
    # labels = c(
        # 10, 20, 30
    # ),
    # col = "#f4646c"
# )

# x = c(1, 4, 9)
# y = c(2, 6, 10)

# paste(
    # "(",
    # x,
    # ", ",
    # y,
    # ")",
    # sep = ""
# )


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





