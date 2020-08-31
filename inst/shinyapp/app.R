


ui <- shiny::fluidPage(

   # Application title
  shiny::titlePanel("Convert odds to probabilities"),

  shiny::tabsetPanel(

    shiny::tabPanel(title = "About",

                    shiny::includeHTML('about_page.html'),

              ),




    shiny::tabPanel("Load data", {


      shiny::sidebarLayout(

        shiny::sidebarPanel(
           ## SELECT FILE.
          shiny::fileInput(inputId = "inputFile",
                     label = "SELECT FILE:",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),

          shiny::radioButtons("sep", "Separator",
                        choices = c('Comma (,)' = ",",
                                    'Semicolon (;)' = ";",
                                    'Tab' = "\t"),
                        selected = ","),

          shiny::radioButtons("decsep", "Decimal separator",
                        choices = c('Comma (,)' = ",",
                                    'Point (.)' = "."),
                        selected = "."),

          shiny::radioButtons("quote", "Quote",
                        choices = c(None = "",
                                    "Double Quote (\")" = '"',
                                    "Single Quote (')" = "'"),
                        selected = '"'),

          shiny::actionButton("loadDataButton", "Load data"),

         ),

        shiny::mainPanel(
           'Use the menu on the left to upload you file with odds data. A preview will be shown when
            you hit the Load Data button. If the preview looks like a nice table, you can proceed
            to the "convert odds" tab. If the preview looks messy, you have most likely selected the
           wrong formating. Select a different formating and try again.',

           shiny::hr(),

           shiny::verbatimTextOutput('inputpreviewmessage'),

           shiny::tableOutput("inputpreview")
         )

       ) # end sidebar layout.
     }), # END LOAD DATA TAB

    shiny::tabPanel("Convert odds", {'CONVERT ODDS HERE'


      shiny::sidebarLayout(

        shiny::sidebarPanel(
          shiny::selectInput("oddscols", "Select columns with odds", choices = '', multiple = TRUE),


          shiny::checkboxGroupInput('conversionmethod', 'Conversion method',
                              choices=c('basic', 'or', 'power', 'additive',
                                        'wpo', 'bb', 'shin'),
                              selected='basic',
                              inline = TRUE),

          shiny::actionButton("convertButton", "Convert"),


          shiny::p(),

          shiny::uiOutput('dlButton')


         ),


        shiny::mainPanel(
           'Use the menu on the left to select the columns that contain the odds you want to convert.
           If your data contains several sets of odds, for example from different bookmakers, you should
           only select one set.
           When you have slected the columns, you can choose the methods you want to use. You can
           choose more than one method. See the "About" tab for more information',

           shiny::hr(),

           shiny::verbatimTextOutput('oddsselectionpreviewmessage'),


           shiny::tableOutput("oddspreview"),
           DT::dataTableOutput("probpreview")
           )


       ) # end sidebar layout
       }) # END CONVERT ODDS TAB


     ),


)



server <- function(session, input, output) {


  rv <- shiny::reactiveValues(displayDownloadButton = FALSE,
                       convertNumeric = FALSE)

  # Parse the input CSV file to data.frame, when the button is clicked.
  inputDF <- shiny::eventReactive(input$loadDataButton, {

    # If a file is not selected, display an error message.
    # If a file is selected, parse it with read.csv.
    if (is.null(input$inputFile$datapath)){
      shiny::showModal(shiny::modalDialog(
        title = "Error loading file",
        "You must select a data file to load.",
        easyClose = TRUE
      ))

      df <- NULL
    } else {
      df <- utils::read.csv(input$inputFile$datapath,
                     header = TRUE,
                     sep = input$sep,
                     quote = input$quote,
                     dec = input$decsep,
                     stringsAsFactors = FALSE)
    }

    df

  })



  # Subset the input data frame columns.
  oddsdata <- shiny::reactive({
    req(inputDF())
    if(is.null(input$oddscols) || input$oddscols == ""){
      NULL #inputDF()
    } else {
      inputDF()[, colnames(inputDF()) %in% input$oddscols]

    }
  })

  # Convert the odds into probabilities.
  conversionres <- shiny::eventReactive(input$convertButton, {

    req(oddsdata())

    # Do not display download button before the results are OK.
    # This makes sure the download button disapears if it is already displayed.
    rv$displayDownloadButton <- FALSE

    # Verify that input columns are numeric
    non_num_cols <- c()
    for (ii in 1:ncol(oddsdata())){
      if (!'numeric' %in% class(oddsdata()[,ii])){
        non_num_cols <- c(non_num_cols, colnames(oddsdata())[ii])
      }
    }


    if (length(non_num_cols) > 0){
      shiny::showModal(shiny::modalDialog(
        title = "Non-numeric column",
        "At least one of the columns you have selected is non-numeric.

        You might have selected a variable that does not contain numbers, or you might have used the wrong decimal separator when you loaded the data.",
        easyClose = TRUE
      ))

      rv$convertNumeric <- FALSE
      rv$displayDownloadButton <- FALSE
    } else {
      rv$convertNumeric <- TRUE
    }



    if(rv$convertNumeric){ # dont convert if not all columns are numeric.

      res <- matrix(ncol=1, nrow=nrow(oddsdata()))
      colnames(res) <- 'MARGIN'

      for (ii in 1:length(input$conversionmethod)){
        ip_res <- implied::implied_probabilities(oddsdata(),
                                        method = input$conversionmethod[ii])

        new_columns <- ip_res$probabilities

        if (ii == 1){
          res[,1] <- ip_res$margin
        }

        # Make names for the new columns, indicating the method.
        new_colnames <- paste(colnames(ip_res$probabilities),
                              input$conversionmethod[ii], sep='_')

        # Add aditional columns.
        if (input$conversionmethod[ii] %in% c('shin', 'bb')) {
          # Column name for additional columns.
          additional_colnames <- paste(input$conversionmethod[ii], 'z', sep='_')
          new_columns <- cbind(new_columns, ip_res$zvalues)
          new_colnames <- c(new_colnames, additional_colnames)
        } else if (input$conversionmethod[ii] == 'or'){
          additional_colnames <- paste(input$conversionmethod[ii], 'oddsratios', sep='_')
          new_columns <- cbind(new_columns, ip_res$odds_ratios)
          new_colnames <- c(new_colnames, additional_colnames)
        } else if (input$conversionmethod[ii] == 'power'){
          additional_colnames <- paste(input$conversionmethod[ii], 'exponents', sep='_')
          new_columns <- cbind(new_columns, ip_res$exponents)
          new_colnames <- c(new_colnames, additional_colnames)
        } else if (input$conversionmethod[ii] == 'wpo'){
          additional_colnames <- paste(colnames(ip_res$probabilities),
                                       input$conversionmethod[ii],
                                       'specificmargins', sep='_')

          new_columns <- cbind(new_columns, ip_res$specific_margins)
          new_colnames <- c(new_colnames, additional_colnames)
        }

        # Set the new columns names.
        colnames(new_columns) <- new_colnames

        res <- cbind(res, new_columns)
      }

      # Display download button if the results seem reasonable.
      if (ncol(res) > 1){
        rv$displayDownloadButton <- TRUE
       }

      res

    }

  })



   # When the data is properly loaded, update the column selection widget.
  shiny::observeEvent(inputDF(), {
     updateSelectInput(session, "oddscols", choices=colnames(inputDF()))
   })


   # Render input data frame as table, and push to output.
   output$inputpreview <- shiny::renderTable({
     head(inputDF())
   })

   # Show a message with the dimensions of the input data frame.
   output$inputpreviewmessage <- shiny::renderText({
     nrows <- nrow(inputDF())
     ncols <- ncol(inputDF())
     sprintf('Data table contains %d rows and %d columns.\nOnly the six first rows are shown.',
             nrows, ncols)

   })

   # Show a message with the dimensions of the input data frame.
   output$oddsselectionpreviewmessage <- shiny::renderText({
     nrows <- nrow(oddsdata())
     ncols <- ncol(oddsdata())
     sprintf('%d columns selected.\nThe data table contains %d rows, but only the six first rows are shown.',
             ncols, nrows)
   })

   # Render the selected odds columns.
   output$oddspreview <- shiny::renderTable({
     req(oddsdata())
     head(oddsdata())
   })




   output$probpreview <- DT::renderDataTable({
     req(conversionres())

      conversionres_df <- conversionres()
      row.names(conversionres_df) <- as.character(1:nrow(conversionres_df))

      DT::formatRound(DT::datatable(conversionres_df, rownames = TRUE),
                      columns = colnames(conversionres_df), digits = 3)

   }, options =  list(pageLength = 10))


   output$downloadData <- shiny::downloadHandler(
     filename = function(){
       fname <- gsub('\\.([^.]*)$', '', input$inputFile$name)
       paste(fname, 'converted.csv', sep='_')},

     content = function(file) {
       utils::write.table(conversionres(), file, row.names = FALSE,
                # Use the same formatting as the input csv table.
                 sep = input$sep,
                 dec = input$decsep)
     }
   )


   output$dlButton <- shiny::renderUI({
     if (rv$displayDownloadButton){
       downloadButton("downloadData", "Save")
     } else {
       p()
     }
   })


}


# Run the application
shinyApp(ui = ui, server = server)

