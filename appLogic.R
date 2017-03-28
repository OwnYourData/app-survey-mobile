# application specific logic
# last update: 2017-03-01

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# get stored surveys
readSurveyItems <- function(){
        app <- currApp()
        surveyItems <- data.frame()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], app[['app_key']])
                surveyItems <- readItems(app, url)
                if(nrow(surveyItems) > 0){
                        rownames(surveyItems) <- surveyItems$name
                        surveyItems <- surveyItems[, c('date', 
                                                       'tags', 
                                                       'options', 
                                                       'result', 
                                                       'resultUrl', 
                                                       'active', 
                                                       'id'), 
                                                   drop=FALSE]
                }
        }
        surveyItems
}

# anything that should run only once during startup
appStart <- function(){
        allItems <- readSurveyItems()
        if(nrow(allItems) > 0){
                # appStatusDateSelect
                currTags <- unique(lapply(
                        strsplit(
                                paste(allItems$tags, collapse = ';'), 
                                ';'), trimws)[[1]])
                currTags <- currTags[currTags != '']
                updateSelectInput(
                        session,
                        'tagSelect',
                        choices = c('alle Bereiche', currTags),
                        selected = 'alle Bereiche')

                # appSourceInput
                updateSelectInput(session, 'surveySelect',
                                  choices = rownames(allItems),
                                  selected = rownames(allItems)[1])
                currOptions <- lapply(strsplit(allItems[1, 'options'], ';'), 
                                      trimws)[[1]]
                updateSelectInput(session, 'forecastSelect', 
                                  choices = currOptions,
                                  selected = currOptions[1])
        } else {
                # appStatusDateSelect
                updateSelectInput(
                        session,
                        'tagSelect',
                        choices = c('alle Bereiche'),
                        selected = 'alle Bereiche')

                # appSourceInput
                updateSelectInput(session, 'surveySelect',
                                  choices = c('keine vorhanden'),
                                  selected = 'keine vorhanden')
                updateSelectInput(session, 'forecastSelect', 
                                  choices = c('leer'),
                                  selected = 'leer')
                
        }
}

observeEvent(input$saveForecastInput, {
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], 
                                       '.response'))
                data <- list(date = as.character(input$dateInput),
                             survey = input$surveySelect,
                             option = input$forecastSelect,
                             confidence = input$confidenceValue,
                             note = input$noteInput,
                             '_oydRepoName' = 'Vorhersage')
                writeItem(app, url, data)
                createAlert(session, 'saveForecastInfo', alertId = 'saveTask',
                            style = 'success', append = FALSE,
                            content = 'Daten gespeichert')
                
                updateDateInput(session, 'dateInput', 
                                value = as.character(Sys.Date()))
                updateSelectInput(session, 'surveySelect', selected = '')
                updateSelectInput(session, 'forecastSelect',
                                  choices = '', selected = '')
                updateSliderInput(session, 'confidenceValue', value = 60)
                updateTextAreaInput(session, 'noteInput', value = '')
        }
})

observeEvent(input$surveySelect, {
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], app[['app_key']])
                selItem <- input$surveySelect
                recs <- readItems(app, url)
                opts <- recs[recs$name == selItem, 'options']
                if(length(opts) > 0){
                        currOptions <- lapply(strsplit(recs[recs$name == selItem, 'options'], ';'), 
                                              trimws)[[1]]
                        updateSelectInput(session, 'forecastSelect', 
                                          choices = currOptions,
                                          selected = currOptions[1])
                } else {
                        updateSelectInput(session, 'forecastSelect', 
                                          choices = c('leer'), 
                                          selected = 'leer')
                }
        }
})

output$predictionList <- DT::renderDataTable(datatable({
        data <- currDataDateSelect()
        app <- currApp()
        url <- itemsUrl(app[['url']],
                        paste0(app[['app_key']], '.response'))
        prediction <- readItems(app, url)
        if(nrow(data) > 0){
                data$order <- 1:nrow(data)
                cnts <- data.frame(table(unlist(prediction$survey)))
                data <- merge(data, cnts, by.x='name', by.y='Var1',
                              all.x=TRUE, all.y=FALSE)
                last <- aggregate(prediction[order(prediction$date), 'option'], 
                                  by=list(prediction[order(prediction$date), 'survey']), 
                                  FUN=tail, n=1)
                data <- merge(data, last, by.x='name', by.y='Group.1',
                              all.x=TRUE, all.y=FALSE)
                data <- data[order(data$order), ]
                data$Freq <- as.numeric(data$Freq)
                data <- data[, c('date', 'name', 'Freq','x', 'result')]
                colnames(data) <- c('Datum', 'Name', '# Prognosen', 'letzte Vorhersage', 'Ergebnis')
        }
        data
        
}, 
options = list(
        language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'),
        aoColumnDefs = list(list(targets=c(3,4), class='dt-right')),
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 10),
selection = 'single'
))
