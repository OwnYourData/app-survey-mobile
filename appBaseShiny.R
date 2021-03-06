# basic reactive functions for accessing PIA
# last update: 2017-03-01

currApp <- reactive({
        input$disconnectPIA
        app <- vector()
        piaMsg <- ''
        pia_url <- piaUrl
        app_key <- appKey
        app_secret <- appSecret
        if(is.null(pia_url) |
           is.null(app_key) | 
           is.null(app_secret)) {
                piaMsg <- paste0('Es sind keine oder nur unvollständige Verbindungsdaten vorhanden. Wähle im Menü ',
                                 icon('gear'),
                                 ' rechts oben "Konfiguration" und trage die Verbindungsdaten zu deinem Datentresor ein!')
        } else {
                if((nchar(pia_url) > 0) & 
                   (nchar(app_key) > 0) & 
                   (nchar(app_secret) > 0)) {
                        app <- setupApp(pia_url, app_key, app_secret)
                        if(length(app) == 0){
                                piaMsg <- paste0('Die angegebenen Verbindungsdaten zum Datentresor sind nicht korrekt oder er kann derzeit nicht erreicht werden. Öffne deinen Datentresor und überprüfe hier im Menü ',
                                                 icon('gear'),
                                                 ' rechts oben "Konfiguration" die Verbindungsdaten.')
                        } else {
                                if(is.na(app[['token']])){
                                        piaMsg <- paste0('Die angegebenen Verbindungsdaten zum Datentresor sind nicht korrekt oder er kann derzeit nicht erreicht werden. Öffne deinen Datentresor und überprüfe hier im Menü ',
                                                         icon('gear'),
                                                         ' rechts oben "Konfiguration" die Verbindungsdaten.')
                                }
                        }
                } else {
                        piaMsg <- paste0('Es sind keine oder nur unvollständige Verbindungsdaten vorhanden. Wähle im Menü ',
                                         icon('gear'),
                                         ' rechts oben "Konfiguration" und überprüfe die Verbindungsdaten zum Datentresor!')
                }
        }
        
        mailConfigMsg <- paste0('Derzeit sind noch keine Verbindungsdaten zum Versenden und Empfangen von Emails konfiguriert. Wähle im Menü ',
                                icon('gear'),
                                ' rechts oben "Konfiguration" und überprüfe die Eingaben!')
        
        if(nchar(piaMsg) > 0){
                createAlert(session, 'piaStatus', alertId = 'myPiaStatus',
                            style = 'danger', append = FALSE,
                            title = 'Verbindung zum Datentresor',
                            content = piaMsg)
                app <- vector()
        } else {
                closeAlert(session, 'myPiaStatus')
        }
        app
})

currData <- reactive({
        # list any input controls that effect currData
        input$pia_urlMobile
        input$app_keyMobile
        input$app_secretMobile
        
        app <- currApp()
        if(length(app) > 0) {
                url <- itemsUrl(app[['url']], app[['app_key']])
                readItems(app, url)
        } else {
                data.frame()
        }
})

currDataDateSelect <- reactive({
        closeAlert(session, 'myDataStatus')
        data <- currData()
        if(nrow(data) > 0){
                myRange <- c(mymin = as.Date(Sys.Date()),
                             mymax = as.Date(Sys.Date()))
                switch(input$dateSelect,
                       '1' = { myRange <- c(mymin = as.Date(Sys.Date()-7),
                                            mymax = as.Date(Sys.Date())) },
                       '2' = { myRange <- c(mymin = as.Date(Sys.Date() - months(1)),
                                            mymax = as.Date(Sys.Date())) },
                       '3' = { myRange <- c(mymin = as.Date(Sys.Date() - months(2)),
                                            mymax = as.Date(Sys.Date())) },
                       '4' = { myRange <- c(mymin = as.Date(Sys.Date() - months(6)),
                                            mymax = as.Date(Sys.Date())) },
                       '5' = { myRange <- c(mymin = as.Date(paste(year(Sys.Date()),'1','1',sep='-')),
                                            mymax = as.Date(paste(year(Sys.Date()),'12','31',sep='-'))) },
                       '6' = { myRange <- c(mymin = as.Date(Sys.Date() - months(12)),
                                            mymax = as.Date(Sys.Date())) },
                       '10'= { myRange <- c(mymin = as.Date('1970-01-01'),
                                            mymax = as.Date('2070-01-01')) },
                       {})
                
                mymin <- myRange['mymin']
                mymax <- myRange['mymax']
                daterange <- seq(mymin, mymax, 'days')
                data <- data[as.Date(data$date) %in% daterange, ]
                if(nrow(data) > 0){
                        tagsel <- input$tagSelect
                        if(tagsel == 'alle Bereiche'){
                                data
                        } else {
                                data <- data[unlist(lapply(lapply(
                                        strsplit(data$tags, ';'), trimws), 
                                        function(x) tagsel %in% x)), ]
                                if(nrow(data) > 0){
                                        data
                                } else {
                                        createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                                                    style = 'warning', append = FALSE,
                                                    title = 'Keine Daten im gewählten Zeitfenster und Bereich',
                                                    content = 'Für das ausgewählte Zeitfenster und den gewählten Bereich sind keine Daten vorhanden.')
                                        data.frame()
                                }
                        }
                } else {
                        createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                                    style = 'warning', append = FALSE,
                                    title = 'Keine Daten im gewählten Zeitfenster',
                                    content = 'Für das ausgewählte Zeitfenster sind keine Daten vorhanden.')
                        data.frame()
                }
        } else {
                createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                            style = 'warning', append = FALSE,
                            title = 'Keine Daten für gewählten Bereich',
                            content = 'Für den gewählten Bereich sind noch keine Daten vorhanden.')
                data.frame()
        }
})
