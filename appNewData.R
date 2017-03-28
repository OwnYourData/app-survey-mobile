# mobile UI to collect data
# last update: 2017-03-01

appNewData <- function(){
        tabPanel('Datenerfassung',
                 helpText('Erfasse hier eine neue Vorhersage.'),
                 dateInput('dateInput',
                           label = 'Datum',
                           language = 'de'),
                 selectInput('surveySelect', 
                             label = 'Umfrage',
                             choices = c('leer'),
                             selected = 'leer'),
                 selectInput('forecastSelect', 
                             label = 'Vorhersage',
                             choices = c('leer'),
                             selected = 'leer'),
                 sliderInput('confidenceValue',
                             'Konfidenz',
                             min = 0, max = 100, value = 60,
                             step = 20, post='%'),
                 tags$label('Notiz:'),
                 br(),
                 tags$textarea(id='noteInput',
                               rows=2, cols=50,
                               ''),
                 br(),br(),
                 bsAlert('saveForecastInfo'),
                 actionButton('saveForecastInput', 'Speichern',
                              icon('save'))
        )
}