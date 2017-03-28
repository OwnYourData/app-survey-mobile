# mobile UI to collect data
# last update: 2017-03-01

source('appSelect.R')

appSurveyList <- function(){
        tabPanel('Umfragen',
                 appSelect(),
                 bsAlert('dataStatus'),
                 DT::dataTableOutput('predictionList')
        )
}
                           