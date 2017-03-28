# top-level framework for mobile version
# last update:2017-03-01

source('appNewData.R')
source('appSurveyList.R')
source('uiPiaConfig.R')

appMobile <- function(){
        navbarPage(
                uiOutput('hdrImageLinkMobile'),
                id='page',
                collapsible=TRUE,
                inverse=FALSE,
                windowTitle=paste0(appTitle, ' | OwnYourData'),
                appNewData(),
                appSurveyList(),
                uiPiaConfig()
        )
}