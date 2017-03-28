FROM oydeu/oyd-shiny_armhf:latest
MAINTAINER "Christoph Fabianek" christoph@ownyourdata.eu

RUN cd /srv/shiny-server \
        && rm -rf * \
        && git clone https://github.com/OwnYourData/app-survey-mobile.git \
        && mv app-survey-mobile/* . \
        && rm -r app-survey-mobile \
        && R -e "source('init.r')"

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]