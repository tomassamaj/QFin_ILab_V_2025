# 1. Use a robust base image that has Shiny and Tidyverse pre-installed
FROM rocker/shiny-verse:latest

# 2. Update system libraries & install dependencies
# libxml2 (for 'broom'), libsodium (for 'shinymanager'), libsqlite3 (for 'RSQLite')
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libsodium-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Install the specific R packages your app needs
# *** FIX: Removed stray 'a' from the line-continuation backslash ***
RUN R -e "install.packages(c( \
    'shinymanager', \
    'sodium', \
    'RSQLite', \
    'corrplot', \
    'RColorBrewer', \
    'zoo', \
    'broom' \
    ), \
    repos='https://cloud.r-project.org/', \
    Ncpus = 2)"

# 4. Copy your app file AND your data files
COPY app.R /srv/shiny-server/app.R
COPY quant_data.RData /srv/shiny-server/quant_data.RData
COPY credentials.sqlite /srv/shiny-server/credentials.sqlite

# 5. Expose the port Shiny runs on
EXPOSE 3838