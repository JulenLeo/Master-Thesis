This folder contains 3 types of R scripts:
* The scripts that start with **RUN-**: these are the scripts than need to be run to do the web scrapping from Transfermarkt.com. They have to be run in this order:
    1. **RUNclubs.R**: to obtain the data of the clubs.
    2. **RUNplayers.R**: to obtain the data of the players.
    3. **RUNinjuries.R**: to obtain the data of the injuries.
* The scripts that start with **get-** and **cleanMarket.R**: these are the script that contain the functions to run the previous scripts **RUN-**.
* The script **AllData.R**: this script does the cleaning and preprocessing of all the data.
