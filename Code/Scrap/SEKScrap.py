import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver import ActionChains
import pandas as pd

## Parser
data = pd.read_excel("C:/Users/dordo/Dropbox/Capstone Project/S&P/S&PCompanies.xlsx",
              engine='openpyxl', sheet_name=1)
codes = data.CIK.values
code = codes[1]

# Open web driver
driver = webdriver.Chrome('C:/Users/dordo/Documents/Daniel/LBS/chromedriver.exe') 
# Navigate
driver.get("https://stockmarketmba.com/symbollookupusingidentifier.php")


## Get codes
save = []
for val in codes:
    ## Find Search bar
    elem = driver.find_element_by_id("search")
    ## Send key codes
    elem.send_keys(str(val))
    try: 
        ## Send enter
        elem.send_keys(Keys.RETURN)
    except: 
        1
    ## Table element
    table = driver.find_element_by_id("searchtable")
    ## Row element
    rows = table.find_element_by_class_name("odd")
    line = [elem.text for elem in rows.find_elements_by_css_selector("*") if elem.text != ""]
    save.append(line)

res = pd.DataFrame(save)
res.columns = ["Symbol", "Description", "Exchange", "Exchange Country",
               "ISIN", "CUSIP", "SEDOL", "CIK", "FIGI", "C1"]
res.to_csv("Prueba.csv")
driver.close()

