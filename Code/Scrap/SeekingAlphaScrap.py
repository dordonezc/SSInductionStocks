## Link seeking alpha
from selenium import webdriver
import pandas as pd
import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project/Data/SeekingAlphaData")
##----------------------------------------------------------------------------##
def get_links(url, driver):
    """Takes url and chrome driver returns list with links of pages to scrap"""
    error = True
    iterations = 0
    while error and iterations < 100:
        error = False
        iterations += 1
        driver.get(url)
        print("Parsing " + str(iterations))
        parent = driver.find_elements_by_css_selector("div.title")
        links = [elem.find_element_by_css_selector("a").get_attribute('href') for elem in parent]
        if len(links) == 0:
            error = True
            aux = input("Help me")
    return links

##----------------------------------------------------------------------------##
# Open web driver with Chrome
driver = webdriver.Chrome('C:/Users/dordo/Documents/Daniel/LBS/chromedriver.exe') 

# First take out links
# Pages to scrap 1-25

## US Economy:
#base_path = "https://seekingalpha.com/market-news/us-economy?page="
#page_s, page_e = 68, 75

## US Tech:
base_path = "https://seekingalpha.com/market-news/tech?page="
page_s, page_e = 270, 319

## US Energy: 
#base_path = "https://seekingalpha.com/market-news/energy?page="
#page_s, page_e = 4, 101  
    
url_list = [base_path + str(x) for x in range(page_s,page_e + 1)]
all_links = []
for link in url_list:
    aux = get_links(link, driver) 
    all_links.append(aux)
store = pd.DataFrame([val for elem in all_links for val in elem])
store.to_csv("LinksTech2018.csv")
driver.close()
