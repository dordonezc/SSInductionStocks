## Download chromedriver and change base path 

import time
from selenium import webdriver
import pandas as pd
import os 
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
#from bs4 import BeautifulSoup
os.chdir("C:/Users/dordo/Dropbox/Capstone Project/Data/SeekingAlphaData")

##---------------------------------------------------------------------------##
## Parser
def get_content(url, driver):
    """Takes url and chrome driver returns tuple with data title and article content"""
    # Navigate
    driver.get(url)
    print("Parsing")
    error = True
    iters = 0
    ## This iterates until the page loads... 
    while error and iters < 100:
        try:
            # Find date
            error = False 
            iters +=1
            date = driver.find_element_by_css_selector("span[data-test-id='post-date']").text
            # Find Title box
            title = driver.find_element_by_css_selector("h1[data-test-id='post-title']").text 
            ## Find Article Content
            article = driver.find_element_by_css_selector("div[data-test-id='content-container']").text    
        except:
            print("Error Parsing. Retrying in " + str(iters))
            error = True
    return date, title, article, url

##----------------------------------------------------------------------------##
## We are scraping opera + vpn
#path = 'C:/Users/dordo/Dropbox/Capstone Project/Driver/operadriver_win64/operadriver.exe'
#webdriver_service = service.Service(path)
#webdriver_service.start()
#driver = webdriver.Remote(webdriver_service.service_url, webdriver.DesiredCapabilities.OPERA)

## Read links
all_links = pd.read_csv("LinksTech.csv")["0"]

## Second take out content: Run by batch to avoid being blocked

## Before moving forward activate ad blocker
driver = webdriver.Chrome('C:/Users/dordo/Documents/Daniel/LBS/chromedriver.exe') 
driver.get('https://www.google.com/')


news_list = []
## Control the slice of the links
for i in range(1403, 1500):
    print("Scraping news " + str(i))
    iters = 0
    error = True
    while error and iters < 10:
        try:
            error = False
            iters += 1
            info = get_content(all_links[i], driver)
        except:
            ## I am not a BOT!
            error = True
            print("Failed in iter " + str(iters))
            if iters == 1:
                driver.get(all_links[i])
            else:
                time.sleep(0.5)
            try:
                ##Robot Wall
                element_present = EC.presence_of_element_located((By.CSS_SELECTOR, "iframe"))
                WebDriverWait(driver, 5).until(element_present)
                iframes = driver.find_elements_by_css_selector("iframe")
                ActionChains(driver).click_and_hold(iframes[0]).perform()
                time.sleep(2.85)
                webdriver.ActionChains(driver).release().perform()
                ## Let the page load
                time.sleep(8)
            except:
                driver.close()
                driver = webdriver.Chrome('C:/Users/dordo/Documents/Daniel/LBS/chromedriver.exe') 
                h = input("Please start paywall blocker")
                driver.get(all_links[i])
    news_list.append(info)

#backup = news_list[:]
#backup2 = news_list[:]
#out_list = backup + news_list
out = pd.DataFrame(out_list)
out.to_csv("Raw/FilesTech" + str(i) + ".csv")

## Thrid close driver
driver.close()

