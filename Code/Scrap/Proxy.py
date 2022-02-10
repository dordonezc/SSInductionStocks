from selenium import webdriver
from selenium.webdriver.chrome.options import DesiredCapabilities
from selenium.webdriver.common.proxy import Proxy, ProxyType
import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project/Data/SeekingAlphaData")
import time

# Add path to your WebDriver according to the browser you are using 
ch_path = 'C:/Users/dordo/Dropbox/Capstone Project/Driver/chromedriver.exe'	

co = webdriver.ChromeOptions()
co.add_argument("log-level=3")
#co.add_argument("--headless")

def get_proxies(co=co):
    driver = webdriver.Chrome(ch_path, options=co)
    driver.get("https://free-proxy-list.net/")

    PROXIES = []
    proxies = driver.find_elements_by_css_selector("tr[role='row']")
    
    for p in proxies:
        result = p.text.split(" ")
        if result[-1] == "yes":
            PROXIES.append(result[0]+":"+result[1])
            
    driver.close()
    return PROXIES


proxy_list = get_proxies()


def proxy_driver(PROXIES, co=co):
    prox = Proxy()

    if PROXIES:
        pxy = PROXIES[-1]
    else:
        print("--- Proxies used up (%s)" % len(PROXIES))
        PROXIES = get_proxies()

    prox.proxy_type = ProxyType.MANUAL
    prox.http_proxy = pxy
    prox.ssl_proxy = pxy

    capabilities = webdriver.DesiredCapabilities.CHROME
    prox.add_to_capabilities(capabilities)

    driver = webdriver.Chrome(options=co, desired_capabilities=capabilities)

    return driver



# --- YOU ONLY NEED TO CARE FROM THIS LINE ---
# creating new driver to use proxy
pd = proxy_driver(ALL_PROXIES)

# code must be in a while loop with a try to keep trying with different proxies
running = True

while running:
    try:
        mycodehere()
        
        # if statement to terminate loop if code working properly
        something()
        
        # you 
    except:
        new = ALL_PROXIES.pop()
        
        # reassign driver if fail to switch proxy
        pd = proxy_driver(ALL_PROXIES)
        print("--- Switched proxy to: %s" % new)
        time.sleep(1)
        
        
# Open web driver
driver = webdriver.Chrome('C:/Users/dordo/Documents/Daniel/LBS/chromedriver.exe') 
url = "https://www.wsj.com/articles/tom-brady-patrick-mahomes-buccaneers-chiefs-super-bowl-11611531150"
driver.get(url)
driver.find_element_by_class_name("article-content").text
driver.close()
