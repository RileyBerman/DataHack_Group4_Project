#Create API Key https://www.slickremix.com/documentation/create-youtube-api-key/

#import libraries
import requests
import pandas as pd
import time

#Keys
API_KEY = "AIzaSyAT_4SIvZS6BHZYgg0JU4AJ_QOsV5pq-Os"
CHANNEL_ID = "UCW8Ws/tdKKKBT6GdtQaXvQ"

#make API call and grab call
response = requests.get('https://api.github.com').json #json object: popular data file sent over JavaScript object containing data in attribute-value pair

print("hello")