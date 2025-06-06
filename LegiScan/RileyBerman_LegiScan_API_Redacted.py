#Contributor: Riley Berman 
#Description: Retrieves datasets containing all bills, votes, and people information for every specified session available in California. Outputs a folder containing all such files in .csv format. 
#Methodology: Using operations (getDataList and getDataset) documented in the LegiScan API https://legiscan.com/legiscan, interacts with the API to retrieve relevant datasets. 
#Note: My personal API key has been redacted for security reasons. This is a Python file. Run this through something like Visual Studio Code. 
#      A user wishing to replicate my own results needs to make their own API key through LegiScan. 

import requests
import zipfile
import os
import io
import base64

#Note: if you want to run this code on your own computer you need to generate your own LegiScan API key 

#Personal API Key (put your own if you are running this code)
API_Key = "redacted"
#Extracting California legislative data
State = "CA"

#Using getDatasetList operation to retreive session_id's and access_key's for getDataset operation
url = f"https://api.legiscan.com/?key={API_Key}&op=getDatasetList&state={State}"
response = requests.get(url)
data = response.json()

#Creating list of session_id, access_key pairs
session_access_pair = []
for element in data["datasetlist"]: 
  session_id = element["session_id"]
  access_key = element["access_key"]
  session_access_pair.append([session_id, access_key])

for pair in session_access_pair:  
  #Using getDataset operation to retrieve base64 encoded ZIP file containing all bills, votes and people data for the specified session (in .csv format)
  #pair[0] is the session_id, pair[1] is the access_key
  url = f"https://api.legiscan.com/?key={API_Key}&op=getDataset&id={pair[0]}&access_key={pair[1]}&format=csv"
  response = requests.get(url)
  data = response.json()

  base64_encoded_zip = data['dataset']['zip']

  #Decoding base64 zip string 
  zip_bytes = base64.b64decode(base64_encoded_zip)

  #Create an in-memory zip file object
  zip_file = io.BytesIO(zip_bytes)

  #Unzipping files and saving them to a specified directory 
  with zipfile.ZipFile(zip_file, 'r') as zf:
    #Specify directory (Data Hack folder on Desktop for me)
    output_directory = "/Users/rileyberman/Desktop/Data Hack"
    
    #Creates directory if nonexistent
    os.makedirs(output_directory, exist_ok=True) 
    
    #Extracts all files to specified directory
    zf.extractall(output_directory)

    print(f"Successfully unzipped files for session_id {pair[0]} with access_key {pair[1]} to: {output_directory}")

'''The code above should be sufficient, however, I have included some code implementing other API operations below for future reference. 

#Using getSessionList operation to get session_id's
url = f"https://api.legiscan.com/?key={API_Key}&op=getSessionList&state={State}"
response = requests.get(url)
data = response.json()

#Getting all available session_id's 
session_ids = []
for session in data["sessions"]: 
  session_ids.append(session["session_id"])

#Using getMasterList operation to get bill_id's for a specific session
url = f"https://api.legiscan.com/?key={API_Key}&op=getMasterList&id={session_ids[1]}"
response = requests.get(url)
data = response.json()

#Getting all available bill_id's
bill_ids = []
for element in data["masterlist"]: 

  #with session_id, there is a first element 'session'
  if element == "session":
    continue 

  bill_ids.append(data["masterlist"][element]["bill_id"])'''
