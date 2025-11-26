import requests

# This is the default IP/PORT
url = "http://localhost:5000/metta"
headers = {"Content-Type": "text/plain"}

# Place your MeTTa code below, start the MWJ server, and run this program under Python.
data = """

!(let $a (is fun!) (cons-atom MeTTa $a))
                    
"""

response = requests.post(url, headers=headers, data=data)

print("Status:", response.status_code)
print("Response:", response.text)

