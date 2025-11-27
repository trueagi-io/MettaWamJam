import requests

# This is the default IP/PORT
url = "http://localhost:5000/metta"
headers = {"Content-Type": "text/plain; charset=utf-8" }

# Place your MeTTa code below, start the MWJ server, and run this program under Python.
data = """

!(let $a (is fun!) (cons-atom MeTTa $a))
                    
"""

response = requests.post(url, headers=headers, data=data.encode("utf-8"))
response.encoding = "utf-8"
print("Status:", response.status_code)
print("Response:", response.text)

