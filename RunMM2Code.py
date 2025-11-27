import requests

# This is the default IP/PORT
url = "http://localhost:5000/metta"
headers = {"Content-Type": "text/plain; charset=utf-8" }

# Place your MM2 code below, start the MWJ server, and run this program under Python.
data = """

!(add-atom &mork (friend sam tim))
!(add-atom &mork (exec 0 (, (friend sam $x))
                         (O (- (friend sam $x))
                            (+ (enemy sam $x)))))
!(mm2-exec &mork 1)
!(match &mork (enemy sam $x) $x)

"""

response = requests.post(url, headers=headers, data=data.encode("utf-8"))
response.encoding = "utf-8"
print("Status:", response.status_code)
print("Response:", response.text)
