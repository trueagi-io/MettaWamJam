import requests

# This is the default IP/PORT
url = "http://localhost:5001/metta"
headers = {"Content-Type": "text/plain; charset=utf-8" }

# Faiss is a library for efficient similarity search and clustering of dense vectors.
# Faiss contains several methods for similarity search.
data = """

!(import! &self (library faiss_ffi lib_faiss))

;providing our own embeddings:
!(new-atom-vectorspace &avs 4)
!(add-atom-vector &avs a (0.1 0.2 0.3 0.4))
!(add-atom-vector &avs b (0.8 0.9 1.0 1.1))
!(add-atom-vector &avs c (0.4 0.3 0.4 0.8))
!(test (atom-of (match-k 1 &avs (0.09 0.19 0.29 0.39)))
       a)

;cheap structural random indexing for atoms:
!(new-atom-vectorspace &sri 20)
!(add-atom-SRI &sri ((red apple) strudel))
!(add-atom-SRI &sri ((yellow banana) strudel))
!(add-atom-SRI &sri (strudel chaos))
!(test (atom-of (match-SRI 1 &sri ((red strawberry) strudel)))
       ((red apple) strudel))

"""

response = requests.post(url, headers=headers, data=data.encode("utf-8"))
response.encoding = "utf-8"
print("Status:", response.status_code)
print("Response:", response.text)

