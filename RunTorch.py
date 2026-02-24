import requests

# This is the default IP/PORT
url = "http://localhost:5001/metta"
headers = {"Content-Type": "text/plain; charset=utf-8" }

# Example of using MeTTa and pytorch.
data = """

;Clean separation from Python calls
(= (tensor $x) (py-call (torch.tensor $x)))
(= (mul $A $B) (py-call (torch.matmul $A $B)))
(= (tensor2list $x) (py-call (.tolist $x)))
(= (size2list $x) (py-call (list $x)))
(= (shape $x) (size2list (py-call (getattr $x shape))))

;Clean program using Tensors:
(= (myprog)
   (let* (($A (tensor ((1 2 3)
                       (4 5 6))))
          ($B (tensor (( 7  8  9 10)
                       (11 12 13 14)
                       (15 16 17 18))))
          ($C (mul $A $B))
          ($S (shape $C))
          ($L (tensor2list $C)))
         ($L $S)))

; test 
!(let $result (myprog) 
	(if (== $result (((74 80 86 92) (173 188 203 218)) (2 4)))
		success!
		failed!))

"""

response = requests.post(url, headers=headers, data=data.encode("utf-8"))
response.encoding = "utf-8"
print("Status:", response.status_code)
print("Response:", response.text)

