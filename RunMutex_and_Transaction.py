import requests

# This is the default IP/PORT
url = "http://localhost:5001/metta"
headers = {"Content-Type": "text/plain; charset=utf-8" }

# Example of using mutex and transaction in order to 
# prevent multiple threads trampling on each other.
# MeTTa can be used as a quasi-database.
data = """

!(add-atom &temp (count 37))

;This only works predictable single-threaded, else there is a data race:
(= (sloppyinc)
   (match &temp (count $x)
          ((remove-atom &temp (count $x))
           (let $inc (+ $x 1)
                     (add-atom &temp (count $inc))))))

;To protect from data race we can use a mutex:
(= (mutexinc)
   (with_mutex testmutex
               (match &temp (count $x)
                      ((remove-atom &temp (count $x))
                       (let $inc (+ $x 1)
                            (add-atom &temp (count $inc)))))))

;Additionally we support transactions, the following illlustrates that remove-atom is undone if the nondet branch fails:
(= (Transaction_rollback_fail_to_inc)
   (transaction (match &temp (count $x)
                       ((remove-atom &temp (count $x))
                        (let $inc (+ $x 1)
                             (add-atom &temp (count $inc)))
                        (empty)))))

;even though swipl is threadsafe and will also not deadlock, this would cause aforementioned race condition between reading and writing:
;!(hyperpose ((sloppyinc) (sloppyinc) (sloppyinc) (sloppyinc) (sloppyinc)))

;while this is fine as all places modifying the (count $n) relation simultaneously (mutexinc only in this case) use the same mutex:
!(hyperpose ((mutexinc) (mutexinc) (mutexinc) (mutexinc) (mutexinc)))

; test
!(let $result (collapse (get-atoms &temp))
	(if (== $result ((count 42)))
		success!
		failed!))


;This won't affect the count since the transaction is setup to fail:
!(Transaction_rollback_fail_to_inc)
!(let $result (collapse (get-atoms &temp)) 
	(if (== $result ((count 42)))
		success!
		failed!))
                    
"""

response = requests.post(url, headers=headers, data=data.encode("utf-8"))
response.encoding = "utf-8"
print("Status:", response.status_code)
print("Response:", response.text)

