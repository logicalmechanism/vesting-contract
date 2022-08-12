k = pow(10,6)
a = 2
b = 1

for i in range(1,k):
    val = a/(i/k) - b
    if val - int(val) == 0:
        if val < 100:
            print("Time in years", int(val)," starting %", 100*i/k,"%")
