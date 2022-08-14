import hashlib

def hashit(value):
    m = hashlib.sha3_256()
    m.update(bytes(str(value).encode('utf-8')))
    return m.hexdigest()

def strToInt(string, value, counter):
    if counter == 0:
        return value

    head = string[:2]
    tail = string[2:]

    value = value + [int("0x"+head, 16)+1]
    return strToInt(tail, value, counter - 1)

def prod(array):
    if len(array)==0: return 1
    else: return array[0]*prod(array[1:])

def reduceInt(num, counter):
    if counter == 0:
        return num
    return reduceInt(num//counter + counter, counter -1)

    # while num > 1:
    #     if num % 2 == 0:
    #         num /= 2
    #     else:
    #         num = 3*num+1
    #     counter += 1
    # return counter
if __name__ == "__main__":
    a = hashit("b")
    b = strToInt(a, [], 32)
    c = prod(strToInt(a, [], 7))

    print(reduceInt(c, 14))
    # print(a,b,c)
    from random import randint


    # longest = []
    # for t in range(10000):
    #     values = []
    #     for i in range(100):
    #         n = randint(123123123,312312312312)
    #         a = hashit(str(n))
    #         c = reduceInt(prod(strToInt(a, [], 7)),14)
    #         if c not in values:
    #             values.append(c)
    #         else:
    #             # print('broken',i, c)
    #             if len(longest) < i:
    #                 longest = values
    #             break
    # print(len(longest))
