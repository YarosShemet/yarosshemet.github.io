pi = 3.14
H = str(input())
if H == "triangle":
    a = float(input()) 
    b = float(input())
    c = float(input())
    p = (a+b+c)/2
    print((p*(p-a)*(p-b)*(p-c)) ** 0.5)
elif H == "rectangle":
    a = int(input())
    b = int(input())
    print(a*b)
elif H == "circle":
    r = int(input())
    print(pi*(r ** 2))

