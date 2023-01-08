a = float(input())
b = float(input())
c = input()
if c == "+":
    print(a+b)
elif c == "-":
    print(a-b)
elif c == "div":
    if b == 0:
        print("Division by 0")
    else:
        print(a // b)
elif c == "mod":
    if b == 0:
        print("Division by 0")
    else:
        print(a % b)
elif c == "/":
    if b == 0:
        print("Division by 0")
    else:
        print(a /b)
elif c == "*":
    print(a*b)
elif c == "pow":
    print(a**b)



