from math import sqrt
a, b, c = [float(input()) for _ in range(3)]
d = b ** 2 - 4 * a * c
if d > 0 and a != 0:
    x1 = (-b + sqrt(d)) / (2 * a)
    x2 = (-b - sqrt(d)) / (2 * a)
    if x1 > x2:
        print(x2, x1, sep = "\n")
    else:
        print(x1, x2, sep = "\n")
elif d == 0 and a != 0:
    print(-b / (2 * a))
elif a == 0:
    print(-c / b)
else:
    print("Нет корней")