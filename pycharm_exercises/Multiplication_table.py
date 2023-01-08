a = int(input())
b = int(input())
c = int(input())
d = int(input())
if a <= b and c <= d:
    print("\t", end='')
    for f in range(c, d+1):
        print(f, end="\t")
    print()
    for i in range(a, b+1):
        print(i, end="\t")
        for g in range(c, d+1):
            print(i * g, end="\t")
        print()
else:
    print("Numbers outside the allowed limits")

