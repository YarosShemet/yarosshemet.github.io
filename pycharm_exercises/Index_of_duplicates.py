lst = [int(i) for i in input().split()]               #mine with features
x = int(input())
s = []
z = 0
if x in lst:
    for i in lst:
        if i == x:
            s.append(z)
            z += 1
        else:
            z += 1
            continue
print(*s)
else:
    print('No duplicates')



lst = [int(i) for i in input().split()]               #mine
x = int(input())
if x in lst:
    for i in range(len(lst)):
        if lst[i] == x:
         print(str(i), end=" ")
else:
    print("No duplicates")