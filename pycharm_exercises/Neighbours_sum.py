s = [int(i) for i in input().split()]             #mine
new_s = []
if len(s) == 1:
    new_s = (s[0])
    print(new_s)
for i in range(len(s)):
    if 0 < i < len(s)-1:
        print(s[i-1]+s[i+1], end=" ")
        i += 1
    elif i == 0 and len(s) != 1:
        print(s[i+1]+s[-1], end=" ")
    elif i == len(s)-1 and len(s) != 1:
        print(s[0]+s[i-1], end=" ")



a = [int(i) for i in input().split()]           #the most beautiful
if len(a)>1:
    for i in range(len(a)):
        print(a[i-1]+a[i+1-len(a)])
else:
    print(a[0])





