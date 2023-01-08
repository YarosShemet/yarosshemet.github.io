c = 1
while c <= 6:
    print('*' * c)
    c += 1

stars = '*'
while len(stars) <= 6:
    print(stars)
    stars += '*'

i = 0
while i < 5:
    print('*')
    if i % 2 == 0:
        print('**')
    if i > 2:
        print('***')
    i = i + 1