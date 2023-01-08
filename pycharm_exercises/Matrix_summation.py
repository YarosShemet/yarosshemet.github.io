n, m = map(int, input().split())
matrix1, matrix2 = [], []
for i in range(n):
    row = input().split()
    row = [int(q) for q in row]
    matrix1.append(row)
s = input() #or print()
for i in range(n):
    row = input().split()
    row = [int(q) for q in row]
    matrix2.append(row)
res_matrix = [[0] * m for _ in range(n)]
for i in range(n):
    for j in range(m):
        res_matrix[i][j] = matrix1[i][j] + matrix2[i][j]
        print(res_matrix[i][j], end=" ")
    print()
