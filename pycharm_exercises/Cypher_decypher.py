print("Welcome to the encryption program!")
direct = input("Select encryption or decryption mode - ")
cnt = int(input("Enter the encoding step: "))
text = input("Text to encryption:")
en_lowercase_letters = 'abcdefghijklmnopqrstuvwxyz'
en_uppercase_letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ru_lowercase_letters = 'абвгдежзийклмнопрстуфхцчшщъыьэюяабвгдежзийклмнопрстуфхцчшщъыьэюя'
ru_uppercase_letters = 'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ'

def cipher(x):
    result = ""
    for i in x:
        if 65 <= ord(x[0]) <= 90 or 97 <= ord(x[0]) <= 122:
            if i.isalpha():
                if i.islower():
                    result += en_lowercase_letters[(en_lowercase_letters.find(i)+cnt) % 26]
                elif i.isupper():
                    result += en_uppercase_letters[(en_uppercase_letters.find(i)+cnt) % 26]
            else:
                result += i
        elif 1040 <= ord(x[0]) <= 1071 or 1072 <= ord(x[0]) <= 1103:
            if i.isalpha():
                if i.islower():
                    result += ru_lowercase_letters[(ru_lowercase_letters.find(i)+cnt) % 32]
                elif i.isupper():
                    result += ru_uppercase_letters[(ru_uppercase_letters.find(i)+cnt) % 32]
            else:
                result += i
    print(result)
def decipher(y):
    result = ""
    for i in y:
        if 65 <= ord(y[0]) <= 90 or 97 <= ord(y[0]) <= 122:
            if i.isalpha():
                if i.islower():
                    result += en_lowercase_letters[(en_lowercase_letters.find(i)-cnt) % 26]
                elif i.isupper():
                    result += en_uppercase_letters[(en_uppercase_letters.find(i)-cnt) % 26]
            else:
                result += i
        elif 1040 <= ord(y[0]) <= 1071 or 1072 <= ord(y[0]) <= 1103:
            if i.isalpha():
                if i.islower():
                    result += ru_lowercase_letters[(ru_lowercase_letters.find(i)-cnt) % 32]
                elif i.isupper():
                    result += ru_uppercase_letters[(ru_uppercase_letters.find(i)-cnt) % 32]
            else:
                result += i
    print(result)


if direct == "encryption":
    cipher(text)
elif direct == "decryption":
    decipher(text)








