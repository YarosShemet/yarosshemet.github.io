from random import choice
digits = '0123456789'
lowercase_letters = 'abcdefghijklmnopqrstuvwxyz'
uppercase_letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
punctuation = '!#$%&*+-=?@^_'
chars = ''
number = int(input("How much password do you need? "))
length = int(input("How much symbols do you need? "))
need_digit = input("Do you need digits in the password? ")
need_upper = input("Do you need upper letters in the password? ")
need_lower = input("Do you need lower letters in the password? ")
need_symbols = input("Do you need special symbols? ")
exceptions = input('Exclude not obvious symbols like "il1Lo0O"? ')
if need_digit.lower() == "yes":
    chars += digits
if need_upper.lower() == "yes":
    chars += uppercase_letters
if need_lower.lower() == "yes":
    chars += lowercase_letters
if need_symbols.lower() == "yes":
    chars += punctuation
if exceptions.lower() == "yes":
    for i in chars:
        if i in "il1Lo0O":
            chars = chars.replace(i, "")
def generate_password(length, chars):
    password = ""
    for i in range(length):
        password += choice(chars)
    return password
print("Save for the future!")
for _ in range(number):
    print(generate_password(length, chars))



