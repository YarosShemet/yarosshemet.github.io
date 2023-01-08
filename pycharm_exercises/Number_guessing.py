import random
print("Welcome to guesser!")
print("Rules", "1) there is a number between 1 and YOUR chosen number", "2) your task is to guess the number", "3) I will tell you whether it is more or less", sep="\n")
def is_valid(num):
    return num.isdigit() and 1 <= int(num) <= int(x)
def number(x):
    while x.isdigit() == False:
        print("Choose UPPER number limit ", end="")
        x = int(input())
        return x
x = input("Choose upper limit: ")
number(x)
n = random.randint(1, int(x))
counter = 1
again = "yes"
while again == "yes":
    while True:
        print("Input the number: ", end="")
        nums = input()
        if is_valid(nums) == True:
            nums = int(nums)
            if nums > int(x):
                print("Your number is above the upper limit. Try again.")
                continue
            if nums < n:
                print("Your number is lower than the one you guessed. Try again.")
                counter += 1
            elif nums > n:
                print("Your number is higher than the one you guessed. Try again.")
                counter += 1
            elif nums == n:
                print("You have guessed! Congratulations! The number of guesses: ", counter)
                counter = 0
                print()
                print("Thank you for the game. If you want to play again, type yes")
                again = input()
                if again.lower() == "yes":
                    print("Do you like to change the upper limit?")
                    again = input()
                    if again.lower() == "yes":
                        x = input("Choose the upper limit: ")
                        number(x)
                        n = random.randint(1, int(x))
                        continue
                else:
                    print("Thank you!")
                    break
        else:
            print(f'Maybe it would be better to input the number from 1 to {x}?')




