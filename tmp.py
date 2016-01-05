import sys

orientation_1 = sys.argv[1]
orientation_2 = sys.argv[2]

def rule(o1, o2):
    if o1 == o2:
        return "ToLeft"
    else:
        return "None"


print(rule(orientation_1, orientation_2))
