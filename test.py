import string

table = dict((key, 0) for key in string.ascii_lowercase)

def initTable():
  table = dict((key, 0) for key in string.ascii_lowercase)

print(table)
initTable()
print(table)