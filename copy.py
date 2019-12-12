import sys
import string



CR = "\n"
########error messages
#report an error
def error(err):
  print("\n")
  print("Error: {0}".format(err))

#report an error and halt
def abort(err):
  error(err)
  sys.exit()

#report what was expecter
def expected(str):
  abort(str + " expected")


########recognizers
#recognize an alphanum
def isAlphaNum(char):
  return isAlpha(char) or isDigit(char)
#recognize an alpha char
def isAlpha(char):
  return ord(str.lower(char)) in range(ord("a"), ord("z") + 1)
#recognuze number
def isDigit(num):
  return ord(num) in range(ord("0"), ord("9") + 1)
#recognize an addop
def isAddop(char):
  return char in ["+", "-"]
#recognize white space
def isSpace(char):
  return char in(" ", "\t")

#######output
#output string with a tab
def emit(string):
  print(f"\t{string}", end = "")
#output string with a new line
def emitLn(string):
  emit(string)
  print()


#initialize program
def init():
  initTable()
  getChar()
  skipSpace()

#lookahead char
look = ""
table = {}

#init the variable area
def initTable():
  table = dict((key, 0) for key in string.ascii_lowercase)

#read new char from input stream
def getChar():
  global look
  look = input("Enter a new char: ")
  if look == "":
    look = "\n"

#match a specific input char
def match(x):
  if look != x:
    expected(f"\'{x}\'")
  else:
    getChar()
    skipSpace()

#get an id
def getName():
  token = ""
  if not isAlpha(look):
    expected("Name")
  while isAlphaNum(look):
    token += str.lower(look)
    getChar()
  skipSpace()
  return token

#get number
def getNum():
  value = 0
  if not isDigit(look):
    expected("Integer")
  while isDigit(look):
    value = 10 * value + ord(look) - ord("0") 
    getChar()
  return value

#recognize and skip over a newline
def newLine():
  if look == "\r":
    getChar()
    if look == "\n":
      getChar()

#skip over leading white space
def skipSpace():
  while isSpace(look):
    getChar()

#input routine
def inputRoutine():
  match("?")
  table[getName()] = input("Enter id")

#output
def output():
  match("!")
  print(table[getName()])

#parse and translate an expression
def expression():
  value = 0
  if isAddop(look):
    value = 0
  else:
    value = term()
  while isAddop(look):
    if look == "+":
      match("+")
      value += term()
    elif look == "-":
      match("-")
      value -= term()
  return value


  #parse and translate an assignment statement
def assignment():
  name = getName()
  match("=")
  table[name] = expression()
  # emitLn(f"LEA {name}(PC),A0")
  # emitLn("MOVE D0,(A0)")


#<term> ::= <factor> [<mulop><factor>]*
#parse and translate a math expression
def term():
  value = factor()
  # factor()
  while look in ["*", "/"]:
    # emitLn("MOVE D0,-(SP)")
    if(look == "*"):
      match("*")
      value *= factor()
    elif(look == "/"):
      match("/")
      value /= factor()
  return value
    # else:
      # expected("Mulop")

#parse and translate a math factor
#<factor> ::= <number> |(expression) | variable
def factor():
  value = 0
  if look == "(":
    match("(")
    value = expression()
    match(")")
  elif isAlpha(look):
    # value = getName()
    temp = getName()
    return table[temp]
    # emitLn(f"MOVE {getName()}(PC),D0")
    # ident()
  else:
    value = getNum()
  return value
#parse and translate an id
#<ident> ::= <expression>
def ident():
  name = getName()
  if look == "(":
    match("(")
    match(")")
    emitLn(f"BSR + {name}")
  else:
    emitLn(f"MOVE {name}(PC),D0")

#main
# look = "+"
# print(plus_minus.get(look, "nothing")())
init()
while not look == ".":
  if look == "?":
    inputRoutine()
  elif look == "!":
    output()
  else:
    assignment()
  newLine()
# assignment()
# table["a"] = 5
print(expression())
# if look != CR:
  # expected("Newline")