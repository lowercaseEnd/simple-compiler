import sys
import string

table = {}

def initTable():
  global table
  table = dict((key, 0) for key in string.ascii_lowercase)

#const TAB = ^I;
#carriage return
CR = "\n"

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

#recognize an alpha char
def isAlpha(char):
  return ord(str.lower(char)) in range(ord("a"), ord("z") + 1)
#recognuze number
def isDigit(num):
  return ord(num) in range(ord("0"), ord("9") + 1)
#recognize an addop
def isAddop(char):
  return char in ["+", "-"]
#output string with a tab
def emit(string):
  print(f"\t{string}", end = "")
#output string with a new line
def emitLn(string):
  emit(string)
  print()
#initialize program
def init():
  getChar()
  initTable()

#lookahead char
look = ""

#read new char from input stream
def getChar():
  global look
  look = input("Enter a new char: ")

#match a specific input char
def match(x):
  if look == x:
    getChar()
  else:
    expected(f"\'{x}\'")

#get an id
def getName():
  if not isAlpha(look):
    expected("Name")
  name = str.lower(look)
  getChar()
  return name

#get number
def getNum():
  value = 0
  if not isDigit(look):
    expected("Integer")
  while isDigit(look):
    value = 10 * value + ord(look) - ord("0")
    getChar()
  return value

#parse and translate an expression
#<expression> ::= <term> [<addop><term]*
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
    # elif look == "-":
    else:
      match("-")
      value -= term()
  return value
#parse and translate an assignment statement
def assignment():
  name = getName()
  match("=")
  table[name] = expression()

#<term> ::= <factor> [<mulop><factor>]*
#parse and translate a math expression
def term():
  value = factor()
  while look in ["*", "/"]:
    if(look == "*"):
      match("*")
      value *= factor()
    else:
      match("/")
      value /= factor()
  return value
#parse and translate a math factor
#<factor> ::= <number> |(expression) | variable
def factor():
  if look == "(":
    match("(")
    expression()
    match(")")
  elif isAlpha(look):
    return table[getName()]
  else:
    return getNum()
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

#recognize and skip over a newline
def newLine():
  if look == "\r":
    getChar()
    if look == "\n":
      getChar()

#input
def inputRoutine():
  match("?")
  table[getName()] = input("Enter a variable ")

#output
def output():
  match("!")
  print(table[getName()])
#main
# look = "+"
# print(plus_minus.get(look, "nothing")())
init()
# print(assignment())
while not look == ".":
  if look == "?":
    inputRoutine()
  elif look == "!":
    output()
  else:
    assignment()
  newLine()

# print(expression())
if look != CR:
  expected("Newline")