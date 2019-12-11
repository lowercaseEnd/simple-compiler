import sys

#const TAB = ^I;

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
  if not isDigit(look):
    expected("Name")
  name = str.lower(look)
  getChar()
  return name

#get number
def getNum():
  if not isDigit(look):
    expected("Integer")
  num = look
  getChar()
  return num

#recognize and translate an add and subtract
def add():
  match("+")
  term()
  emitLn("ADD (SP)+,D0")

def subtract():
  match("-")
  term()
  emitLn("SUB (SP)+,D0")
  emitLn("NEG D0")

#recognize and translate multiply and divide
def multiply():
  match("*")
  factor()
  emitLn("MULS (SP)+,D0")

def divide():
  match("/")
  factor()
  emitLn("MOVE (SP)+,D1")
  emitLn("DIVS D1,D0")

#dict of ops
plus_minus = {
  "+": add,
  "-": subtract
}
#parse and translate an expression
def expression():
  if isAddop(look):
    emitLn("CLR D0")
  else:
    term()
  while isAddop(look):
    emitLn("MOVE D0,-(SP)")
    if(look == "+"):
      add()
    elif(look == "-"):
      subtract()
    # else:
      # expected("Addop")
  # global plus_minus
  # plus_minus.get(look, expected("Addop"))()
#<term> ::= <factor> [<mulop><factor>]*
#<expression> ::= <term> [<addop><term]*
#<factor> ::=(<expression>)
#parse and translate a math expression
def term():
  factor()
  while look in ["*", "/"]:
    emitLn("MOVE D0,-(SP)")
    if(look == "*"):
      multiply()
    elif(look == "/"):
      divide()
    # else:
      # expected("Mulop")

#parse and translate a math factor
def factor():
  if look == "(":
    match("(")
    expression()
    match(")")
  else:
    emitLn(f"MOVE #{getNum()},D0")
    
#main
# look = "+"
# print(plus_minus.get(look, "nothing")())
init()
expression()