import sys

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

#lookahead char
look = ""
lCount = 0 # label counter
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
#<expression> ::= <term> [<addop><term]*
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

#parse and translate an assignment statement
def assignment():
  name = getName()
  match("=")
  expression()
  emitLn(f"LEA {name}(PC),A0")
  emitLn("MOVE D0,(A0)")


#<term> ::= <factor> [<mulop><factor>]*
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
#<factor> ::= <number> |(expression) | variable
def factor():
  if look == "(":
    match("(")
    expression()
    match(")")
  elif isAlpha(look):
    # emitLn(f"MOVE {getName()}(PC),D0")
    ident()
  else:
    emitLn(f"MOVE #{getNum()},D0")
    
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

#<program> ::= <block> END
#<block> ::= [<statement>]*
#parse and translate a program
def doProgram():
  block()
  if look != "e":
    expected("End")
  emitLn("END")

#recognize and translate a statement block
def block():
  while not look in ["e"]:
    if look == "i":
      doIf()
    else:
      other()

#recognize and translate other
def other():
  emitLn(getName())

#generate a unique label
def newLabel():
  global lCount
  string = lCount
  lCount += 1
  return "L" + str(string)

def postLabel(label):
  print(f"{label}:")


#parse and translate a boolean condition
def condition():
  emitLn("<condition>")
#BEQ <=> branch if false
#BNE <=> branch if true
#IF <condition> <block> [ELSE <block>] ENDIF


###########syntax-directed translation
#if
#<condition> {L1 = newLabel
# L2 = newLabel
# emit(BEQ L1)}
#<block>
#ELSE {EMIT(BRA L2)
# postLabel(L1) }
#<block>
#ENDIF {postLabel(L2)}

#recognize and translate an if construct
def doIf():
  label = ""
  match("i")
  label1 = newLabel()
  label2 = label1
  emitLn(f"BEQ {label1}")
  # condition()
  # emitLn(f"BEQ {label}")
  block()
  if look == "l":
    match("l")
    label2 = newLabel()
    emitLn(f"BRA {L2}")
    postLabel(label1)
    block()
  match("e")
  postLabel(label2)



#main
# look = "+"
# print(plus_minus.get(look, "nothing")())
init()
doProgram()
# assignment()
if look != CR:
  expected("Newline")