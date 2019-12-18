import re
import argparse
import sys
from enum import Enum

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option


class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND = 'Identifier not found'
    DUPLICATE_ID = 'Duplicate id found'


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None):
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f'{self.__class__.__name__}: {message}'


class LexerError(Error):
    pass


class ParserError(Error):
    pass


class SemanticError(Error):
    pass

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis


class TokenType(Enum):

    PLUS = '+'
    MINUS = '-'
    MUL = '*'

    FLOAT_DIV = '/'
    LPAREN = '('
    RPAREN = ')'

    GT = '>'
    LT = '<'
    LTE = '<='
    GTE = '>='
    NE = '<>'
    EE = '='
    COMMA = ','
    SEMI = ';'
    DOT = '.'
    ARROW = '->'

    # keywords
    PROGRAM = 'program'
    BEGIN = 'begin'
    INT = 'int'
    INT_DIV = 'div'
    REAL = 'real'
    IF = 'if'
    FUN = 'fun'
    WRITE = 'write'
    READ = 'read'
    THEN = 'then'
    ELSE = 'else'
    ID = 'ID'
    FOR = 'for'
    WHILE = 'while'
    TO = 'to'
    DO = 'do'
    ASSIGN = 'ass'
    VAR = 'var'
    TRUE = 'true'
    FALSE = 'false'
    INT_CONST = 'int_const'
    REAL_CONST = 'real_const'
    BOOL = 'bool'
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    END = 'end'
    EOF = 'EOF'


class Token(object):
    def __init__(self, type, value, lineno=None, column=None):
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self):
        """String representation of the class instance.
        Example:
            >>> Token(TokenType.INT, 7, lineno=5, column=10)
            Token(TokenType.INT, 7, position=5:10)
        """
        return 'Token({type}, {value}, position={lineno}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self):
        return self.__str__()


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords


RESERVED_KEYWORDS = _build_reserved_keywords()

# LEXER


class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == '\n':
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()

    def exp(self):
        pass

    def number(self):
        """Return a (multidigit) number consumed from the input."""
        result = ''
        # Create a new token with current line and column number
        token = Token(type=None, value=None,
                      lineno=self.lineno, column=self.column)
        bin_match = re.compile("[01]+")
        oct_match = re.compile("[0-7]+")

        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        # check for binary number
        if bin_match.fullmatch(result):
            if self.current_char is not None and str.lower(self.current_char) == 'b':
                self.advance()
                if self.current_char is not None and str.lower(self.current_char) in ("a", "b", "c", "d", "e", "f"):
                    while self.current_char is not None and (self.current_char.isdigit() or self.current_char in("a", "b", "c", "d", "e", "f")):
                        result += self.current_char
                        self.advance()
                    token.type = TokenType.INT_CONST
                    token.value = int(result, 16)
                    return token
                if self.current_char is not None and self.current_char.isspace() or self.current_char == ';':
                    token.type = TokenType.INT_CONST
                    token.value = int(result, 2)
                    return token
                else:
                    self.error()

        # check for octal number
        if oct_match.fullmatch(result):
            if self.current_char is not None and str.lower(self.current_char) == 'o':
                self.advance()
                if self.current_char is not None and self.current_char.isspace() or self.current_char == ';':
                    token.type = TokenType.INT_CONST
                    token.value = int(result, 8)
                    return token
                else:
                    self.error()
        #check for exponent
        if self.current_char == 'e':
            self.advance()
            positive = False if self.current_char == "-" else True
            power = ""
            if self.current_char in ("+", "-"):
                self.advance()
                while self.current_char is not None and self.current_char.isdigit():
                    power += self.current_char
                    self.advance()
                if positive == True:
                    res = int(result) * pow(10, int(power))
                else:
                    res = int(result) * pow(10, -int(power))
                token.type = TokenType.INT_CONST
                token.value = int(res)
                return token
            while self.current_char is not None and self.current_char.isdigit():
                power += self.current_char
                self.advance()
            res = int(result) * pow(10, int(power))
            print(power)
            token.type = TokenType.INT_CONST
            token.value = int(res)
            return token
        # check for hex number
        if self.current_char is not None and str.lower(self.current_char) in ("a", "b", "c", "d", "e", "f"):
            while self.current_char is not None:
                if self.current_char.isspace() or self.current_char == ';':
                    break
                if not self.current_char.isdigit() and self.current_char not in ("a", "b", "c", "d", "e", "f"):
                    self.error()
                result += self.current_char
                self.advance()
            token.type = TokenType.INT_CONST
            token.value = int(result, 16)
            return token

        # check for real number
        if self.current_char == ".":
            result += self.current_char
            self.advance()
            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
                if self.current_char == 'e':
                    self.advance()
                    positive = False if self.current_char == "-" else True
                    power = ""
                    if self.current_char in ("+", "-"):
                        self.advance()
                        while self.current_char is not None and self.current_char.isdigit():
                            power += self.current_char
                            self.advance()
                        print(power)
                        if positive == True:
                            result = float(result) * pow(10, int(power))
                        else:
                            result = float(result) * pow(10, 1 / int(power))
                        token.type = TokenType.REAL_CONST
                        token.value = float(result)
                        return token
                    while self.current_char is not None and self.current_char.isdigit():
                        power += self.current_char
                        self.advance()
                    res = float(result) * pow(10, int(power))
                    print(f"Res - {res}")
                    token.type = TokenType.REAL_CONST
                    token.value = float(res)
                    return token
            token.type = TokenType.REAL_CONST
            token.value = float(result)
        elif str.lower(self.current_char) == 'd' or self.current_char.isspace() or self.current_char == ';':
            token.type = TokenType.INT_CONST
            token.value = int(result)
        else:
            self.error()

        return token

    # check for reserved keyword or assign an id
    def _id(self):
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None,
                      lineno=self.lineno, column=self.column)

        value = ''
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(str.lower(value))
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()
        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:

            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isdigit():
                return self.number()

            if self.current_char.isalpha():
                return self._id()

            if self.current_char == '>':
                self.advance()
                if self.current_char == "=":
                    self.advance()
                    token = Token(
                        type=TokenType.GTE,
                        value=TokenType.GTE.value,  # ':='
                        lineno=self.lineno,
                        column=self.column,
                    )
                    return token
                else:
                    token = Token(
                        type=TokenType.GT,
                        value=TokenType.GT.value,  # ':='
                        lineno=self.lineno,
                        column=self.column,
                    )
                    return token

            if self.current_char == '-' and self.peek() == '>':
                self.advance()
                self.advance()
                token = Token(
                    type=TokenType.ARROW,
                    value=TokenType.ARROW.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                return token

            if self.current_char == '<':
                self.advance()
                if self.current_char == "=":
                    self.advance()
                    token = Token(
                        type=TokenType.LTE,
                        value=TokenType.LTE.value,
                        lineno=self.lineno,
                        column=self.column,
                    )
                    return token
                elif self.current_char == ">":
                    token = Token(
                        type=TokenType.NE,
                        value=TokenType.NE.value,
                        lineno=self.lineno,
                        column=self.column,
                    )
                    return token
                else:
                    token = Token(
                        type=TokenType.LT,
                        value=TokenType.LT.value,  #
                        lineno=self.lineno,
                        column=self.column,
                    )
                    return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)


# AST
class AST(object):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

# compound statements


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self):
        self.children = []


class Assign(AST):
    """Assign AST node represents an assignment statement.
    Its left variable is for storing a Var node and its right
    variable is for storing a node returned by the expr parser method"""

    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""

    def __init__(self, token):
        self.token = token
        self.value = token.value

# empty statement


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, block):
        self.block = block


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class IfNode(AST):
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case


class ForNode:
    def __init__(self, assign, end_value_node, body_node):
        self.var_name = assign.left.value
        self.start_value_node = assign.right
        self.end_value_node = end_value_node
        self.body_node = body_node


class WhileNode:
    def __init__(self, condition_node, body_node):
        self.condition_node = condition_node
        self.body_node = body_node


class WriteNode:
    def __init__(self, ids):
        self.ids = ids
# PARSER


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

    def get_next_token(self):
        return self.lexer.get_next_token()

    def error(self, error_code, token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def program(self):
        """program : PROGRAM block DOT"""
        self.eat(TokenType.PROGRAM)
        block_node = self.block()
        program_node = Program(block_node)
        self.eat(TokenType.DOT)
        return program_node

    def block(self):
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self):
        """declarations : VAR type_spec (variable_declaration SEMI)+
                        | empty
        """
        declarations = []
        if self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)

            while self.current_token.type in (TokenType.INT, TokenType.REAL, TokenType.BOOL):
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        return declarations

    def variable_declaration(self):
        """variable_declaration : int | real | bool | ID (COMMA ID)*"""
        typeNode = self.type_spec()
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)

        # type_node = self.type_spec()
        var_declarations = [
            VarDecl(var_node, typeNode)
            for var_node in var_nodes
        ]
        return var_declarations

    def type_spec(self):
        """type_spec : INT
                     | REAL
                     | BOOL
        """
        token = self.current_token
        if self.current_token.type == TokenType.INT:
            self.eat(TokenType.INT)
        elif self.current_token.type == TokenType.BOOL:
            self.eat(TokenType.BOOL)
        else:
            self.eat(TokenType.REAL)
        node = Type(token)
        return node

    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        """
        statement_list : statement
                      | statement SEMI statement_list
        """
        node = self.statement()
        results = [node]

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        return results

    #########

    def statement(self):
        """
        statement : compound_statement
                  | if_statement
                  | assignment_statemen
                  | for_statement
                  | while_statement
                  | empty
        """
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
        elif self.current_token.type == TokenType.IF:
            node = self.if_statement()
        elif self.current_token.type == TokenType.FOR:
            node = self.for_statement()
        elif self.current_token.type == TokenType.WHILE:
            node = self.while_statement()
        elif self.current_token.type == TokenType.WRITE:
            node = self.write_statement()
        else:
            node = self.empty()
        return node

    def write_statement(self):
        fun_token = self.current_token
        self.eat(TokenType.WRITE)
        self.eat(TokenType.LPAREN)
        args = []
        args.append(self.current_token)
        print(args)
        print(self.current_token)
        self.eat(TokenType.ID)
        print(self.current_token)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            args.append(self.current_token)
            self.eat(TokenType.ID)
        self.eat(TokenType.RPAREN)

        return WriteNode(fun_token, )

    def for_statement(self):
        """for_statement: for <assignment> to <expr> do <expr>"""
        self.eat(TokenType.FOR)
        ass = self.assignment_statement()
        self.eat(TokenType.TO)
        end_value = self.expr()
        self.eat(TokenType.DO)
        body = self.compound_statement()
        return ForNode(ass, end_value, body)

    def while_statement(self):
        """while_statement: while <expr>  do <expr>"""
        self.eat(TokenType.WHILE)
        condition = self.expr()
        self.eat(TokenType.DO)
        body = self.compound_statement()
        return WhileNode(condition, body)

    def if_statement(self):
        """if_statement : if <expr> then <statement> [else <statement>]"""
        cases = []
        else_case = None
        self.eat(TokenType.IF)
        condition = self.expr()
        self.eat(TokenType.THEN)
        expr = self.compound_statement()
        cases.append((condition, expr))
        # while self.current_token.type == TokenType.ELIF:
        #   self.eat(TokenType.ELIF)
        #   condition = self.expr()
        #   self.eat(TokenType.THEN)
        #   expr = self.expr()
        #   cases.append((condition, expr))

        if self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            else_case = self.compound_statement()

        return IfNode(cases, else_case)

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(TokenType.ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def factor(self):
        """factor : (PLUS | MINUS | NOT) factor| int | real | true | false | LPAREN expr RPAREN | variables"""
        token = self.current_token
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.NOT:
            self.eat(TokenType.NOT)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.INT_CONST:
            self.eat(TokenType.INT_CONST)
            return Num(token)
        elif token.type == TokenType.TRUE:
            self.eat(TokenType.TRUE)
            token.value = 1
            return Num(token)
        elif token.type == TokenType.FALSE:
            self.eat(TokenType.FALSE)
            token.value = 0
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        """term : factor ((MUL | DIV | and) factor)*"""
        node = self.factor()

        while self.current_token.type in (TokenType.MUL, TokenType.INT_DIV, TokenType.FLOAT_DIV, TokenType.AND):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
                # result = result * self.factor()
            elif token.type == TokenType.INT_DIV:
                self.eat(TokenType.INT_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)
            elif token.type == TokenType.AND:
                self.eat(TokenType.AND)
                # result = result // self.factor()
            node = BinOp(left=node, op=token, right=self.factor())
        return node

    def oper(self):
        """oper   : term ((PLUS | MINUS | or) term)*"""
        node = self.term()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS, TokenType.OR):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
                # result = result + self.factor()
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)
            elif token.type == TokenType.OR:
                self.eat(TokenType.OR)
                # result = result - self.factor()
            node = BinOp(left=node, op=token, right=self.term())
        return node

    def expr(self):
        # result = self.term()
        node = self.oper()

        while self.current_token.type in (TokenType.GT, TokenType.EE, TokenType.NE, TokenType.GTE, TokenType.LT, TokenType.LTE):
            token = self.current_token
            if token.type == TokenType.GT:
                self.eat(TokenType.GT)
            elif token.type == TokenType.GTE:
                self.eat(TokenType.GTE)
            elif token.type == TokenType.LT:
                self.eat(TokenType.LT)
            elif token.type == TokenType.LTE:
                self.eat(TokenType.LTE)
            elif token.type == TokenType.NE:
                self.eat(TokenType.NE)
            elif token.type == TokenType.EE:
                self.eat(TokenType.EE)

            node = BinOp(left=node, op=token, right=self.oper())

        return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : VAR (variable_declaration SEMI)+
                     | empty

        variable_declaration : ID (COMMA ID)* COLON type_spec

        type_spec : INT | REAL

        compound_statement : BEGIN statement_list END

        statement_list : statement
                      | statement SEMI statement_list

        statement : compound_statement
                 | assignment_statement
                 | empty

        assignment_statement : variable ASSIGN expr

        empty :

        expr : term ((PLUS | MINUS) term)*

        term : factor ((MUL | INT_DIV | FLOAT_DIV) factor)*

       factor : PLUS factor
              | MINUS factor
              | INT_CONST
              | REAL_CONST
              | LPAREN expr RPAREN
              | variable

        variable: ID

        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node

# AST Walkers


class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')


class Number:
    def __init__(self, value):
        self.value = value

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                raise SemanticError(
                    error_code=ErrorCode.UNEXPECTED_TOKEN,
                    token=other,
                    message=f'Error dividing by zero',
                )

            return Number(self.value / other.value)

    def get_comparison_ee(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value))

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value))

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value))

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value))

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value))

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value))

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value))

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value))

    def notted(self):
        return Number(1 if self.value == 0 else 0)

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)


# SYMBOL TABLE and semantic analizer
class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class SymbolTable(object):
    def __init__(self):
        self._symbols = {}
        self._init_builtins()

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INT'))
        self.insert(BuiltinTypeSymbol('REAL'))
        self.insert(BuiltinTypeSymbol('BOOL'))
        self.insert(Symbol('True', Number(1)))
        self.insert(Symbol('False', Number(0)))

    def __str__(self):
        symtab_header = 'Symbol table contents'
        lines = ['\n', symtab_header, '_' * len(symtab_header)]
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        print('Insert: %s' % symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print('Lookup: %s' % name)
        symbol = self._symbols.get(name)
        # 'symbol' is either an instance of the Symbol class or None
        return symbol


class ProcedureSymbol(Symbol):
    def __init__(self, name, params=None):
        super().__init__(name)
        # a list of formal parameters
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__


class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INT'))
        self.insert(BuiltinTypeSymbol('REAL'))
        self.insert(BuiltinTypeSymbol('BOOL'))
        self.insert(Symbol('True', Number(1)))
        self.insert(Symbol('False', Number(0)))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
            ('Enclosing scope',
             self.enclosing_scope.scope_name if self.enclosing_scope else None
             )
        ):
            lines.append('%-15s: %s' % (header_name, header_value))
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value.type))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol):
        self.log(f'Insert: {symbol.name}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        self.log(f'Lookup: {name}. (Scope name: {self.scope_name})')
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def error(self, error_code, token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        self.log('ENTER scope: global')
        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=1,
            enclosing_scope=self.current_scope,  # None
        )
        global_scope._init_builtins()
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log('LEAVE scope: global')

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        self.current_scope.insert(proc_symbol)

        self.log(f'ENTER scope: {proc_name}')
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)

        self.log(procedure_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f'LEAVE scope: {proc_name}')

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )

        self.current_scope.insert(var_symbol)

    def visit_Assign(self, node):
        # right-hand side
        self.visit(node.right)
        # left-hand side
        self.visit(node.left)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_Num(self, node):
        pass

    def visit_UnaryOp(self, node):
        pass

    def visit_IfNode(self, node):
        pass

    def visit_ForNode(self, node):
        pass

    def visit_WhileNode(self, node):
        pass

    def visit_WriteNode(self, node):
        pass

# INTERPRETER


class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.GLOBAL_MEMORY = {}

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        # Do nothing
        pass

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_BinOp(self, node):
        result = 0
        if node.op.type == TokenType.PLUS:
            result = self.visit(node.left).added_to(self.visit(node.right))
        elif node.op.type == TokenType.MINUS:
            result = self.visit(node.left).subbed_by(self.visit(node.right))
        elif node.op.type == TokenType.MUL:
            result = self.visit(node.left).multed_by(self.visit(node.right))
        elif node.op.type == TokenType.FLOAT_DIV:
            result = self.visit(node.left).dived_by(self.visit(node.right))
        elif node.op.type == TokenType.INT_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.GT:
            result = self.visit(node.left).get_comparison_gt(
                self.visit(node.right))
        elif node.op.type == TokenType.GTE:
            result = self.visit(node.left).get_comparison_gte(
                self.visit(node.right))
        elif node.op.type == TokenType.LT:
            result = self.visit(node.left).get_comparison_lt(
                self.visit(node.right))
        elif node.op.type == TokenType.LTE:
            result = self.visit(node.left).get_comparison_lte(
                self.visit(node.right))
        elif node.op.type == TokenType.EE:
            result = self.visit(node.left).get_comparison_ee(
                self.visit(node.right))
        elif node.op.type == TokenType.NE:
            result = self.visit(node.left).get_comparison_ne(
                self.visit(node.right))
        elif node.op.type == TokenType.AND:
            result = self.visit(node.left).anded_by(self.visit(node.right))
        elif node.op.type == TokenType.OR:
            result = self.visit(node.left).ored_by(self.visit(node.right))

        return result

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            return self.visit(node.expr).multed_by(Number(1))
        elif op == TokenType.MINUS:
            return self.visit(node.expr).multed_by(Number(-1))
        elif op == TokenType.NOT:
            return self.visit(node.expr).notted()

    def visit_NoOp(self, node):
        pass

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)
        self.GLOBAL_MEMORY[var_name] = var_value

    def visit_Var(self, node):
        var_name = node.value
        var_value = self.GLOBAL_MEMORY.get(var_name)
        return var_value

    def visit_Num(self, node):
        return Number(node.value)

    def visit_IfNode(self, node):

        for condition, expr in node.cases:
            condition_value = self.visit(condition)

            if condition_value.is_true():
                expr_value = self.visit(expr)
                return expr_value

        if node.else_case:
            else_value = self.visit(node.else_case)
            return else_value

        return None

    def visit_ForNode(self, node):
        start_value = self.visit(node.start_value_node)
        # print(start_value)
        end_value = self.visit(node.end_value_node)
        step_value = Number(1)
        i = start_value.value
        # print(f"Value - {i}")

        if step_value.value >= 0:
            def condition(): return i < end_value.value
            # print(i)
        else:
            def condition(): return i > end_value.value

        while condition():
            self.GLOBAL_MEMORY[node.var_name] = Number(i)
            # context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value

            self.visit(node.body_node)

        return None

    def visit_WhileNode(self, node):
        while True:
            condition = self.visit(node.condition_node)

            if not condition.is_true():
                break

            self.visit(node.body_node)

        return None

    def visit_WriteNode(self, node):
        pass

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)


def main():
    parser = argparse.ArgumentParser(
        description='Interpreter'
    )
    parser.add_argument('inputfile', help='Pascal source file')
    parser.add_argument(
        '--scope',
        help='Print scope information',
        action='store_true',
    )
    args = parser.parse_args()
    global _SHOULD_LOG_SCOPE
    _SHOULD_LOG_SCOPE = args.scope

    text = open(args.inputfile, 'r').read()

    lexer = Lexer(text)
    try:
        parser = Parser(lexer)
        tree = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)

    semantic_analyzer = SemanticAnalyzer()
    try:
        semantic_analyzer.visit(tree)
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

    interpreter = Interpreter(tree)
    res = interpreter.interpret()
    if res:
        print(res)
    for k, v in sorted(interpreter.GLOBAL_MEMORY.items()):
        print('{} = {}'.format(k, v))


if __name__ == '__main__':
    main()
