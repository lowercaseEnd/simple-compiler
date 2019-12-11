"use strict"

const readline = require("readline").createInterface({
    input: process.stdin,
    output: process.stdout
});

// const TAB = I;

//lookahead char
let look = "";

//read new char from input
let getChar = function() {
    return new Promise((resolve, reject) => {
    readline.question("Enter a character: ", (str) => {
        readline.close()
        resolve(str[0]);
    })
})};


function error(err) {
    console.log(`\nError: ${err}.`);
}
//end the execution
function abort(err) {
    error(err);
    throw new Error("");
}

function expected(value) {
    abort(value + "Expected");
}
//match specific input
function match(char) {
    if(look == char) {
        return getChar();
    }   else {
        expected("\"" + char + "\"");
    }
}

//recognize an aplha char
function isAlpha(char) {
    return /A-Z/.test(char.toUpperCase());
}

//recognize a decimal
function isDigit(char) {
    return /\d/.test(char);
}

//get an id
function getName() {
    if(!isAlpha(look))
        expected("name");
    let ch = look.toUpperCase();
    getChar();
    return ch;
}

//get a number
function getNum () {
    if(!isDigit(look))
        expected("name");
    let ch = look.toUpperCase();
    getChar();
    return ch;
}

//write a string with a TAB
function emit(string) {
    process.stdout.write(`\t${string}`)
}
//write a string with newline
function emitNl(string) {
    console.log(`${string}\n`);
}

//initialize
function init() {
    getChar().then((char) => {
        return char;
    });
}

//main
getChar().then((char) => {
    console.log(char);
});