package edu.zhangfan.compiler.frontend;

import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.sun.xml.internal.xsom.impl.Ref;


enum Terminal {
    IDENTIFIER, NUMBER, SYMBOL,
    // operator
    PLUS, MINUS, MULTIPLY, DIVIDE,
    EQUAL, NOT_EQUAL,
    LESS_THAN, NO_GREATER_THAN,
    GREATER_THAN, NO_LESS_THAN,
    ASSIGNMENT,
    // keyword
    MAIN,
    LET, VAR, ARRAY,
    IF, THEN, ELSE, FI,
    WHILE, DO, OD,
    CALL, FUNCTION, PROCEDURE, RETURN,
    // markers
    LEFT_BRACKET, RIGHT_BRACKET,
    LEFT_BRACE, RIGHT_BRACE,
    LEFT_PAREN, RIGHT_PAREN,
    SEMICOLON, DOT, COMMA;


    static final BiMap<String, Terminal> ARITHMETIC_OPERATOR_MAP = ImmutableBiMap.<String, Terminal>builder()
            .put("+", PLUS).put("-", MINUS).put("*", MULTIPLY).put("/", DIVIDE)
            .build();

    static final BiMap<String, Terminal> RELATION_OPERATOR_MAP = ImmutableBiMap.<String, Terminal>builder()
            .put("==", EQUAL).put("!=", NOT_EQUAL)
            .put("<", LESS_THAN).put("<=", NO_GREATER_THAN)
            .put(">", GREATER_THAN).put(">=", NO_LESS_THAN)
            .build();

    static final BiMap<String, Terminal> MARKER_MAP = ImmutableBiMap.<String, Terminal>builder()
            .put("[", LEFT_BRACKET).put("]", RIGHT_BRACKET)
            .put("{", LEFT_BRACE).put("}", RIGHT_BRACE)
            .put("(", LEFT_PAREN).put(")", RIGHT_PAREN)
            .put(";", SEMICOLON).put(".", DOT).put(",", COMMA)
            .put("<-", ASSIGNMENT)
            .build();
    static final BiMap<String, Terminal> KEYWORD_MAP = ImmutableBiMap.<String, Terminal>builder()
            .put("main", MAIN)
            .put("let", LET).put("var", VAR).put("array", ARRAY)
            .put("if", IF).put("then", THEN).put("else", ELSE).put("fi", FI)
            .put("while", WHILE).put("do", DO).put("od", OD)
            .put("call", CALL).put("function", FUNCTION).put("procedure", PROCEDURE).put("return", RETURN)
            .build();

    static final BiMap<String, Terminal> FULL_MAPS = ImmutableBiMap.<String, Terminal>builder()
            .putAll(RELATION_OPERATOR_MAP)
            .putAll(MARKER_MAP)
            .putAll(KEYWORD_MAP)
            .putAll(ARITHMETIC_OPERATOR_MAP)
            .build();

    static Terminal fromString(String str) {
        return FULL_MAPS.get(str);
    }

    static boolean isKeyword(String str) {
        return KEYWORD_MAP.containsKey(str);
    }

    static boolean isKeyword(Terminal terminal) {
        return KEYWORD_MAP.containsValue(terminal);
    }

    static boolean isRelOp(Terminal terminal) {
        return RELATION_OPERATOR_MAP.containsValue(terminal);
    }

    boolean oneOf(Terminal... terminals) {
        for (Terminal terminal : terminals) {
            if (this == terminal) {
                return true;
            }
        }
        return false;
    }
}


