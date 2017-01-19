package edu.zhangfan.compiler.frontend;


import java.util.*;
import java.util.function.Predicate;

// TODO placeholder name for now.

class Token {
    Terminal terminal;
    Integer value = null;

    Integer getValue() {
        return this.value;
    }

    Token(Terminal terminal) {
        this.terminal = terminal;
        this.value = null;
    }

    Token(Terminal terminal, Integer value) {
        this.terminal = terminal;
        this.value = value;
    }

}

/**
 * Scanner as a finite state machine.
 */
class Scanner {
    private SourceReader reader;
    private StateMachine stateMachine;
    private Token nextToken;
    private Map<String, Integer> symbolTable;


    Scanner(SourceReader reader) throws SyntaxError {
        this.reader = reader;
        this.symbolTable = new HashMap<>();

        // hardcoded state machine
        // first-level symbols
        final State INITIAL_STATE = new State(null);
        final State IDENTIFIER = new State(Terminal.IDENTIFIER);
        final State NUMBER = new State(Terminal.NUMBER);
        final State FIRST_EQUAL_SIGN = new State(null);
        final State FIRST_EXCLAMATION_MARK = new State(null);
        final State LESS_THAN_SIGN = new State(Terminal.SYMBOL);
        final State GREATER_THAN_SIGN = new State(Terminal.SYMBOL);
        final State SYMBOL = new State(Terminal.SYMBOL);

        // second-level symbols
        final State HYPHEN_AFTER_LEFT_ANGLE_BRACKET = new State(Terminal.SYMBOL);
        final State SECOND_EQUAL_SIGN = new State(Terminal.SYMBOL);
        // construct transition map
        Map<State, List<Transition>> transitionMap = new HashMap<>();
        transitionMap.put(INITIAL_STATE, Arrays.asList(
                new Transition(INITIAL_STATE, Character::isWhitespace),
                new Transition(IDENTIFIER, Character::isLetter),
                new Transition(NUMBER, Character::isDigit),
                new Transition(FIRST_EQUAL_SIGN, isSymbol('=')),
                new Transition(LESS_THAN_SIGN, isSymbol('<')),
                new Transition(GREATER_THAN_SIGN, isSymbol('>')),
                new Transition(FIRST_EXCLAMATION_MARK, isSymbol('!')),
                new Transition(SYMBOL, isOneOf('+', '-', '*', '/', '(', ')', '[', ']', '{', '}', ';', '.'))
        ));
        transitionMap.put(IDENTIFIER, Collections.singletonList(
                new Transition(IDENTIFIER, Character::isLetterOrDigit)
        ));
        transitionMap.put(NUMBER, Collections.singletonList(
                new Transition(NUMBER, Character::isDigit)
        ));
        for (State s : Arrays.asList(
                FIRST_EQUAL_SIGN,
                FIRST_EXCLAMATION_MARK, GREATER_THAN_SIGN)) {
            transitionMap.put(s, Collections.singletonList(
                    new Transition(SECOND_EQUAL_SIGN, isSymbol('='))
            ));
        }
        transitionMap.put(LESS_THAN_SIGN, Arrays.asList(
                new Transition(SECOND_EQUAL_SIGN, isSymbol('=')),
                new Transition(HYPHEN_AFTER_LEFT_ANGLE_BRACKET, isSymbol('-'))
        ));

        this.stateMachine = new StateMachine(INITIAL_STATE, INITIAL_STATE, transitionMap);

        next();
    }

    Token peek() {
        return nextToken;
    }

    boolean hasNext() {
        return this.nextToken != null;
    }

    void next() throws SyntaxError {
        if (!reader.hasNext()) {
            this.nextToken = null;
            return;
        }

        StringBuilder accumulator = new StringBuilder();

        while (reader.hasNext() && stateMachine.consume(reader.peek())) {
            // TODO better ways of skipping whitespaces
            if (!(stateMachine.currentState == stateMachine.initialState &&
                    Character.isWhitespace(reader.peek()))) {
                accumulator.append(reader.peek());
            }
            if (reader.hasNext()) {
                reader.next();
            } else {
                break;
            }
        }
        if (stateMachine.currentState.terminal != null) {
            // construct token based on currentState and accumulated string.
            Terminal terminal = stateMachine.currentState.terminal;
            String tokenString = accumulator.toString();
            if (terminal == Terminal.NUMBER) {
                nextToken = new Token(terminal, Integer.valueOf(tokenString));
            } else if (terminal == Terminal.IDENTIFIER) {
                if (Terminal.isKeyword(tokenString)) {
                    nextToken = new Token(Terminal.fromString(tokenString));
                } else {
                    if (symbolTable.containsKey(tokenString)) {
                        nextToken = new Token(terminal, symbolTable.get(tokenString));
                    } else {
                        symbolTable.put(tokenString, symbolTable.size());
                        nextToken = new Token(terminal, symbolTable.get(tokenString));
                    }
                }
            } else {
                nextToken = new Token(Terminal.fromString(tokenString));
            }
            stateMachine.reset();
        } else {
            throw new SyntaxError();
        }

    }


    private static Predicate<Character> isSymbol(char symbol) {
        return (c) -> c == symbol;
    }

    private static Predicate<Character> isOneOf(Character... symbols) {
        Set<Character> symbol_set = new HashSet<>(symbols.length);
        Collections.addAll(symbol_set, symbols);
        return symbol_set::contains;
    }

}

class SyntaxError extends Exception {
}

class Transition {
    State to;
    Predicate<Character> accept;

    Transition(State to, Predicate<Character> accept) {
        this.to = to;
        this.accept = accept;
    }
}

class State {

    Terminal terminal;

    State(Terminal terminal) {
        this.terminal = terminal;
    }
}

class StateMachine {

    State currentState;
    final State initialState;
    private Map<State, List<Transition>> transitionMap;

    StateMachine(State initialState, State currentState,
                        Map<State, List<Transition>> transitionMap) {
        this.initialState = initialState;
        this.currentState = currentState;
        this.transitionMap = transitionMap;
    }

    boolean consume(Character in) {
        if (transitionMap.containsKey(currentState)) {
            for (Transition transition : transitionMap.get(currentState)) {
                if (transition.accept.test(in)) {
                    currentState = transition.to;
                    return true;
                }
            }
        }
        return false;
    }

    void reset() {
        currentState = initialState;
    }
}

