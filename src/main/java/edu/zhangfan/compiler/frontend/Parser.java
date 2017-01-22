package edu.zhangfan.compiler.frontend;

import com.google.common.collect.ImmutableSet;

import java.util.Collection;
import java.util.function.Predicate;


/**
 * TODO performance optimization
 * 1. reduce the number of times checking if a NT is fulfilled.
 */

class Summary {
    Integer value;

    Summary(Integer value) {
        this.value = value;
    }
}

@FunctionalInterface
interface Rule {
    Summary apply() throws SyntaxError;
}

class Production {

    final ImmutableSet<Terminal> first;
    final Rule rule;

    Production(ImmutableSet<Terminal> first, Rule rule) {
        this.first = first;
        this.rule = rule;
    }

    Summary consume() throws SyntaxError {
        return rule.apply();
    }

}

class Grammar {

    static Scanner scanner;

    static private boolean fulfill(Predicate<Terminal> expectation) {
        return expectation.test(scanner.peek().terminal);
    }

    static private boolean fulfill(Terminal... terminals) {
        return fulfill(isOneOf(terminals));
    }

    static private boolean fulfill(Production p) {
        return fulfill(isOneOf(p.first));
    }

    static private void assure(Predicate<Terminal> condition) throws SyntaxError {
        if (!(fulfill(condition))) {
            error();
        }
    }

    static private void assure(Production p) throws SyntaxError {
        assure(isOneOf(p.first));
    }

    static private void assure(Terminal... terminals) throws SyntaxError {
        assure(isOneOf(terminals));
    }

    static void consume(Terminal... terminals) throws SyntaxError {
        assure(isOneOf(terminals));
        scanner.next();
    }

    static Summary consume(Production... productions) throws SyntaxError {
        for (Production production : productions) {
            if (fulfill(production)) {
                return production.consume();
            }
        }
        error();
        return null;
    }

    static void error() throws SyntaxError {
        throw new SyntaxError();
    }

    static private Predicate<Terminal> is(Terminal terminal) {
        return (t) -> t == terminal;
    }

    static private Predicate<Terminal> isOneOf(Terminal... terminals) {
        ImmutableSet<Terminal> terminalSet = ImmutableSet.copyOf(terminals);
        return terminalSet::contains;
    }

    static private Predicate<Terminal> isOneOf(Collection<Terminal> terminals) {
        ImmutableSet<Terminal> terminalSet = ImmutableSet.copyOf(terminals);
        return terminalSet::contains;
    }


    // todo identifier and number should be terminal.
    final static Production IDENTIFIER = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::identifierRule);

    static private Summary identifierRule() throws SyntaxError {
        // todo identifier summary
        assure(Terminal.IDENTIFIER);
        Summary summary = new Summary(scanner.peek().value);

        scanner.next();

        return summary;
    }

    final static Production NUMBER = new Production(ImmutableSet.of(Terminal.NUMBER), Grammar::numberRule);

    static private Summary numberRule() throws SyntaxError {
        assure(Terminal.NUMBER);
        Integer value = scanner.peek().value;
        scanner.next();
        return new Summary(value);
    }

    final static Production DESIGNATOR = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::designatorRule);

    static private Summary designatorRule() throws SyntaxError {
        Summary summary = consume(IDENTIFIER);

        while (fulfill(Terminal.LEFT_BRACKET)) {
            scanner.next();
            consume(EXPRESSSION);
            consume(Terminal.RIGHT_BRACKET);
        }

        return summary;
    }

    final static Production FACTOR = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::factorRule);

    static private Summary factorRule() throws SyntaxError {
        Summary summary = null;

        if (fulfill(Terminal.LEFT_PAREN)) {
            scanner.next();
            summary = consume(EXPRESSSION);
            consume(Terminal.RIGHT_PAREN);
        } else {
            summary = consume(DESIGNATOR, NUMBER, FUNCTION_CALL);
        }

        return summary;
    }

    final static Production TERM = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::termRule);

    static private Summary termRule() throws SyntaxError {
        Summary summary = consume(FACTOR);
        Summary operand = null;

        while (fulfill(Terminal.MULTIPLY, Terminal.DIVIDE)) {
            scanner.next();
            // todo summary
            operand = consume(FACTOR);
        }

        return summary;
    }

    final static Production EXPRESSSION = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::expressionRule);

    static private Summary expressionRule() throws SyntaxError {
        Summary summary = consume(TERM);
        Summary operand = null;
        while (fulfill(Terminal.PLUS, Terminal.MINUS)) {
            scanner.next();
            // TODO summary
            operand = consume(TERM);
        }

        return summary;
    }

    final static Production RELATION = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::relationRule);

    static private Summary relationRule() throws SyntaxError {
        Summary summary = EXPRESSSION.consume();
        if (fulfill(Terminal::isRelOp)) {
            Terminal relOp = scanner.peek().terminal;
            scanner.next();

            Summary operand2 = EXPRESSSION.consume();

            // todo summary.
            switch (relOp) {
                case EQUAL:
                    break;
                case NOT_EQUAL:
                    break;
                case LESS_THAN:
                    break;
                case NO_GREATER_THAN:
                    break;
                case GREATER_THAN:
                    break;
                case NO_LESS_THAN:
                    break;
                default:
                    error();

            }
        } else {
            error();
        }

        return summary;
    }

    final static Production ASSIGNMENT = new Production(ImmutableSet.of(Terminal.LET), Grammar::assignmentRule);

    static private Summary assignmentRule() throws SyntaxError {
        Summary summary = null;

        consume(Terminal.LET);
        consume(DESIGNATOR);
        consume(Terminal.ASSIGNMENT);
        consume(EXPRESSSION);

        return summary;
    }

    final static Production FUNCTION_CALL = new Production(ImmutableSet.of(Terminal.CALL), Grammar::functionCallRule);

    static private Summary functionCallRule() throws SyntaxError {
        Summary summary = null;
        consume(Terminal.CALL);
        consume(IDENTIFIER);

        if (fulfill(Terminal.LEFT_PAREN)) {
            scanner.next();
            if (fulfill(EXPRESSSION)) {
                EXPRESSSION.consume();
                while (fulfill(Terminal.COMMA)) {
                    scanner.next();
                    consume(EXPRESSSION);
                }
            }
            consume(Terminal.RIGHT_PAREN);
        }

        return summary;
    }

    final static Production IF_STATEMENT = new Production(ImmutableSet.of(Terminal.IF), Grammar::ifStatementRule);

    static private Summary ifStatementRule() throws SyntaxError {
        Summary summary = null;

        consume(Terminal.IF);
        consume(RELATION);

        consume(Terminal.THEN);

        consume(STATEMENT_SEQUENCE);

        if (fulfill(Terminal.ELSE)) {
            scanner.next();
            consume(STATEMENT_SEQUENCE);
        }

        consume(Terminal.FI);

        return summary;
    }

    final static Production WHILE_STATEMENT = new Production(ImmutableSet.of(Terminal.WHILE), Grammar::whileStatementRule);

    static private Summary whileStatementRule() throws SyntaxError {
        Summary summary = null;

        consume(Terminal.WHILE);
        consume(RELATION);
        consume(Terminal.DO);
        consume(STATEMENT_SEQUENCE);
        consume(Terminal.OD);

        return summary;
    }

    final static Production RETURN_STATEMENT = new Production(ImmutableSet.of(Terminal.RETURN), Grammar::returnStatementRule);

    static private Summary returnStatementRule() throws SyntaxError {
        Summary summary = null;

        consume(Terminal.RETURN);

        if (fulfill(EXPRESSSION)) {
            consume(EXPRESSSION);
        }

        return summary;
    }

    final static Production STATEMENT = new Production(
            ImmutableSet.of(Terminal.LET, Terminal.CALL, Terminal.IF, Terminal.WHILE, Terminal.RETURN),
            Grammar::statementRule);

    static private Summary statementRule() throws SyntaxError {
        Summary summary = null;
        consume(ASSIGNMENT, FUNCTION_CALL, IF_STATEMENT, WHILE_STATEMENT, RETURN_STATEMENT);
        return summary;
    }

    final static Production STATEMENT_SEQUENCE = new Production(
            ImmutableSet.of(Terminal.LET, Terminal.CALL, Terminal.IF, Terminal.WHILE, Terminal.RETURN),
            Grammar::statementSequenceRule);

    static private Summary statementSequenceRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(STATEMENT);

        while (fulfill(Terminal.SEMICOLON)) {
            scanner.next();
            consume(STATEMENT);
        }

        return summary;
    }

    final static Production TYPE_DECLARATION = new Production(
            ImmutableSet.of(Terminal.VAR, Terminal.ARRAY), Grammar::typeDeclarationRule);

    static private Summary typeDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(Terminal.VAR, Terminal.ARRAY);
        consume(Terminal.LEFT_BRACKET);
        consume(NUMBER);
        consume(Terminal.RIGHT_BRACKET);

        while (fulfill(Terminal.LEFT_BRACKET)) {
            scanner.next();

            NUMBER.consume();

            consume(Terminal.RIGHT_BRACKET);
        }

        return summary;
    }

    final static Production VARIABLE_DECLARATION = new Production(
            ImmutableSet.of(Terminal.VAR, Terminal.ARRAY), Grammar::variableDeclarationRule);

    static private Summary variableDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(TYPE_DECLARATION);
        consume(IDENTIFIER);

        while (fulfill(Terminal.COMMA)) {
            scanner.next();
            consume(IDENTIFIER);
        }

        consume(Terminal.SEMICOLON);

        return summary;
    }

    final static Production FUNCTION_DECLARATION = new Production(
            ImmutableSet.of(Terminal.FUNCTION, Terminal.PROCEDURE), Grammar::functionDeclarationRule);

    static private Summary functionDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(Terminal.FUNCTION, Terminal.PROCEDURE);

        consume(IDENTIFIER);

        if (fulfill(FORMAL_PARAMETER)) {
            consume(FORMAL_PARAMETER);
        }

        consume(Terminal.SEMICOLON);
        consume(FUNCTION_BODY);
        consume(Terminal.SEMICOLON);

        return summary;
    }

    final static Production FORMAL_PARAMETER = new Production(
            ImmutableSet.of(Terminal.LEFT_PAREN), Grammar::formalParameterRule);

    static private Summary formalParameterRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(Terminal.LEFT_PAREN);

        if (fulfill(IDENTIFIER)) {
            consume(IDENTIFIER);
            while (fulfill(Terminal.COMMA)) {
                scanner.next();
                consume(IDENTIFIER);
            }
        }

        consume(Terminal.RIGHT_PAREN);
        return summary;
    }

    final static Production FUNCTION_BODY = new Production(
            ImmutableSet.of(Terminal.VAR, Terminal.ARRAY, Terminal.LEFT_BRACE), Grammar::functionBodyRule);

    static private Summary functionBodyRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        while (fulfill(VARIABLE_DECLARATION)) {
            consume(VARIABLE_DECLARATION);
        }

        consume(Terminal.LEFT_BRACE);

        if (fulfill(STATEMENT_SEQUENCE)) {
            consume(STATEMENT_SEQUENCE);
        }

        consume(Terminal.RIGHT_BRACE);

        return summary;
    }

    final static Production COMPUTATION = new Production(ImmutableSet.of(Terminal.MAIN), Grammar::computationRule);

    static private Summary computationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(Terminal.MAIN);

        while (fulfill(VARIABLE_DECLARATION)) {
            consume(VARIABLE_DECLARATION);
        }

        while (fulfill(FUNCTION_DECLARATION)) {
            consume(FUNCTION_DECLARATION);
        }

        consume(Terminal.LEFT_BRACE);

        consume(STATEMENT_SEQUENCE);

        consume(Terminal.RIGHT_BRACE);

        consume(Terminal.DOT);

        return summary;
    }

}

public class Parser {

    private Scanner scanner;

    public Parser(Scanner scanner) {
        this.scanner = scanner;
    }

}

