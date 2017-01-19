package edu.zhangfan.compiler.frontend;

import com.google.common.collect.ImmutableSet;
import edu.zhangfan.compiler.Utils;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;


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

    static Summary consume(Production p) throws SyntaxError {
        return p.consume();
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

    static void error() throws SyntaxError {
        throw new SyntaxError();
    }

    // todo identifier and number should be terminal.
    final static Production IDENTIFIER = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::identifierRule);

    static private Summary identifierRule() throws SyntaxError {
        // todo identifier summary
        assure(is(Terminal.IDENTIFIER));
        Summary summary = new Summary(scanner.peek().value);

        scanner.next();

        return summary;
    }

    final static Production NUMBER = new Production(ImmutableSet.of(Terminal.NUMBER), Grammar::numberRule);

    static private Summary numberRule() throws SyntaxError {
        assure(is(Terminal.NUMBER));
        Integer value = scanner.peek().value;
        scanner.next();
        return new Summary(value);
    }

    final static Production DESIGNATOR = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::designatorRule);

    static private Summary designatorRule() throws SyntaxError {
        if (scanner.peek().terminal == Terminal.IDENTIFIER) {
            while (scanner.peek().terminal == Terminal.LEFT_BRACKET) {
                scanner.next();
                EXPRESSSION.consume();
                if (scanner.peek().terminal == Terminal.LEFT_BRACKET) {
                    scanner.next();
                } else {
                    error();
                }
            }
        } else {
            error();
        }
        return null;
    }

    final static Production FACTOR = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::factorRule);

    static private Summary factorRule() throws SyntaxError {
        Summary summary = null;

        switch (scanner.peek().terminal) {
            case IDENTIFIER:
                summary = DESIGNATOR.consume();
                break;

            case NUMBER:
                summary = NUMBER.consume();
                break;

            case LEFT_PAREN:
                scanner.next();
                summary = EXPRESSSION.consume();
                if (fulfill(is(Terminal.RIGHT_PAREN))) {
                    scanner.next();
                } else {
                    error();
                }
                break;

            case CALL:
                summary = FUNCTION_CALL.consume();
                break;

            default:
                error();
        }

        return summary;
    }

    final static Production TERM = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::termRule);

    static private Summary termRule() throws SyntaxError {
        Summary summary = FACTOR.consume();

        while (fulfill(isOneOf(Terminal.MULTIPLY, Terminal.DIVIDE))) {
            scanner.next();
            // TODO
            FACTOR.consume();
        }

        return summary;
    }

    final static Production EXPRESSSION = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::expressionRule);

    static private Summary expressionRule() throws SyntaxError {
        Summary summary = TERM.consume();
        while (fulfill(isOneOf(Terminal.PLUS, Terminal.MINUS))) {
            scanner.next();
            // TODO
            TERM.consume();
        }

        return summary;
    }

    final static Production RELATION = new Production(
            ImmutableSet.of(Terminal.IDENTIFIER, Terminal.NUMBER, Terminal.LEFT_PAREN, Terminal.CALL),
            Grammar::relationRule);

    static private Summary relationRule() throws SyntaxError {
        Summary summary = EXPRESSSION.consume();
        if (Terminal.isRelOp(scanner.peek().terminal)) {
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

        if (scanner.peek().terminal == Terminal.LET) {
            scanner.next();
            DESIGNATOR.consume();
            if (fulfill(is(Terminal.ASSIGNMENT))) {
                scanner.next();
                EXPRESSSION.consume();
            } else {
                error();
            }
        }

        return summary;
    }

    final static Production FUNCTION_CALL = new Production(ImmutableSet.of(Terminal.CALL), Grammar::functionCallRule);

    static private Summary functionCallRule() throws SyntaxError {
        Summary summary = null;
        assure(is(Terminal.CALL));
        scanner.next();
        summary = IDENTIFIER.consume();

        if (fulfill(is(Terminal.LEFT_PAREN))) {
            scanner.next();
            if (fulfill(isOneOf(EXPRESSSION.first))) {
                EXPRESSSION.consume();
                while (fulfill(is(Terminal.COMMA))) {
                    scanner.next();
                    EXPRESSSION.consume();
                }
            }
            assure(is(Terminal.RIGHT_PAREN));
            scanner.next();
        }

        return summary;
    }

    final static Production IF_STATEMENT = new Production(ImmutableSet.of(Terminal.IF), Grammar::ifStatementRule);

    static private Summary ifStatementRule() throws SyntaxError {
        Summary summary = null;

        assure(is(Terminal.IF));
        scanner.next();

        RELATION.consume();

        assure(is(Terminal.THEN));
        scanner.next();

        statementSequence.consume();

        if (fulfill(is(Terminal.ELSE))) {
            scanner.next();
            statementSequence.consume();
        }

        assure(is(Terminal.FI));
        scanner.next();

        return summary;
    }

    final static Production whileStatement = new Production(ImmutableSet.of(Terminal.WHILE), Grammar::whileStatementRule);

    static private Summary whileStatementRule() throws SyntaxError {
        Summary summary = null;

        assure(is(Terminal.WHILE));
        scanner.next();

        RELATION.consume();

        assure(is(Terminal.DO));
        scanner.next();

        statementSequence.consume();

        assure(is(Terminal.OD));
        scanner.next();

        return summary;
    }

    final static Production returnStatement = new Production(ImmutableSet.of(Terminal.RETURN), Grammar::returnStatementRule);

    static private Summary returnStatementRule() throws SyntaxError {
        Summary summary = null;

        assure(is(Terminal.RETURN));

        if (fulfill(isOneOf(EXPRESSSION.first))) {
            EXPRESSSION.consume();
        }

        return summary;
    }

    final static Production COMPUTATION = new Production(ImmutableSet.of(Terminal.MAIN), Grammar::computationRule);

    final static Production statement = new Production(
            ImmutableSet.of(Terminal.LET, Terminal.CALL, Terminal.IF, Terminal.WHILE, Terminal.RETURN),
            Grammar::statementRule);

    static private Summary statementRule() throws SyntaxError {
        Summary summary = null;

        for (Production p : Arrays.asList(ASSIGNMENT, FUNCTION_CALL, IF_STATEMENT, whileStatement, returnStatement)) {
            if (fulfill(isOneOf(p.first))) {
                summary = p.consume();
                return summary;
            }
        }

        error();
        return summary;
    }

    final static Production statementSequence = new Production(
            ImmutableSet.of(Terminal.LET, Terminal.CALL, Terminal.IF, Terminal.WHILE, Terminal.RETURN),
            Grammar::statementSequenceRule);

    static private Summary statementSequenceRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        statement.consume();

        while (fulfill(is(Terminal.SEMICOLON))) {
            statement.consume();
        }

        return summary;
    }

    final static Production typeDeclaration = new Production(
            ImmutableSet.of(Terminal.VAR, Terminal.ARRAY), Grammar::typeDeclarationRule);

    static private Summary typeDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        assure(isOneOf(typeDeclaration.first));
        scanner.next();

        assure(is(Terminal.LEFT_BRACKET));
        scanner.next();

        NUMBER.consume();

        assure(is(Terminal.RIGHT_BRACKET));
        scanner.next();

        while (fulfill(is(Terminal.LEFT_BRACKET))) {
            scanner.next();

            NUMBER.consume();

            assure(is(Terminal.RIGHT_BRACKET));
            scanner.next();
        }

        return summary;
    }

    final static Production variableDeclaration = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::variableDeclarationRule);

    static private Summary variableDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        typeDeclaration.consume();

        IDENTIFIER.consume();

        while (fulfill(is(Terminal.COMMA))) {
            scanner.next();
            IDENTIFIER.consume();
        }

        assure(is(Terminal.SEMICOLON));
        scanner.next();

        return summary;
    }

    final static Production functionDeclaration = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::functionDeclarationRule);

    static private Summary functionDeclarationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        assure(isOneOf(Terminal.FUNCTION, Terminal.PROCEDURE));
        scanner.next();

        IDENTIFIER.consume();

        if (fulfill(formalParameter)) {
            formalParameter.consume();
        }

        assure(Terminal.SEMICOLON);
        scanner.next();

        functionBody.consume();

        assure(Terminal.SEMICOLON);
        scanner.next();

        return summary;
    }

    final static Production formalParameter = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::formalParameterRule);

    static private Summary formalParameterRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        assure(Terminal.LEFT_PAREN);
        scanner.next();

        if (fulfill(IDENTIFIER)) {
            IDENTIFIER.consume();
            while (fulfill(Terminal.COMMA)) {
                scanner.next();
                IDENTIFIER.consume();
            }
        }

        assure(Terminal.RIGHT_PAREN);
        scanner.next();

        return summary;
    }

    final static Production functionBody = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::functionBodyRule);

    static private Summary functionBodyRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        while (fulfill(variableDeclaration)) {
            variableDeclaration.consume();
        }

        assure(Terminal.LEFT_BRACE);
        scanner.next();

        if (fulfill(statementSequence)) {
            statementSequence.consume();
        }

        consume(Terminal.RIGHT_BRACE);

        return summary;
    }

    final static Production computation = new Production(ImmutableSet.of(Terminal.IDENTIFIER), Grammar::computationRule);

    static private Summary computationRule() throws SyntaxError {
        // todo summary
        Summary summary = null;

        consume(Terminal.MAIN);

        while (fulfill(variableDeclaration)) {
            consume(variableDeclaration);
        }

        while (fulfill(functionDeclaration)) {
            consume(functionDeclaration);
        }

        consume(Terminal.LEFT_BRACE);

        consume(statementSequence);

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

