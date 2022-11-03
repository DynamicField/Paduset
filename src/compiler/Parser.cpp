//
// Created by jeuxj on 29/10/2022.
//

#include "Parser.h"
#include <deque>
#include <cassert>
#include <memory>

using namespace Paduset;

DiagnosedNode<ProgramDeclarationNode> Parser::ReadProgramDeclaration() {
    NextRequiredToken(TokenKind::KW_Program, Delimiter::NodeBegin);

    std::string name;
    if (const Token& nameToken = NextToken(TokenKind::I_Identifier)) {
        name = *nameToken.stringContent;
    }

    std::vector<VariableDeclarationNode> variables;
    if (NextToken(TokenKind::KW_Variables)) {
        while (std::optional<VariableDeclarationNode> declaration = AcceptVariableDeclaration()) {
            variables.push_back(declaration.value());
        }
    }

    NextRequiredToken(TokenKind::KW_Begin);

    std::vector<std::unique_ptr<StatementNode>> statements = ReadStatementList(
            [](const Token& t) { return t.kind == TokenKind::KW_End; });

    NextRequiredToken(TokenKind::KW_End);

    auto programDeclaration = NewStackedNode<ProgramDeclarationNode>(name, variables, std::move(statements));
    auto result = DiagnosedNode(std::move(programDeclaration), std::move(diagnostics));
    diagnostics = std::make_unique<DiagnosticSet>();

    return result;
}

std::vector<std::unique_ptr<StatementNode>>
Parser::ReadStatementList(const std::function<bool(const Token&)>& isTokenEnd) {
    std::vector<std::unique_ptr<StatementNode>> statements;
    while (!isTokenEnd(PeekToken()) &&
           PeekToken().kind != TokenKind::I_Garbage &&
           PeekToken().kind != TokenKind::Empty) {
        if (auto&& statement = AcceptStatement()) {
            statements.push_back(std::move(statement));
        } else {
            const Token& token = NextToken();

            Diagnostic diag{DiagnosticSeverity::Error,
                            "Texte inattendu dans la liste d'instructions.",
                            TokenSpan{token}};
            diagnostics->AddDiagnostic(diag);
        }
    }
    return statements;
}

std::optional<VariableDeclarationNode> Parser::AcceptVariableDeclaration() {
    if (const Token& firstVarName = NextToken(TokenKind::I_Identifier, Delimiter::NodeBegin)) {
        std::vector<std::string> names{*firstVarName.stringContent};
        while (NextToken(TokenKind::S_Comma)) {
            if (const Token& otherName = NextRequiredToken(TokenKind::I_Identifier)) {
                names.push_back(*otherName.stringContent);
            }
        }

        NextRequiredToken(TokenKind::OP_Declare);

        const Token& typeToken = NextToken();

        std::optional<ValueType> type = TokenToType(typeToken);
        if (!type.has_value()) {
            for (auto& name : names) {
                Diagnostic diag{DiagnosticSeverity::Error,
                                "Type invalide pour la variable '" + name + "'.",
                                TokenSpan{typeToken}};
                diagnostics->AddDiagnostic(diag);
            }

            type = ValueType::Error;
        }

        return NewStackedNode<VariableDeclarationNode>(std::move(names), type.value());
    } else {
        return {};
    }
}

const Token& Parser::NextToken(Delimiter delimiter) {
    if (tokenIterator == tokens.end()) {
        return EmptyToken;
    }

    // First get the token from the iterator, *then* advance.
    // So we don't ignore the first token... And get a fine currentToken.
    do {
        auto& token = *tokenIterator;
        currentToken = token;

        if (token.kind != TokenKind::I_Garbage) {
            Delimit(token, delimiter);
            tokenIterator++;
            return token;
        } else {
            tokenIterator++;
        }
    } while (true);
}

const Token& Parser::NextToken(TokenKind kind, Delimiter delimiter) {
    auto& token = PeekToken(kind);
    if (token) {
        return NextToken(delimiter);
    } else {
        return EmptyToken;
    }
}

const Token& Parser::NextRequiredToken(TokenKind kind, Delimiter delimiter) {
    auto& token = NextToken(delimiter);
    if (token.kind != kind) {
        std::string errorMessage = std::string("Texte de type '") + TokenName(kind) +
                                   "' attendu avant '" + TokenName(token.kind) + "'.";
        CharSpan preTokenCursor{token.span.startChar, token.span.startChar}; // 1-char cursor
        Diagnostic diag{DiagnosticSeverity::Error, errorMessage, TokenSpan{token}, preTokenCursor};
        diagnostics->AddDiagnostic(diag);

        // What do we do now? Let's assume that the user has put a valid token.
        madeUpTokens.push_back(token);
        Token& assumption = madeUpTokens.back();
        assumption.span = preTokenCursor;
        assumption.kind = kind;
        return assumption;
    }
    return token;
}

const Token& Parser::PeekToken(int offset) {
    auto newIterator = tokenIterator;
    for (int i = 0; i < offset - 1; ++i) {
        newIterator++;
        if (newIterator->kind == TokenKind::I_Garbage) {
            i--;
        }
    }
    if (newIterator == tokens.end()) {
        return EmptyToken;
    }
    return *newIterator;
}

const Token& Parser::PeekToken(TokenKind kind, int offset) {
    auto& token = PeekToken(offset);
    if (token.kind == kind) {
        return token;
    } else {
        return EmptyToken;
    }
}

std::optional<ValueType> Parser::TokenToType(const Token& token) {
    switch (token.kind) {
        case TokenKind::KW_Integer:
            return ValueType::Integer;
        case TokenKind::KW_Real:
            return ValueType::Real;
        case TokenKind::KW_Boolean:
            return ValueType::Boolean;
        case TokenKind::KW_String:
            return ValueType::String;
        default:
            return {};
    }
}

std::unique_ptr<ExpressionNode> Parser::AcceptExpression() {
    // Shunting-yard algorithm (but a bit different)
    // TODO: Handle order correctly, for example
    /*
        2*3+1
        2 3 * 1 +
        -------
        1+2*3
        1 2 3 * +
     */
    // both resolve to 2*3+1
    // seems to work quite good now no?
    std::deque<Token> operandStack;
    std::vector<std::tuple<Token, std::unique_ptr<ExpressionNode>>> reverseOutput;
    std::unique_ptr<ExpressionNode> finalExpression;
    int termsInLine = 0;
    int operandsInLine = 0;
    int parenthesesPending = 0;

    const auto nextTerm = [&]() -> std::unique_ptr<ExpressionNode> {
        if (const Token& integer = NextToken(TokenKind::L_Integer, Delimiter::SingleTokenNode)) {
            return NewStackedNodePtr<NumberLiteralExpressionNode>(integer.content.intValue);
        } else if (const Token& real = NextToken(TokenKind::L_Real, Delimiter::SingleTokenNode)) {
            return NewStackedNodePtr<NumberLiteralExpressionNode>(real.content.floatValue);
        } else if (const Token& variable = NextToken(TokenKind::I_Identifier, Delimiter::SingleTokenNode)) {
            return NewStackedNodePtr<VariableExpressionNode>(*variable.stringContent);
        } else if (const Token& string = NextToken(TokenKind::L_String, Delimiter::SingleTokenNode)) {
            return NewStackedNodePtr<StringLiteralExpressionNode>(*string.stringContent);
        } else {
            return nullptr;
        }
    };

    const auto nextOperand = [&]() -> const Token& {
        const Token& token = NextToken(TokenKind::OP_Add) ||
               NextToken(TokenKind::OP_Subtract) ||
               NextToken(TokenKind::OP_Multiply) ||
               NextToken(TokenKind::OP_Divide) ||
               NextToken(TokenKind::OP_IntegerDivide) ||
               NextToken(TokenKind::OP_Modulo) ||
               NextToken(TokenKind::OP_Equals) ||
               NextToken(TokenKind::OP_NotEquals) ||
               NextToken(TokenKind::OP_GreaterThan) ||
               NextToken(TokenKind::OP_GreaterOrEqualThan) ||
               NextToken(TokenKind::OP_LowerThan) ||
               NextToken(TokenKind::OP_LowerOrEqualThan) ||
               NextToken(TokenKind::S_LeftParens);

        if (token) {
            return token;
        } else if (parenthesesPending > 0) {
            return NextToken(TokenKind::S_RightParens);
        } else {
            return EmptyToken;
        }
    };

    bool awkwardPause = false;
    const auto extractExpression = [&]() {
        // Example:
        //    8 7 2 - -         left: 7 ; right: 2
        // -> 8 (5) -           left: 8 ; right: 5 (finalExpression)
        // -> 3                 done!

        std::unique_ptr<ExpressionNode> left;
        std::unique_ptr<ExpressionNode>& right = finalExpression;
        Token infix;

        if (!right && termsInLine >= 2 && operandsInLine >= 1) {
            infix = std::get<0>(reverseOutput.back());
            reverseOutput.pop_back();
            right = std::move(std::get<1>(reverseOutput.back()));
            reverseOutput.pop_back();
            left = std::move(std::get<1>(reverseOutput.back()));
            reverseOutput.pop_back();

            assert(infix);
            assert(right);
            assert(left);

            termsInLine -= 2;
            operandsInLine -= 1;
        } else if (termsInLine >= 1 && operandsInLine >= 1) {
            infix = std::get<0>(reverseOutput.back());
            reverseOutput.pop_back();
            left = std::move(std::get<1>(reverseOutput.back()));
            reverseOutput.pop_back();

            assert(infix);
            assert(right);

            termsInLine -= 1;
            operandsInLine -= 1;
        } else {
            return;
        }

        std::unique_ptr<ExpressionNode>& lhs = awkwardPause ? right : left;
        std::unique_ptr<ExpressionNode>& rhs = awkwardPause ? left : right;
        awkwardPause = false;

        TokenSpan span = lhs->FindMeta<TokenNodeMeta>()->Span()
                .CombineWith(rhs->FindMeta<TokenNodeMeta>()->Span());

        switch (infix.kind) {
            case TokenKind::OP_Add:
                finalExpression = NewNodePtr<AddExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_Subtract:
                finalExpression = NewNodePtr<SubtractExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_Multiply:
                finalExpression = NewNodePtr<MultiplyExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_Divide:
                finalExpression = NewNodePtr<DivideExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_Equals:
                finalExpression = NewNodePtr<EqualExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_NotEquals:
                finalExpression = NewNodePtr<NotEqualExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_GreaterThan:
                finalExpression = NewNodePtr<GreaterExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_GreaterOrEqualThan:
                finalExpression = NewNodePtr<GreaterOrEqualExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_LowerThan:
                finalExpression = NewNodePtr<LowerExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            case TokenKind::OP_LowerOrEqualThan:
                finalExpression = NewNodePtr<LowerOrEqualExpressionNode>(span, std::move(lhs), std::move(rhs));
                break;
            default:
                throw std::runtime_error("Unknown token type in expression parsing.");
        }
    };

    const auto popOperand = [&]() {
        reverseOutput.emplace_back(operandStack.front(), nullptr);
        operandStack.pop_front();
        operandsInLine++;

        extractExpression();
    };

    const auto addNextOperand = [&]() -> bool {
        retryAfterParenthesis:
        if (const Token& nextOp = nextOperand()) {
            if (nextOp.kind == TokenKind::S_LeftParens) {
                parenthesesPending++;
                operandStack.push_front(nextOp);
            } else if (nextOp.kind == TokenKind::S_RightParens) {
                const Token& rightParens = nextOp;

                // Empty out everything before the right parenthesis, before
                // other operands do the same!
                while (!operandStack.empty() && operandStack.front().kind != TokenKind::S_LeftParens) {
                    popOperand();
                }
                if (!operandStack.empty()) {
                    const Token& leftParens = operandStack.front();
                    TokenSpan span = TokenSpan(leftParens, rightParens);
                    // fixme: this doesn't when we have a single expression with parentheses like (5)
                    finalExpression = NewNodePtr<ParenthesesExpressionNode>(span, std::move(finalExpression));

                    operandStack.pop_front(); // Bye left parenthesis
                    parenthesesPending--;

                    // I know. This is a crime. But a whole do block would be overkill.
                    goto retryAfterParenthesis;
                } else {
                    Diagnostic diag{DiagnosticSeverity::Error,
                                    "Parenthèse manquante",
                                    TokenSpan(rightParens)};
                    diagnostics->AddDiagnostic(diag);
                }
            } else {
                // Let other "more-important" operands do their job until:
                // - We hit the "left-parenthesis wall"
                // - We have higher or equal precedence than them
                while (!operandStack.empty() && operandStack.front().kind != TokenKind::S_LeftParens && (
                        TokenToPrecedence(operandStack.front()) > TokenToPrecedence(nextOp) ||
                        TokenToPrecedence(operandStack.front()) == TokenToPrecedence(nextOp))) { // and left associative
                    popOperand();
                }
                // happens when we have 2 3 * | 1 +
                //                        ----^ here!
                // So we swap the lhs and rhs when this happens, so the order
                // doesn't get ruined.
                if (termsInLine == 0 && operandsInLine == 0) {
                    awkwardPause = true;
                }
                operandStack.push_front(nextOp);
            }
            return true;
        } else {
            return false;
        }
    };

    do {
        if (auto term = nextTerm()) {
            reverseOutput.emplace_back(EmptyToken, std::move(term));
            termsInLine++;
        } else if (operandStack.empty() && !PeekToken(TokenKind::S_LeftParens)) {
            // We have nothing at all, nothing to see here.
            return nullptr;
        } else if (!operandStack.empty()) {
            // Else... Operand with no other term?? sus
            Diagnostic diag{DiagnosticSeverity::Error,
                            "L'opérateur n'a pas de valeur à droite.",
                            TokenSpan{operandStack.front()}};
            diagnostics->AddDiagnostic(diag);

            // What do we do now? Well, just assume that this operator never existed.
            // Like people who denied covid-19 essentially.
            operandStack.pop_front();
        }
    } while (addNextOperand());

    // Empty out any left operands
    while (!operandStack.empty()) {
        popOperand();
    }

    // Single value
    if (finalExpression == nullptr && termsInLine == 1) {
        finalExpression = std::move(std::get<1>(reverseOutput[0]));
    }

    return finalExpression;
}

std::unique_ptr<ExpressionNode> Parser::AcceptExpressionOrError() {
    if (auto expression = AcceptExpression()) {
        return expression;
    } else {
        TokenSpan span{currentToken};
        Diagnostic diag{DiagnosticSeverity::Error, "Une valeur est attendue ici.", span};
        diagnostics->AddDiagnostic(diag);

        // What do? Just invent an error expression.
        TokenSpan imaginarySpan{currentToken};
        return NewNodePtr<ErrorExpressionNode>(imaginarySpan, diag);
    }
}


std::unique_ptr<StatementNode> Parser::AcceptStatement() {
    if (auto assign = AcceptAssignStatement()) {
        return assign;
    } else if (auto write = AcceptWriteStatement()) {
        return write;
    } else if (auto read = AcceptReadStatement()) {
        return read;
    } else if (auto iff = AcceptIfStatement()) {
        return iff;
    } else {
        return nullptr;
    }
}

std::unique_ptr<AssignStatementNode> Parser::AcceptAssignStatement() {
    if (auto [id, assign, s] = NextTokens<Delimiter::NodeBegin>(TokenKind::I_Identifier, TokenKind::OP_Assign); s) {
        auto variableExpr = NewNode<VariableExpressionNode>(TokenSpan{id}, *id.stringContent);
        auto expression = AcceptExpressionOrError();

        return NewStackedNodePtr<AssignStatementNode>(variableExpr, std::move(expression));
    }

    return nullptr;
}

std::unique_ptr<WriteStatementNode> Parser::AcceptWriteStatement() {
    if (auto writeToken = NextToken(TokenKind::KW_Write, Delimiter::NodeBegin)) {
        NextRequiredToken(TokenKind::S_LeftParens);
        auto expression = AcceptExpressionOrError();
        NextRequiredToken(TokenKind::S_RightParens);

        return NewStackedNodePtr<WriteStatementNode>(std::move(expression));
    }

    return nullptr;
}

std::unique_ptr<ReadStatementNode> Parser::AcceptReadStatement() {
    if (auto writeToken = NextToken(TokenKind::KW_Read, Delimiter::NodeBegin)) {
        NextRequiredToken(TokenKind::S_LeftParens);
        auto expression = AcceptExpressionOrError();
        NextRequiredToken(TokenKind::S_RightParens);

        return NewStackedNodePtr<ReadStatementNode>(std::move(expression));
    }

    return nullptr;
}

std::unique_ptr<IfStatementNode> Parser::AcceptIfStatement() {
    auto ifToken = NextToken(TokenKind::KW_If, Delimiter::NodeBegin);

    if (!ifToken) {
        return nullptr;
    }

    std::unique_ptr<ExpressionNode> ifCondition;
    std::vector<std::unique_ptr<StatementNode>> ifStatements;
    std::vector<IfStatementNode::ElseIfBlock> elseIfBlocks;
    std::vector<std::unique_ptr<StatementNode>> elseStatements;

    // If, Else or End
    Token& condToken = ifToken;
    while (condToken.kind == TokenKind::KW_If || condToken.kind == TokenKind::KW_Else) {
        std::unique_ptr<ExpressionNode> condition;
        bool isIf = condToken.kind == TokenKind::KW_If;
        bool isElseIf = condToken.kind == TokenKind::KW_Else && NextToken(TokenKind::KW_If);
        bool hasCondition = isIf || isElseIf;

        if (hasCondition) {
            NextRequiredToken(TokenKind::S_LeftParens);
            condition = AcceptExpressionOrError();
            NextRequiredToken(TokenKind::S_RightParens);

            NextRequiredToken(TokenKind::KW_Then);
        }

        if (isIf) {
            ifCondition = std::move(condition);
            ifStatements = ReadStatementList([](const Token& token) {
                return token.kind == TokenKind::KW_Else || token.kind == TokenKind::KW_End;
            });
        } else if (isElseIf) {
            auto elseIfStatements = ReadStatementList([](const Token& token) {
                return token.kind == TokenKind::KW_Else || token.kind == TokenKind::KW_End;
            });
            elseIfBlocks.emplace_back(std::move(condition), std::move(elseIfStatements));
        } else { // (isElse)
            // Maybe that we should do more advanced parsing and "allow"
            // multiple ELSE blocks with an error instead.
            elseStatements = ReadStatementList([](const Token& token) {
                return token.kind == TokenKind::KW_End;
            });
        }

        condToken = NextToken();
    }

    if (currentToken.kind == TokenKind::KW_End) {
        // END IF <-> FIN SI
        NextRequiredToken(TokenKind::KW_If);
    }

    return NewStackedNodePtr<IfStatementNode>(
            std::move(ifCondition),
            std::move(ifStatements),
            std::move(elseIfBlocks),
            std::move(elseStatements)
    );

}

const Token& Parser::Rewind(int offset) {
    for (int i = 0; i < offset; ++i) {
        tokenIterator--;
        if (tokenIterator->kind == TokenKind::I_Garbage) {
            i--;
        }
    }
    auto& token = *tokenIterator;
    currentToken = token;
    return token;
}

OperatorPrecedence Parser::TokenToPrecedence(const Token& token) {
    switch (token.kind) {
        case TokenKind::OP_Add:
        case TokenKind::OP_Subtract:
        case TokenKind::OP_Equals:
        case TokenKind::OP_NotEquals:
        case TokenKind::OP_LowerThan:
        case TokenKind::OP_LowerOrEqualThan:
        case TokenKind::OP_GreaterThan:
        case TokenKind::OP_GreaterOrEqualThan:
            return OperatorPrecedence::Linear;
        case TokenKind::OP_Multiply:
        case TokenKind::OP_Divide:
        case TokenKind::OP_IntegerDivide:
            return OperatorPrecedence::Multiplicative;
        case TokenKind::OP_Modulo:
            return OperatorPrecedence::UnderLinear;
        default:
            throw std::runtime_error("No precedence for token.");
    }
}

void Parser::NodeBegin(const Token& token) {
    nodeSpanStack.push_front(TokenSpan{token, EmptyToken});
}

TokenSpan Parser::FinalizeNodeSpan() {
    auto& span = nodeSpanStack.front();
    if (!span.end) {
        span.end = currentToken;
    }
    nodeSpanStack.pop_front();
    return span;
}

void Parser::Delimit(const Token& token, Parser::Delimiter delimiter) {
    switch (delimiter) {
        case Delimiter::None:
            break;
        case Delimiter::NodeBegin:
            NodeBegin(token);
            break;
        case Delimiter::SingleTokenNode:
            NodeSingleToken(token);
            break;
    }
}

void Parser::NodeSingleToken(const Token& token) {
    nodeSpanStack.push_front(TokenSpan{token, token});
}

bool Parser::NotIncoming(TokenKind kind) {
    const auto& token = PeekToken();
    return token.kind != kind && token.kind != TokenKind::Empty;
}

