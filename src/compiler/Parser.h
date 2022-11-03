//
// Created by jeuxj on 29/10/2022.
//

#ifndef PADUSET_PARSER_H
#define PADUSET_PARSER_H

#include <utility>

#include "AST.h"
#include "Tokens.h"
#include "Diagnostic.h"
#include <optional>
#include <deque>
#include <functional>

namespace Paduset {
    template<typename Trash>
    using ForceToken = Token;

    class Parser {
    public:
        explicit Parser(std::vector<Token> tokens) : tokens(std::move(tokens)) {
            tokenIterator = this->tokens.begin();
        }

        DiagnosedNode<ProgramDeclarationNode> ReadProgramDeclaration();

    private:
        std::optional<VariableDeclarationNode> AcceptVariableDeclaration();

        std::unique_ptr<ExpressionNode> AcceptExpression();

        std::unique_ptr<ExpressionNode> AcceptExpressionOrError();

        std::unique_ptr<StatementNode> AcceptStatement();

        std::unique_ptr<AssignStatementNode> AcceptAssignStatement();

        std::unique_ptr<WriteStatementNode> AcceptWriteStatement();

        std::unique_ptr<ReadStatementNode> AcceptReadStatement();

        std::unique_ptr<IfStatementNode> AcceptIfStatement();

        std::vector<std::unique_ptr<StatementNode>>
        ReadStatementList(const std::function<bool(const Token&)>& isTokenEnd);

        enum class Delimiter {
            None,
            NodeBegin,
            SingleTokenNode
        };

        const Token& NextToken(Delimiter delimiter = Delimiter::None);

        const Token& NextToken(TokenKind kind, Delimiter delimiter = Delimiter::None);

        template<Delimiter D = Delimiter::None, typename... T>
        std::tuple<ForceToken<T>..., bool> NextTokens(T... kinds) {
            std::tuple<ForceToken<T>..., bool> tuple;
            int i = 0;
            bool dead = false;
            const Token* first = nullptr;
            const auto next = [&](TokenKind kind) -> const Token& {
                if (dead) {
                    return EmptyToken;
                }
                i++;
                if (const Token& token = NextToken(kind)) {
                    // First token: remember it.
                    if (i == 1) {
                        first = &token;
                    }
                    // Last token: sequence success!
                    // Make sure to delimit it.
                    if (D != Delimiter::None && i == sizeof...(T)) {
                        Delimit(*first, D);
                    }
                    return token;
                } else {
                    dead = true;
                    Rewind(i - 1); // Minus 1 to only rewind the tokens we have visited.
                    return EmptyToken;
                }
            };
            return {next(kinds)..., !dead};
        }

        const Token& NextRequiredToken(TokenKind kind, Delimiter delimiter = Delimiter::None);

        const Token& PeekToken(int offset = 1);

        const Token& PeekToken(TokenKind kind, int offset = 1);

        bool NotIncoming(TokenKind kind);

        const Token& Rewind(int offset = 1);

        static std::optional<ValueType> TokenToType(const Token& token);

        static OperatorPrecedence TokenToPrecedence(const Token& token);

        template<typename N, typename... Args>
        std::unique_ptr<N> NewStackedNodePtr(Args&& ... args) {
            static_assert(std::is_assignable_v<Node*&, N*>);

            std::unique_ptr<N> ptr = std::make_unique<N>(std::forward<Args>(args)...);
            ptr->template UpdateMeta<TokenNodeMeta>(TokenNodeMeta(FinalizeNodeSpan()));

            return ptr;
        }

        template<typename N, typename... Args>
        N NewStackedNode(Args&& ... args) {
            static_assert(std::is_assignable_v<Node*&, N*>);

            N node{std::forward<Args>(args)...};
            node.template UpdateMeta<TokenNodeMeta>(TokenNodeMeta(FinalizeNodeSpan()));

            return node;
        }

        template<typename N, typename... Args>
        N NewNode(const TokenSpan& tokenSpan, Args&& ... args) {
            static_assert(std::is_assignable_v<Node*&, N*>);

            N node{std::forward<Args>(args)...};
            node.template UpdateMeta<TokenNodeMeta>(TokenNodeMeta(tokenSpan));

            return node;
        }

        template<typename N, typename... Args>
        std::unique_ptr<N> NewNodePtr(const TokenSpan& tokenSpan, Args&& ... args) {
            static_assert(std::is_assignable_v<Node*&, N*>);

            std::unique_ptr<N> ptr = std::make_unique<N>(std::forward<Args>(args)...);
            ptr->template UpdateMeta<TokenNodeMeta>(TokenNodeMeta(tokenSpan));

            return ptr;
        }

        void Delimit(const Token& token, Delimiter delimiter);

        void NodeBegin(const Token& token);

        void NodeSingleToken(const Token& token);

        TokenSpan FinalizeNodeSpan();

        Token currentToken = EmptyToken;
        std::vector<Token>::iterator tokenIterator;
        std::vector<Token> tokens;
        std::vector<Token> madeUpTokens{};
        std::deque<TokenSpan> nodeSpanStack{};

        std::unique_ptr<DiagnosticSet> diagnostics{new DiagnosticSet()};
    };
}

#endif //PADUSET_PARSER_H
