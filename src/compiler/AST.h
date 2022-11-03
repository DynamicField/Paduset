//
// Created by jeuxj on 29/10/2022.
//

#ifndef PADUSET_AST_H
#define PADUSET_AST_H

#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <memory>
#include <optional>
#include <cassert>
#include "Tokens.h"
#include "Diagnostic.h"

namespace Paduset {

    enum class NodeKind {
        ProgramDeclaration,
        VariableDeclaration,
        AssignmentStatement,
        VariableExpression,
        NumberLiteralExpression,
        StringLiteralExpression,
        AddExpression,
        SubtractExpression,
        MultiplyExpression,
        DivideExpression,
        ModuloExpression,
        IntegerDivideExpression,
        EqualExpression,
        NotEqualExpression,
        GreaterExpression,
        GreaterOrEqualExpression,
        LowerExpression,
        LowerOrEqualExpression,
        ParenthesesExpression,
        ErrorExpression,
        // For now, those are hardcoded statements; maybe we'll have
        // more flexible functions in the future.
        WriteStatement,
        ReadStatement,
        IfStatement
    };

    enum class ValueType {
        Integer,
        Real,
        String,
        Boolean,
        Error
    };

    enum class OperatorPrecedence {
        UnderLinear,
        Linear,
        Multiplicative,
        Parentheses
    };

    constexpr const char* ValueTypeName(ValueType value) {
        switch (value) {
            case ValueType::Integer:
                return "Integer";
            case ValueType::Real:
                return "Real";
            case ValueType::String:
                return "String";
            case ValueType::Boolean:
                return "Boolean";
            default:
                return "Noidea";
        }
    }


    // Visitor stuff

    class NodeVisitor {
    public:
        virtual void Visit(class VariableDeclarationNode& node) {}

        virtual void Visit(class ProgramDeclarationNode& node) {}

        virtual void Visit(class NumberLiteralExpressionNode& node) {}

        virtual void Visit(class StringLiteralExpressionNode& node) {}

        virtual void Visit(class VariableExpressionNode& node) {}

        virtual void Visit(class AssignStatementNode& node) {}

        virtual void Visit(class WriteStatementNode& node) {}

        virtual void Visit(class ReadStatementNode& node) {}

        virtual void Visit(class IfStatementNode& node) {}

        virtual void Visit(class AddExpressionNode& node) {}

        virtual void Visit(class SubtractExpressionNode& node) {}

        virtual void Visit(class MultiplyExpressionNode& node) {}

        virtual void Visit(class DivideExpressionNode& node) {}

        virtual void Visit(class EqualExpressionNode& node) {}

        virtual void Visit(class NotEqualExpressionNode& node) {}

        virtual void Visit(class GreaterExpressionNode& node) {}

        virtual void Visit(class GreaterOrEqualExpressionNode& node) {}

        virtual void Visit(class LowerExpressionNode& node) {}

        virtual void Visit(class LowerOrEqualExpressionNode& node) {}

        virtual void Visit(class ParenthesesExpressionNode& node) {}

        virtual void Visit(class ErrorExpressionNode& node) {}

        void VisitNode(class Node& node);
    };

    class RecursiveNodeVisitor : public NodeVisitor {
    public:
        void Visit(class VariableDeclarationNode& node) override;

        void Visit(class ProgramDeclarationNode& node) override;

        void Visit(class NumberLiteralExpressionNode& node) override;

        void Visit(class StringLiteralExpressionNode& node) override;

        void Visit(class VariableExpressionNode& node) override;

        void Visit(class AssignStatementNode& node) override;

        void Visit(class WriteStatementNode& node) override;

        void Visit(class ReadStatementNode& node) override;

        void Visit(class IfStatementNode& node) override;

        void Visit(class AddExpressionNode& node) override;

        void Visit(class SubtractExpressionNode& node) override;

        void Visit(class MultiplyExpressionNode& node) override;

        void Visit(class DivideExpressionNode& node) override;

        void Visit(class EqualExpressionNode& node) override;

        void Visit(class NotEqualExpressionNode& node) override;

        void Visit(class GreaterExpressionNode& node) override;

        void Visit(class GreaterOrEqualExpressionNode& node) override;

        void Visit(class LowerExpressionNode& node) override;

        void Visit(class LowerOrEqualExpressionNode& node) override;

        void Visit(class ParenthesesExpressionNode& node) override;

        void Visit(class ErrorExpressionNode& node) override;
    };

    template<typename T>
    class NodeVisitorRetVal : NodeVisitor {
    public:
        virtual T VisitRet(class Node& node) {
            VisitNode(node);
            return outValue;
        }

    protected:
        T outValue;
    };

    class NodeMeta {
    public:
        virtual ~NodeMeta() = default;
    };

    class TokenNodeMeta : public NodeMeta {
    public:
        explicit TokenNodeMeta(TokenSpan tokenSpan);

    public:
        [[nodiscard]] const Paduset::TokenSpan& Span() const {
            return span;
        }

    private:
        Paduset::TokenSpan span;
    };

    class SemanticNodeMeta : public NodeMeta {
    };

    class CompilerNodeMeta : public NodeMeta {
    };

    template<typename N, NodeKind K>
    class GenericBaseNode : public N {
        [[nodiscard]] NodeKind Kind() const final {
            return K;
        }
    };

    class Node {
    public:
        virtual ~Node() = default;

        [[nodiscard]] virtual NodeKind Kind() const = 0;

        template<NodeKind K>
        using Base = GenericBaseNode<Node, K>;

        virtual void Accept(NodeVisitor& visitor) = 0;

        // Very cursed template stuff. But it works. Nice.

        template<typename F>
        const F* FindMeta() const {
            auto& facetOpt = GetMetaField<F>();
            if constexpr (std::is_same_v<F, TokenNodeMeta>) {
                if (facetOpt.has_value()) {
                    return &facetOpt.value();
                } else {
                    return nullptr;
                }
            } else {
                return dynamic_cast<const F*>(facetOpt.get());
            }
        }

        template<typename F>
        F* FindMeta() {
            return const_cast<F*>(std::as_const(*this).FindMeta<F>());
        }

        template<typename F>
        void InvalidateMeta() {
            GetMetaField<F>().reset();
        }

        template<typename F>
        const F& UpdateMeta(const F& newMeta) {
            if constexpr (std::is_same_v<F, TokenNodeMeta>) {
                GetMetaField<F>() = newMeta;
            } else {
                GetMetaField<F>().reset(new F(newMeta));
            }

            return newMeta;
        }

        template<typename F>
        const F& UpdateMeta(std::unique_ptr<F>&& newMeta) {
            const F& metaVal = *newMeta;
            GetMetaField<F>() = std::move(newMeta);

            return metaVal;
        }

    private:
        template<typename F>
        [[nodiscard]] auto& GetMetaField() const {
            if constexpr (std::is_same_v<F, TokenNodeMeta>) {
                return tokenMeta;
            } else if constexpr (std::is_assignable_v<SemanticNodeMeta*&, F*>) {
                return semanticMeta;
            } else if constexpr (std::is_assignable_v<CompilerNodeMeta*&, F*>) {
                return compilerMeta;
            } else {
                throw std::runtime_error("Unknown meta.");
            }
        }

        template<typename F>
        auto& GetMetaField() {
            auto& field = std::as_const(*this).GetMetaField<F>();

            using ConstRemoved = std::remove_const_t<std::remove_reference_t<decltype(field)>>&;
            return const_cast<ConstRemoved>(field);
        }

        std::optional<TokenNodeMeta> tokenMeta;
        std::shared_ptr<SemanticNodeMeta> semanticMeta;
        std::shared_ptr<CompilerNodeMeta> compilerMeta;
    };

    class StatementNode : public Node {
    public:
        template<NodeKind K>
        using Base = GenericBaseNode<StatementNode, K>;
    };

    class ExpressionNode : public Node {
    public:
        template<NodeKind K>
        using Base = GenericBaseNode<ExpressionNode, K>;
    };

    class BinaryExpressionNode : public ExpressionNode {
    public:
        BinaryExpressionNode(std::unique_ptr<ExpressionNode>&& left, std::unique_ptr<ExpressionNode>&& right)
                : left(std::move(left)), right(std::move(right)) {}

        [[nodiscard]] const ExpressionNode& Left() const {
            return *left;
        }

        [[nodiscard]] const ExpressionNode& Right() const {
            return *right;
        }

        [[nodiscard]] ExpressionNode& Left() {
            return *left;
        }

        [[nodiscard]] ExpressionNode& Right() {
            return *right;
        }

        [[nodiscard]] virtual OperatorPrecedence Precedence() const = 0;

    private:
        std::unique_ptr<ExpressionNode> left;
        std::unique_ptr<ExpressionNode> right;
    };

    template<NodeKind K, OperatorPrecedence P>
    class BinaryExpressionNodeBase : public BinaryExpressionNode {
    public:
        using BinaryExpressionNode::BinaryExpressionNode;

        [[nodiscard]] NodeKind Kind() const override {
            return K;
        }

        [[nodiscard]] OperatorPrecedence Precedence() const final {
            return P;
        }
    };

    class VariableDeclarationNode final : public Node::Base<NodeKind::VariableDeclaration> {
    public:
        VariableDeclarationNode(std::vector<std::string>&& names, ValueType type) : names(std::move(names)), type(type) {}

        [[nodiscard]] const std::vector<std::string>& Names() const {
            return names;
        }

        [[nodiscard]] ValueType Type() const {
            return type;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::vector<std::string> names;
        ValueType type;
    };

    class ProgramDeclarationNode final : public Node::Base<NodeKind::ProgramDeclaration> {
    public:
        ProgramDeclarationNode(std::string name,
                               std::vector<VariableDeclarationNode> variables,
                               std::vector<std::unique_ptr<StatementNode>>&& statements) :
                name(std::move(name)),
                variables(std::move(variables)),
                statements(std::move(statements)) {}

        [[nodiscard]] const std::string& Name() const {
            return name;
        }

        [[nodiscard]] const std::vector<VariableDeclarationNode>& Variables() const {
            return variables;
        }

        [[nodiscard]] const std::vector<std::unique_ptr<StatementNode>>& Statements() const {
            return statements;
        }

        [[nodiscard]] std::vector<VariableDeclarationNode>& Variables() {
            return variables;
        }

        [[nodiscard]] std::vector<std::unique_ptr<StatementNode>>& Statements() {
            return statements;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::string name;
        std::vector<VariableDeclarationNode> variables;
        std::vector<std::unique_ptr<StatementNode>> statements;
    };

    class NumberLiteralExpressionNode final : public ExpressionNode::Base<NodeKind::NumberLiteralExpression> {
    public:
        explicit NumberLiteralExpressionNode(int intValue) : valueType(ValueType::Integer) {
            value.integerValue = intValue;
        }

        explicit NumberLiteralExpressionNode(float floatValue) : valueType(ValueType::Real) {
            value.floatValue = floatValue;
        }

        [[nodiscard]] enum ValueType ValueType() const {
            return valueType;
        }

        [[nodiscard]] int GetValueAsInt() const {
            if (valueType != ValueType::Integer) {
                throw std::runtime_error("No int in number literal.");
            }
            return value.integerValue;
        }

        [[nodiscard]] float GetValueAsFloat() const {
            if (valueType != ValueType::Real) {
                throw std::runtime_error("No float in number literal.");
            }
            return value.floatValue;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        union {
            int integerValue;
            float floatValue;
        } value{};
        enum ValueType valueType;
    };

    class StringLiteralExpressionNode final : public ExpressionNode::Base<NodeKind::StringLiteralExpression> {
    public:
        explicit StringLiteralExpressionNode(std::string value) : value(std::move(value)) {
        }

        const std::string& Value() const {
            return value;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::string value;
    };

    class VariableExpressionNode : public ExpressionNode::Base<NodeKind::VariableExpression> {
    public:
        explicit VariableExpressionNode(std::string name) : name(std::move(name)) {}

        [[nodiscard]] const std::string& Name() const {
            return name;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::string name;
    };

    class AssignStatementNode : public StatementNode::Base<NodeKind::AssignmentStatement> {
    public:
        AssignStatementNode(VariableExpressionNode variable, std::unique_ptr<ExpressionNode>&& expression)
                : variable(std::move(variable)), expression(std::move(expression)) {}

        [[nodiscard]] const VariableExpressionNode& Variable() const {
            return variable;
        }

        [[nodiscard]] const ExpressionNode& Expression() const {
            return *expression;
        }

        [[nodiscard]] VariableExpressionNode& Variable() {
            return variable;
        }

        [[nodiscard]] ExpressionNode& Expression() {
            return *expression;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        VariableExpressionNode variable;
        std::unique_ptr<ExpressionNode> expression;
    };

    class WriteStatementNode : public StatementNode::Base<NodeKind::WriteStatement> {
    public:
        explicit WriteStatementNode(std::unique_ptr<ExpressionNode>&& expression) : expression(std::move(expression)) {}

        [[nodiscard]] const ExpressionNode& Expression() const {
            return *expression;
        }

        ExpressionNode& Expression() {
            return *expression;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::unique_ptr<ExpressionNode> expression;
    };

    class ReadStatementNode : public StatementNode::Base<NodeKind::ReadStatement> {
    public:
        explicit ReadStatementNode(std::unique_ptr<ExpressionNode>&& expression) : expression(std::move(expression)) {}

        [[nodiscard]] const ExpressionNode& Expression() const {
            return *expression;
        }

        ExpressionNode& Expression() {
            return *expression;
        }

        // Only usable after successful semantic analysis.
        [[nodiscard]] const VariableExpressionNode& Variable() const {
            const auto* variable = dynamic_cast<VariableExpressionNode*>(expression.get());
            assert(variable);
            return *variable;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::unique_ptr<ExpressionNode> expression;
    };

    class IfStatementNode : public StatementNode::Base<NodeKind::IfStatement> {
    public:
        struct ElseIfBlock {
            ElseIfBlock(std::unique_ptr<ExpressionNode>&& condition,
                        std::vector<std::unique_ptr<StatementNode>>&& statements)
                    : condition(std::move(condition)),
                      statements(std::move(statements)) {}

            const ExpressionNode& Condition() const {
                return *condition;
            }

            const std::vector<std::unique_ptr<StatementNode>>& Statements() const {
                return statements;
            }

            ExpressionNode& Condition() {
                return *condition;
            }

            std::vector<std::unique_ptr<StatementNode>>& Statements() {
                return statements;
            }

        private:
            std::unique_ptr<ExpressionNode> condition;
            std::vector<std::unique_ptr<StatementNode>> statements;
        };

        IfStatementNode(std::unique_ptr<ExpressionNode>&& condition,
                        std::vector<std::unique_ptr<StatementNode>>&& thenStatements,
                        std::vector<ElseIfBlock>&& elseIfBlocks,
                        std::vector<std::unique_ptr<StatementNode>>&& elseStatements)
                : condition(std::move(condition)),
                  thenStatements(std::move(thenStatements)),
                  elseIfBlocks(std::move(elseIfBlocks)),
                  elseStatements(std::move(elseStatements)) {}

        [[nodiscard]] const ExpressionNode& Condition() const {
            return *condition;
        }

        ExpressionNode& Condition() {
            return *condition;
        }

        const std::vector<std::unique_ptr<StatementNode>>& ThenStatements() const {
            return thenStatements;
        }

        std::vector<std::unique_ptr<StatementNode>>& ThenStatements() {
            return thenStatements;
        }

        const std::vector<ElseIfBlock>& ElseIfBlocks() const {
            return elseIfBlocks;
        }

        std::vector<ElseIfBlock>& ElseIfBlocks() {
            return elseIfBlocks;
        }

        const std::vector<std::unique_ptr<StatementNode>>& ElseStatements() const {
            return elseStatements;
        }

        std::vector<std::unique_ptr<StatementNode>>& ElseStatements() {
            return elseStatements;
        }

        bool HasElse() const {
            return !elseStatements.empty();
        }

        bool HasElseIfs() const {
            return !elseIfBlocks.empty();
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::unique_ptr<ExpressionNode> condition;
        std::vector<std::unique_ptr<StatementNode>> thenStatements;
        std::vector<ElseIfBlock> elseIfBlocks;
        std::vector<std::unique_ptr<StatementNode>> elseStatements;
    };


    template<NodeKind K>
    using LinearOp = BinaryExpressionNodeBase<K, OperatorPrecedence::Linear>;

    template<NodeKind K>
    using MultOp = BinaryExpressionNodeBase<K, OperatorPrecedence::Linear>;


    class AddExpressionNode : public LinearOp<NodeKind::AddExpression> {
        using LinearOp<NodeKind::AddExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class SubtractExpressionNode : public LinearOp<NodeKind::SubtractExpression> {
        using LinearOp<NodeKind::SubtractExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class MultiplyExpressionNode : public MultOp<NodeKind::MultiplyExpression> {
        using MultOp<NodeKind::MultiplyExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class DivideExpressionNode : public MultOp<NodeKind::DivideExpression> {
        using MultOp<NodeKind::DivideExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class EqualExpressionNode : public LinearOp<NodeKind::EqualExpression> {
        using LinearOp<NodeKind::EqualExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class NotEqualExpressionNode : public LinearOp<NodeKind::NotEqualExpression> {
        using LinearOp<NodeKind::NotEqualExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class GreaterExpressionNode : public LinearOp<NodeKind::GreaterExpression> {
        using LinearOp<NodeKind::GreaterExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class GreaterOrEqualExpressionNode : public LinearOp<NodeKind::GreaterOrEqualExpression> {
        using LinearOp<NodeKind::GreaterOrEqualExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class LowerExpressionNode : public LinearOp<NodeKind::LowerExpression> {
        using LinearOp<NodeKind::LowerExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class LowerOrEqualExpressionNode : public LinearOp<NodeKind::LowerOrEqualExpression> {
        using LinearOp<NodeKind::LowerOrEqualExpression>::BinaryExpressionNodeBase;

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }
    };

    class ParenthesesExpressionNode : public ExpressionNode::Base<NodeKind::ParenthesesExpression> {
    public:
        explicit ParenthesesExpressionNode(std::unique_ptr<ExpressionNode>&& expression)
                : expression(std::move(expression)) {

        }

        [[nodiscard]] const ExpressionNode& Expression() const {
            return *expression;
        }

        ExpressionNode& Expression() {
            return *expression;
        }

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        std::unique_ptr<ExpressionNode> expression;
    };

    class ErrorExpressionNode : public ExpressionNode::Base<NodeKind::ErrorExpression> {
    public:
        explicit ErrorExpressionNode(Diagnostic diag) : relevantDiagnostic(std::move(diag)) {}

        void Accept(NodeVisitor& visitor) override {
            visitor.Visit(*this);
        }

    private:
        Diagnostic relevantDiagnostic;
    };
}

#endif //PADUSET_AST_H
