//
// Created by jeuxj on 31/10/2022. (spooky)
//

#ifndef PADUSET_SEMANTICANALYSER_H
#define PADUSET_SEMANTICANALYSER_H

#include "Diagnostic.h"
#include "AST.h"
#include <unordered_map>

namespace Paduset {
    class ExpressionSemanticNodeMeta : public SemanticNodeMeta {
    public:
        ExpressionSemanticNodeMeta(ValueType contextualType, ValueType baseType)
                : contextualType(contextualType),
                  baseType(baseType) {}

        explicit ExpressionSemanticNodeMeta(ValueType contextualType)
                : contextualType(contextualType),
                  baseType(contextualType) {}

        [[nodiscard]] ValueType ContextualType() const {
            return contextualType;
        }

        [[nodiscard]] ValueType& ContextualType() {
            return contextualType;
        }

        [[nodiscard]] ValueType BaseType() const {
            return baseType;
        }

        [[nodiscard]] ValueType& BaseType() {
            return baseType;
        }

        [[nodiscard]] bool HasImplicitCast() const {
            return contextualType != baseType;
        }

        [[nodiscard]] const std::vector<ExpressionNode*>& LifetimeBoundNodes() const {
            return lifetimeBoundNodes;
        }

        [[nodiscard]] std::vector<ExpressionNode*>& LifetimeBoundNodes() {
            return lifetimeBoundNodes;
        }

        void RegisterChildNodeLifetime(ExpressionNode& node);

    private:
        ValueType contextualType;
        ValueType baseType;
        std::vector<ExpressionNode*> lifetimeBoundNodes{};
    };

    class SemanticAnalyser : private RecursiveNodeVisitor {
    public:
        void AnalyseNode(DiagnosedNode<ProgramDeclarationNode>& prog);

    private:
        void Visit(NumberLiteralExpressionNode& node) override;

        void Visit(StringLiteralExpressionNode& node) override;

        void Visit(VariableExpressionNode& node) override;

        void Visit(AddExpressionNode& node) override;

        void Visit(SubtractExpressionNode& node) override;

        void Visit(MultiplyExpressionNode& node) override;

        void Visit(DivideExpressionNode& node) override;

        void Visit(ErrorExpressionNode& node) override;

        void Visit(VariableDeclarationNode& node) override;

        void Visit(AssignStatementNode& node) override;

        void Visit(IfStatementNode& node) override;

        void Visit(ReadStatementNode& node) override;

        void Visit(WriteStatementNode& node) override;

        void Visit(EqualExpressionNode& node) override;

        void Visit(NotEqualExpressionNode& node) override;

        void Visit(GreaterExpressionNode& node) override;

        void Visit(GreaterOrEqualExpressionNode& node) override;

        void Visit(LowerExpressionNode& node) override;

        void Visit(LowerOrEqualExpressionNode& node) override;

        void Visit(ParenthesesExpressionNode& node) override;

        void AnalyseBinaryExpression(BinaryExpressionNode& node);

        bool ApplyRealTypePromotion(BinaryExpressionNode& node);

        VariableDeclarationNode* FindVariable(const std::string& name);

        static bool AreTypesAssignable(ValueType to, ValueType from);

        DiagnosticSet* diagnostics{nullptr};
        ProgramDeclarationNode* program{nullptr};
        std::unordered_map<std::string, VariableDeclarationNode*> variablesInScope;
    };
};

#endif //PADUSET_SEMANTICANALYSER_H
