//
// Created by jeuxj on 29/10/2022.
//

#include "AST.h"

#include <utility>

using namespace Paduset;

void NodeVisitor::VisitNode(Node& node) {
    node.Accept(*this);
}

TokenNodeMeta::TokenNodeMeta(struct TokenSpan tokenSpan) : span(std::move(tokenSpan)) {}

void RecursiveNodeVisitor::Visit(VariableDeclarationNode& node) {

}

void RecursiveNodeVisitor::Visit(StringLiteralExpressionNode& node) {
}

void RecursiveNodeVisitor::Visit(ProgramDeclarationNode& node) {
    for (VariableDeclarationNode& var : node.Variables()) {
        var.Accept(*this);
    }
    for (std::unique_ptr<StatementNode>& statement : node.Statements()) {
        statement->Accept(*this);
    }
}

void RecursiveNodeVisitor::Visit(NumberLiteralExpressionNode& node) {

}

void RecursiveNodeVisitor::Visit(VariableExpressionNode& node) {

}

void RecursiveNodeVisitor::Visit(AssignStatementNode& node) {
    node.Variable().Accept(*this);
    node.Expression().Accept(*this);
}

void RecursiveNodeVisitor::Visit(WriteStatementNode& node) {
    node.Expression().Accept(*this);
}

void RecursiveNodeVisitor::Visit(ReadStatementNode& node) {
    node.Expression().Accept(*this);
}

void RecursiveNodeVisitor::Visit(IfStatementNode& node) {
    node.Condition().Accept(*this);
    for (auto& statement: node.ThenStatements()) {
        statement->Accept(*this);
    }

    for (auto& elseIf : node.ElseIfBlocks()) {
        elseIf.Condition().Accept(*this);
        for (auto& statement : elseIf.Statements()) {
            statement->Accept(*this);
        }
    }

    for (auto& statement: node.ElseStatements()) {
        statement->Accept(*this);
    }
}

void RecursiveNodeVisitor::Visit(AddExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(SubtractExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(MultiplyExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(DivideExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(EqualExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(NotEqualExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(GreaterExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(GreaterOrEqualExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(LowerOrEqualExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(LowerExpressionNode& node) {
    node.Left().Accept(*this);
    node.Right().Accept(*this);
}

void RecursiveNodeVisitor::Visit(ParenthesesExpressionNode& node) {
    node.Expression().Accept(*this);
}

void RecursiveNodeVisitor::Visit(ErrorExpressionNode& node) {

}