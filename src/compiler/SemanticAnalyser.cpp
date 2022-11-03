//
// Created by jeuxj on 31/10/2022.
//

#include <cassert>
#include "SemanticAnalyser.h"

using namespace Paduset;

void SemanticAnalyser::AnalyseNode(DiagnosedNode<ProgramDeclarationNode>& prog) {
    this->diagnostics = &prog.Diagnostics();
    this->program = &prog.Node();

    VisitNode(*program);
}

void SemanticAnalyser::Visit(NumberLiteralExpressionNode& node) {
    if (node.ValueType() == ValueType::Integer) {
        node.UpdateMeta(ExpressionSemanticNodeMeta(ValueType::Integer));
    } else {
        node.UpdateMeta(ExpressionSemanticNodeMeta(ValueType::Real));
    }
}

void SemanticAnalyser::Visit(VariableExpressionNode& node) {
    auto entry = variablesInScope.find(node.Name());
    if (entry != variablesInScope.end()) {
        node.UpdateMeta(ExpressionSemanticNodeMeta(entry->second->Type()));
    } else {
        Diagnostic diag{DiagnosticSeverity::Error,
                        "La variable '" + node.Name() + "' n'existe pas.",
                        node};
        diagnostics->AddDiagnostic(diag);

        node.UpdateMeta(ExpressionSemanticNodeMeta(ValueType::Error));
    }
}

// We need to call the RecursiveNodeVisitor first to have our child nodes
// with semantic info filled in.

void SemanticAnalyser::Visit(IfStatementNode& node) {
    RecursiveNodeVisitor::Visit(node);

    const auto checkCondition = [this](const ExpressionNode& node) {
        ValueType type = node.FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
        if (type != ValueType::Boolean && type != ValueType::Error) {
            Diagnostic diag{DiagnosticSeverity::Error, "La valeur n'est pas un booléen.", node};
            diagnostics->AddDiagnostic(diag);
        }
    };

    checkCondition(node.Condition());
    for (const auto& item: node.ElseIfBlocks()) {
        checkCondition(item.Condition());
    }
}

void SemanticAnalyser::Visit(AddExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);
    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(SubtractExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);
    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(MultiplyExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);
    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(DivideExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);
    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(ErrorExpressionNode& node) {
    node.UpdateMeta(ExpressionSemanticNodeMeta(ValueType::Error));
}

void SemanticAnalyser::Visit(VariableDeclarationNode& node) {
    for (const auto& name: node.Names()) {
        if (variablesInScope.find(name) != variablesInScope.end()) {
            Diagnostic diag{DiagnosticSeverity::Error,
                            "La variable '" +name + "' est déjà déclarée.",
                            node};
            diagnostics->AddDiagnostic(diag);
        }

        variablesInScope[name] = &node;
    }
}

void SemanticAnalyser::AnalyseBinaryExpression(BinaryExpressionNode& node) {
    auto leftSemantic = node.Left().FindMeta<ExpressionSemanticNodeMeta>();
    auto rightSemantic = node.Right().FindMeta<ExpressionSemanticNodeMeta>();
    assert(leftSemantic && rightSemantic);

    ValueType finalType{ValueType::Error};

    NodeKind kind = node.Kind();

    if (kind == NodeKind::AddExpression &&
        (leftSemantic->BaseType() == ValueType::String || rightSemantic->BaseType() == ValueType::String)) {
        if (leftSemantic->BaseType() != ValueType::String) {
            leftSemantic->ContextualType() = ValueType::String;
        } else if (rightSemantic->BaseType() != ValueType::String) {
            rightSemantic->ContextualType() = ValueType::String;
        }
        finalType = ValueType::String;
    } else if (kind == NodeKind::AddExpression ||
               kind == NodeKind::SubtractExpression ||
               kind == NodeKind::MultiplyExpression ||
               kind == NodeKind::DivideExpression) {
        const auto invalidType = [](const ExpressionSemanticNodeMeta* semantic) {
            return semantic->BaseType() != ValueType::Integer && semantic->BaseType() != ValueType::Real;
        };
        const auto reportInvalidType = [this](Node& node, const ExpressionSemanticNodeMeta* semantic) {
            // TODO: specify the type
            Diagnostic diag{DiagnosticSeverity::Error,
                            "Type invalide.",
                            node};
            diagnostics->AddDiagnostic(diag);
        };

        bool invalidLeft = invalidType(leftSemantic);
        bool invalidRight = invalidType(rightSemantic);
        if (!invalidLeft && !invalidRight) { // All good! Either Integer or Real
            if (kind == NodeKind::DivideExpression) {
                // This one is pretty weird. Both are going to be reals, even if they are all integers.
                leftSemantic->ContextualType() = ValueType::Real;
                rightSemantic->ContextualType() = ValueType::Real;

                finalType = ValueType::Real;
            } else {
                // float + int -> float
                if (ApplyRealTypePromotion(node)) {
                    finalType = ValueType::Real;
                } else {
                    finalType = ValueType::Integer;
                }
            }
        } else {
            if (invalidLeft) {
                reportInvalidType(node.Left(), leftSemantic);
            }
            if (invalidRight) {
                reportInvalidType(node.Right(), rightSemantic);
            }

            if (invalidLeft && !invalidRight) {
                finalType = rightSemantic->BaseType();
            } else if (!invalidLeft) { // invalidRight
                finalType = leftSemantic->BaseType();
            } else {
                finalType = ValueType::Error;
            }
        }
    } else if (kind == NodeKind::EqualExpression || kind == NodeKind::NotEqualExpression) {
        ApplyRealTypePromotion(node);

        if (leftSemantic->ContextualType() != rightSemantic->ContextualType()) {
            Diagnostic diag{DiagnosticSeverity::Error,
                            "Types non comparables.",
                            node};
            diagnostics->AddDiagnostic(diag);
        }

        finalType = ValueType::Boolean;
    } else if (kind == NodeKind::GreaterExpression || kind == NodeKind::GreaterOrEqualExpression ||
               kind == NodeKind::LowerExpression || kind == NodeKind::LowerOrEqualExpression) {
        ApplyRealTypePromotion(node);

        if (leftSemantic->ContextualType() == ValueType::Integer &&
            rightSemantic->ContextualType() == ValueType::Integer) {
            // Works!
        } else if (leftSemantic->ContextualType() == ValueType::Real &&
                   rightSemantic->ContextualType() == ValueType::Real) {
            // Works too!
        } else {
            Diagnostic diag{DiagnosticSeverity::Error,
                            "Types non comparables.",
                            node};
            diagnostics->AddDiagnostic(diag);
        }

        finalType = ValueType::Boolean;
    }
    assert(finalType != ValueType::Error);

    ExpressionSemanticNodeMeta meta = ExpressionSemanticNodeMeta(finalType);
    meta.RegisterChildNodeLifetime(node.Left());
    meta.RegisterChildNodeLifetime(node.Right());
    node.UpdateMeta(meta);
    // TODO: Integer div/Modulo
}

bool SemanticAnalyser::ApplyRealTypePromotion(BinaryExpressionNode& node) {
    auto leftSemantic = node.Left().FindMeta<ExpressionSemanticNodeMeta>();
    auto rightSemantic = node.Right().FindMeta<ExpressionSemanticNodeMeta>();

    if (leftSemantic->BaseType() == ValueType::Real && rightSemantic->BaseType() == ValueType::Integer) {
        rightSemantic->ContextualType() = ValueType::Real;
        return true;
    } else if (leftSemantic->BaseType() == ValueType::Integer && rightSemantic->BaseType() == ValueType::Real) {
        leftSemantic->ContextualType() = ValueType::Real;
        return true;
    }

    return false;
}

void SemanticAnalyser::Visit(AssignStatementNode& node) {
    RecursiveNodeVisitor::Visit(node);

    const std::string& varName = node.Variable().Name(); // This shouldn't even be an expression btw.
    if (auto* variable = FindVariable(varName)) {
        auto* semantic = node.Expression().FindMeta<ExpressionSemanticNodeMeta>();
        if (AreTypesAssignable(variable->Type(), semantic->BaseType())) {
            // All good! Let's see if there's a cast to do...
            if (variable->Type() != semantic->BaseType()) {
                semantic->ContextualType() = variable->Type();
            }
        } else {
            Diagnostic diag{DiagnosticSeverity::Error,
                            "Impossible d'assigner la variable '" + varName + "' avec cette valeur.",
                            node.Expression()};
            diagnostics->AddDiagnostic(diag);
        }
    } else {
        // Nothing, error already given before.
    }
}

void SemanticAnalyser::Visit(StringLiteralExpressionNode& node) {
    node.UpdateMeta(ExpressionSemanticNodeMeta(ValueType::String));
}

void SemanticAnalyser::Visit(WriteStatementNode& node) {
    RecursiveNodeVisitor::Visit(node);

    node.Expression().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType() = ValueType::String;
}

VariableDeclarationNode* SemanticAnalyser::FindVariable(const std::string& name) {
    auto iterator = variablesInScope.find(name);
    if (iterator == variablesInScope.end()) {
        return nullptr;
    } else {
        return (*iterator).second;
    }
}

bool SemanticAnalyser::AreTypesAssignable(ValueType to, ValueType from) {
    return to == from || (to == ValueType::Real && from == ValueType::Integer);
}

void SemanticAnalyser::Visit(ReadStatementNode& node) {
    RecursiveNodeVisitor::Visit(node);

    // To be upgraded when other assignable things than variables exist (such as structs?)
    if (node.Expression().Kind() != NodeKind::VariableExpression) {
        Diagnostic diag{DiagnosticSeverity::Error, "La valeur à lire n'est pas une variable.", node.Expression()};
        diagnostics->AddDiagnostic(diag);
    } else if (FindVariable(node.Variable().Name()) == nullptr) {
        Diagnostic diag{DiagnosticSeverity::Error,
                        "La variable '" + node.Variable().Name() + "' n'existe pas.",
                        node.Expression()};
        diagnostics->AddDiagnostic(diag);
    }
}

void SemanticAnalyser::Visit(EqualExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(NotEqualExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(GreaterExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(GreaterOrEqualExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(LowerExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(LowerOrEqualExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    AnalyseBinaryExpression(node);
}

void SemanticAnalyser::Visit(ParenthesesExpressionNode& node) {
    RecursiveNodeVisitor::Visit(node);

    ValueType& type = node.Expression().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
    node.UpdateMeta(ExpressionSemanticNodeMeta(type));
}

void ExpressionSemanticNodeMeta::RegisterChildNodeLifetime(ExpressionNode& node) {
    lifetimeBoundNodes.push_back(&node);
}
