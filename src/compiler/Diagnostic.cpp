//
// Created by jeuxj on 31/10/2022.
//
#include "Diagnostic.h"
#include "AST.h"

using namespace Paduset;

void DiagnosticSet::AddDiagnostic(const Diagnostic& diagnostic) {
    if (diagnostic.severity == DiagnosticSeverity::Error) {
        hasError = true;
    }
    diagnostics.push_back(diagnostic);
}

void DiagnosticSet::PrintReport(std::ostream& out) const {
    for (const auto& diag: diagnostics) {
        switch (diag.severity) {
            case DiagnosticSeverity::Information:
                std::cout << "Information : ";
                break;
            case DiagnosticSeverity::Warning:
                std::cout << "Avertissement : ";
                break;
            case DiagnosticSeverity::Error:
                std::cout << "Erreur : ";
                break;
        }
        std::cout << diag.message << " (" << diag.charSpan.startChar << ", " << diag.charSpan.endChar << ")\n";
    }
}

Diagnostic::Diagnostic(DiagnosticSeverity severity, std::string message, const Node& node) :
        severity(severity), message(std::move(message)), tokenSpan(), charSpan(0, 0) {
    if (auto* tokenMeta = node.FindMeta<TokenNodeMeta>()) {
        tokenSpan = tokenMeta->Span();
    }
    charSpan = tokenSpan.ToCharSpan();
}
