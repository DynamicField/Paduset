//
// Created by jeuxj on 31/10/2022.
//

#ifndef PADUSET_DIAGNOSTIC_H
#define PADUSET_DIAGNOSTIC_H

#include <string>
#include <utility>
#include <memory>
#include "Tokens.h"

namespace Paduset {
    enum class DiagnosticSeverity {
        Information,
        Warning,
        Error
    };

    struct Diagnostic {
        Diagnostic(DiagnosticSeverity severity, std::string message, const class Node& node);

        Diagnostic(DiagnosticSeverity severity, std::string message, TokenSpan tokenSpan) :
                severity(severity),
                message(std::move(message)),
                tokenSpan(std::move(tokenSpan)),
                charSpan(this->tokenSpan.ToCharSpan()) {}

        Diagnostic(DiagnosticSeverity severity, std::string message, TokenSpan tokenSpan, CharSpan charSpan) :
                severity(severity),
                message(std::move(message)),
                tokenSpan(std::move(tokenSpan)),
                charSpan(charSpan) {}

        DiagnosticSeverity severity;
        std::string message;
        TokenSpan tokenSpan;
        CharSpan charSpan;
    };

    class DiagnosticSet {
    public:
        void AddDiagnostic(const Diagnostic& diagnostic);

        void PrintReport(std::ostream& out) const;

        [[nodiscard]] const std::vector<Diagnostic>& Get() const {
            return diagnostics;
        }

        [[nodiscard]] bool HasError() const {
            return hasError;
        }

    private:
        std::vector<Diagnostic> diagnostics{};
        bool hasError = false;
    };

    template<typename N>
    class DiagnosedNode {
    public:
        DiagnosedNode(N&& node, std::unique_ptr<DiagnosticSet>&& diagnostics) : node(std::move(node)),
                                                                                diagnostics(std::move(diagnostics)) {}


        [[nodiscard]] const N& Node() const {
            return node;
        }

        [[nodiscard]] N& Node() {
            return node;
        }

        [[nodiscard]] DiagnosticSet& Diagnostics() const {
            return *diagnostics;
        }

        [[nodiscard]] bool Success() const {
            return !diagnostics->HasError();
        }

    private:
        N node;
        std::unique_ptr<DiagnosticSet> diagnostics;
    };
};

#endif //PADUSET_DIAGNOSTIC_H
