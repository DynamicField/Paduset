//
// Created by jeuxj on 29/10/2022.
//

#ifndef PADUSET_PADUSETCOMPILER_H
#define PADUSET_PADUSETCOMPILER_H

#include <filesystem>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "AST.h"
#include "StandardLibrary.h"
#include "SemanticAnalyser.h"

namespace Paduset {
    class CompilationContext {
        friend class PadusetCompiler;

    public:
        explicit CompilationContext(const std::string& programName)
                : llvm(OpaqueLLVMContext()), irBuilder(LLVM()), module{std::make_unique<llvm::Module>(programName, LLVM())},
                  stringNewLine(irBuilder.CreateGlobalStringPtr("\n", "newline", 0, module.get())),
                  integerType(llvm::Type::getInt32Ty(LLVM())),
                  realType(llvm::Type::getFloatTy(LLVM())),
                  standardLib(StandardLibrary(LLVM(), *module)) {
        }

        // Disable any copy mechanism to avoid accidental duplicates.
        CompilationContext(const CompilationContext&) = delete;

        CompilationContext operator=(const CompilationContext& other) = delete;

        llvm::LLVMContext& LLVM() {
            return *llvm;
        }

        std::unique_ptr<llvm::LLVMContext>& LLVMPtr() {
            return llvm;
        }

        llvm::IRBuilder<>& IRBuilder() {
            return irBuilder;
        }

        llvm::Module& Module() {
            return *module;
        }

        std::unique_ptr<llvm::Module>& ModulePtr() {
            return module;
        }

        std::unordered_map<std::string, llvm::AllocaInst*>& Variables() {
            return variables;
        }

        std::unordered_map<std::string, llvm::GlobalVariable*>& StringLiteralConstants() {
            return stringLiteralConstants;
        }

        llvm::Constant* StringNewLine() const {
            return stringNewLine;
        }

        [[nodiscard]] const StandardLibrary& StandardLib() const {
            return standardLib;
        }

        [[nodiscard]] StandardLibrary& StandardLib() {
            return standardLib;
        }

        [[nodiscard]] llvm::Type* IntegerType() const {
            return integerType;
        }

        [[nodiscard]] llvm::Type* RealType() const {
            return realType;
        }

    private:
        std::unique_ptr<llvm::LLVMContext> llvm;
        llvm::IRBuilder<> irBuilder;
        std::unique_ptr<llvm::Module> module;
        std::unordered_map<std::string, llvm::AllocaInst*> variables{};
        std::unordered_map<std::string, llvm::GlobalVariable*> stringLiteralConstants{};
        llvm::Constant* stringNewLine;
        llvm::Type* integerType;
        llvm::Type* realType;
        StandardLibrary standardLib;

        static std::unique_ptr<llvm::LLVMContext> OpaqueLLVMContext() {
            auto ptr = std::make_unique<llvm::LLVMContext>();
            ptr->enableOpaquePointers();
            return ptr;
        }
    };

    struct IRCompiledProgram {
        std::unique_ptr<llvm::LLVMContext> context;
        std::unique_ptr<llvm::Module> module;
    };

    struct NativeCompiledProgram {
        std::filesystem::path objectFile;
    };

    class PadusetCompiler {
    public:
        explicit PadusetCompiler(ProgramDeclarationNode& program)
                : program(program),
                  context(this->program.Name()) {
        }

    public:
        IRCompiledProgram CompileIR();

        NativeCompiledProgram CompileNative(const IRCompiledProgram& compiled, const std::filesystem::path& path);

    private:
        llvm::Function* GenerateProgramIR();

        llvm::Value* GenerateExpressionIR(ExpressionNode& expression);

        void GenerateStatementIR(StatementNode& statement);

        llvm::Value* ToString(llvm::Value* sourceVal, ValueType sourceType);

        void CleanExpression(ExpressionNode& expression);

        ProgramDeclarationNode& program;
        CompilationContext context;
    };
};
#endif //PADUSET_PADUSETCOMPILER_H
