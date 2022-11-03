//
// Created by jeuxj on 29/10/2022.
//

#include "PadusetCompiler.h"

#include <memory>
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Linker/Linker.h"
#include "SemanticAnalyser.h"

using namespace Paduset;

struct ExpressionCompilerNodeMeta : public CompilerNodeMeta {
    ExpressionCompilerNodeMeta(llvm::Value* baseValue, llvm::Value* contextualValue, bool temporary = false)
            : baseValue(baseValue),
              contextualValue(contextualValue),
              temporary(temporary) {}

    ExpressionCompilerNodeMeta(llvm::Value* baseValue, bool temporary = false)
            : baseValue(baseValue), contextualValue(baseValue), temporary(temporary) {}

    llvm::Value* baseValue;
    llvm::Value* contextualValue;
    bool temporary = false;
};

struct StringCompilerNodeMeta : public ExpressionCompilerNodeMeta {
    StringCompilerNodeMeta(llvm::Value* baseValue, bool isCString, bool temporary) :
            ExpressionCompilerNodeMeta(baseValue, temporary),
            isCString(isCString) {
    }

    StringCompilerNodeMeta(const ExpressionCompilerNodeMeta& source, bool isCString, bool temporary) :
            ExpressionCompilerNodeMeta(source), isCString(isCString) {
        this->temporary = temporary;
    }

    bool isCString;
};

IRCompiledProgram PadusetCompiler::CompileIR() {
    GenerateProgramIR();

//    context.Module().print(llvm::errs(), nullptr);
//    context.StandardLib().Module().print(llvm::errs(), nullptr);

    bool linkError = llvm::Linker::linkModules(context.Module(), std::move(context.StandardLib().ModulePtr()));
    if (linkError) {
        throw std::runtime_error("IR linking failed!");
    }

    return {std::move(context.LLVMPtr()), std::move(context.ModulePtr())};
}

llvm::Function* PadusetCompiler::GenerateProgramIR() {
    auto* functionType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context.LLVM()), false);
    auto* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "main", context.Module());
    auto* entryBlock = llvm::BasicBlock::Create(context.LLVM(), "entry", function);

    context.IRBuilder().SetInsertPoint(entryBlock);

    context.IRBuilder().CreateCall(context.StandardLib().Hi());

    for (const VariableDeclarationNode& variableDef: program.Variables()) {
        llvm::Type* type;
        switch (variableDef.Type()) {
            case ValueType::Integer:
                type = context.StandardLib().IntegerType();
                break;
            case ValueType::Real:
                type = context.StandardLib().RealType();
                break;
            case ValueType::String:
                type = context.StandardLib().StringType();
                break;
            case ValueType::Boolean:
                type = context.StandardLib().BoolType();
                break;
            default:
                throw std::runtime_error("Unknown type.");
        }
        for (const auto& varName: variableDef.Names()) {
            llvm::AllocaInst* stackVar = context.IRBuilder().CreateAlloca(type, nullptr, varName);

            // Initialize the variable.
            if (variableDef.Type() == ValueType::String) {
                context.IRBuilder().CreateCall(context.StandardLib().StringMakeDefault(), {stackVar});
            } else if (variableDef.Type() == ValueType::Integer) {
                context.IRBuilder().CreateStore(llvm::ConstantInt::get(context.StandardLib().IntegerType(), 0, true),
                                                stackVar);
            } else if (variableDef.Type() == ValueType::Real) {
                context.IRBuilder().CreateStore(llvm::ConstantFP::get(context.StandardLib().RealType(), 0.0),
                                                stackVar);
            } else if (variableDef.Type() == ValueType::Boolean) {
                context.IRBuilder().CreateStore(llvm::ConstantInt::get(context.StandardLib().BoolType(), 0),
                                                stackVar);
            }
            context.Variables()[varName] = stackVar;
        }

    }
    for (const std::unique_ptr<StatementNode>& statement: program.Statements()) {
        GenerateStatementIR(*statement);
    }

    // Return 0 to say that everything went 100% well.
    context.IRBuilder().CreateRet(llvm::ConstantInt::get(context.LLVM(), llvm::APInt(32, 0, false)));

    return function;
}

llvm::Value* PadusetCompiler::GenerateExpressionIR(ExpressionNode& expression) {

    struct ExprGenVisitor : public NodeVisitorRetVal<llvm::Value*> {
        explicit ExprGenVisitor(PadusetCompiler* self) : context(self->context), self(*self) {}

        llvm::Value* VisitRet(Node& node, bool acceptStrConst) {
            acceptStringConstant = acceptStrConst;
            return VisitRet(node);
        }

        llvm::Value* VisitRet(Node& node) override {
            assert(currentNode == nullptr && "Visitors are for one-time use only.");
            currentNode = reinterpret_cast<ExpressionNode*>(&node); // Safe since we only visit expressions.

            auto* value = NodeVisitorRetVal::VisitRet(node);

            // Reset any flags
            acceptStringConstant = false;

            return value;
        }

        void Visit(VariableExpressionNode& node) override {
            llvm::AllocaInst* allocInstance = context.Variables()[node.Name()];
            auto type = allocInstance->getAllocatedType();
            if (node.FindMeta<ExpressionSemanticNodeMeta>()->BaseType() == ValueType::String) {
                Return(allocInstance);
            } else {
                Return(context.IRBuilder().CreateLoad(type, allocInstance));
            }
        }

        void Visit(NumberLiteralExpressionNode& node) override {
            if (node.ValueType() == ValueType::Integer) {
                Return(llvm::ConstantInt::get(context.LLVM(), llvm::APInt(32, node.GetValueAsInt(), true)));
            } else { // Float
                Return(llvm::ConstantFP::get(context.LLVM(), llvm::APFloat(node.GetValueAsFloat())));
            }
        }

        void Visit(StringLiteralExpressionNode& node) override {
            auto found = context.StringLiteralConstants().find(node.Value());
            llvm::GlobalVariable* constant;

            if (found == context.StringLiteralConstants().end()) {
                constant = context.IRBuilder().CreateGlobalString(node.Value(), "str_literal");
                context.StringLiteralConstants()[node.Value()] = constant;
            } else {
                constant = found->second;
            }

            if (acceptStringConstant) {
                Return(std::make_unique<StringCompilerNodeMeta>(constant, true, false));
            } else {
                auto ptr = context.IRBuilder().CreateGEP(llvm::Type::getInt8Ty(context.LLVM()), constant,
                                                         llvm::ConstantInt::get(context.LLVM(), llvm::APInt(32, 0)),
                                                         "str_ptr");

                auto alloc = context.IRBuilder().CreateAlloca(context.StandardLib().StringType(), nullptr,
                                                              "pad_str_lit");
                context.IRBuilder().CreateCall(context.StandardLib().StringMake(), {ptr, alloc});

                Return(std::make_unique<StringCompilerNodeMeta>(alloc, false, true));
            }
        }

        void Visit(AddExpressionNode& node) override {
            auto* semantic = node.FindMeta<ExpressionSemanticNodeMeta>();

            // The left-hand side must be copied.
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left(), false);
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right(), true);

            if (semantic->BaseType() == ValueType::String) {
                // Here we make the left-most expression not temporary,
                // to append to it continuously, and then put the whole expression
                // temporary.
                // This makes for a "chain" of string appends.
                node.Left().FindMeta<ExpressionCompilerNodeMeta>()->temporary = false;

                if (node.Right().FindMeta<StringCompilerNodeMeta>()->isCString) {
                    context.IRBuilder().CreateCall(context.StandardLib().StringAppendC(), {lhs, rhs});
                } else {
                    context.IRBuilder().CreateCall(context.StandardLib().StringAppendP(), {lhs, rhs});
                }
                Return(std::make_unique<StringCompilerNodeMeta>(lhs, false, true));
            } else if (semantic->BaseType() == ValueType::Integer) {
                Return(context.IRBuilder().CreateAdd(lhs, rhs, "int_add"));
            } else { // real
                Return(context.IRBuilder().CreateFAdd(lhs, rhs, "real_add"));
            }
        }

        void Visit(SubtractExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            if (node.FindMeta<ExpressionSemanticNodeMeta>()->BaseType() == ValueType::Integer) {
                Return(context.IRBuilder().CreateSub(lhs, rhs, "int_sub"));
            } else { // real
                Return(context.IRBuilder().CreateFSub(lhs, rhs, "real_sub"));
            }
        }

        void Visit(MultiplyExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            if (node.FindMeta<ExpressionSemanticNodeMeta>()->BaseType() == ValueType::Integer) {
                Return(context.IRBuilder().CreateMul(lhs, rhs, "int_mul"));
            } else { // real
                Return(context.IRBuilder().CreateFMul(lhs, rhs, "real_mul"));
            }
        }

        void Visit(DivideExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            if (node.FindMeta<ExpressionSemanticNodeMeta>()->BaseType() == ValueType::Integer) {
                Return(context.IRBuilder().CreateSDiv(lhs, rhs, "int_div"));
            } else { // real
                Return(context.IRBuilder().CreateFDiv(lhs, rhs, "real_div"));
            }
        }

        void Visit(EqualExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left(), true);
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right(), true);

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpEQ(lhs, rhs, "int_eq"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpOEQ(lhs, rhs, "real_eq"));
            } else if (operandsType == ValueType::String) {
                llvm::Value* val = StringEquality(node, lhs, rhs);
                Return(val);
            } else if (operandsType == ValueType::Boolean) {
                Return(context.IRBuilder().CreateICmpEQ(lhs, rhs, "bool_eq"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(NotEqualExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left(), true);
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right(), true);

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpNE(lhs, rhs, "int_eq"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpONE(lhs, rhs, "real_eq"));
            } else if (operandsType == ValueType::String) {
                llvm::Value* val = StringEquality(node, lhs, rhs);
                Return(context.IRBuilder().CreateNeg(val, "str_eq_not"));
            } else if (operandsType == ValueType::Boolean) {
                Return(context.IRBuilder().CreateICmpNE(lhs, rhs, "bool_eq"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(GreaterExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpSGT(lhs, rhs, "int_gt"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpOGT(lhs, rhs, "real_gt"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(GreaterOrEqualExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpSGE(lhs, rhs, "int_geq"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpOGE(lhs, rhs, "real_geq"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(LowerExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpSLT(lhs, rhs, "int_lt"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpOLT(lhs, rhs, "real_lt"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(LowerOrEqualExpressionNode& node) override {
            llvm::Value* lhs = ExprGenVisitor(&self).VisitRet(node.Left());
            llvm::Value* rhs = ExprGenVisitor(&self).VisitRet(node.Right());

            ValueType& operandsType = node.Left().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType();
            if (operandsType == ValueType::Integer) {
                Return(context.IRBuilder().CreateICmpSLE(lhs, rhs, "int_leq"));
            } else if (operandsType == ValueType::Real) {
                Return(context.IRBuilder().CreateFCmpOLE(lhs, rhs, "real_leq"));
            } else {
                throw std::runtime_error("Incomparable types.");
            }
        }

        void Visit(ParenthesesExpressionNode& node) override {
            Return(ExprGenVisitor(&self).VisitRet(node.Expression()));
        }

        llvm::Value* StringEquality(BinaryExpressionNode& node, llvm::Value* lhs, llvm::Value* rhs) {
            bool leftCStr = node.Left().FindMeta<StringCompilerNodeMeta>()->isCString;
            bool rightCStr = node.Right().FindMeta<StringCompilerNodeMeta>()->isCString;

            llvm::Value* cBoolValue;
            if (!leftCStr && !rightCStr) {
                cBoolValue = context.IRBuilder().CreateCall(context.StandardLib().StringEqualsP(), {lhs, rhs},
                                                            "str_eq_pad");
            } else if (leftCStr && rightCStr) {
                cBoolValue = context.IRBuilder().CreateCall(context.StandardLib().StringEqualsCC(), {lhs, rhs},
                                                            "str_eq_c");
            } else {
                llvm::Value* cOperand = leftCStr ? lhs : rhs;
                llvm::Value* padOperand = rightCStr ? lhs : rhs;
                cBoolValue = context.IRBuilder().CreateCall(context.StandardLib().StringEqualsC(),
                                                            {padOperand, cOperand}, "str_eq_mixed");
            }

            llvm::Value* val = context.IRBuilder().CreateTrunc(cBoolValue, context.StandardLib().BoolType());
            return val;
        }

        void Return(llvm::Value* value, bool temporary = false) {
            auto isStr = currentNode->FindMeta<ExpressionSemanticNodeMeta>()->BaseType() == ValueType::String;
            if (isStr) {
                auto meta = std::make_unique<StringCompilerNodeMeta>(value, acceptStringConstant, temporary);
                meta->temporary = temporary;
                Return(std::move(meta));
            } else {
                Return(std::make_unique<ExpressionCompilerNodeMeta>(value, temporary));
            }
        }

        void Return(std::unique_ptr<ExpressionCompilerNodeMeta>&& meta) {
            // Apply any implicit cast.
            const auto* semantic = currentNode->FindMeta<ExpressionSemanticNodeMeta>();

            if (semantic->HasImplicitCast()) {
                // Do the conversion. Maybe that this should be somewhere else.
                if (semantic->BaseType() == ValueType::Integer &&
                    semantic->ContextualType() == ValueType::Real) {
                    meta->contextualValue = context.IRBuilder().CreateSIToFP(meta->baseValue, context.realType,
                                                                             "implicit_itr");
                } else if (semantic->BaseType() == ValueType::Real &&
                           semantic->ContextualType() == ValueType::Integer) {
                    meta->contextualValue = context.IRBuilder().CreateFPToSI(meta->baseValue, context.integerType,
                                                                             "implicit_rti");
                } else if (semantic->BaseType() != ValueType::String &&
                           semantic->ContextualType() == ValueType::String) {
                    meta = std::make_unique<StringCompilerNodeMeta>(*meta, false, true);
                    meta->contextualValue = self.ToString(meta->baseValue, semantic->BaseType());
                } else {
                    throw std::runtime_error("Unable to cast.");
                }
            }
            for (ExpressionNode* nodeToKill: semantic->LifetimeBoundNodes()) {
                if (nodeToKill->FindMeta<ExpressionCompilerNodeMeta>()->temporary) {
                    self.CleanExpression(*nodeToKill);
                }
            }

            outValue = meta->contextualValue;
            currentNode->UpdateMeta(std::move(meta));
        }

        CompilationContext& context;
        PadusetCompiler& self;
        ExpressionNode* currentNode = nullptr;
        bool acceptStringConstant = false;
    };
    return ExprGenVisitor(this).VisitRet(expression);
}

void PadusetCompiler::GenerateStatementIR(StatementNode& statement) {
    struct Visitor : public NodeVisitor {
        explicit Visitor(PadusetCompiler* self) : context(self->context), self(*self) {}

        void Visit(AssignStatementNode& assign) override {
            llvm::AllocaInst* stackVariable = context.Variables()[assign.Variable().Name()];
            llvm::Value* value = self.GenerateExpressionIR(assign.Expression());
            if (assign.Expression().FindMeta<ExpressionSemanticNodeMeta>()->ContextualType() == ValueType::String) {
                context.IRBuilder().CreateCall(context.StandardLib().StringAssign(), {stackVariable, value});
            } else {
                context.IRBuilder().CreateStore(value, stackVariable);
            }
            self.CleanExpression(assign.Expression());
        }

        void Visit(WriteStatementNode& node) override {
            auto* value = self.GenerateExpressionIR(node.Expression());
            auto strPtr = context.IRBuilder().CreateStructGEP(context.StandardLib().StringType(), value, 0,
                                                              "str_buffer_ptr");
            auto buffer = context.IRBuilder().CreateLoad(llvm::PointerType::get(context.LLVM(), 0), strPtr,
                                                         "str_buffer");
            context.IRBuilder().CreateCall(context.StandardLib().Printf(), {buffer});
            context.IRBuilder().CreateCall(context.StandardLib().Printf(), {context.StringNewLine()});
            self.CleanExpression(node.Expression());
        }

        void Visit(ReadStatementNode& node) override {
            const VariableExpressionNode& variableExpr = node.Variable();
            ValueType type = variableExpr.FindMeta<ExpressionSemanticNodeMeta>()->BaseType();
            llvm::AllocaInst* stackVariable = context.Variables()[variableExpr.Name()];

            if (type == ValueType::Integer) {
                llvm::Value* consoleInt = context.IRBuilder().CreateCall(context.StandardLib().ReadInt());
                context.IRBuilder().CreateStore(consoleInt, stackVariable);
            } else if (type == ValueType::Real) {
                llvm::Value* consoleReal = context.IRBuilder().CreateCall(context.StandardLib().ReadReal());
                context.IRBuilder().CreateStore(consoleReal, stackVariable);
            } else if (type == ValueType::String) {
                context.IRBuilder().CreateCall(context.StandardLib().ReadString(), stackVariable);
            }
        }

        void Visit(IfStatementNode& node) override {
            // TODO: Expression lifetime cleanup?

            llvm::Function* parentFunction = context.IRBuilder().GetInsertBlock()->getParent();

            llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(context.LLVM(), "then", parentFunction);
            // First block: A dumb jump (condition, true -> elif_do, false -> <next_check>)
            std::vector<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*>> elseIfLlvmBlocks;
            for (size_t i = 0; i < node.ElseIfBlocks().size(); i++) {
                llvm::BasicBlock* check = llvm::BasicBlock::Create(context.LLVM(), "elif_check", parentFunction);
                llvm::BasicBlock* doer = llvm::BasicBlock::Create(context.LLVM(), "elif_do", parentFunction);
                elseIfLlvmBlocks.emplace_back(check, doer);
            }
            llvm::BasicBlock* elseBlock = node.HasElse() ? llvm::BasicBlock::Create(context.LLVM(), "else",
                                                                                    parentFunction) : nullptr;
            llvm::BasicBlock* contBlock = llvm::BasicBlock::Create(context.LLVM(), "cont", parentFunction);

            llvm::Value* condition = self.GenerateExpressionIR(node.Condition());
            if (node.HasElseIfs()) {
                context.IRBuilder().CreateCondBr(condition, thenBlock, std::get<0>(elseIfLlvmBlocks[0]));
            } else if (node.HasElse()) {
                context.IRBuilder().CreateCondBr(condition, thenBlock, elseBlock);
            } else {
                context.IRBuilder().CreateCondBr(condition, thenBlock, contBlock);
            }

            context.IRBuilder().SetInsertPoint(thenBlock);
            for (auto& thenStatement: node.ThenStatements()) {
                thenStatement->Accept(*this);
            }
            context.IRBuilder().CreateBr(contBlock);

            if (node.HasElseIfs()) {
                size_t size = node.ElseIfBlocks().size();
                for (size_t i = 0; i < size; i++) {
                    auto& elseIf = node.ElseIfBlocks()[i];
                    auto [llvmCheckBlock, llvmDoBlock] = elseIfLlvmBlocks[i];
                    llvm::BasicBlock* nextBlock = i != size - 1 ?
                                                  std::get<0>(elseIfLlvmBlocks[i + 1]) :
                                                  node.HasElse() ? elseBlock : contBlock;

                    context.IRBuilder().SetInsertPoint(llvmCheckBlock);
                    llvm::Value* elseIfCondition = self.GenerateExpressionIR(elseIf.Condition());
                    context.IRBuilder().CreateCondBr(elseIfCondition, llvmDoBlock, nextBlock);

                    context.IRBuilder().SetInsertPoint(llvmDoBlock);
                    for (auto& elseIfStatement: elseIf.Statements()) {
                        elseIfStatement->Accept(*this);
                    }
                    context.IRBuilder().CreateBr(contBlock);
                }
            }

            if (node.HasElse()) {
                context.IRBuilder().SetInsertPoint(elseBlock);
                for (auto& elseStatement: node.ElseStatements()) {
                    elseStatement->Accept(*this);
                }
                context.IRBuilder().CreateBr(contBlock);
            }

            context.IRBuilder().SetInsertPoint(contBlock);
        }

        CompilationContext& context;
        PadusetCompiler& self;
    };

    Visitor(this).VisitNode(statement);
}


void PadusetCompiler::CleanExpression(ExpressionNode& expression) {
    auto* compiler = expression.FindMeta<ExpressionCompilerNodeMeta>();
    if (!compiler->temporary) {
        return;
    }
    auto* semantic = expression.FindMeta<ExpressionSemanticNodeMeta>();
    if (semantic->ContextualType() == ValueType::String) {
        auto deleteFunc = context.StandardLib().StringDelete();
        context.IRBuilder().CreateCall(deleteFunc, compiler->contextualValue);
    }
}

llvm::Value* PadusetCompiler::ToString(llvm::Value* sourceVal, ValueType sourceType) {
    if (sourceType == ValueType::Integer) {
        auto stackVar = context.IRBuilder().CreateAlloca(context.StandardLib().StringType(), nullptr, "int_to_str");
        context.IRBuilder().CreateCall(context.StandardLib().StringFromInteger(), {sourceVal, stackVar});
        return stackVar;
    } else if (sourceType == ValueType::Real) {
        auto stackVar = context.IRBuilder().CreateAlloca(context.StandardLib().StringType(), nullptr, "real_to_str");
        context.IRBuilder().CreateCall(context.StandardLib().StringFromReal(), {sourceVal, stackVar});
        return stackVar;
    } else if (sourceType == ValueType::Boolean) {
        auto stackVar = context.IRBuilder().CreateAlloca(context.StandardLib().StringType(), nullptr, "bool_to_str");
        context.IRBuilder().CreateCall(context.StandardLib().StringFromBool(), {sourceVal, stackVar});
        return stackVar;
    } else if (sourceType == ValueType::String) {
        return sourceVal;
    } else {
        throw std::runtime_error("Unable to convert to string.");
    }
}

NativeCompiledProgram
PadusetCompiler::CompileNative(const IRCompiledProgram& compiled, const std::filesystem::path& path) {
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();

    std::string err;
    if (auto target = llvm::TargetRegistry::lookupTarget(targetTriple, err)) {
        llvm::TargetOptions options;
        auto* targetMachine = target->createTargetMachine(targetTriple, "generic", "", options,
                                                          llvm::Optional<llvm::Reloc::Model>());

        compiled.module->setDataLayout(targetMachine->createDataLayout());
        compiled.module->setTargetTriple(targetTriple);

        std::error_code errorCode;
        llvm::raw_fd_ostream objectFile{path.string(), errorCode};
        if (errorCode) {
            throw std::runtime_error(errorCode.message());
        }

        llvm::legacy::PassManager passManager;
        if (targetMachine->addPassesToEmitFile(passManager, objectFile, nullptr, llvm::CGFT_ObjectFile)) {
            // When this return true, something went wrong... Which doesn't make much sense but whatever.
            throw std::runtime_error("Cannot emit an object file with the target machine.");
        }

        passManager.run(*compiled.module);
        objectFile.flush();

        return NativeCompiledProgram{path};
    } else {
        throw std::runtime_error(err);
    }
}
