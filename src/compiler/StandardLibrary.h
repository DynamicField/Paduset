//
// Created by jeuxj on 03/11/2022.
//

#ifndef PADUSET_STANDARDLIBRARY_H
#define PADUSET_STANDARDLIBRARY_H

#include <memory>
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
namespace Paduset {
    class StandardLibrary {
    public:
        StandardLibrary(llvm::LLVMContext& context, llvm::Module& rootModule);

        std::unique_ptr<llvm::Module>& ModulePtr() {
            return module;
        }

        // For now, this module doesn't serve any purpose.
        // When we'll add function with bodies to that module, it will be really useful.
        llvm::Module& Module() const {
            return *module;
        }

        llvm::Type* IntegerType() const {
            return integerType;
        }

        llvm::Type* RealType() const {
            return realType;
        }

        llvm::Type* BoolType() const {
            return boolType;
        }

        llvm::Type* CBoolType() const {
            return cBoolType;
        }

        llvm::StructType* StringType() const {
            return stringType;
        }

        llvm::Function* Hi() const {
            return hi;
        }

        llvm::Function* StringDelete() const {
            return stringDelete;
        }

        llvm::Function* StringDeleteTemp() const {
            return stringDeleteTemp;
        }

        llvm::Function* StringMake() const {
            return stringMake;
        }

        llvm::Function* StringMakeDefault() const {
            return stringMakeDefault;
        }

        llvm::Function* StringFromReal() const {
            return stringFromReal;
        }

        llvm::Function* StringFromInteger() const {
            return stringFromInteger;
        }

        llvm::Function* StringFromBool() const {
            return stringFromBool;
        }

        llvm::Function* StringEqualsP() const {
            return stringEqualsP;
        }

        llvm::Function* StringEqualsC() const {
            return stringEqualsC;
        }

        llvm::Function* StringEqualsCC() const {
            return stringEqualsCC;
        }

        llvm::Function* StringAppendP() const {
            return stringAppendP;
        }

        llvm::Function* StringAppendC() const {
            return stringAppendC;
        }

        llvm::Function* StringAssign() const {
            return stringAssign;
        }

        llvm::Function* Printf() const {
            return printf;
        }

        llvm::Function* ReadInt() const {
            return readInt;
        }

        llvm::Function* ReadReal() const {
            return readReal;
        }

        llvm::Function* ReadString() const {
            return readString;
        }

        llvm::IRBuilder<>& Builder() {
            return builder;
        }

    private:
        llvm::Module& rootModule;
        std::unique_ptr<llvm::Module> module{nullptr};
        llvm::IRBuilder<> builder;
        llvm::LLVMContext& context;

        llvm::Type* pointerType;
        llvm::Type* integerType;
        llvm::Type* realType;
        llvm::StructType* stringType;
        llvm::Type* boolType;
        llvm::Type* cBoolType;
        llvm::Type* voidType;

        llvm::Function* hi;

        llvm::Function* stringDelete;
        llvm::Function* stringDeleteTemp;
        llvm::Function* stringMake;
        llvm::Function* stringMakeDefault;
        llvm::Function* stringAppendP;
        llvm::Function* stringAppendC;
        llvm::Function* stringAssign;
        llvm::Function* stringFromReal;
        llvm::Function* stringFromInteger;
        llvm::Function* stringFromBool;
        llvm::Function* stringEqualsP;
        llvm::Function* stringEqualsC;
        llvm::Function* stringEqualsCC;
        llvm::Function* printf;

        llvm::Function* readInt;
        llvm::Function* readReal;
        llvm::Function* readString;

        // Null terminated & has length.
        llvm::StructType* MakeStringType();
        llvm::Function* MakeStringFromBoolFunction();
        llvm::Function* ExternalFunc(llvm::FunctionType* funcType, const char* name);
    };
}

#endif //PADUSET_STANDARDLIBRARY_H
