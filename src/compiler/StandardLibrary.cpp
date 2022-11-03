//
// Created by jeuxj on 03/11/2022.
//

#include "StandardLibrary.h"
#include "llvm/IR/IRBuilder.h"

using namespace Paduset;

StandardLibrary::StandardLibrary(llvm::LLVMContext& context, llvm::Module& rootModule) :
        context{context},
        builder{context},
        module{std::make_unique<llvm::Module>("standard", context)},
        rootModule{rootModule},
        pointerType{llvm::PointerType::get(context, 0)},
        integerType{llvm::Type::getInt32Ty(context)},
        realType{llvm::Type::getFloatTy(context)},
        boolType{llvm::Type::getInt1Ty(context)},
        cBoolType{llvm::Type::getInt8Ty(context)},
        voidType{llvm::Type::getVoidTy(context)},
        stringType{MakeStringType()},
        hi(ExternalFunc(llvm::FunctionType::get(llvm::Type::getVoidTy(context), false), "PadusetCore_Hi")),
        stringMake(ExternalFunc(llvm::FunctionType::get(voidType, {pointerType, pointerType}, false),
                                "PadusetString_Make")),
        stringMakeDefault(
                ExternalFunc(llvm::FunctionType::get(voidType, {pointerType}, false), "PadusetString_MakeDefault")),
        stringDelete(ExternalFunc(llvm::FunctionType::get(llvm::Type::getVoidTy(context), {pointerType}, false),
                                  "PadusetString_Delete")),
        stringDeleteTemp(ExternalFunc(llvm::FunctionType::get(llvm::Type::getVoidTy(context), {stringType}, false),
                                      "PadusetString_DeleteTemp")),
        stringAppendP(ExternalFunc(llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::PointerType::get(context, 0), llvm::PointerType::get(context, 0)},
                false
        ), "PadusetString_AppendP")),
        stringAppendC(ExternalFunc(llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::PointerType::get(context, 0), llvm::PointerType::get(context, 0)},
                false
        ), "PadusetString_AppendC")),
        stringAssign(ExternalFunc(llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::PointerType::get(context, 0), llvm::PointerType::get(context, 0)},
                false
        ), "PadusetString_Assign")),
        stringFromInteger(ExternalFunc(llvm::FunctionType::get(voidType, {integerType, pointerType}, false),
                                       "PadusetString_FromInteger")),
        stringFromReal(ExternalFunc(llvm::FunctionType::get(voidType, {realType, pointerType}, false),
                                    "PadusetString_FromReal")),
        stringFromBool(MakeStringFromBoolFunction()),
        stringEqualsP(ExternalFunc(llvm::FunctionType::get(cBoolType, {pointerType, pointerType}, false),
                                   "PadusetString_EqualsP")),
        stringEqualsC(ExternalFunc(llvm::FunctionType::get(cBoolType, {pointerType, pointerType}, false),
                                   "PadusetString_EqualsC")),
        stringEqualsCC(ExternalFunc(llvm::FunctionType::get(cBoolType, {pointerType, pointerType}, false),
                                   "PadusetString_EqualsCC")),
        readInt(ExternalFunc(llvm::FunctionType::get(integerType, {}, false), "PadusetConsole_ReadInt")),
        readReal(ExternalFunc(llvm::FunctionType::get(realType, {}, false), "PadusetConsole_ReadReal")),
        readString(ExternalFunc(llvm::FunctionType::get(voidType, {pointerType}, false), "PadusetConsole_ReadString")),
        printf(ExternalFunc(
                llvm::FunctionType::get(llvm::Type::getVoidTy(context), llvm::PointerType::get(context, 0), true),
                "PadusetConsole_Printf")) {
}

llvm::StructType* StandardLibrary::MakeStringType() {
    return llvm::StructType::create(context, {llvm::PointerType::get(context, 0), llvm::Type::getInt32Ty(context),
                                              llvm::Type::getInt32Ty(context)},
                                    "PadusetString");
}

llvm::Function* StandardLibrary::ExternalFunc(llvm::FunctionType* funcType, const char* name) {
    auto* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, rootModule);
    function->setCallingConv(llvm::CallingConv::C);

    return function;
}

llvm::Function* StandardLibrary::MakeStringFromBoolFunction() {
    auto functionType = llvm::FunctionType::get(voidType, {boolType, pointerType}, false);
    auto externalFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                                                   "StdLib_PadusetString_FromBool", rootModule);
    auto function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                                           "StdLib_PadusetString_FromBool", module.get());


    auto trueStr = builder.CreateGlobalStringPtr("VRAI", "_trueBoolStr", 0, module.get());
    auto falseStr = builder.CreateGlobalStringPtr("FAUX", "_falseBoolStr", 0, module.get());

    auto boolValue = function->getArg(0);
    auto outputString = function->getArg(1);

    auto ifBlock = llvm::BasicBlock::Create(context, "if", function);
    auto thenBlock = llvm::BasicBlock::Create(context, "then", function);
    auto elseBlock = llvm::BasicBlock::Create(context, "else", function);

    builder.SetInsertPoint(ifBlock);
    builder.CreateCondBr(boolValue, thenBlock, elseBlock);

    builder.SetInsertPoint(thenBlock);
    builder.CreateCall(stringMake, {trueStr, outputString});
    builder.CreateRetVoid();

    builder.SetInsertPoint(elseBlock);
    builder.CreateCall(stringMake, {falseStr, outputString});
    builder.CreateRetVoid();

    builder.ClearInsertionPoint();

    return externalFunction;
}
