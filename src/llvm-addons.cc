/* Implemented patch contents from LLVM mail list per
 * http://lists.cs.uiuc.edu/pipermail/llvmdev/2008-May/014744.html
 */

#include <llvm-c/Core.h>

#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

#if NOS_LLVM_VERSION == 29
#include <llvm/Target/TargetSelect.h>
#elif NOS_LLVM_VERSION == 31
#include <llvm/Support/TargetSelect.h>
#else
#error "Current NOS_LLVM_VERSION is not supported"
#endif

#include <llvm/Intrinsics.h>
#include <llvm/Support/Host.h>

#include <vector>


extern "C" {

    char *
    LLVMDumpModuleToString(LLVMModuleRef M) {
        std::string out;
        llvm::raw_string_ostream stream(out);
        llvm::unwrap(M)->print(stream, 0);
        return strdup(out.c_str());
    }

    LLVMValueRef
    LLVMGetIntrinsicDeclaration(LLVMModuleRef M, unsigned ID, LLVMTypeRef *ParamTypes, unsigned ParamCount) {

#if NOS_LLVM_VERSION == 29
        std::vector<const llvm::Type*> Tys;
        for (LLVMTypeRef *I = ParamTypes, *E = ParamTypes + ParamCount; I != E; ++I) {
            Tys.push_back(llvm::unwrap(*I));
        }
        return llvm::wrap(llvm::Intrinsic::getDeclaration
                          (llvm::unwrap(M), llvm::Intrinsic::ID(ID),
                           &Tys[0], ParamCount));
#elif NOS_LLVM_VERSION == 31
        /* In 3.1, Tys argument has different constness */
        std::vector<llvm::Type*> Tys;
        for (LLVMTypeRef *I = ParamTypes, *E = ParamTypes + ParamCount; I != E; ++I) {
            Tys.push_back(llvm::unwrap(*I));
        }
        return llvm::wrap(llvm::Intrinsic::getDeclaration
                          (llvm::unwrap(M), llvm::Intrinsic::ID(ID),
                           llvm::ArrayRef<llvm::Type*>(Tys)));
#else
#error "Current NOS_LLVM_VERSION is not supported"
#endif

    }

    unsigned int
    LLVMGetIntrinsicCount__() {
        return llvm::Intrinsic::num_intrinsics;
    }

    char *
    LLVMGetIntrinsicName__(unsigned ID) {
        std::string name = llvm::Intrinsic::getName(llvm::Intrinsic::ID(ID));
        return strdup(name.c_str());
    }

    LLVMBool
    LLVMInitializeNativeTarget__(void) {
        /* This is the contents of the original
         * LLVMInitializeNativeTarget, plus initialization
         * of ASM printer */
#ifdef LLVM_NATIVE_TARGET
        LLVM_NATIVE_TARGETINFO();
        LLVM_NATIVE_TARGET();
        LLVM_NATIVE_TARGETMC();
        LLVM_NATIVE_ASMPRINTER();
        LLVM_NATIVE_ASMPARSER();
        return 0;
#else
        return 1;
#endif
    }

    char *
    LLVMGetDefaultTargetTriple__() {
        return strdup(llvm::sys::getDefaultTargetTriple().c_str());
    }
}
