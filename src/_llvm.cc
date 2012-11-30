/* Implemented patch contents from LLVM mail list per
 * http://lists.cs.uiuc.edu/pipermail/llvmdev/2008-May/014744.html
 */

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

#include <llvm/Intrinsics.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Linker.h>

#include <vector>


extern "C" {

    void
    LLVMParseEnvironmentOptions(const char * Program, const char * Var, const char * Overview) {
        llvm::cl::ParseEnvironmentOptions(Program, Var, Overview);
    }

    const char *
    LLVMGetModuleName(LLVMModuleRef M) {
        return llvm::unwrap(M)->getModuleIdentifier().c_str();
    }

    char *
    LLVMDumpModuleToString(LLVMModuleRef M) {
        std::string out;
        llvm::raw_string_ostream stream(out);
        llvm::unwrap(M)->print(stream, 0);
        return strdup(out.c_str());
    }

    LLVMValueRef
    LLVMGetIntrinsicDeclaration(LLVMModuleRef M, unsigned ID, LLVMTypeRef *ParamTypes, unsigned ParamCount) {
        std::vector<llvm::Type*> Tys;
        for (LLVMTypeRef *I = ParamTypes, *E = ParamTypes + ParamCount; I != E; ++I) {
            Tys.push_back(llvm::unwrap(*I));
        }
        return llvm::wrap(llvm::Intrinsic::getDeclaration
                          (llvm::unwrap(M), llvm::Intrinsic::ID(ID),
                           llvm::ArrayRef<llvm::Type*>(Tys)));
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

    /**
     * See llvm::sys::getDefaultTargetTriple()
     *
     * Result must be freed with LLVMDisposeMessage.
     */
    char *
    LLVMGetDefaultTargetTriple__() {
        return strdup(llvm::sys::getDefaultTargetTriple().c_str());
    }

    LLVMModuleRef
    LLVMGetParentModule__(LLVMBuilderRef B) {
        llvm::Module *M = llvm::unwrap(B)->GetInsertBlock()->getParent()->getParent();
        return llvm::wrap(M);
    }


    /**
     * See TargetRegistry::lookupTarget()
     *
     * Returns null pointer if cannot find appropriate target; in such
     * case, *error must be freed with LLVMDisposeMessage()
     */
    LLVMTargetRef
    LLVMLookupTarget__(char * triple, char ** error) {
        std::string s;
        const llvm::Target * target = llvm::TargetRegistry::lookupTarget(triple, s);
        if (target == 0) {
            *error = strdup(s.c_str());
        }
        return llvm::wrap(target);
    }


    /* Copied from LLVM 3.2 trunk */

    enum LLVMLinkerMode__ {
      DestroySource = 0, // Allow source module to be destroyed.
      PreserveSource = 1 // Preserve the source module.
    };

    LLVMBool
    LLVMLinkModules__(LLVMModuleRef Dest, LLVMModuleRef Src, LLVMLinkerMode__ Mode, char **OutMessages) {
        std::string Messages;
        LLVMBool Result = llvm::Linker::LinkModules
            (llvm::unwrap(Dest), llvm::unwrap(Src), Mode, OutMessages? &Messages : 0);
        if (OutMessages)
            *OutMessages = strdup(Messages.c_str());
        return Result;
    }


}
