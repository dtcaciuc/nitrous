/* Implemented patch contents from LLVM mail list per
 * http://lists.cs.uiuc.edu/pipermail/llvmdev/2008-May/014744.html
 */

#include <llvm-c/Core.h>
#include <llvm/Support/raw_ostream.h>


extern "C" {

    char *
    LLVMDumpModuleToString(LLVMModuleRef M) {
        std::string out;
        llvm::raw_string_ostream stream(out);
        llvm::unwrap(M)->print(stream, 0);
        return strdup(out.c_str());
    }

}
