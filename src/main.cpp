#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static cl::OptionCategory MyToolCategory("c2julia options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

class C2JuliaVisitor : public RecursiveASTVisitor<C2JuliaVisitor> {
public:
  explicit C2JuliaVisitor(ASTContext *Context) : Context(Context) {}

  bool VisitFunctionDecl(FunctionDecl *d) {
    if (isMainFile(d)) {
      llvm::outs() << d->getName() << '\n';
      auto &&params = d->parameters();
      for (auto &&p : params) {
        llvm::outs() << '\t' << p->getName()
                     << "::" << p->getOriginalType().getAsString() << '\n';
      }
    }

    return true;
  }

  bool isMainFile(Decl *d) {
    auto &&mgr = Context->getSourceManager();
    auto entry = mgr.getFileEntryForID(mgr.getFileID(d->getLocation()));
    return entry && mgr.isMainFile(*entry);
  }

private:
  ASTContext *Context;
};

class C2JuliaConsumer : public clang::ASTConsumer {
public:
  explicit C2JuliaConsumer(ASTContext *Context) : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  C2JuliaVisitor Visitor;
};

class C2JuliaAction : public clang::ASTFrontendAction {
public:
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &Compiler,
                    llvm::StringRef InFile) override {
    return std::make_unique<C2JuliaConsumer>(&Compiler.getASTContext());
  }
};

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(
      argc, argv, MyToolCategory, cl::NumOccurrencesFlag::OneOrMore);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  C2JuliaAction ac;
  return Tool.run(newFrontendActionFactory<C2JuliaAction>().get());
}