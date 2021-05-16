#include "clang/AST/ASTConsumer.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
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
static llvm::cl::OptionCategory MyToolCategory("c2julia options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static llvm::cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static llvm::cl::extrahelp MoreHelp("\nMore help text...\n");

// A visitor to leverage the dispatcher of RecursiveASTVisitor
// But I need to manually call traverse so that I do not have to
// manage context explicitly, so most of these visitor functions
// return false to terminate the traverse process. I.e, Only the
// dispatcher is used.
// Everything is just dumped to stdout for this moment.
class C2JuliaVisitor : public ConstStmtVisitor<C2JuliaVisitor>,
                       public ConstDeclVisitor<C2JuliaVisitor> {
public:
  explicit C2JuliaVisitor(ASTContext *Context) : Context(Context) {}

  auto &&outs() { return llvm::outs(); }
  auto &&errs() { return llvm::errs(); }

  // Expose shadowed methods
  void Visit(const Decl *d) {
    return ConstDeclVisitor<C2JuliaVisitor>::Visit(d);
  }
  void Visit(const Stmt *s) {
    return ConstStmtVisitor<C2JuliaVisitor>::Visit(s);
  }

  void VisitDecl(const Decl *d) {
    if (!isMainFile(d))
      return;
    errs() << d->getDeclKindName() << "Decl is not implemented.\n";
    d->dump(errs());
  }
  void VisitStmt(const Stmt *s) {
    errs() << s->getStmtClassName() << " is not implemented.\n";
    s->dump(errs(), *Context);
  }

  void VisitFunctionDecl(const FunctionDecl *d) {
    if (!isMainFile(d))
      return;

    outs() << "function " << d->getName() << '(';
    bool first = true;
    for (auto p : d->parameters()) {
      if (!first)
        outs() << ", ";
      first = false;
      // TODO: Use real types
      outs() << p->getName() << "::" << p->getOriginalType().getAsString();
    }
    outs() << ")\n";

    if (const Stmt *b = d->getBody()) {
      for (const Stmt *c : b->children()) {
        Visit(c);
      }
    }

    outs() << "end\n";
    return;
  }

  void VisitCompoundStmt(const CompoundStmt *s) {
    // The case where a CompoundStmt is actually just a container will be
    // handled explicitly in respective methods for its parent type.
    outs() << "let\n";
    for (const Stmt *c : s->children()) {
      Visit(c);
      outs() << "\n";
    }
    outs() << "end\n";
  }

  void VisitDeclStmt(const DeclStmt *s) {
    for (const Decl *c : s->decls()) {
      Visit(c);
    }
  }

  void VisitVarDecl(const VarDecl *d) {
    outs() << d->getName();
    if (d->hasInit()) {
      outs() << " = ";
      Visit(d->getInit());
    }
    if (d->isEscapingByref()) {
      errs() << d->getName() << " is escapeing by ref.\n";
    }
  }

  void VisitImplicitCastExpr(const ImplicitCastExpr *e) {
    // Unfortunately, StmtVisitor does not handle cast expressions. So I handle
    // them explicitly.
    llvm::SmallString<32> after;
    switch (e->getCastKind()) {
    case CK_LValueToRValue:
      // Ignore
      break;

    case CK_IntegralCast:
      outs() << "(";
      after = " % C";
      if (e->getType()->isSignedIntegerType()) {
        after += "u";
      }
      // TODO: Is this safe?
      after += e->getType().getAsString();
      after += ")";
      break;

    default:
      errs() << "Unhandled cast kind: " << e->getCastKindName() << "\n";
    }
    Visit(e->getSubExpr());
    outs() << after;
  }

  void VisitUnaryOperator(const UnaryOperator *o) {
    auto opname = UnaryOperator::getOpcodeStr(o->getOpcode());
    llvm::SmallString<16> after;
    switch (o->getOpcode()) {
    case UO_PreInc:
      outs() << "(";
      after = " += 1)";
      break;
    case UO_PreDec:
      outs() << "(";
      after = " -= 1)";
      break;
    case UO_PostInc:
      outs() << "(@post_inc ";
      after = ")";
      break;
    case UO_PostDec:
      outs() << "(@post_dec ";
      after = ")";
      break;
    case UO_Deref:
      after = "[]";
      break;
    // case UO_AddrOf:
    //   // Not handled
    //   break;
    case UO_LNot:
    case UO_Not:
    case UO_Minus:
    case UO_Plus:
      outs() << opname;
      break;
    default:
      errs() << "Unhandled unary operator: " << opname << "\n";
    }
    Visit(o->getSubExpr());
    outs() << after;
  }

  void VisitUnaryAddrOf(const UnaryOperator *o) {
    auto *ref = dyn_cast<DeclRefExpr>(o->getSubExpr());
    if (!ref) {
      errs() << "Subexpression of & must be a reference to a variable\n";
      abort();
    }
    auto *decl = dyn_cast<VarDecl>(ref->getDecl());
    if (decl) {
      if (decl->isEscapingByref()) {
        // This is the special case, do not call Visit
        // Visit(decl);
        outs() << decl->getName();
      } else {
        errs() << "A use of & operator is not analysed.\n";
      }
    } else {
      errs() << "Subexpression of & must be a reference to a variable\n";
      abort();
    }
  }

  void VisitDeclRefExpr(const DeclRefExpr *e) {
    outs() << e->getDecl()->getName();
    auto *decl = dyn_cast<VarDecl>(e->getDecl());
    if (decl && decl->isEscapingByref())
      outs() << "[]";
  }

  void Visit(const ASTContext *ctx) {
    for (Decl *d : Context->getTranslationUnitDecl()->decls()) {
      Visit(d);
    }
  }

  bool isMainFile(const Decl *d) {
    auto &&mgr = Context->getSourceManager();
    auto entry = mgr.getFileEntryForID(mgr.getFileID(d->getLocation()));
    return entry && mgr.isMainFile(*entry);
  }

private:
  ASTContext *Context;
};

// I must know in advance whether a local variable "leaks" by pointers. Instead
// of building external dictionaries, maybe I could reuse a few bits in the Decl
// type.
class ReferenceMarker : public clang::RecursiveASTVisitor<ReferenceMarker> {
public:
  bool VisitDeclRefExpr(DeclRefExpr *e) {
    VarDecl *d = dyn_cast<VarDecl>(e->getDecl());
    if (!d) {
      llvm::errs() << e->getDecl()->getName() << " is not a VarDecl.\n";
      return true;
    }
    if (d->isEscapingByref()) {
      llvm::errs() << d->getName() << "'s attribute isEscapingByUse is true!\n";
      abort();
      return true;
    }
    d->setEscapingByref();
    llvm::outs() << d->getName() << " is escaping by ref: " << d->isEscapingByref() << '\n';
    return true;
  }
};

class C2JuliaConsumer : public clang::ASTConsumer {
public:
  explicit C2JuliaConsumer(ASTContext *Context) : Visitor(Context) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    ReferenceMarker rm;
    rm.TraverseAST(Context);
    Visitor.Visit(&Context);
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