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
  explicit C2JuliaVisitor(ASTContext *Context)
      : Context(Context), EscapingSet(), isAddrOf(false) {}

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

    outs() << "@C function " << getEscapedName(d) << '(';
    bool first = true;
    for (auto p : d->parameters()) {
      if (!first)
        outs() << ", ";
      first = false;
      outs() << getEscapedName(p) << "::" << getTypeName(p->getOriginalType());
    }
    outs() << ")::" << getTypeName(d->getReturnType()) << "\n";

    // Parameters are also local variables
    for (auto p : d->parameters()) {
      if (!inRefSet(p))
        continue;
      outs() << getEscapedName(p) << " = Ref(";
      outs() << getEscapedName(p) << ")\n";
    }

    if (const Stmt *b = d->getBody()) {
      for (const Stmt *c : b->children()) {
        Visit(c);
        outs() << "\n";
      }
    }

    outs() << "end\n";
    return;
  }

  void VisitCompoundStmt(const CompoundStmt *s) {
    // The case where a CompoundStmt is actually just a container will be
    // handled explicitly in respective methods for its parent type.
    outs() << "let\n";
    VisitCompoundNoLet(s);
    outs() << "end";
  }

  void VisitDeclStmt(const DeclStmt *s) {
    for (const Decl *c : s->decls()) {
      Visit(c);
    }
  }

  void VisitVarDecl(const VarDecl *d) {
    if (inRefSet(d)) {
      outs() << getEscapedName(d) << " = Ref{";
      outs() << getTypeName(d->getType());
      outs() << "}()";
      if (d->hasInit()) {
        outs() << "\n" << getEscapedName(d) << "[] = ";
        Visit(d->getInit());
      }
    } else {
      outs() << getEscapedName(d) << "::";
      outs() << getTypeName(d->getType()) << " = ";
      if (d->hasInit()) {
        Visit(d->getInit());
      } else {
        outs() << "undef";
      }
    }
  }

  void VisitImplicitCastExpr(const ImplicitCastExpr *e) {
    // Unfortunately, StmtVisitor does not handle cast expressions. So I handle
    // them explicitly.
    llvm::SmallString<32> after;
    switch (e->getCastKind()) {
    case CK_LValueToRValue:
    case CK_NoOp:
    case CK_ArrayToPointerDecay:
    case CK_IntegralCast:
      // Ignore
      break;

      // after = " % ";
      // after += getTypeName(e->getType());
      // break;

    case CK_BitCast:
      //! TODO: Work out the real rule here
      // if (e->getType()->isPointerType() && !e->getType()->isVoidPointerType()) {
      //   outs() << getTypeName(e->getType()) << "(";
      //   after = ")";
      // }
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
      outs() << "@post_inc(";
      after = ")";
      break;
    case UO_PostDec:
      outs() << "@post_dec(";
      after = ")";
      break;
    case UO_Deref:
      after = "[]";
      break;
    // case UO_AddrOf:
    //   // See VisitUnaryAddrOf
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
    auto *subexpr = o->getSubExpr();
    auto *ref = dyn_cast<DeclRefExpr>(subexpr);
    if (ref) {
      // Maybe just incorporate into @pointer?
      isAddrOf = true;
      VisitDeclRefExpr(ref);
      isAddrOf = false;
      return;
    } else if (auto *memref = dyn_cast<MemberExpr>(subexpr)) {
      outs() << "@pointer(";
      VisitMemberExpr(memref);
      outs() << ")";
      return;
    } else if (auto *arrayref = dyn_cast<ArraySubscriptExpr>(subexpr)) {
      outs() << "@pointer(";
      VisitArraySubscriptExpr(arrayref);
      outs() << ")";
      return;
    }
    errs() << "Subexpression of & must be a reference to a variable\n";
    o->dump();
    abort();
  }

  void VisitMemberExpr(const MemberExpr *e) {
    Visit(e->getBase());
    outs() << "." << getEscapedName(e->getMemberDecl());
  }

  void VisitDeclRefExpr(const DeclRefExpr *e) {
    auto *decl = e->getDecl();
    outs() << getEscapedName(decl);
    if (!isAddrOf && inRefSet(decl))
      outs() << "[]";
  }

  void VisitCompoundAssignOperator(const CompoundAssignOperator *o) {
    auto lhs = o->getLHS();
    auto rhs = o->getRHS();
    auto type = lhs->getType();
    std::string typeName;
    if (!type->isPointerType()) {
      typeName = getTypeName(type);
    }

    auto op = BinaryOperator::getOpcodeStr(o->getOpcode());
    // We need to make sure the types match the assigned-to (left) operand.
    //! TODO: Use less type conversions and check whether there are non-integer
    //! conversions
    //! WARNING: a %= 3 (a is unsigned char) will be a %= 3 % UInt8, which is
    //! incorrect. It must be a = (a % Int(3)) % UInt8.
    Visit(lhs);
    outs() << " " << op << "(";
    Visit(rhs);
    outs() << ") % " << typeName;
  }

  void VisitBinaryOperator(const BinaryOperator *o) {
    auto op = BinaryOperator::getOpcodeStr(o->getOpcode());
    outs() << "(";
    Visit(o->getLHS());
    outs() << ")" << op << "(";
    Visit(o->getRHS());
    outs() << ")";
  }

  void VisitBinAssign(const BinaryOperator *o) {
    auto lhs = o->getLHS();
    auto rhs = o->getRHS();
    auto type = getTypeName(lhs->getType());
    auto op = BinaryOperator::getOpcodeStr(o->getOpcode());
    bool sameType =
        lhs->getType().getCanonicalType() == rhs->getType().getCanonicalType();
    Visit(lhs);
    outs() << " " << op << " ";
    if (!sameType)
      outs() << type << "(";
    Visit(rhs);
    if (!sameType)
      outs() << ")";
  }

  void VisitIntegerLiteral(const IntegerLiteral *l) {
    auto v = l->getValue();
    outs() << v.toString(10, true);
  }

  void VisitStringLiteral(const clang::StringLiteral *l) {
    // outs() << '\"' << l->getString() << '\"';
    l->outputString(outs());
  }

  void VisitCharacterLiteral(const CharacterLiteral *l) {
    outs() << l->getValue();
  }

  void VisitCallExpr(const CallExpr *e) {
    if (auto func = e->getDirectCallee()) {
      outs() << getEscapedName(func);
    } else {
      // What about (funcarray + 2)(1, 2)
      outs() << "(";
      Visit(e->getCallee());
      outs() << ")";
    }
    outs() << "(";
    for (size_t i = 0; i < e->getNumArgs(); i++) {
      if (i != 0) {
        outs() << ", ";
      }
      Visit(e->getArg(i));
    }
    outs() << ")";
  }

  void VisitIfStmt(const IfStmt *s) {
    outs() << "if @bool ";
    Visit(s->getCond());
    outs() << "\n";
    VisitCompoundNoLet(s->getThen());

    const Stmt *elseclause = s->getElse();
    while (elseclause) {
      if (auto *nestedif = dyn_cast<IfStmt>(elseclause)) {
        outs() << "elseif @bool ";
        Visit(nestedif->getCond());
        outs() << "\n";
        VisitCompoundNoLet(nestedif->getThen());
        elseclause = nestedif->getElse();
      } else {
        outs() << "else\n";
        VisitCompoundNoLet(elseclause);
        break;
      }
    }
    outs() << "end";
  }

  void VisitForStmt(const ForStmt *s) {
    outs() << "@cfor ";
    Visit(s->getInit());
    outs() << " @bool(";
    Visit(s->getCond());
    outs() << ") ";
    Visit(s->getInc());
    outs() << " ";
    Visit(s->getBody());
  }

  void VisitWhileStmt(const WhileStmt *s) {
    outs() << "while @bool ";
    Visit(s->getCond());
    outs() << "\n";
    VisitCompoundNoLet(s->getBody());
    outs() << "end";
  }

  void VisitBreakStmt(const BreakStmt *s) { outs() << "break"; }

  void VisitReturnStmt(const ReturnStmt *s) {
    outs() << "return ";
    auto *retval = s->getRetValue();
    if (retval)
      Visit(s->getRetValue());
  }

  void VisitArraySubscriptExpr(const ArraySubscriptExpr *e) {
    Visit(e->getBase());
    outs() << "[";
    Visit(e->getIdx());
    outs() << "]";
  }

  void VisitUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr *e) {
    outs() << "sizeof(" << getTypeName(e->getArgumentType()) << ")";
  }

  void VisitParenExpr(const ParenExpr *e) { Visit(e->getSubExpr()); }
  void VisitCStyleCastExpr(const CStyleCastExpr *e) { Visit(e->getSubExpr()); }

  void Visit(const ASTContext *ctx) {
    outs() << "baremodule C\nusing C2Julia\n";
    for (Decl *d : Context->getTranslationUnitDecl()->decls()) {
      Visit(d);
      outs() << "\n";
    }
    outs() << "end\n";
  }

  // Visit a single statement, or multiple statements wrapped in a CompoundStmt.
  // Like VisitCompoundStmt but does not add let ... end
  void VisitCompoundNoLet(const Stmt *s) {
    if (isa<CompoundStmt>(s)) {
      for (const Stmt *c : s->children()) {
        Visit(c);
        outs() << "\n";
      }
    } else {
      Visit(s);
      outs() << "\n";
    }
  }

  bool isMainFile(const Decl *d) const {
    auto &&mgr = Context->getSourceManager();
    auto entry = mgr.getFileEntryForID(mgr.getFileID(d->getLocation()));
    return entry && mgr.isMainFile(*entry);
  }

  auto &getEscapingSet() { return EscapingSet; }

  bool inRefSet(const Decl *d) const {
    return std::binary_search(EscapingSet.cbegin(), EscapingSet.cend(), d);
  }

  SmallString<32> getEscapedName(const NamedDecl *d) {
    SmallString<32> name;
    if (JuliaKeywordSet.contains(d->getName())) {
      name = "__";
    }
    name += d->getName();
    return name;
  }

  static std::string getTypeName(const QualType &type) {
    std::string res;

    if (type->isIntegerType() && type->isBuiltinType()) {
      if (type->isUnsignedIntegerType()) {
        res = type.getAsString();
        res[sizeof("unsigned")-1] = '_';
      } else {
        res = type.getAsString();
      }
    } else if (type->isVoidPointerType()) {
      res = "Any";
    } else if (type->isPointerType()) {
      res = "Pointer{";
      res += getTypeName(type->getPointeeType());
      res += "}";
    } else if (type->isVoidType()) {
      res = "Cvoid";
    } else {
      // TODO: more types
      res = type.getAsString();
    }

    return res;
  }

private:
  ASTContext *Context;
  // Set of variables escaping by the & operator
  std::vector<const Decl *> EscapingSet;

  // See VisitUnaryAddrOf
  bool isAddrOf;

  StringSet<> JuliaKeywordSet{
      "baremodule", "begin", "break", "catch", "const", "continue", "do",
      "else", "elseif", "end", "export", "false", "finally", "for", "function",
      "global", "if", "import", "let", "local", "macro", "module", "quote",
      "return", "struct", "true", "try", "using", "while",
      // Not keywords, but julia functions
      "Ref", "ref"};
};

// I must know in advance whether a local variable "leaks" by pointers. Instead
// of building external dictionaries, maybe I could reuse a few bits in the Decl
// type.
class ReferenceMarker : public clang::RecursiveASTVisitor<ReferenceMarker> {
public:
  ReferenceMarker(std::vector<const Decl *> &rs) : RefSet(rs) {}

  bool VisitUnaryOperator(UnaryOperator *o) {
    if (o->getOpcode() == UO_AddrOf) {
      // TODO: what are the posible subexpressions?
      auto *ref = dyn_cast<DeclRefExpr>(o->getSubExpr());
      if (ref) {
        RefSet.push_back(ref->getDecl());
      }
    }
    return true;
  }

  void sortRefSet() {
    std::sort(RefSet.begin(), RefSet.end());
    auto end = std::unique(RefSet.begin(), RefSet.end());
    RefSet.resize(end - RefSet.begin());
  }

private:
  std::vector<const Decl *> &RefSet;
};

class C2JuliaConsumer : public clang::ASTConsumer {
public:
  explicit C2JuliaConsumer(ASTContext *Context) : Visitor(Context) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    auto &rs = Visitor.getEscapingSet();
    ReferenceMarker rm(rs);
    rm.TraverseAST(Context);
    rm.sortRefSet();

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