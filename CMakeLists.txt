set(LLVM_LINK_COMPONENTS support)

add_clang_executable(c2julia
  src/main.cpp
  )
target_link_libraries(c2julia
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )