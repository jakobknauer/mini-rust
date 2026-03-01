; ModuleID = 'stdlib'
source_filename = "stdlib"
target triple = "x86_64-pc-linux-gnu"

define i1 @"not::<bool>"(i1 %0) {
entry:
  %1 = xor i1 %0, true
  ret i1 %1
}

define i32 @"neg::<i32>"(i32 %0) {
entry:
  %1 = sub i32 0, %0
  ret i32 %1
}
