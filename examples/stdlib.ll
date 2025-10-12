; ModuleID = 'stdlib'
source_filename = "stdlib"

define i32 @"add::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = add i32 %0, %1
  ret i32 %2
}

define i32 @"mul::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = mul i32 %0, %1
  ret i32 %2
}
