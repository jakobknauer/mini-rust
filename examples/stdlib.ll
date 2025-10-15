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

define i1 @"eq::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp eq i32 %0, %1
  ret i1 %2
}

define i1 @"eq::<bool>"(i1 %0, i1 %1) {
entry:
  %2 = icmp eq i1 %0, %1
  ret i1 %2
}

define i1 @"eq::<()>"({} %0, {} %1) {
entry:
  ret i1 true
}
