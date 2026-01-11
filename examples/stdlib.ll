; ModuleID = 'stdlib'
source_filename = "stdlib"
target triple = "x86_64-pc-linux-gnu"

define i32 @"add::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = add i32 %0, %1
  ret i32 %2
}

define i32 @"sub::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = sub i32 %0, %1
  ret i32 %2
}

define i32 @"mul::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = mul i32 %0, %1
  ret i32 %2
}

define i32 @"div::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = sdiv i32 %0, %1
  ret i32 %2
}

define i32 @"rem::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = srem i32 %0, %1
  ret i32 %2
}

define i1 @"eq::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp eq i32 %0, %1
  ret i1 %2
}

define i1 @"ne::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp ne i32 %0, %1
  ret i1 %2
}

define i1 @"eq::<bool>"(i1 %0, i1 %1) {
entry:
  %2 = icmp eq i1 %0, %1
  ret i1 %2
}

define i1 @"ne::<bool>"(i1 %0, i1 %1) {
entry:
  %2 = icmp ne i1 %0, %1
  ret i1 %2
}

define i1 @"eq::<()>"({} %0, {} %1) {
entry:
  ret i1 true
}

define i1 @"ne::<()>"({} %0, {} %1) {
entry:
  ret i1 false
}

define i1 @"bitor::<bool>"(i1 %0, i1 %1) {
entry:
  %2 = or i1 %0, %1
  ret i1 %2
}

define i1 @"bitand::<bool>"(i1 %0, i1 %1) {
entry:
  %2 = and i1 %0, %1
  ret i1 %2
}

define i1 @"lt::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp slt i32 %0, %1
  ret i1 %2
}

define i1 @"gt::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp sgt i32 %0, %1
  ret i1 %2
}

define i1 @"le::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp sle i32 %0, %1
  ret i1 %2
}

define i1 @"ge::<i32>"(i32 %0, i32 %1) {
entry:
  %2 = icmp sge i32 %0, %1
  ret i1 %2
}

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
