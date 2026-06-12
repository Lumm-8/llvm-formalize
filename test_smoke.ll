; Simple smoke test for translateToStp pass
; Tests: arithmetic, store, load, and registerOutput

@.str = private constant [5 x i8] c"tmp_a"

define void @test_simple() {
entry:
  %a = alloca i32, align 4
  store i32 42, ptr %a, align 4
  %val = load i32, ptr %a, align 4
  %sum = add i32 %val, 8
  store i32 %sum, ptr %a, align 4
  ; registerOutput("tmp_a", &a, sizeof(a))
  call void @registerOutput(ptr @.str, ptr %a, i32 4)
  ret void
}

declare void @registerOutput(ptr, ptr, i32)
