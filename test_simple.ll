; Simplified test — function with arguments, no alloca/store/load
define i32 @test_args(i32 %a, i32 %b) {
  %sum = add i32 %a, %b
  ret i32 %sum
}
