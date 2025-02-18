@arr = global [3 x i64] [i64 10, i64 20, i64 30]

define i64 @sum_array(i64* %arr_ptr) {
  %x1 = getelementptr i64, i64* %arr_ptr, i64 0
  %val1 = load i64, i64* %x1
  
  %x2 = getelementptr i64, i64* %arr_ptr, i64 1
  %val2 = load i64, i64* %x2
  
  %x3 = getelementptr i64, i64* %arr_ptr, i64 2
  %val3 = load i64, i64* %x3

  %1 = add i64 %val1, %val2
  %2 = add i64 %1, %val3

  ret i64 %2
}

define i64 @main(i64 %argc, i8** %argv) {
entry:
  %arr_ptr = getelementptr [3 x i64], [3 x i64]* @arr, i64 0, i64 0
  %sum_result = call i64 @sum_array(i64* %arr_ptr)

  %bool = icmp sgt i64 %sum_result, 100
  br i1 %bool, label %if_true, label %if_false

if_true:
  %first_elem_ptr = getelementptr [3 x i64], [3 x i64]* @arr, i64 0, i64 0
  %first_elem_val = load i64, i64* %first_elem_ptr
  ret i64 %first_elem_val

if_false:
  %last_elem_ptr = getelementptr [3 x i64], [3 x i64]* @arr, i64 0, i64 2
  %last_elem_val = load i64, i64* %last_elem_ptr
  ret i64 %last_elem_val
}