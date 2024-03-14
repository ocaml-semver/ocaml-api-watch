let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]
