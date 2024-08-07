(*
 Tests
*)

open Concatenator_lib

let test_contains_any list1 list2 should =
  let result = Concatenator_lib.Utilities.contains_any list1 list2 in
  let expected = should in
  Alcotest.(check bool) "list contains the thing" result expected

let test_split_by_delims path splitted_path =
  let result = Utilities.split_by_delimiters Utilities.path_splitters path in
  let expected = splitted_path in
  Alcotest.(check (list string)) "list should be equal" result expected

let test_file_walk path =
  let result =
    File_manager.walk_files path
      ~filter:(File_manager.get_concatenator_filters ".ml" [])
  in
  Alcotest.(check (list string)) "contains some elements" result []

let () =
  let open Alcotest in
  run "Concatenator Tests"
    [
      ( "Utilities",
        [
          test_case "list contains" `Quick (fun () ->
              test_contains_any [ 1; 2; 3; 4 ] [ 1 ] true);
          test_case "list not contains" `Quick (fun () ->
              test_contains_any [ 1; 2; 3; 4 ] [ 5 ] false);
          test_case "splitted list with \\" `Quick (fun () ->
              test_split_by_delims "C:/users/jfast/desktop"
                [ "C:"; "users"; "jfast"; "desktop" ]);
          test_case "splitted list with /" `Quick (fun () ->
              test_split_by_delims "C:\\users\\jfast\\desktop"
                [ "C:"; "users"; "jfast"; "desktop" ]);
        ] );
      ( "File Manager",
        [
          test_case "test find ml files" `Quick (fun () ->
              test_file_walk "/mnt/c/users/jfast/Desktop");
        ] );
    ]
