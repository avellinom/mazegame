open Maze

let data_dir_prefix = "data" ^ Filename.dir_sep
let m = Maze.of_file (data_dir_prefix ^ "example.mz")
let main () = Maze.print_maze m
let () = main ()
