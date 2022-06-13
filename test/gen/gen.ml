let gen_rule file =
  Printf.printf {|
(rule
 (alias runtest)
 (target %s.corrected)
 (deps (:t %s))
 (action
   (with-stdout-to %%{target}
     (run %%{bin:neocamlformat} %%{t}))))
|} file file

let () =
  let dir_content = Sys.readdir "." in
  Array.sort String.compare dir_content;
  Array.iter (fun file ->
    if Filename.check_suffix file ".ml" ||
        Filename.check_suffix file ".mli" then
      gen_rule file
  ) dir_content
