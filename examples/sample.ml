let () = for i = 1 to 10 do
    let x = (10 / i) [@catch 20] in
    (Printf.printf "Iteration n°%d, value of x :%d\n" i x)
  done

module X = struct
  let y = 10000
end


let () = print_endline "fin"
