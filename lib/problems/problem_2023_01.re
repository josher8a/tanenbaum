let year = 2023;
let day = 1;

module Part_1 = {
  let extractDigit = str => {
    str
    |> String.to_seq
    |> Seq.fold_left(
         (acc, char) =>
           switch (char) {
           | '0' .. '9' =>
             switch (acc) {
             | (None, _) => (Some(char), Some(char))
             | (Some(first), _) => (Some(first), Some(char))
             }
           | _ => acc
           },
         (None, None),
       )
    |> (
      acc =>
        switch (acc) {
        | (Some(first), Some(last)) =>
          int_of_string(Char.escaped(first) ++ Char.escaped(last))
        | _ => 0
        }
    );
  };

  let run = (input: string): result(string, string) => {
    Ok(
      input
      |> String.split_on_char('\n')
      |> List.map(extractDigit)
      |> List.fold_left((acc, num) => acc + num, 0)
      |> string_of_int,
    );
  };
};

module Part_2 = {
  let run = (input: string): result(string, string) => Ok(input);
};
