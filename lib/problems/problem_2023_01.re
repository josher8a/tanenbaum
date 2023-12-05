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
  let replaceLiteralDigits = str => {
    str
    |> Str.global_replace("one" |> Str.regexp, "one1one")
    |> Str.global_replace("two" |> Str.regexp, "two2two")
    |> Str.global_replace("three" |> Str.regexp, "three3three")
    |> Str.global_replace("four" |> Str.regexp, "four4four")
    |> Str.global_replace("five" |> Str.regexp, "five5five")
    |> Str.global_replace("six" |> Str.regexp, "six6six")
    |> Str.global_replace("seven" |> Str.regexp, "seven7seven")
    |> Str.global_replace("eight" |> Str.regexp, "eight8eight")
    |> Str.global_replace("nine" |> Str.regexp, "nine9nine")
    |> Str.global_replace("zero" |> Str.regexp, "zero0zero");
  };

  let run = (input: string): result(string, string) => {
    Ok(
      input
      |> String.split_on_char('\n')
      |> List.map(x => x |> replaceLiteralDigits |> Part_1.extractDigit)
      |> List.fold_left((acc, num) => acc + num, 0)
      |> string_of_int,
    );
  };
};
