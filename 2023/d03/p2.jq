def parse_input:
    rtrimstr("\n") | split("\n")
;

def find_symbols_in_line:
    match("\\*"; "g")
;

def find_symbols:
    . as $input
    | [
        range(length)
        | [
            . as $line
            | ($input[.] | find_symbols_in_line)
            | ["\($line),\(.offset)"]
          ] 
        | add
      ]
    | add 
;

def find_numbers_in_line:
    match("[0-9]+"; "g")
;

def gen_2($line; $l; $r):
    if $l > $r then
        null
    else
        {"\($line-1),\($l)": true,"\($line+1),\($l)": true} + gen_2($line; $l+1; $r)
    end
;

def generate_coords($match; $line):
    {
        "\($line),\($match.offset-1)": true,
        "\($line),\($match.offset+$match.length)": true
    } + gen_2($line; $match.offset-1; $match.offset+$match.length) 
;

def find_numbers:
    . as $input
    | range(length)
    | [
        . as $line
        | ($input[.] | find_numbers_in_line)
        | {"n": .string, coords: generate_coords(.; $line)}
      ]
;

def overlap($numbers; $symbols):
    $symbols[]
    | [(. as $s | $numbers[] | select(.coords[$s]))]
    | select(length | . == 2)
    | map(.n | tonumber)
    | .[0] * .[1]
;

parse_input
| find_symbols as $symbols
| ([find_numbers] | add) as $numbers
| [overlap($numbers; $symbols)]
| add
