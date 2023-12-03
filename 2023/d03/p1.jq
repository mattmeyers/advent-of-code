def parse_input:
    rtrimstr("\n") | split("\n")
;

def find_symbols_in_line:
    match("[^.0-9]"; "g")
;

def find_symbols:
    . as $input
    | [
        range(length)
        | [
            . as $line
            | ($input[.] | find_symbols_in_line)
            | {"\($line),\(.offset)": true}
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
        ["\($line-1),\($l)","\($line+1),\($l)"] + gen_2($line; $l+1; $r)
    end
;

def generate_coords($match; $line):
    [
        "\($line),\($match.offset-1)",
        "\($line),\($match.offset+$match.length)"
    ] + gen_2($line; $match.offset-1; $match.offset+$match.length) 
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

def find_overlaps($numbers; $symbols):
    [$numbers[] | if (.coords | map($symbols[.]) | any) then (.n | tonumber) else 0 end]
;

parse_input
| find_symbols as $symbols
| ([find_numbers] | add) as $numbers
| find_overlaps($numbers; $symbols)
| add
