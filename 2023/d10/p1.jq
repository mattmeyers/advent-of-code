def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(. / "")
;

def find_start($arr):
    def _find($row):
        ($arr[$row] | index("S"))
        | if . != null then {"row": $row, "col": .} else _find($row+1) end
    ;

    _find(0)
;

def up: 1;
def right: 2;
def down: 3;
def left: 4;

def next_pos($char; $pos):
    $pos |
    if $char == "-" then
        if .direction == left then
            .col |= . - 1
        elif .direction == right then
            .col |= . + 1
        else
            error("")
        end
    elif $char == "|" then
        if .direction == up then
            .row |= . - 1
        elif .direction == down then
            .row |= . + 1
        else
            error("")
        end
    elif $char == "7" then
        if .direction == right then
            .row |= . + 1
            | .direction |= down
        elif .direction == up then
            .col |= . - 1
            | .direction |= left
        else
            error("")
        end
    elif $char == "L" then
        if .direction == down then
            .col |= . + 1
            | .direction |= right
        elif .direction == left then
            .row |= . - 1
            | .direction |= up
        else
            error("")
        end
    elif $char == "J" then
        if .direction == right then
            .row |= . - 1
            | .direction |= up
        elif .direction == down then 
            .col |= . - 1
            | .direction |= left
        else
            error("")
        end
    elif $char == "F" then
        if .direction == up then
            .col |= . + 1
            | .direction |= right
        elif .direction == left then
            .row |= . + 1
            | .direction |= down
        else
            error("")
        end
    end
;

def walk($arr; $pos; $depth):
    if $arr[$pos.row][$pos.col] == "S" then
        $depth
    elif ($pos.row < 0 or $pos.col < 0) then
        null
    elif ($arr[$pos.row][$pos.col] == ".") then
        null
    else
        walk($arr; next_pos($arr[$pos.row][$pos.col]; $pos); $depth+1)
    end
;

parse_input as $arr
| find_start($arr)
| [
    {"row":.row,"col":(.col - 1),"direction": left} ,
    {"row":(.row-1),"col":.col,"direction": up} ,
    {"row":.row,"col":(.col+1),"direction": right}, 
    {"row":(.row+1),"col":.col,"direction": down}
  ]
| map(walk($arr; .; 1)?)
| max
| . / 2


