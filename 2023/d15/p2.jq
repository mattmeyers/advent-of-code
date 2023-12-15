def parse_input: rtrimstr("\n") | split(",");

def hash_word:
    explode | reduce .[] as $c (0; (17 * (. + $c)) % 256)
;

def pop: "pop";
def set: "set";

def parse_instruction:
    if endswith("-") then
        {
            "label": .[:-1],
            "instruction": pop,
            "hash": (.[:-1] | hash_word)
        }
    else
        split("=")
        | {
            "label": first,
            "focal_length": (last | tonumber),
            "instruction": set,
            "hash": (first | hash_word)
          }
    end
;

def init_boxes($n):
    reduce range($n) as $_ ([]; . + [[]])
;

def do_pop($box; $label):
    $box | map(select(.label != $label))
;

def do_set($box; $label; $focal_length):
    def _do_set($box):
        if ($box | length) == 0 then
            [{"label": $label, "focal_length": $focal_length}]
        elif $box[0].label == $label then
            [{"label": $label, "focal_length": $focal_length}] + $box[1:]
        else
            [$box[0]] + _do_set($box[1:])
        end
    ;

    _do_set($box)
;

def apply_instruction($instruction; $box):
    if $instruction.instruction == pop then
        do_pop($box; $instruction.label)
    else
        do_set($box; $instruction.label; $instruction.focal_length)
    end
;


def run:
    reduce .[] as $instruction (
        init_boxes(256);
        .[$instruction.hash] = apply_instruction($instruction; .[$instruction.hash])
    )
;

def box_power($box_number; $lenses):
    reduce ($lenses | to_entries)[] as $lens (
        0;
        . + ($box_number * ($lens.key + 1) * $lens.value.focal_length)
    )
;

def focusing_power:
    to_entries
    | reduce .[] as $box (0; . + box_power($box.key+1; $box.value))
;

parse_input | map(parse_instruction) | run | focusing_power
