def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(split(""))
    | {"board": ., "rows": length, "cols": (first | length)}
;

def dir_u: "up";
def dir_r: "right";
def dir_d: "down";
def dir_l: "left";

def move_deltas: {
    ".": {
        (dir_u): [{"dir": dir_u, "dr": -1, "dc": 0}],
        (dir_r): [{"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_d): [{"dir": dir_d, "dr": 1, "dc": 0}],
        (dir_l): [{"dir": dir_l, "dr": 0, "dc": -1}],
    },
    "-": {
        (dir_u): [{"dir": dir_l, "dr": 0, "dc": -1}, {"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_r): [{"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_d): [{"dir": dir_l, "dr": 0, "dc": -1}, {"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_l): [{"dir": dir_l, "dr": 0, "dc": -1}],
    },
    "|": {
        (dir_u): [{"dir": dir_u, "dr": -1, "dc": 0}],
        (dir_r): [{"dir": dir_u, "dr": -1, "dc": 0}, {"dir": dir_d, "dr": 1, "dc": 0}],
        (dir_d): [{"dir": dir_d, "dr": 1, "dc": 0}],
        (dir_l): [{"dir": dir_u, "dr": -1, "dc": 0}, {"dir": dir_d, "dr": 1, "dc": 0}],
    },
    "/": {
        (dir_u): [{"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_r): [{"dir": dir_u, "dr": -1, "dc": 0}],
        (dir_d): [{"dir": dir_l, "dr": 0, "dc": -1}],
        (dir_l): [{"dir": dir_d, "dr": 1, "dc": 0}],
    },
    "\\": {
        (dir_u): [{"dir": dir_l, "dr": 0, "dc": -1}],
        (dir_r): [{"dir": dir_d, "dr": 1, "dc": 0}],
        (dir_d): [{"dir": dir_r, "dr": 0, "dc": 1}],
        (dir_l): [{"dir": dir_u, "dr": -1, "dc": 0}],
    },
};

def off_board($board; $pos):
    ($pos.row < 0)
    or ($pos.row >= $board.rows)
    or ($pos.col < 0)
    or ($pos.col >= $board.cols)
;

def in_cache($cache; $pos):
    $cache["\($pos.dir),\($pos.row),\($pos.col)"] // false
;

def run($board):
    def step($state):
        if off_board($board; $state.pos) then
            $state
        elif in_cache($state.cache; $state.pos) then
            $state
        else
            move_deltas
            | .[$board.board[$state.pos.row][$state.pos.col]][$state.pos.dir]
            | reduce .[] as $delta (
                $state;
                step({
                    "positions": (.positions + [[.pos.row, .pos.col]]),
                    "cache": (.cache | .["\($state.pos.dir),\($state.pos.row),\($state.pos.col)"] |= true),
                    "pos": {"dir": $delta.dir, "row": (.pos.row + $delta.dr), "col": ($state.pos.col + $delta.dc)},
                })
              )
            | {
                "positions": (.positions + [[$state.pos.row, $state.pos.col]]),
                "cache": .cache,
                "pos": $state.pos
            }
        end
    ;

    step({"positions": [], "cache": {}, "pos": {"dir": dir_r, "row": 0, "col": 0}})
;

parse_input
| run(.)
| .positions
| unique 
| map(select(.[0] >= 0 and .[1] >= 0))
| length
