def ltrimspace:
    [while(startswith(" "); ltrimstr(" "))] | last | ltrimstr(" ")
;

def parse_line:
    split(":")
    | last
    | ltrimspace
    | split("\\W+"; "")
    | map(tonumber)
;

def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(parse_line)
;

def calculate_distance($charge_time; $total_time):
    $charge_time * ($total_time - $charge_time)
;

def find_record_breakers($duration; $record):
    reduce
        range(1; $duration) as $charge_time
        (
            0;
            if calculate_distance($charge_time; $duration) > $record then
                . + 1
            else
                .
            end
        )
;

def tail:
    if length then .[1:] else [] end
;

def run($durations; $records):
    if ($durations | length) == 0 then
        null
    else
        find_record_breakers($durations[0]; $records[0])
        | [.] + run(($durations | tail); ($records | tail))
    end
;

parse_input
| . as [$durations, $records]
| run($durations; $records)
| reduce .[] as $i (1; . * $i)

