def ltrimspace:
    [while(startswith(" "); ltrimstr(" "))] | last | ltrimstr(" ")
;

def parse_line:
    split(":")
    | last
    | ltrimspace
    | split("\\W+"; "")
    | join("")
    | tonumber
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

parse_input | find_record_breakers(.[0]; .[1])
