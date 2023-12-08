def map_card:
    {
        "A": 14,
        "K": 13,
        "Q": 12,
        "J": 1,
        "T": 10
    }[.] // tonumber
;

def count_jokers:
    reduce .[] as $c (0; if $c == "J" then . + 1 else . end)
;

def classify_hand:
    . / ""
    | count_jokers as $jokers
    | group_by(.)
    | map(length)
    | sort
    | reverse
    | if first == 5 then # Five of a kind
        6
      elif first == 4 then # Four of a kind
        if $jokers > 0 then 6 else 5 end
      elif first == 3 and last == 2 then # Full house
        if $jokers > 0 then 6 else 4 end
      elif first == 3 then # Three of a kind
        if $jokers > 0 then 5 else 3  end
      elif first == 2 and length == 3 then # Two pair
        if $jokers == 2 then 5 elif $jokers == 1 then 4 else 2 end
      elif first == 2 then # One pair
        if $jokers > 0 then 3 else 1 end
      else # High card
        if $jokers > 0 then 1 else 0 end
      end
;

def parse_line:
    split(" ")
    | {
        "hand": first,
        "hand_values": (first | . / "" | map(map_card)),
        "hand_type": (first | classify_hand),
        "bid": (last | tonumber)
      }
;

def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(parse_line)
;

def calculate_winnings:
    def f($bids; $n):
        if ($bids | length) == 0 then
            0
        else
            ($bids[0] * $n) + f($bids[1:]; $n + 1)
        end
    ;

    f(.;1)
;

parse_input
| sort_by(.hand_type, .hand_values)
| map(.bid)
| calculate_winnings


