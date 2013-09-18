-module(erlfish).
-export([interpret/1]).

interpret(L) -> read_chars(0, L).

read_chars(-1, L) -> read_chars(0, L);
read_chars(256, L) -> read_chars(0, L);
read_chars(Value, [$i|T]) -> read_chars(Value+1, T); % [i]ncrement
read_chars(Value, [$d|T]) -> read_chars(Value-1, T); % [d]ecrement
read_chars(Value, [$s|T]) -> read_chars(Value*Value, T); % [s]quare
read_chars(Value, [$o|T]) -> io:fwrite("~w~n", [Value]), read_chars(Value, T); % [o]utput
read_chars(Value, [_|T]) -> read_chars(Value, T);
read_chars(_,_) -> ok.

