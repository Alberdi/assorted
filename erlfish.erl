-module(erlfish).
-export([interpret/1]).

interpret(L) -> read_chars(0, L).

read_chars(-1, L) -> read_chars(0, L);
read_chars(256, L) -> read_chars(0, L);
read_chars(Value, [105|T]) -> read_chars(Value+1, T); % [i]ncrement
read_chars(Value, [100|T]) -> read_chars(Value-1, T); % [d]ecrement
read_chars(Value, [115|T]) -> read_chars(Value*Value, T); % [s]quare
read_chars(Value, [111|T]) -> io:fwrite("~w~n", [Value]), read_chars(Value, T); % [o]utput
read_chars(Value, [_|T]) -> read_chars(Value, T);
read_chars(_,_) -> ok.

