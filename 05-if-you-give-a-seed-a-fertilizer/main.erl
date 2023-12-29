-module(main).
-export([main/0]).

-record(mapEntry, {
  origin,
  destination,
  size
}).

parse_number_list([X | XS]) -> 
  {K, _} = string:to_integer(X),
  [K | parse_number_list(XS)];
parse_number_list([]) -> [].

parse_seed_numbers(SeedsSection) ->
  [_ | SeedNumbers] = string:split(SeedsSection, ": "),
  SeedNumbersWithoutHeader = string:split(SeedNumbers, ": "),
  SplitSeedNumbers = string:split(SeedNumbersWithoutHeader, " ", all),
  parse_number_list(SplitSeedNumbers).

parse_map_entry_line(MapEntryLine) ->
  [D, O, S] = parse_number_list(string:split(MapEntryLine, " ", all)),
  #mapEntry{origin=O, destination=D, size=S}.

parse_map_entries([MapEntryLine | MapEntryLines]) -> [parse_map_entry_line(MapEntryLine) | parse_map_entries(MapEntryLines)];
parse_map_entries([]) -> [].

parse_maps([MapSection | MapSections]) ->
  MapLines = string:split(MapSection, "\n", all),
  MapLinesWithoutHeader = lists:nthtail(1, MapLines),
  [parse_map_entries(MapLinesWithoutHeader) | parse_maps(MapSections)];
parse_maps([]) -> [].

parse_input(InputData) ->
  [SeedsSection | MapSections] = string:split(InputData, "\n\n", all),
  ParsedSeedNumbers = parse_seed_numbers(SeedsSection),
  ParsedMaps = parse_maps(MapSections),
  {ParsedSeedNumbers, ParsedMaps}.

should_map_seed_number(SeedNumber, MapEntry) ->
  (SeedNumber >= MapEntry#mapEntry.origin) and (SeedNumber < MapEntry#mapEntry.origin + MapEntry#mapEntry.size).

map_seed_number(SeedNumber, MapEntry) ->
  MapEntry#mapEntry.destination + (SeedNumber - MapEntry#mapEntry.origin).

trm(SeedNumber, MapEntries) ->
  R = lists:search(fun(MapEntry) -> should_map_seed_number(SeedNumber, MapEntry) end, MapEntries),
  case R of
    {value, MapEntry} -> map_seed_number(SeedNumber, MapEntry);
    _ -> SeedNumber
  end.

map_seed_to_location(SeedNumber, Maps) -> 
  lists:foldl(fun(X, Acc) -> trm(Acc, X) end, SeedNumber, Maps).

map_seed_list_to_locations(SeedNumbers, Maps) ->
  lists:map(fun(X) -> map_seed_to_location(X, Maps) end, SeedNumbers).

map_seed_numbers_to_ranges([Start, Length | SeedNumbers]) -> [{Start, Length} | map_seed_numbers_to_ranges(SeedNumbers)];
map_seed_numbers_to_ranges([]) -> [].

kvk({Start, Length}, Maps) ->
  SeedNumbers = lists:seq(Start, Start + Length - 1),
  SeedLocations = map_seed_list_to_locations(SeedNumbers, Maps),
  lists:min(SeedLocations).

mkd(SeedRanges, Maps) -> 
  X = lists:map(fun(X) -> kvk(X, Maps) end, SeedRanges),
  lists:min(X).

main() ->
  {ok, InputData} = file:read_file("input.txt"),
  {SeedNumbers, Maps} = parse_input(InputData),
  SeedLocations = map_seed_list_to_locations(SeedNumbers, Maps),
  io:fwrite("Smallest location for each seed: ~w~n", [lists:min(SeedLocations)]),
  L = map_seed_numbers_to_ranges(SeedNumbers),
  X = mkd(L, Maps),
  io:fwrite("Smallest location for seed ranges: ~w~n", [X]).
