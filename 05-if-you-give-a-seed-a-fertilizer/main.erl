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

map_seed_number_with_entry(SeedNumber, MapEntry) ->
  MapEntry#mapEntry.destination + (SeedNumber - MapEntry#mapEntry.origin).

map_seed_number(SeedNumber, Map) ->
  SearchResult = lists:search(fun(MapEntry) -> should_map_seed_number(SeedNumber, MapEntry) end, Map),
  case SearchResult of
    {value, MapEntry} -> map_seed_number_with_entry(SeedNumber, MapEntry);
    _ -> SeedNumber
  end.

map_seed_to_location(SeedNumber, Maps) -> 
  lists:foldl(fun(Map, Acc) -> map_seed_number(Acc, Map) end, SeedNumber, Maps).

map_seed_list_to_locations(SeedNumbers, Maps) ->
  lists:map(fun(X) -> map_seed_to_location(X, Maps) end, SeedNumbers).

map_seed_numbers_to_ranges([Start, Length | SeedNumbers]) -> [{Start, Length} | map_seed_numbers_to_ranges(SeedNumbers)];
map_seed_numbers_to_ranges([]) -> [].

mz(SeedNumbers, Maps) ->
  lists:foldl(fun(X, Acc) -> lists:min([map_seed_to_location(X, Maps), Acc]) end, 999999999999999999, SeedNumbers).

ptn({Start, Length}, MapEntry) ->
  End = Start + Length,
  EntryEnd = MapEntry#mapEntry.origin + MapEntry#mapEntry.size,

  StartsAfter = Start > MapEntry#mapEntry.origin,
  StartsBefore = Start < MapEntry#mapEntry.origin,
  StartsBetween = (Start > MapEntry#mapEntry.origin) and (Start < EntryEnd),

  EndsAfter = End > EntryEnd,
  EndsBefore = End < EntryEnd,
  EndsBetween = (End > MapEntry#mapEntry.origin) and (End < EntryEnd),

  DoesNotIntersect = (End < MapEntry#mapEntry.origin) or (Start > (MapEntry#mapEntry.origin + MapEntry#mapEntry.size)),
  IsStartIntersecting = StartsBefore and EndsBetween,
  IsEndIntersecting = StartsBetween and EndsAfter,
  DoesContain = StartsBefore and EndsAfter,
  IsContained = StartsAfter and EndsBefore,

  if
    DoesNotIntersect -> [{Start, Length}];
    IsStartIntersecting ->
      [{Start, (MapEntry#mapEntry.origin - Start)}, {MapEntry#mapEntry.destination, Length - (MapEntry#mapEntry.origin - Start)}];
    IsEndIntersecting ->
      [{}];
    DoesContain ->
      [{}];
    IsContained ->
      [{}];
  end.

qze([SeedRange | SeedRanges], [MapEntry, MapEntries]) -> [ptn(SeedRange, MapEntry) | ]

abd(SeedRanges, [Map | Maps]) -> 
  M = qze(SeedRanges, Map),
  abd(M, Maps).
abd(SeedRange, []) -> 

kvk({_, 0}, _, Min) -> Min;
kvk({Start, Length}, Maps, Min) ->
  T = Start + Length - 1,
  K = map_seed_to_location(T, Maps),
  X = lists:min([Min, K]),
  Y = Length - 1,
  kvk({Start, Y}, Maps, X).

mkd(SeedRanges, Maps) -> 
  lists:foldl(fun(X, Acc) -> lists:min([kvk(X, Maps, 999999999999999999), Acc]) end, 999999999999999999, SeedRanges).

main() ->
  {ok, InputData} = file:read_file("input.txt"),
  {SeedNumbers, Maps} = parse_input(InputData),
  SeedLocations = map_seed_list_to_locations(SeedNumbers, Maps),
  io:fwrite("Smallest location for each seed: ~w~n", [lists:min(SeedLocations)]),
  L = map_seed_numbers_to_ranges(SeedNumbers),
  X = mkd(L, Maps),
  io:fwrite("Smallest location for seed ranges: ~w~n", [X]).

%SeedNumbers = lists:seq(Start, Start + Length - 1),
  %lists:foldl(fun(X, Acc) -> lists:min([map_seed_to_location(X, Maps), Acc]) end, 999999999999999999, lists:seq(Start, Start + Length - 1)).
%mz(SeedNumbers, Maps);