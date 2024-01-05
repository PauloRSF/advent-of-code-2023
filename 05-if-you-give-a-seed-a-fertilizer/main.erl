-module(main).
-export([main/0]).

-import(string, [to_integer/1, split/2, split/3]).
-import(lists, [nthtail/2, search/2, foldl/3, map/2, append/1, append/2, min/1, seq/2, last/1, nth/2, sort/2, filter/2]).

-record(mapEntry, {
  origin,
  destination,
  size
}).

parse_number_list([X | XS]) -> 
  {K, _} = to_integer(X),
  [K | parse_number_list(XS)];
parse_number_list([]) -> [].

parse_seed_numbers(SeedsSection) ->
  [_ | SeedNumbers] = split(SeedsSection, ": "),
  SeedNumbersWithoutHeader = split(SeedNumbers, ": "),
  SplitSeedNumbers = split(SeedNumbersWithoutHeader, " ", all),
  parse_number_list(SplitSeedNumbers).

parse_map_entry_line(MapEntryLine) ->
  [D, O, S] = parse_number_list(split(MapEntryLine, " ", all)),
  #mapEntry{origin=O, destination=D, size=S}.

parse_map_entries([MapEntryLine | MapEntryLines]) -> [parse_map_entry_line(MapEntryLine) | parse_map_entries(MapEntryLines)];
parse_map_entries([]) -> [].

parse_maps([MapSection | MapSections]) ->
  MapLines = split(MapSection, "\n", all),
  MapLinesWithoutHeader = nthtail(1, MapLines),
  [parse_map_entries(MapLinesWithoutHeader) | parse_maps(MapSections)];
parse_maps([]) -> [].

parse_input(InputData) ->
  [SeedsSection | MapSections] = split(InputData, "\n\n", all),
  ParsedSeedNumbers = parse_seed_numbers(SeedsSection),
  ParsedMaps = parse_maps(MapSections),
  {ParsedSeedNumbers, ParsedMaps}.

should_map_seed_number(SeedNumber, MapEntry) ->
  (SeedNumber >= MapEntry#mapEntry.origin) and (SeedNumber < MapEntry#mapEntry.origin + MapEntry#mapEntry.size).

map_seed_number_with_entry(SeedNumber, MapEntry) ->
  MapEntry#mapEntry.destination + (SeedNumber - MapEntry#mapEntry.origin).

map_seed_number(SeedNumber, Map) ->
  SearchResult = search(fun(MapEntry) -> should_map_seed_number(SeedNumber, MapEntry) end, Map),
  case SearchResult of
    {value, MapEntry} -> map_seed_number_with_entry(SeedNumber, MapEntry);
    _ -> SeedNumber
  end.

map_seed_to_location(SeedNumber, Maps) -> 
  foldl(fun(Map, Acc) -> map_seed_number(Acc, Map) end, SeedNumber, Maps).

map_seed_list_to_locations(SeedNumbers, Maps) ->
  map(fun(X) -> map_seed_to_location(X, Maps) end, SeedNumbers).

map_seed_numbers_to_ranges([Start, Length | SeedNumbers]) -> [{Start, Length} | map_seed_numbers_to_ranges(SeedNumbers)];
map_seed_numbers_to_ranges([]) -> [].

ptn(SeedRange, []) -> [SeedRange];
ptn({Start, Length}, [MapEntry | MapEntries]) ->
  End = Start + Length,
  EntryEnd = MapEntry#mapEntry.origin + MapEntry#mapEntry.size,

  StartsAfter = Start > MapEntry#mapEntry.origin,
  StartsBefore = Start =< MapEntry#mapEntry.origin,
  StartsBetween = (Start > MapEntry#mapEntry.origin) and (Start < EntryEnd),

  EndsAfter = End > EntryEnd,
  EndsBefore = End =< EntryEnd,
  EndsBetween = (End > MapEntry#mapEntry.origin) and (End < EntryEnd),

  DoesNotIntersect = (End < MapEntry#mapEntry.origin) or (Start > (MapEntry#mapEntry.origin + MapEntry#mapEntry.size)),
  IsStartIntersecting = StartsBefore and EndsBetween,
  IsEndIntersecting = StartsBetween and EndsAfter,
  DoesContain = StartsBefore and EndsAfter,
  IsContained = StartsAfter and EndsBefore,

  if
    DoesNotIntersect -> ptn({Start, Length}, MapEntries);
    IsStartIntersecting ->
      append([ptn({Start, (MapEntry#mapEntry.origin - Start)}, MapEntries), [{MapEntry#mapEntry.destination, Length - (MapEntry#mapEntry.origin - Start)}]]);
    IsEndIntersecting ->
      append([[{map_seed_number_with_entry(Start, MapEntry), EntryEnd - Start}], ptn({EntryEnd, Length - (EntryEnd - Start)}, MapEntries)]);
    DoesContain ->
      append([ptn({Start, MapEntry#mapEntry.origin - Start}, MapEntries), [{MapEntry#mapEntry.destination, MapEntry#mapEntry.size}], ptn({EntryEnd, End - EntryEnd}, MapEntries)]);
    IsContained ->
      [{map_seed_number_with_entry(Start, MapEntry), Length}];
    true -> ptn({Start, Length}, MapEntries)
  end.

abd(SeedRanges, []) -> SeedRanges;
abd(SeedRanges, [Map | Maps]) ->
  Z = append(map(fun(SeedRange) -> ptn(SeedRange, Map) end, SeedRanges)),
  abd(Z, Maps).

compare_seed_ranges({AStart, _}, {BStart, _}) -> AStart =< BStart.

map_seed_ranges_to_location_ranges([], _) -> [];
map_seed_ranges_to_location_ranges([SeedRange | SeedRanges], Maps) -> append(abd([SeedRange], Maps), map_seed_ranges_to_location_ranges(SeedRanges, Maps)).

main() ->
  {ok, InputData} = file:read_file("input.txt"),
  {SeedNumbers, Maps} = parse_input(InputData),

  SeedLocations = map_seed_list_to_locations(SeedNumbers, Maps),

  io:fwrite("Smallest location for each seed: ~w~n", [min(SeedLocations)]),

  SeedRanges = map_seed_numbers_to_ranges(SeedNumbers),
  SeedRangesLocations = map_seed_ranges_to_location_ranges(SeedRanges, Maps),
  OrderedSeedRangesLocations = sort(fun compare_seed_ranges/2, SeedRangesLocations),
  {SmallestSeedRangeLocation, _} = nth(1, OrderedSeedRangesLocations),

  io:fwrite("Smallest location for seed ranges: ~w~n", [OrderedSeedRangesLocations]).