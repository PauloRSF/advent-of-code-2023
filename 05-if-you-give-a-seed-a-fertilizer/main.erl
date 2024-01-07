-module(main).

-export([main/0]).

-import(maps, [get/2]).
-import(string, [to_integer/1, split/2, split/3]).
-import(lists,
        [nthtail/2,
         foldl/3,
         map/2,
         append/1,
         append/2,
         min/1,
         nth/2,
         sort/2,
         filter/2,
         flatten/1]).

% -------------------
% Input parsing
% -------------------

parse_number_list([X | XS]) ->
    {K, _} = to_integer(X),
    [K | parse_number_list(XS)];
parse_number_list([]) ->
    [].

parse_seed_numbers(SeedsSection) ->
    [_ | SeedNumbers] = split(SeedsSection, ": "),
    SeedNumbersWithoutHeader = split(SeedNumbers, ": "),
    SplitSeedNumbers = split(SeedNumbersWithoutHeader, " ", all),
    parse_number_list(SplitSeedNumbers).

parse_map_entry_line(MapEntryLine) ->
    [Destination, Origin, Length] = parse_number_list(split(MapEntryLine, " ", all)),
    #{origin => Origin,
      destination => Destination,
      length => Length}.

parse_map_entries([]) ->
    [];
parse_map_entries([MapEntryLine | MapEntryLines]) ->
    [parse_map_entry_line(MapEntryLine) | parse_map_entries(MapEntryLines)].

parse_maps([]) ->
    [];
parse_maps([MapSection | MapSections]) ->
    MapLines = split(MapSection, "\n", all),
    MapLinesWithoutHeader = nthtail(1, MapLines),
    [parse_map_entries(MapLinesWithoutHeader) | parse_maps(MapSections)].

parse_input(InputData) ->
    [SeedsSection | MapSections] = split(InputData, "\n\n", all),
    ParsedSeedNumbers = parse_seed_numbers(SeedsSection),
    ParsedMaps = parse_maps(MapSections),
    {ParsedSeedNumbers, ParsedMaps}.

% -------------------
% First puzzle
% -------------------

seed_number_is_inside_entry(SeedNumber, MapEntry) ->
    (SeedNumber >= get(origin, MapEntry))
    and (SeedNumber < get(origin, MapEntry) + get(length, MapEntry)).

map_seed_number_with_entry(SeedNumber, MapEntry) ->
    get(destination, MapEntry) + (SeedNumber - get(origin, MapEntry)).

map_seed_number(SeedNumber, []) ->
    SeedNumber;
map_seed_number(SeedNumber, [MapEntry | MapEntries]) ->
    SeedShouldBeMapped = seed_number_is_inside_entry(SeedNumber, MapEntry),
    if SeedShouldBeMapped ->
           map_seed_number_with_entry(SeedNumber, MapEntry);
       true ->
           map_seed_number(SeedNumber, MapEntries)
    end.

map_seed_to_location(SeedNumber, Maps) ->
    foldl(fun(Map, Acc) -> map_seed_number(Acc, Map) end, SeedNumber, Maps).

map_seed_list_to_locations([], _) ->
    [];
map_seed_list_to_locations([SeedNumber | SeedNumbers], Maps) ->
    [map_seed_to_location(SeedNumber, Maps) | map_seed_list_to_locations(SeedNumbers, Maps)].

% -------------------
% Second puzzle
% -------------------

map_seed_numbers_to_ranges([]) ->
    [];
map_seed_numbers_to_ranges([Start, Length | SeedNumbers]) ->
    [{Start, Length} | map_seed_numbers_to_ranges(SeedNumbers)].

map_seed_range_to_location_ranges(SeedRange, []) ->
    [SeedRange];
map_seed_range_to_location_ranges({_, 0}, _) ->
    [];
map_seed_range_to_location_ranges({RangeStart, RangeLength}, [MapEntry | MapEntries]) ->
    RangeEnd = RangeStart + RangeLength,
    EntryStart = get(origin, MapEntry),
    EntryEnd = EntryStart + get(length, MapEntry),

    RangeStartsAfterEntry = RangeStart > EntryStart,
    RangeStartsBeforeEntry = RangeStart =< EntryStart,
    RangeStartsInsideEntry = (RangeStart > EntryStart) and (RangeStart < EntryEnd),

    RangeEndsAfterEntry = RangeEnd > EntryEnd,
    RangeEndsBeforeEntry = RangeEnd =< EntryEnd,
    RangeEndsInsideEntry = (RangeEnd > EntryStart) and (RangeEnd < EntryEnd),

    %               Map entry range
    %          |-----------------------|
    %      Seed range
    % |------------------|
    IsStartIntersecting = RangeStartsBeforeEntry and RangeEndsInsideEntry,
    %      Map entry range
    % |-----------------------|
    %                    Seed range
    %               |------------------|
    IsEndIntersecting = RangeStartsInsideEntry and RangeEndsAfterEntry,
    %           Map entry range
    %      |-----------------------|
    %              Seed range
    % |----------------------------------|
    DoesContain = RangeStartsBeforeEntry and RangeEndsAfterEntry,
    %        Map entry range
    % |----------------------------|
    %           Seed range
    %     |------------------|
    IsContained = RangeStartsAfterEntry and RangeEndsBeforeEntry,

    if IsStartIntersecting ->
           map_seed_range_to_location_ranges({RangeStart, EntryStart - RangeStart}, MapEntries)
           ++ [{get(destination, MapEntry), RangeLength - (EntryStart - RangeStart)}];
       IsEndIntersecting ->
           [{map_seed_number_with_entry(RangeStart, MapEntry), EntryEnd - RangeStart}
            | map_seed_range_to_location_ranges({EntryEnd, RangeLength - (EntryEnd - RangeStart)},
                                                MapEntries)];
       DoesContain ->
           map_seed_range_to_location_ranges({RangeStart, EntryStart - RangeStart}, MapEntries)
           ++ [{get(destination, MapEntry), get(length, MapEntry)}]
           ++ map_seed_range_to_location_ranges({EntryEnd, RangeEnd - EntryEnd}, MapEntries);
       IsContained ->
           [{map_seed_number_with_entry(RangeStart, MapEntry), RangeLength}];
       true ->
           map_seed_range_to_location_ranges({RangeStart, RangeLength}, MapEntries)
    end.

clip_ranges_with_maps(SeedRanges, []) ->
    SeedRanges;
clip_ranges_with_maps(SeedRanges, [Map | Maps]) ->
    ClippedRanges =
        map(fun(SeedRange) -> map_seed_range_to_location_ranges(SeedRange, Map) end, SeedRanges),
    clip_ranges_with_maps(flatten(ClippedRanges), Maps).

map_seed_ranges_to_location_ranges([], _) ->
    [];
map_seed_ranges_to_location_ranges([SeedRange | SeedRanges], Maps) ->
    [clip_ranges_with_maps([SeedRange], Maps) | map_seed_ranges_to_location_ranges(SeedRanges,
                                                                                   Maps)].

get_seed_range_with_smallest_location(SeedRangesLocations) ->
    OrderedSeedRangesLocations =
        sort(fun({AStart, _}, {BStart, _}) -> AStart =< BStart end, flatten(SeedRangesLocations)),
    nth(1, OrderedSeedRangesLocations).

main() ->
    {ok, InputData} = file:read_file("input.txt"),
    {SeedNumbers, Maps} = parse_input(InputData),

    SeedLocations = map_seed_list_to_locations(SeedNumbers, Maps),

    io:fwrite("Smallest location for each seed: ~w~n", [min(SeedLocations)]),

    SeedRanges = map_seed_numbers_to_ranges(SeedNumbers),
    SeedRangesLocations = map_seed_ranges_to_location_ranges(SeedRanges, Maps),
    {SmallestSeedRangeLocation, _} =
        get_seed_range_with_smallest_location(SeedRangesLocations),

    io:fwrite("Smallest location for seed ranges: ~w~n", [SmallestSeedRangeLocation]).
