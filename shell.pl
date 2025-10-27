% Interactive UI: menu, genre browser, recommend, surprise, top, search, watchlist.

:- use_module(main).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic watchlist/1.

% -------- small utilities --------
non_space(C) :- \+ code_type(C, space).
is_blank(S) :-
  string(S), string_codes(S, Cs),
  include(non_space, Cs, NonBlank), NonBlank == [].

press_enter_to_continue :-
  format('~n(Press ENTER to return to the menu)~n> '),
  flush_output, read_line_to_string(user_input, _).

string_trim(S0, S) :-
  string_codes(S0, Cs0), trim_leading(Cs0, Cs1),
  reverse(Cs1, R0), trim_leading(R0, R1),
  reverse(R1, Cs), string_codes(S, Cs).
trim_leading([C|R], Out) :- code_type(C, space), !, trim_leading(R, Out).
trim_leading(L, L).

strip_quotes(S0, S) :-
  ( sub_string(S0, 0, 1, _, "\""), sub_string(S0, _, 1, 0, "\"")
  -> sub_string(S0, 1, _, 1, S)
  ;  sub_string(S0, 0, 1, _, "'"),  sub_string(S0, _, 1, 0, "'")
  -> sub_string(S0, 1, _, 1, S)
  ;  S = S0 ).

resolve_title(UserIn0, Canonical) :-
  string_trim(UserIn0, T1), strip_quotes(T1, T2), string_lower(T2, IL),
  findall(TC,
          ( anime_fact(_, TC, _, _, _, _, _),
            ( string(TC) -> TCS = TC ; atom_string(TC, TCS) ),
            string_lower(TCS, TL), IL = TL ),
          Matches),
  Matches \= [], Matches = [Canonical|_].

% -------- genre browser --------
genre_counts_sorted(PairsDesc) :-
  all_genres(Genres),
  findall(C-G, (member(G, Genres), findall(T, has_genre_ci(T,G), Ts), length(Ts,C)), Pairs),
  keysort(Pairs, Asc), reverse(Asc, PairsDesc).

print_genre_counts(Pairs) :- print_genre_counts(1, Pairs).
print_genre_counts(_, []).
print_genre_counts(I, [C-G|R]) :-
  format('(~w) ~w  [~w titles]~n', [I, G, C]),
  I1 is I+1, print_genre_counts(I1, R).

ask_genre_interactive(Genre) :-
  format('> Preferred genre (e.g., "Comedy", "Action").~n'),
  format('  Tip: type "?" to list all genres with counts, or enter a number from that list.~n'),
  ask_genre_loop(Genre).
ask_genre_loop(Genre) :-
  format('> '), read_line_to_string(user_input, G0), string_trim(G0, G1),
  ( G1 = "" -> format('Please enter a genre or "?".~n'), ask_genre_loop(Genre)
  ; G1 = "?" ->
      nl, format('--- Available Genres ---~n'),
      genre_counts_sorted(Ps), print_genre_counts(Ps),
      format('------------------------~n'),
      ask_pick_genre_from_pairs(Ps, Genre)
  ; catch(number_string(N, G1), _, fail) ->
      genre_counts_sorted(Ps),
      ( nth1(N, Ps, _C-G), Genre = G -> true ; format('Index out of range.~n'), ask_genre_loop(Genre) )
  ; Genre = G1 ).

ask_pick_genre_from_pairs(Pairs, Genre) :-
  format('Choose a genre by number, or type a name, or "?" to re-list:~n> '),
  read_line_to_string(user_input, In0), string_trim(In0, In),
  ( In = "" -> format('Please choose.~n'), ask_pick_genre_from_pairs(Pairs, Genre)
  ; In = "?" -> nl, print_genre_counts(Pairs), ask_pick_genre_from_pairs(Pairs, Genre)
  ; catch(number_string(N, In), _, fail) ->
      ( nth1(N, Pairs, _C-G), Genre = G -> true ; format('Out of range.~n'), ask_pick_genre_from_pairs(Pairs, Genre) )
  ; Genre = In ).

% -------- menu --------
start :- main_menu.

main_menu :-
  nl, format('=================================================~n'),
  format('   Anime Recommendation Expert (Interactive)     ~n'),
  format('=================================================~n'),
  format('[1] Recommend (with filters)~n'),
  format('[2] Surprise me~n'),
  format('[3] Top charts~n'),
  format('[4] Search titles~n'),
  format('[5] Watchlist~n'),
  format('[6] Help~n'),
  format('[0] Quit~n> '),
  read_line_to_string(user_input, Choice),
  handle_choice(Choice), !, main_menu.
main_menu.

handle_choice(C) :- is_blank(C), !.
handle_choice("1") :- cmd_recommend.
handle_choice("2") :- cmd_surprise.
handle_choice("3") :- cmd_top.
handle_choice("4") :- cmd_search.
handle_choice("5") :- cmd_watchlist.
handle_choice("6") :- help.
handle_choice("0") :- format('Bye!~n'), halt.
handle_choice(_)    :- format('Unknown option. Type 6 for help.~n').

% -------- commands --------
cmd_recommend :-
  nl, format('> Let\'s gather your preferences...~n'),
  ask_preferences(Prefs),
  ask_age_filter(AgeOpt),
  ask_length_filter(BucketOpt),
  find_and_display_recs(Prefs, AgeOpt, BucketOpt),
  nl, press_enter_to_continue.

find_and_display_recs(Prefs, AgeOpt, BucketOpt) :-
  nl, format('Searching for anime that match your preferences...~n'),
  findall(Score-Title, calculate_score(Prefs, Title, Score), Pairs0),
  keysort(Pairs0, Asc), reverse(Asc, Desc),
  take_first(10, Desc, TopPairs),
  pairs_values(TopPairs, Titles0),
  apply_age_filter(AgeOpt, Titles0, Titles1),
  apply_length_filter(BucketOpt, Titles1, Titles2),
  ( Titles2 = [] ->
      nl, format('--- No Recommendations Found ---~n'),
      format('Try being less specific or change filters.~n')
  ; nl, format('--- Recommendations ---~n'),
    forall(member(T, Titles2), ( print_card(T), explain_breakdown(Prefs, T) )),
    format('--------------------------------~n')
  ).

apply_age_filter(none, In, In).
apply_age_filter(some(Age), In, Out) :- include(ok_for_age(Age), In, Out).
apply_length_filter(none, In, In).
apply_length_filter(some(Bucket), In, Out) :- include(length_bucket_match(Bucket), In, Out).
length_bucket_match(Bucket, Title) :- length_bucket(Title, Bucket).

explain_breakdown(Prefs, Title) :-
  explain_score(Prefs, Title, GS, SS, LS),
  Total is GS + SS + LS,
  format('   Why this: genre ~w + score ~w + length ~w  => total ~w/100~n', [GS,SS,LS,Total]).

cmd_surprise :-
  nl, format('> Genre for a short, high-rated surprise (e.g., "Comedy", "Action"):~n> '),
  read_line_to_string(user_input, G0),
  ( G0 = "" -> surprise_any(T)
  ; surprise_me(G0, T) -> true
  ; format('No eligible short shows found for that genre.~n'), !
  ),
  nl, format('Surprise pick!~n'),
  print_card(T),
  prompt_yes_no('Add to watchlist? (y/n): ', YN),
  ( YN = yes -> add_to_watchlist(T), format('Added.~n') ; true ),
  press_enter_to_continue.

surprise_any(Title) :-
  findall(T, ( length_bucket(T, short),
               anime_fact(_,T,_,_,S,_,_), number(S), S>=7.0 ),
          L),
  L \= [], random_member(Title, L).

cmd_top :-
  nl, prompt_int('How many results? (default 5): ', 1, 100, 5, N),
  format('Optional genre (blank for all):~n> '), read_line_to_string(user_input, G0),
  ask_length_filter(BucketOpt),
  ( G0 = "" -> top_by_score(N, Out0)
  ; top_by_score_in_genre(G0, N, Out0)
  ),
  findall(T, member(rec(T,_), Out0), TitlesAll),
  apply_length_filter(BucketOpt, TitlesAll, Titles),
  ( Titles = [] ->
      format('No items for those filters.~n')
    ; nl, format('--- Top Charts ---~n'),
      print_indexed_list(Titles),
      format('------------------~n')
  ),
  press_enter_to_continue.

cmd_search :-
  nl, format('Search title contains:~n> '),
  read_line_to_string(user_input, Q),
  findall(T, search_title_ci(Q, T), L),
  ( L = [] ->
      format('No matches.~n'), press_enter_to_continue
    ; nl, format('--- Matches ---~n'),
      print_indexed_list(L), format('---------------~n'),
      prompt_yes_no('Add any to watchlist? (y/n): ', YN),
      ( YN = yes -> choose_title_from(L, TPicked), add_to_watchlist(TPicked), format('Added.~n') ; true ),
      press_enter_to_continue
  ).

cmd_watchlist :-
  nl, format('[a]dd  [r]emove  [s]how  [c]lear~n> '),
  read_line_to_string(user_input, C),
  ( C="a" ->
      format('Title to add (case-insensitive, quotes/spaces ok): '),
      read_line_to_string(user_input,T0),
      ( add_to_watchlist(T0) -> format('Added.~n') ; format('Not found in KB; not added.~n') ),
      press_enter_to_continue
  ; C="r" ->
      format('Title to remove (case-insensitive): '),
      read_line_to_string(user_input,T0),
      ( resolve_title(T0, T), retractall(watchlist(T)) -> format('Removed.~n') ; format('Not on watchlist.~n') ),
      press_enter_to_continue
  ; C="s" ->
      findall(W, watchlist(W), L),
      ( L = [] ->
          format('Watchlist is empty.~n')
      ; nl, format('--- Your Watchlist ---~n'),
        forall(member(TT, L), print_card(TT)),
        watchlist_total_hours(H),
        format('Total watch time approx. ~1f hours (24 min/ep).~n', [H]),
        format('-----------------------~n')
      ),
      flush_output, press_enter_to_continue
  ; C="c" ->
      retractall(watchlist(_)), format('Watchlist cleared.~n'), press_enter_to_continue
  ; format('Unknown option.~n'), press_enter_to_continue
  ).

% -------- input / print helpers --------
ask_preferences(prefs(Genre, MinScore, MaxEps)) :-
  ask_genre_interactive(Genre),
  prompt_number('> Minimum MAL score (0.0 - 10.0, default 0): ', 0.0, 10.0, 0.0, MinScore),
  prompt_int('> Maximum episodes (e.g., 26; default 9999): ', 1, 100000, 9999, MaxEps).

ask_age_filter(Out) :-
  format('Optional: minimum viewer age for filtering (blank to skip):~n> '),
  read_line_to_string(user_input, S),
  ( S == "" -> Out = none
  ; ( catch(number_string(Age, S), _, fail), integer(Age), Age>=0, Age=<120 ) -> Out = some(Age)
  ; format('Invalid age. Skipping age filter.~n'), Out = none ).

ask_length_filter(Out) :-
  format('Optional length bucket [short/medium/long] (blank for any):~n> '),
  read_line_to_string(user_input, B0), string_lower(B0, B),
  ( B == "" -> Out = none
  ; member(B, ["short","medium","long"]) -> atom_string(Bucket, B), Out = some(Bucket)
  ; format('Invalid bucket. Skipping length filter.~n'), Out = none ).

print_indexed_list(L) :- print_indexed_list(1, L).
print_indexed_list(_, []).
print_indexed_list(I, [T|R]) :-
  format('(~w) ', [I]), print_card(T),
  I1 is I+1, print_indexed_list(I1, R).

decode_html_entities(S0, SOut) :-
  to_s(S0, S1),
  replace_all(S1, "&#039;", "'", R1),
  replace_all(R1,  "&amp;",  "&", R2),
  replace_all(R2,  "&quot;", "\"", R3),
  replace_all(R3,  "&lt;",    "<", R4),
  replace_all(R4,  "&gt;",    ">", SOut).

replace_all(S, From, To, Out) :-
  ( sub_string(S, Before, _, _, From) ->
      string_length(From, LFrom),
      sub_string(S, 0, Before, _, Prefix),
      Start is Before + LFrom, string_length(S, SL),
      SuffixLen is SL - Start, sub_string(S, Start, SuffixLen, 0, Suffix),
      replace_all(Suffix, From, To, ReplacedSuffix),
      string_concat(Prefix, To, Temp), string_concat(Temp, ReplacedSuffix, Out)
  ; Out = S ).

to_s(X, S) :- ( string(X) -> S=X ; atom(X) -> atom_string(X,S) ; number(X) -> number_string(X,S) ; term_string(X,S) ).

print_card(Title0) :-
  ( title_details(Title0, Genres, Eps, S, Studio0, Rating0) ->
      decode_html_entities(Title0, Title),
      decode_html_entities(Studio0, Studio),
      decode_html_entities(Rating0, Rating),
      format('~n[+] ~w~n', [Title]),
      format('    Episodes: ~w | Score: ~w | Studio: ~w | Rating: ~w~n', [Eps, S, Studio, Rating]),
      format('    Genres: ~w~n', [Genres]),
      ( watchlist(Title0) -> format('    On your watchlist~n') ; true )
  ; format('~n[+] ~w~n', [Title0]),
    format('    (details unavailable in KB)~n') ).

watchlist_total_minutes(M) :-
  findall(Eps, (watchlist(T), anime_fact(_,T,_,Eps,_,_,_)), EpsList),
  sum_list(EpsList, Episodes), M is Episodes * 24.
watchlist_total_hours(H) :- watchlist_total_minutes(M), H is M / 60.

add_to_watchlist(Typed0) :-
  ( resolve_title(Typed0, Canon)
  -> ( watchlist(Canon) -> true ; assertz(watchlist(Canon)) )
  ; fail ).

choose_title_from(L, TOut) :-
  length(L, Len),
  format('Type the index (1-~w) or exact title:~n> ', [Len]),
  read_line_to_string(user_input, In0),
  ( catch(number_string(N, In0), _, fail), integer(N), N>=1, N=<Len
  -> nth1(N, L, TOut)
  ; string_trim(In0, In1), strip_quotes(In1, In2), string_lower(In2, IL),
    member(T, L), to_s(T, TS), decode_html_entities(TS, TD), string_lower(TD, TL),
    IL = TL, !, TOut = T
  ; format('Not found. Try again.~n'), choose_title_from(L, TOut) ).

prompt_yes_no(Prompt, Ans) :-
  format(Prompt), read_line_to_string(user_input, S0), string_lower(S0, S),
  ( member(S, ["y","yes","yeah","yep"]) -> Ans = yes
  ; member(S, ["n","no","nope"])        -> Ans = no
  ; format('Please answer y/n.~n'), prompt_yes_no(Prompt, Ans) ).

prompt_int(Prompt, Min, Max, Default, Out) :-
  format(Prompt), read_line_to_string(user_input, S),
  ( S = "" -> Out = Default
  ; ( catch(number_string(N, S), _, fail), integer(N), N>=Min, N=<Max -> Out = N
    ; format('Please enter an integer between ~w and ~w.~n', [Min, Max]),
      prompt_int(Prompt, Min, Max, Default, Out) ) ).

prompt_number(Prompt, Min, Max, Default, Out) :-
  format(Prompt), read_line_to_string(user_input, S),
  ( S = "" -> Out = Default
  ; ( catch(number_string(N, S), _, fail), number(N), N>=Min, N=<Max -> Out = N
    ; format('Please enter a number between ~w and ~w.~n', [Min, Max]),
      prompt_number(Prompt, Min, Max, Default, Out) ) ).

pairs_values([], []).
pairs_values([_-V|T], [V|R]) :- pairs_values(T, R).

help :-
  nl,
  format('--- Help ---~n'),
  format('1. Recommend: Enter genre, min score, and max episodes. Optional age and length filters.~n'),
  format('   - At the genre prompt, type "?" to see all genres with counts, then pick by number or name.~n'),
  format('   - Results include an explanation of the score breakdown (genre + score + length).~n'),
  format('2. Surprise me: Give a genre; picks a short, >= 7.0 item at random (or press ENTER for any).~n'),
  format('3. Top charts: Show top-scoring items; optionally filter by genre and length.~n'),
  format('4. Search titles: Substring search; print cards; you can add to watchlist (by index or title).~n'),
  format('5. Watchlist: Add/remove/show/clear and total time (24 min per episode).~n'),
  format('0. Quit: Exit the program.~n'),
  format('--------------~n').
