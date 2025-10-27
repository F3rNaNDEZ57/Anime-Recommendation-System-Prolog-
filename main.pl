% ===================================================================
% --- 1. LIBRARIES AND DIRECTIVES
% ===================================================================

:- use_module(library(csv)).    % For reading CSV files
:- use_module(library(random)).  % For the "surprise me" feature
:- dynamic anime_fact/7.        % Tells Prolog we will create anime_fact(...) facts at runtime

% ===================================================================
% --- 2. KNOWLEDGE BASE LOADER
% ===================================================================

% --- Main Predicate (Loader) ---
load_animes(FileName) :-
    format('~nAttempting to load Knowledge Base from: ~w...~n', [FileName]),
    retractall(anime_fact(_,_,_,_,_,_,_)), % Clear old facts
    
    % Use the flexible 2-argument csv_read_file
    csv_read_file(FileName, Rows),
    
    Rows = [_Header | DataRows], % Skip the header row
    
    process_rows(DataRows),
    format('~n✅ Knowledge Base loaded.~n').

% --- List Processing Helpers ---
process_rows([]). % Base case: no more rows
process_rows([Head|Tail]) :- 
    process_row(Head),
    process_rows(Tail).

% --- PREDICATE 1: The "Happy Path" (CORRECTED COLUMNS) ---
% Tries to process a good row.
process_row(Row) :-
    % Safety check: ensure the row has at least 28 columns
    functor(Row, _, Arity),
    Arity >= 28,

    % === EXTRACT DATA (Corrected Columns) ===
    arg(1, Row, ID),          % Column 1: anime_id
    arg(2, Row, Title),       % Column 2: title
    arg(8, Row, Episodes),    % Column 8: episodes
    arg(14, Row, Rating),     % Column 14: rating
    arg(15, Row, Score),      % Column 15: score
    arg(27, Row, Studio),     % Column 27: studio
    arg(28, Row, Genres_Str), % Column 28: genre
    % ========================================

    % Validation: We need a number ID and an atom for genres
    number(ID),
    atom(Genres_Str),
    !, % Commit to this rule if validation passes

    % Data Cleaning (Genres)
    % Split by comma, remove spaces and any '[]" characters
    split_string(Genres_Str, ",", " \"'[]", GenresList),
    
    % Create the fact in memory
    assertz(anime_fact(ID, Title, GenresList, Episodes, Score, Studio, Rating)).

% --- PREDICATE 2: The "Safety Net" ---
% Catches rows that failed Predicate 1
process_row(Row) :-
    % Use arg(2) to get title, if possible
    (arg(2, Row, Title) ->
        format('~n[Skipped] Bad data for: ~w (e.g., missing genre or bad row structure).~n', [Title])
    ;   % Fallback if we can't even get the title
        format('~n[Skipped] Unreadable row: ~w~n', [Row])
    ),
    !. % Use a cut here to prevent backtracking to Fallback 3

% --- PREDICATE 3: Fallback Safety Net ---
% This will catch any remaining failures, though it's unlikely
process_row(_) :-
    format('~n[Skipped] Unreadable row.~n').

% ===================================================================
% --- 3. RECOMMENDATION RULES ("THE EXPERT")
% ===================================================================

% --- 3.1 Helper Predicates (for Advanced Rules) ---

% Helper to convert any term to a (lowercase) string
to_str(X, S) :- 
    (string(X) -> S = X 
    ; atom(X) -> atom_string(X, S) 
    ; number(X) -> number_string(X, S) 
    ; term_string(X, S)
    ).

% Case-insensitive member check
ci_member(Item, List) :-
    to_str(Item, S0), string_lower(S0, S), % Lowercase Item
    member(X, List),                       % Iterate list
    to_str(X, Y0), string_lower(Y0, Y),    % Lowercase list element
    S = Y.                                 % Check for match

% Helper to get first N elements from a list
take_first(0, _, []):-!.
take_first(_, [], []).
take_first(N, [H|T], [H|R]):- N1 is N - 1, take_first(N1, T, R).

% Helper for age gate
rating_min_age("G", 0).
rating_min_age("PG", 10).
rating_min_age("TV-PG", 10).
rating_min_age("PG-13", 13).
rating_min_age("TV-14", 14).
rating_min_age("R", 17).
rating_min_age("R+", 17).
rating_min_age("TV-MA", 17).
rating_min_age("Rx", 18).


% --- 3.2 Simple/Complex Rules (UPGRADED to use ci_member) ---

% Rule 1: Recommend by Genre (UPGRADED)
recommend_by_genre(Genre, Title, Reason) :-
    anime_fact(_, Title, GenresList, _, _, _, _),
    ci_member(Genre, GenresList), % <--- UPGRADED
    format(atom(Reason), 'Matches your preferred genre: ~w', [Genre]).

% Rule 2: Recommend by Score (Safe)
recommend_high_score(MinScore, Title, Reason) :-
    anime_fact(_, Title, _, _, Score, _, _),
    number(Score),     % IMPORTANT: Check if Score is a number
    Score >= MinScore,
    format(atom(Reason), 'Has a high score of ~w (>= ~w)', [Score, MinScore]).

% Rule 3: Recommend by Studio
recommend_by_studio(Studio, Title, Reason) :-
    anime_fact(_, Title, _, _, _, Studio, _),
    Studio \= '',
    format(atom(Reason), 'Is made by your preferred studio: ~w', [Studio]).

% Rule 4: Recommend by Genre AND Score (UPGRADED)
recommend_genre_and_score(Genre, MinScore, Title, Reason) :-
    anime_fact(_, Title, GenresList, _, Score, _, _),
    number(Score),     % Safety check
    Score >= MinScore,
    ci_member(Genre, GenresList), % <--- UPGRADED
    format(atom(Reason), 'Matches genre ~w and has a high score of ~w (>= ~w)', [Genre, Score, MinScore]).

% Rule 5: Recommend "Binge-Worthy" (Genre AND Length) (UPGRADED)
recommend_genre_and_length(Genre, MaxEps, Title, Reason) :-
    anime_fact(_, Title, GenresList, Episodes, _, _, _),
    number(Episodes),  % Safety check
    Episodes < MaxEps,
    ci_member(Genre, GenresList), % <--- UPGRADED
    format(atom(Reason), 'Matches genre ~w and is short (~w episodes < ~w)', [Genre, Episodes, MaxEps]).

% Rule 6: Recommend by Studio AND Genre (UPGRADED)
recommend_studio_and_genre(Studio, Genre, Title, Reason) :-
    anime_fact(_, Title, GenresList, _, _, Studio, _),
    Studio \= '',      % Safety check
    ci_member(Genre, GenresList), % <--- UPGRADED
    format(atom(Reason), 'Is made by ~w and matches genre ~w', [Studio, Genre]).


% --- 3.3 Advanced Rules & Features (Your New Rules) ---

% 1. Case-insensitive title search
search_title_ci(Q, Title) :-
  anime_fact(_, Title, _, _, _, _, _),
  to_str(Title, T), string_lower(T, TL),
  to_str(Q, QS),    string_lower(QS, QL),
  sub_string(TL, _, _, _, QL).

% 2. Case-insensitive genre check (helper)
has_genre_ci(Title, Genre) :-
  anime_fact(_, Title, Genres, _, _, _, _),
  ci_member(Genre, Genres).

% 3. Length buckets
length_bucket(Title, short)  :- anime_fact(_,Title,_,E,_,_,_), number(E), E =< 13.
length_bucket(Title, medium) :- anime_fact(_,Title,_,E,_,_,_), number(E), E >= 14, E =< 26.
length_bucket(Title, long)   :- anime_fact(_,Title,_,E,_,_,_), number(E), E >= 27.

% 4. Quick top-N by score
top_by_score(N, Out) :-
  findall(S-Title, (anime_fact(_,Title,_,_,S,_,_), number(S)), Pairs),
  keysort(Pairs, SAsc), reverse(SAsc, SDesc),
  take_first(N, SDesc, Top), findall(rec(T,S), member(S-T,Top), Out).

top_by_score_in_genre(Genre, N, Out) :-
  findall(S-Title,
    ( anime_fact(_,Title,G,_,S,_,_), number(S), ci_member(Genre,G) ),
    Pairs),
  keysort(Pairs,SAsc), reverse(SAsc,SDesc),
  take_first(N,SDesc,Top), findall(rec(T,S), member(S-T,Top), Out).

top_by_score_in(Genre, Bucket, N, Out) :-
  findall(S-Title,
    ( anime_fact(_,Title,G,_,S,_,_), number(S),
      ci_member(Genre,G), length_bucket(Title, Bucket) ),
    Pairs),
  keysort(Pairs,SAsc), reverse(SAsc,SDesc),
  take_first(N,SDesc,Top), findall(rec(T,S), member(S-T,Top), Out).

% 5. Age gate based on rating
ok_for_age(Age, Title) :-
  anime_fact(_, Title, _, _, _, _, Rating),
  ( rating_min_age(Rating, Min) -> Age >= Min ; true ). % Allow if rating is unknown

% 6. "Surprise me" random pick (filtered)
surprise_me(Genre, Title) :-
  findall(T, (has_genre_ci(T, Genre),
              length_bucket(T, short),
              anime_fact(_,T,_,_,S,_,_), number(S), S >= 7.0), L),
  L \= [], % Ensure list is not empty
  random_member(Title, L).

% 7. Quick watch-time estimate
watch_time(Title, MinutesPerEp, Mins, Hours) :-
  anime_fact(_, Title, _, Eps, _, _, _),
  number(Eps),
  Mins is Eps * MinutesPerEp,
  Hours is Mins / 60.

% Default watch_time with 24 mins/ep
watch_time(Title, Mins, Hours) :- watch_time(Title, 24, Mins, Hours).


% --- 3.4 Utility Rules (for UI) ---

% Finds all unique, sorted genres from the KB
all_genres(Genres) :-
    setof(G, 
          Title^GenresList^(
              anime_fact(_, Title, GenresList, _, _, _, _),
              member(G, GenresList) % Keep member/2 here, it's faster for extraction
          ), 
          Genres).

% Finds all unique, sorted studios from the KB
all_studios(Studios) :-
    setof(Studio, 
          ID^Title^Genres^Eps^Score^Rating^(
              anime_fact(ID, Title, Genres, Eps, Score, Studio, Rating),
              Studio \= '' % Ignore empty studio names
          ), 
          Studios).

% ===================================================================
% --- 4. AUTO-LOADER (RUNS ON STARTUP)
% ===================================================================

% This predicate finds the CSV file relative to *this* file
% and then calls load_animes/1.
initialize_kb :-
    % Get directory of this .pl file
    prolog_load_context(directory, Dir), 
    
    % Build the full, cross-platform path
    atomic_list_concat([Dir, 'AnimeList.csv'], '/', Path), 
    
    % Use write/1 to print the path atom as-is
    write('Attempting to load Knowledge Base from: '), writeln(Path),
    
    load_animes(Path).

% --- STARTUP DIRECTIVE ---
% This line automatically runs initialize_kb when the file is loaded.
:- initialize_kb.

% ===================================================================
% --- 5. TEST PREDICATE (Corrected)
% ===================================================================

% --- Helper to print a simple list ---
print_simple_list([]).
print_simple_list([H|T]) :-
    format('  - ~w~n', [H]),
    print_simple_list(T).

% --- Main Test Predicate ---
% Run this with ?- test_all.
test_all :-
    format('~n--- RUNNING ALL TESTS ---~n'),
    
    % Test 1: Case-insensitive title search
    format('~n[1] Testing Title Search (ci) for "bebop":~n'),
    (search_title_ci("bebop", T) -> format('  - Found: ~w~n', [T]) ; format('  - Not found.~n')),
    
    % Test 2: Case-insensitive genre matching
    format('~n[2] Testing Genre Search (ci) for "sci-fi":~n'),
    (has_genre_ci(T2, "sci-fi") -> format('  - Found: ~w~n', [T2]) ; format('  - Not found.~n')),
    
    % Test 3: Length buckets (FIXED)
    format('~n[3] Testing Length Bucket using "bebop" search:~n'),
    ( (search_title_ci("bebop", T3), length_bucket(T3, B)) -> 
        format('  - Found "~w" is: ~w~n', [T3, B]) 
    ;   format('  - Not found.~n')
    ),

    % Test 4: Top-N rules
    format('~n[4a] Testing Top 3 By Score:~n'),
    (top_by_score(3, R) -> print_simple_list(R) ; format('  - Failed.~n')),
    format('~n[4b] Testing Top 3 "drama":~n'),
    (top_by_score_in_genre("drama", 3, R2) -> print_simple_list(R2) ; format('  - Failed.~n')),
    format('~n[4c] Testing Top 3 "action" + "short":~n'),
    (top_by_score_in("action", short, 3, R3) -> print_simple_list(R3) ; format('  - Failed.~n')),
    
    % (Note on 4c: It correctly finds Gintama movies/OVAs, which ARE short. This is correct!)

    % Test 5: Age gate
    format('~n[5] Testing Age Gate for age 12 (should find many titles):~n'),
    (ok_for_age(12, T5) -> 
        format('  - Found OK title: ~w~n', [T5]) 
    ;   format('  - Failed.~n')
    ),

    % Test 6: "Surprise me"
    format('~n[6] Testing "Surprise Me" for "comedy":~n'),
    (surprise_me("comedy", T6) -> format('  - Surprise: ~w~n', [T6]) ; format('  - Not found.~_~n')),

    % Test 7: Watch-time estimate (FIXED)
    format('~n[7] Testing Watch Time using "bebop" search:~n'),
    ( (search_title_ci("bebop", T7), watch_time(T7, M, H)) -> 
        format('  - Found "~w": ~w mins (~w hrs)~n', [T7, M, H]) 
    ;   format('  - Not found.~n')
    ),
    
    format('~n--- ALL TESTS COMPLETE ---~n').