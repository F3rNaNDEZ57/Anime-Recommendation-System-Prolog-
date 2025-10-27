% ===================================================================
% --- 0. MODULE DEFINITION (Updated)
% ===================================================================
:- module(main, [
    % Master scoring predicate
    calculate_score/3,

    % Advanced rules
    search_title_ci/2,
    has_genre_ci/2,
    length_bucket/2,
    top_by_score/2,
    top_by_score_in_genre/3,
    top_by_score_in/4,
    ok_for_age/2,
    surprise_me/2,
    watch_time/3,
    watch_time/4,

    % Explainers / details
    title_details/6,
    explain_score/5,

    % Utility rules
    all_genres/1,
    all_studios/1,
    take_first/3,
    anime_fact/7
]).

% ===================================================================
% --- 1. LIBRARIES AND DIRECTIVES
% ===================================================================

:- use_module(library(csv)).     % For reading CSV files
:- use_module(library(random)).  % For the "surprise me" feature
:- dynamic anime_fact/7.         % We will create anime_fact(...) facts at runtime

% ===================================================================
% --- 2. KNOWLEDGE BASE LOADER
% ===================================================================

% --- Main Predicate (Loader) ---
load_animes(FileName) :-
    format('~nAttempting to load Knowledge Base from: ~w...~n', [FileName]),
    retractall(anime_fact(_,_,_,_,_,_,_)), % Clear old facts
    csv_read_file(FileName, Rows),
    Rows = [_Header | DataRows], % Skip the header row
    process_rows(DataRows),
    format('~nKnowledge Base loaded.~n').

% --- List Processing Helpers ---
process_rows([]).
process_rows([Head|Tail]) :-
    process_row(Head),
    process_rows(Tail).

% --- PREDICATE 1: The "Happy Path" (CORRECTED COLUMNS) ---
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

    number(ID),
    atom(Genres_Str),
    !,

    % Data Cleaning (Genres)
    split_string(Genres_Str, ",", " \"'[]", GenresList),

    % Create the fact in memory
    assertz(anime_fact(ID, Title, GenresList, Episodes, Score, Studio, Rating)).

% --- PREDICATE 2: The "Safety Net" ---
process_row(Row) :-
    (arg(2, Row, Title) ->
        format('~n[Skipped] Bad data for: ~w (e.g., missing genre or bad row structure).~n', [Title])
    ;   format('~n[Skipped] Unreadable row: ~w~n', [Row])
    ),
    !.

% --- PREDICATE 3: Fallback ---
process_row(_) :-
    format('~n[Skipped] Unreadable row.~n').

% ===================================================================
% --- 3. RECOMMENDATION ENGINE ("THE EXPERT")
% ===================================================================

% --- 3.1 Helper Predicates ---

% Helper to convert any term to a (lowercase) string
to_str(X, S) :-
    ( string(X) -> S = X
    ; atom(X)   -> atom_string(X, S)
    ; number(X) -> number_string(X, S)
    ; term_string(X, S)
    ).

% Case-insensitive member check
ci_member(Item, List) :-
    to_str(Item, S0), string_lower(S0, S),
    member(X, List),
    to_str(X, Y0), string_lower(Y0, Y),
    S = Y.

% Helper to get first N elements from a list
take_first(0, _, []) :- !.
take_first(_, [], []).
take_first(N, [H|T], [H|R]) :- N1 is N - 1, take_first(N1, T, R).

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

% ==========================================================
% --- 3.2 SCORING RULES ---
% ==========================================================

% Scores 40 points if the genre matches
score_genre(prefs(Genre,_,_), GenresList, 40) :-
    ci_member(Genre, GenresList), !.
score_genre(_, _, 0).

% Scores 30 points if the score is high enough
score_score(prefs(_,MinScore,_), Score, 30) :-
    number(Score), Score >= MinScore, !.
score_score(_, _, 0).

% Scores 30 points if the length is a good fit
score_length(prefs(_,_,MaxEps), Episodes, 30) :-
    number(Episodes), Episodes > 0, Episodes < MaxEps, !.
score_length(_, _, 0).

% ==========================================================
% --- 3.3 Master Scoring Predicate ---
% ==========================================================
calculate_score(Prefs, Title, TotalScore) :-
    anime_fact(_, Title, GenresList, Episodes, Score, _, _),
    score_genre(Prefs, GenresList, GenreScore), GenreScore > 0,
    score_score(Prefs, Score, ScoreScore),
    score_length(Prefs, Episodes, LengthScore),
    TotalScore is GenreScore + ScoreScore + LengthScore.

% --- 3.4 Advanced Rules & Features ---

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
  findall(S-Title, (anime_fact(_,Title,_,E,S,_,_), number(S), number(E), E > 0), Pairs),
  keysort(Pairs, SAsc), reverse(SAsc, SDesc),
  take_first(N, SDesc, Top),
  findall(rec(T,S), member(S-T,Top), Out).

top_by_score_in_genre(Genre, N, Out) :-
  findall(S-Title,
    ( anime_fact(_,Title,G,E,S,_,_), number(S), number(E), E > 0, ci_member(Genre,G) ),
    Pairs),
  keysort(Pairs,SAsc), reverse(SAsc,SDesc),
  take_first(N,SDesc,Top),
  findall(rec(T,S), member(S-T,Top), Out).

top_by_score_in(Genre, Bucket, N, Out) :-
  findall(S-Title,
    ( anime_fact(_,Title,G,E,S,_,_), number(S), number(E), E > 0,
      ci_member(Genre,G), length_bucket(Title, Bucket) ),
    Pairs),
  keysort(Pairs,SAsc), reverse(SAsc,SDesc),
  take_first(N,SDesc,Top),
  findall(rec(T,S), member(S-T,Top), Out).

% 5. Age gate based on rating
ok_for_age(Age, Title) :-
  anime_fact(_, Title, _, _, _, _, Rating),
  ( rating_min_age(Rating, Min) -> Age >= Min ; true ).

% 6. "Surprise me" random pick (filtered)
surprise_me(Genre, Title) :-
  findall(T, (has_genre_ci(T, Genre),
              length_bucket(T, short),
              anime_fact(_,T,_,_,S,_,_), number(S), S >= 7.0), L),
  L \= [],
  random_member(Title, L).

% 7. Quick watch-time estimate
watch_time(Title, MinutesPerEp, Mins, Hours) :-
  anime_fact(_, Title, _, Eps, _, _, _),
  number(Eps),
  Mins is Eps * MinutesPerEp,
  Hours is Mins / 60.

watch_time(Title, Mins, Hours) :- watch_time(Title, 24, Mins, Hours).

% --- 3.5 Utility Rules (for UI) ---
all_genres(Genres) :-
    setof(G,
          Title^GenresList^(
              anime_fact(_, Title, GenresList, _, _, _, _),
              member(G, GenresList)
          ),
          Genres).

all_studios(Studios) :-
    setof(Studio,
          ID^Title^Genres^Eps^Score^Rating^(
              anime_fact(ID, Title, Genres, Eps, Score, Studio, Rating),
              Studio \= ''
          ),
          Studios).

title_details(Title, Genres, Episodes, Score, Studio, Rating) :-
  anime_fact(_, Title, Genres, Episodes, Score, Studio, Rating).

explain_score(Prefs, Title, GenreScore, ScoreScore, LengthScore) :-
  anime_fact(_, Title, Genres, Episodes, RawScore, _, _),
  score_genre(Prefs, Genres, GenreScore),
  score_score(Prefs, RawScore, ScoreScore),
  score_length(Prefs, Episodes, LengthScore).

% ===================================================================
% --- 4. AUTO-LOADER (RUNS ON STARTUP)
% ===================================================================

initialize_kb :-
    prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, 'AnimeList.csv'], '/', Path),
    write('Attempting to load Knowledge Base from: '), writeln(Path),
    load_animes(Path).

:- initialize_kb.
