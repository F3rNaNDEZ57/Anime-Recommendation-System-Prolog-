:- module(main, [
    calculate_score/3,
    search_title_ci/2,
    has_genre_ci/2,
    length_bucket/2,
    top_by_score/2,
    top_by_score_in_genre/3,
    ok_for_age/2,
    surprise_me/2,
    title_details/6,
    explain_score/5,
    all_genres/1,
    take_first/3,
    anime_fact/7
]).

:- use_module(library(csv)).
:- use_module(library(random)).
:- dynamic anime_fact/7.

% ---------- CSV loader ----------
load_animes(FileName) :-
    format('~nAttempting to load Knowledge Base from: ~w...~n', [FileName]),
    retractall(anime_fact(_,_,_,_,_,_,_)),
    csv_read_file(FileName, Rows),
    Rows = [_Header|DataRows],
    process_rows(DataRows),
    format('~nKnowledge Base loaded.~n').

process_rows([]).
process_rows([H|T]) :- process_row(H), process_rows(T).

process_row(Row) :-
    functor(Row, _, Arity), Arity >= 28,
    arg(1, Row, ID),          % anime_id
    arg(2, Row, Title),       % title
    arg(8, Row, Episodes),    % episodes
    arg(14, Row, Rating),     % rating
    arg(15, Row, Score),      % score
    arg(27, Row, Studio),     % studio
    arg(28, Row, GenresAtom), % genre
    number(ID), atom(GenresAtom), !,
    split_string(GenresAtom, ",", " \"'[]", GenresList),
    assertz(anime_fact(ID, Title, GenresList, Episodes, Score, Studio, Rating)).
process_row(Row) :-
    ( arg(2, Row, Title)
    -> format('~n[Skipped] Bad data for: ~w~n', [Title])
    ;  format('~n[Skipped] Unreadable row.~n')
    ), !.
process_row(_) :- format('~n[Skipped] Unreadable row.~n').

% ---------- helpers ----------
to_str(X, S) :- ( string(X) -> S=X ; atom(X) -> atom_string(X,S) ; number(X) -> number_string(X,S) ; term_string(X,S) ).
ci_member(Item, List) :-
    to_str(Item, S0), string_lower(S0, S),
    member(X, List),  to_str(X, Y0), string_lower(Y0, Y),
    S = Y.
take_first(0, _, []) :- !.
take_first(_, [], []).
take_first(N, [H|T], [H|R]) :- N1 is N-1, take_first(N1, T, R).

% age thresholds
rating_min_age("G", 0).
rating_min_age("PG", 10).
rating_min_age("TV-PG", 10).
rating_min_age("PG-13", 13).
rating_min_age("TV-14", 14).
rating_min_age("R", 17).
rating_min_age("R+", 17).
rating_min_age("TV-MA", 17).
rating_min_age("Rx", 18).

% ---------- scoring ----------
score_genre(prefs(Genre,_,_), GenresList, 40) :- ci_member(Genre, GenresList), !.
score_genre(_, _, 0).

score_score(prefs(_,MinScore,_), Score, 30) :- number(Score), Score >= MinScore, !.
score_score(_, _, 0).

score_length(prefs(_,_,MaxEps), Episodes, 30) :- number(Episodes), Episodes > 0, Episodes < MaxEps, !.
score_length(_, _, 0).

calculate_score(Prefs, Title, Total) :-
    anime_fact(_, Title, GenresList, Episodes, Score, _, _),
    score_genre(Prefs, GenresList, GS), GS > 0,
    score_score(Prefs, Score, SS),
    score_length(Prefs, Episodes, LS),
    Total is GS+SS+LS.

% ---------- queries ----------
search_title_ci(Q, Title) :-
  anime_fact(_, Title, _, _, _, _, _),
  to_str(Title, T), string_lower(T, TL),
  to_str(Q, QS),    string_lower(QS, QL),
  sub_string(TL, _, _, _, QL).

has_genre_ci(Title, Genre) :-
  anime_fact(_, Title, Genres, _, _, _, _),
  ci_member(Genre, Genres).

length_bucket(Title, short)  :- anime_fact(_,Title,_,E,_,_,_), number(E), E =< 13.
length_bucket(Title, medium) :- anime_fact(_,Title,_,E,_,_,_), number(E), E >= 14, E =< 26.
length_bucket(Title, long)   :- anime_fact(_,Title,_,E,_,_,_), number(E), E >= 27.

top_by_score(N, Out) :-
  findall(S-Title, (anime_fact(_,Title,_,E,S,_,_), number(S), number(E), E > 0), Pairs),
  keysort(Pairs, Asc), reverse(Asc, Desc),
  take_first(N, Desc, Top),
  findall(rec(T,S), member(S-T, Top), Out).

top_by_score_in_genre(Genre, N, Out) :-
  findall(S-Title,
         ( anime_fact(_,Title,G,E,S,_,_), number(S), number(E), E>0, ci_member(Genre,G) ),
         Pairs),
  keysort(Pairs, Asc), reverse(Asc, Desc),
  take_first(N, Desc, Top),
  findall(rec(T,S), member(S-T, Top), Out).

ok_for_age(Age, Title) :-
  anime_fact(_, Title, _, _, _, _, Rating),
  ( rating_min_age(Rating, Min) -> Age >= Min ; true ).

surprise_me(Genre, Title) :-
  findall(T, (has_genre_ci(T, Genre),
              length_bucket(T, short),
              anime_fact(_,T,_,_,S,_,_), number(S), S >= 7.0), L),
  L \= [], random_member(Title, L).

title_details(Title, Genres, Episodes, Score, Studio, Rating) :-
  anime_fact(_, Title, Genres, Episodes, Score, Studio, Rating).

explain_score(Prefs, Title, GS, SS, LS) :-
  anime_fact(_, Title, Genres, Episodes, RawScore, _, _),
  score_genre(Prefs, Genres, GS),
  score_score(Prefs, RawScore, SS),
  score_length(Prefs, Episodes, LS).

all_genres(Genres) :-
  setof(G, Title^GL^(anime_fact(_,Title,GL,_,_,_,_), member(G,GL)), Genres).

% ---------- auto-load ----------
initialize_kb :-
    prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, 'AnimeList.csv'], '/', Path),
    write('Attempting to load Knowledge Base from: '), writeln(Path),
    load_animes(Path).

:- initialize_kb.
