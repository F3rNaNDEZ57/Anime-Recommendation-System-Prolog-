:- use_module(library(csv)). % Load the CSV library


load_animes(FileName) :-
    csv_read_file(FileName, Rows, [functor(anime_row)]),
    process_rows(Rows).