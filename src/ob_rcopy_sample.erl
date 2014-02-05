%% --------------------------------------------------------------------
%% @author Daisuke YAMAGUCHI <gutio81@gmail.com>
%% @version 0.0.1
%% @end
%% --------------------------------------------------------------------

-module(ob_rcopy_sample).

-define(CHUNK_10MB, 1024 * 1024 * 10).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all/0, split_file_copy1/0, split_file_copy2/0, file_copy/0, dir_copy/0]).

all() ->
    split_file_copy1(),
    split_file_copy2(),
    dir_copy(),
    file_copy().

split_file_copy1() ->
    ob_rcopy:copy({node(), "./sample_data/source/15mb"}, {node(), "./sample_data/dest/15mb"}, ?CHUNK_10MB).

split_file_copy2() ->
    ob_rcopy:copy({node(), "./sample_data/source/20mb"}, {node(), "./sample_data/dest/20mb"}, ?CHUNK_10MB).

dir_copy() ->
    ob_rcopy:copy({node(), "./sample_data/source/parent_dir"}, {node(), "./sample_data/dest/parent_dir"}, ?CHUNK_10MB).

file_copy() -> 
    ob_rcopy:copy({node(), "./sample_data/source/10mb"}, {node(), "./sample_data/dest/10mb"}, ?CHUNK_10MB).

