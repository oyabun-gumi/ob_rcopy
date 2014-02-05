%% @author yamaguchi
%% @doc @todo Add description to rcopy.

-module(ob_rcopy_sample).

-define(CHUNK_10MB, 1024 * 1024 * 10).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all/0, copy/0, file_copy/0, dir_copy/0]).

all() ->
    copy(),
    dir_copy(),
    file_copy().

copy() ->
    rcopy:copy({node(), "./sample_data/source/15mb"}, {node(), "./sample_data/dest/15mb"}, ?CHUNK_10MB).

dir_copy() ->
    rcopy:dir_copy({node(), "./sample_data/source/parent_dir"}, {node(), "./sample_data/dest/parent_dir"}, ?CHUNK_10MB).

file_copy() -> 
    rcopy:file_copy({node(), "./sample_data/source/10mb"}, {node(), "./sample_data/dest/10mb"}, ?CHUNK_10MB).

