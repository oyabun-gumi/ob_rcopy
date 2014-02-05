%% @author yamaguchi
%% @doc @todo Add description to rcopy.

-module(ob_rcopy).

-include_lib("kernel/include/file.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([copy/3, file_copy/3, dir_copy/3]).

copy(Source = {SourceNode, SourcePath}, Dest = {DestNode, DestPath}, Chunk) ->
    case rpc:call(SourceNode, file, read_file_info, [SourcePath]) of
        {ok, FileInfo} when FileInfo#file_info.type =:= directory ->
            dir_copy(Source, Dest, Chunk);
        {ok, FileInfo} when FileInfo#file_info.type =:= regular ->
            rpc:call(DestNode, filelib, ensure_dir, [DestPath]),
            file_copy(Source, Dest, Chunk);
        {ok, _OtherFileType} ->
            skip;
        {error, Reason} ->
            {error, Reason}
    end.

dir_copy({SourceNode, SourcePath}, {DestNode, DestPath}, Chunk) ->
    case rpc:call(SourceNode, file, list_dir, [SourcePath]) of
        {ok, Filenames} ->
            lists:foreach(
                fun(Filename) ->
                    SourceTargetPath = SourcePath ++ "/" ++ Filename,
                    DestTargetPath = DestPath ++ "/" ++ Filename,
                    copy({SourceNode, SourceTargetPath}, {DestNode, DestTargetPath}, Chunk)
                end,
            Filenames);
        {error, Reason} ->
            {error, Reason}
    end.

file_copy(Source = {SourceNode, SourcePath}, Dest, Chunk) -> 
    case rpc:call(SourceNode, file, read_file_info, [SourcePath]) of
        {ok, FileInfo} when FileInfo#file_info.size > Chunk -> 
            split_file_copy(Source, Dest, Chunk);
        {ok, _FileInfo} -> 
            file_copy(Source, Dest);
        {error, Reason} ->
            {error, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

file_copy({SourceNode, SourcePath}, {DestNode, DestPath}) ->
    {ok, Bin} = rpc:call(SourceNode, file, read_file, [SourcePath]),
    rpc:call(DestNode, file, write_file, [DestPath, Bin]).

split_file_copy({SourceNode, SourcePath}, {DestNode, DestPath}, Chunk) ->
    {ok, ReadFile} = rpc:call(SourceNode, file, open, [SourcePath, read]),
    {ok, WriteFile} = rpc:call(DestNode, file, open, [DestPath, write]),
    try
        split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk)
    after
        ok = rpc:call(SourceNode, file, close, [ReadFile]),
        ok = rpc:call(DestNode, file, close, [WriteFile])
    end.

split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk) ->
    {ok, Read} = rpc:call(SourceNode, file, read, [ReadFile, Chunk]),
    ok = rpc:call(DestNode, file, write, [WriteFile, Read]),
    case length(Read) of
        Chunk -> split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk);
        _ -> ok
    end.
