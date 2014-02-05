%% --------------------------------------------------------------------
%% @author Daisuke YAMAGUCHI <gutio81@gmail.com>
%% @version 0.0.1
%% @end
%% --------------------------------------------------------------------

-module(ob_rcopy).

-include_lib("kernel/include/file.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([copy/3]).

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
    end;

copy(SourcePath, Dest = {_DestNode, _DestPath}, Chunk) ->
    copy({node(), SourcePath}, Dest, Chunk);

copy(Source = {_SourceNode, _SourcePath}, DestPath, Chunk) ->
    copy(Source, {node(), DestPath}, Chunk);

copy(SourcePath, DestPath, Chunk) ->
    copy({node(), SourcePath}, {node(), DestPath}, Chunk).

%% ====================================================================
%% Internal functions
%% ====================================================================

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
            file_copy_at_once(Source, Dest);
        {error, Reason} ->
            {error, Reason}
    end.

file_copy_at_once({SourceNode, SourcePath}, {DestNode, DestPath}) ->
    {ok, Bin} = rpc:call(SourceNode, file, read_file, [SourcePath]),
    rpc:call(DestNode, file, write_file, [DestPath, Bin]).

split_file_copy({SourceNode, SourcePath}, {DestNode, DestPath}, Chunk) ->
    {ok, ReadFile} = rpc:call(SourceNode, file, open, [SourcePath, read]),
    {ok, WriteFile} = rpc:call(DestNode, file, open, [DestPath, write]),
    try
        split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk)
    after
        rpc:call(SourceNode, file, close, [ReadFile]),
        rpc:call(DestNode, file, close, [WriteFile])
    end.

split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk) ->
    Read = rpc:call(SourceNode, file, read, [ReadFile, Chunk]),
    case Read of
        {ok, ReadData} -> 
            case rpc:call(DestNode, file, write, [WriteFile, ReadData]) of
                ok ->
                    split_file_copy_loop(SourceNode, ReadFile, DestNode, WriteFile, Chunk);
                {error, Reason} ->
                    {error, Reason}
            end;
        eof -> ok;
        {error, Reason} ->
            {error, Reason}
    end.
