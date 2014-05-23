%%% @doc zv's custom prompt
-module(zv_shell).
-export([prompt_func/1]).

-define(Reset,  "\e[0m").
-define(Green,  "\e[0;92m").
-define(Yellow, "\e[0;93m").
-define(Blue,   "\e[0;94m").
-define(Purple, "\e[0;95m").
-define(Cyan,   "\e[0;96m").
-define(White,  "\e[0;97m").

prompt_func([{history, N}]) -> prompt_func(self(), node(), N).

prompt_func(Pid, 'nonode@nohost', N) ->
    io_lib:format(?Purple "~p " ?White "~B> " ?Reset, [Pid, N]);
prompt_func(Pid, Node, N) ->
    io_lib:format(?Purple "~p " ?Blue "(~s) " ?White "~B>> " ?Reset, [Pid, Node, N]).
