%% src_text.pl
%% swi-prolog
%% compile: ['src_text.pl']
%%
%% Sample prolog source to be injected in a pengine upon creation before
%% the pengine will attempt to solve any queries. Read into string/binary
%% and passed as argument src_test.
%%
%% Author Kim Hammar limmen@github.com <kimham@kth.se>

%%%===================================================================
%%% Facts
%%%===================================================================

pengine_child(pingu).
pengine_child(pongi).
pengine_child(pingo).
pengine_child(pinga).

pengine_master(papa, pingu).
pengine_master(mama, pongi).