%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc record definitions. 
%% @end
%%%-------------------------------------------------------------------

%% state of the pengine, see http://pengines.swi-prolog.org/docs/documentation.html for documentation.
-record(pengine_state, {
          server :: string(),
          id :: binary(),
          ping_timer = make_ref() :: reference()
         }).

%% state of the master process
-record(master_state, {
          table_id :: ets:tid() | undefined
         }).


%% state of the table mgnr
-record(table_mngr_state, {table_id :: ets:tid() | undefined}).
