-module(ubt_fsm).

init(Action, Info) ->
    case Action of
        connect ->
            client();
        listen ->
            server()
    end.
    % a_fsm_handle.

server(State, Action, Info) ->
    %listen -> listen
    case State of
        closed ->            
            loop(State, Info);
        listen ->
            %close -> closed
            % r_syn -> syn_rcvd
            loop(State, Info);
        syn_rcvd ->
            % close -> fin_wait_1
            loop(State, Info);
        established ->
            loop(State, Info)
    end.

client(State, Action, Info) ->
    %connect -> syn_sent
    case State of
        closed ->
            %listen -> listen
            loop(State, Info);
        syn_sent ->
            % r_syn_ack -> established
            loop(State, Info);
        established ->
            loop(State, Info)
    end.
    
active_close() ->
    case State of
        %%
        fin_wait_1 ->
            loop(State, Info);
        fin_wait_2 ->
            loop(State, Info);
        timed_wait ->
            loop(State, Info);
        closing ->
            loop(State, Info)
    end.

passive_close() ->
    case State of
        %%
        close_wait ->
            loop(State, Info);
        last_ack ->
            loop(State, Info)
    end

