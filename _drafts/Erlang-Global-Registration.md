    roger@roger-pc:~$ erl -sname foo
    Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:12:12] [async-threads:10] [kernel-poll:false]
    
    Eshell V5.10.4  (abort with ^G)
    <0.39.0> (foo@roger-pc) 1> global:register_name(highlander, self()).
    yes
    <0.39.0> (foo@roger-pc) 2> global:register
    register_name/2           register_name/3           register_name_external/2  
    register_name_external/3  registered_names/0        
    <0.39.0> (foo@roger-pc) 2> global:registered_names().
    [highlander]
    <0.39.0> (foo@roger-pc) 3> global:whereis_name(highlander).
    <0.39.0>
    ** exception error: killed 
    <0.39.0> (foo@roger-pc) 4> 
    
    
    
    roger@roger-pc:~$ erl -sname bar
    Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:12:12] [async-threads:10] [kernel-poll:false]
    
    Eshell V5.10.4  (abort with ^G)
    <0.39.0> (bar@roger-pc) 1> global:register_name(highlander, self()).
    yes
    <0.39.0> (bar@roger-pc) 2> global:register  
    register_name/2           register_name/3           register_name_external/2  
    register_name_external/3  registered_names/0        
    <0.39.0> (bar@roger-pc) 2> global:registered_names().
    [highlander]
    <0.39.0> (bar@roger-pc) 3> global:whereis_name(highlander).
    <0.39.0>
    <0.39.0> (bar@roger-pc) 4> net_adm:ping('foo@roger-pc').
    pong
    <0.39.0> (bar@roger-pc) 5> 
    =INFO REPORT==== 24-Sep-2014::16:40:19 ===
    global: Name conflict terminating {highlander,<6886.39.0>}
    
