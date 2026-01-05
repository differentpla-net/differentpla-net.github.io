I want to explore whether ranch:start_listener() / cowboy:start_plain() is better than start_link, and how to use
start_link when the returned pid is itself a supervisor.

Start with spawn, then proc_lib, then various other bits, then work up to supervisors, and then custom supervisors
(ranch) and then complicated supervision trees.

Look at what other libraries do, such as poolboy. Maybe see if I can remember how our yamux thing did it (ended up very
similar to RabbitMQ, so maybe start there).

Consider Elixir's DynamicSupervisor, supervisor[23], etc.