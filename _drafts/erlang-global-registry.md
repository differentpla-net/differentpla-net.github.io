
TODO: It's not clear to me how you'd detect that the global singleton has died (or the node died) and start a
replacement...? I guess that -- because you can use `global` directly, there's nothing automatic. You could probably
have your process attempt to register. If it failed, it could just monitor the winner and try to register when the
winner dies. Not sure how that would scale. Alternatively, wrap your process in another process to hide that logic.
Kinda like a global_sup or something.
