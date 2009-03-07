multi\_fsm is a wrapper around gen\_fsm for dispatching on multiple states. Modifies gen\_fsm callbacks by removing StateName parameters and return values. Adds callback to identify which state to dispatch on. gen\_fsm APIs not overlaid by this moudle are applicable.

An example:

    baby_fsm:start(henry).
    baby_fsm:eat(henry).
    baby_fsm:drink(henry).

Your contributions are welcome.
