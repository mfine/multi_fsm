.OBJDIR: ebin
.SUFFIXES: .erl .beam

.erl.beam:
	erlc -pz ebin -o ebin $<

MODS = src/multi_fsm
EXPS = examples/baby_fsm

all: compile

compile: ${MODS:%=%.beam}

examples: ${EXPS:%=%.beam}

clean:
	rm -f ebin/*.beam
