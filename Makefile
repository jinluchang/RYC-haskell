all : ryc ryc-midi

ryc : *.hs
	ghc -Wall -O2 -o ryc Interpreter.hs

ryc-midi : *.hs
	ghc -Wall -O2 -o ryc-midi MidiGen.hs

test : ryc-midi
	for i in input/* ; do echo $$i ; ./ryc-midi $$i ; done

clean :
	rm ryc ryc-midi *.hi *.o *.mid *.ogg *.mp3 || :
