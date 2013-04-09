all : ryc ryc-midi
	# ./ryc-midi < input/example4
	# timidity a.mid

ryc : *.hs
	ghc -Wall -O2 -o ryc Interpreter.hs

ryc-midi : *.hs
	ghc -Wall -O2 -o ryc-midi MidiGen.hs

clean :
	rm ryc ryc-midi *.hi *.o a.mid || :
