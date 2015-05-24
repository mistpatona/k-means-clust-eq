in1 : inp1.txt cvk1.hs
	runhaskell cvk1.hs < inp1.txt
# > $@

cities = Cherkasy Sumy Donetsk Kirovograd Zaporozhie Khmelnitsky Rovno KrivoyRog

$(cities) : load2w.txt
	grep -i ^$@ load2w.txt | cut -d\  -f 2,3,4 > $@.txt

load2w.txt : load_2w_max_01
	cat load_2w_max_01 | tr "," "." > $@

default_mul.txt : load2w.txt
	cat load2w.txt | ./default_mul.sh  > $@

cvk1 : cvk1.hs
	ghc cvk1.hs

all_muls.txt : sortmul.hs default_mul.txt  Kirovograd.muls Sumy.muls Cherkasy.muls Donetsk.muls Zaporozhie.muls Khmelnitsky.muls Rovno.muls KrivoyRog.muls 
	cat default_mul.txt *.muls | runhaskell sortmul.hs > $@

Donetsk.muls.wide : cvk1 Donetsk.cap
	./cvk1 6650 < Donetsk.cap > $@

Zaporozhie.muls.wide : cvk1 Zaporozhie.cap
	./cvk1 5836 < Zaporozhie.cap > $@

Khmelnitsky.muls.wide : cvk1 Khmelnitsky.cap
	./cvk1 17050 < Khmelnitsky.cap > $@

Rovno.muls.wide : cvk1 Rovno.cap
	./cvk1 28040 < Rovno.cap > $@

KrivoyRog.muls.wide : cvk1 KrivoyRog.cap
	./cvk1 25540 < KrivoyRog.cap > $@

Sumy.muls.wide : cvk1 Sumy.cap
	./cvk1 28550 < Sumy.cap > $@

Cherkasy.muls.wide : cvk1 Cherkasy.cap
	./cvk1 6140 < Cherkasy.cap > $@

Kirovograd.muls.wide : cvk1 Kirovograd.cap
	./cvk1 4540 < Kirovograd.cap > $@

%.muls : %.muls.wide
	cat $*.muls.wide | ./mul_shrink.sh > $@

%.cap : load2w.txt
	grep -i ^$* load2w.txt | cut -d\  -f 2,3,4 > $@

