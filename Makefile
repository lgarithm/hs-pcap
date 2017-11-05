OPTS = -W -O -hidir .build -odir .build

all: pcap_read

dir:
	mkdir -p bin

pcap_read: dir
	ghc $(OPTS) pcap-read.hs -o bin/pcap-read

clean:
	-rm -fr .build

test:
	runhaskell Misc/ParseTest.hs
	runhaskell Misc/UtilsTest.hs
	runhaskell Net/AllTest.hs
