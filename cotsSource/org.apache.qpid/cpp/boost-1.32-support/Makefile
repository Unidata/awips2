.PHONY: default apply

default:
	@echo "Run 'make apply' to patch the source"

apply: main.patch ../src/boost
	patch -d .. -p0 < main.patch
	cat supressions >> ../src/tests/.valgrind.supp

../src/boost: boost.tar.gz
	tar -C .. -xvzf boost.tar.gz
