TESTS=$(sort $(basename $(wildcard *.lama)))

LAMAC=../src/lamac

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.lama
	@echo $@
	LAMA=../runtime $(LAMAC)  $< && `which time` -f "Recursive\t%Us" ./$@
	LAMA=../runtime $(LAMAC) -b $< && `which time` -f "Iterative\t%Us" ../byterun/byterun $@.bc -i

clean:
	$(RM) test*.log *.s *~ $(TESTS) *.i
