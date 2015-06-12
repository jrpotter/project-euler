LIB	   = Euler
TMP        = tmp

CC         = ghc

HS_DIRS   := $(wildcard *-*)
HS_FILES  := $(foreach var, $(HS_DIRS), $(wildcard $(var)/*.hs))
OBJ_FILES := $(HS_FILES:.hs=.o)

all: $(OBJ_FILES)

%.o : %.hs
	$(CC) -outputdir=$(dir $<)$(TMP) -I$(LIB) $<

.PHONY: clean
clean: clean_tmp
	$(foreach var, $(HS_DIRS), find $(var) \! -name "*.*" -type f -exec rm {} \;;)

.PHONY: clean_tmp
clean_tmp:
	$(foreach var, $(HS_DIRS), rm -rf $(var)/$(TMP);)
	rm -rf $(LIB)/*.o $(LIB)/*.hi

