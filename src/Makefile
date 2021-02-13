FC=gfortran
FFLAGS=-O3 -Wall -Wextra -std=f2008
SRC=polygon_reader.f90 main.f90
OBJ=$(SRC:.f90=.o)
DEPFLAGS=-M -cpp

deps:
	@$(FC) $(DEPFLAGS) $(SRC)

make.deps: deps

include make.deps

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

polygon_clipper: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	@rm -f *.mod *.o polygon_clipper





