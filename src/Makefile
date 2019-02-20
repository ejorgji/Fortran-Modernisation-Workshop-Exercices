FFLAGS = -O2 -I. -I/usr/local/netcdf-4.6.1/include -I/usr/local/plplot-5.13.0/lib/fortran/modules/plplot 
LDFLAGS = -L. -lfmw -L/usr/local/netcdf-4.6.1/lib -lnetcdff -lnetcdf -L/usr/local/plplot-5.13.0/lib -lplplotfortran -lplplot 

fd1d_heat_explicit.exe: fd1d_heat_explicit.o libfmw.a 
		nagfor $^ $(LDFLAGS) -o $@ 

fd1d_heat_explicit.o: fd1d_heat_explicit.f90 Solver_mod.o IO_mod.o CFL_mod.o RHS_mod.o Types_mod.o
		nagfor $(FFLAGS) -c $^ 

libfmw.a: CFL_mod.o IO_mod.o Solver_mod.o RHS_mod.o Types_mod.o
		ar rcs $@ $^ 
		
Solver_mod.o: Solver_mod.f90 RHS_mod.o Types_mod.o
		nagfor $(FFLAGS) -c $^ 

IO_mod.o: IO_mod.f90 Types_mod.o
		nagfor $(FFLAGS) -c $^ 

CFL_mod.o: CFL_mod.f90 Types_mod.o
		nagfor $(FFLAGS) -c $^ 

RHS_mod.o: RHS_mod.f90 Types_mod.o
		nagfor $(FFLAGS) -c $^ 

Types_mod.o: Types_mod.f90 
		nagfor $(FFLAGS) -c $< 

.PHONY: clean 

clean:
		rm -f *.mod *.o *.png *.exe *.a

pfunit:
		${PFUNIT}/bin/pFUnitParser.py testCFL.pf testCFL.F90 -I.
		nagfor -I${PFUNIT}/mod -I. -c testCFL.F90
		nagfor -I${PFUNIT}/mod ${PFUNIT}/include/driver.F90 CFL_mod.o testCFL.o -L${PFUNIT}/lib -lpfunit -I. -o tests.exe