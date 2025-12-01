FC=f77 -cpp
FD=f77
PGPLOT_DIR=/usr/local/pgplot
OEXT=.o
EEXT=
RM=rm -rf
FFLAGS=-DSMALL
FDFLAGS=-static -L$(PGPLOT_DIR) -Wl,-rpath=$(PGPLOT_DIR)
LIBS=-lpgplot -lX11 -lxcb -lXau -lXdmcp -lpng -lz 
SRCS=pgs.f p1_1.f p2.f pgdemo1.f pgdemo10.f pgdemo11.f pgdemo12.f pgdemo13.f \
pgdemo14.f pgdemo15.f pgdemo16.f pgdemo17.f pgdemo2.f pgdemo3.f \
pgdemo4.f pgdemo5.f pgdemo6.f pgdemo7.f pgdemo8.f pgdemo9.f
OBJS = $(SRCS:.f=$(OEXT))
TARGETS = $(OBJS:$(OEXT)=$(EEXT))
.PHONY: all clean

all: $(OBJS) $(TARGETS)

.f.o:
	$(FC) -c $(FFLAGS) -o $@ $<

.o$(EEXT):
	$(FD) -o $@ $^ $(FDFLAGS) $(LIBS)

clean:
	$(RM) $(OBJS) $(TARGETS)

