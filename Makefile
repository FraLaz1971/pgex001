FC=f77 -cpp
FD=f77
PGPLOT_DIR=/usr/local/pgplot
OEXT=.o
EEXT=
RM=rm -rf
FFLAGS=-DSMALL
FDFLAGS=-static -L$(PGPLOT_DIR) -Wl,-rpath=$(PGPLOT_DIR)
LIBS=-lpgplot -lX11 -lxcb -lXau -lXdmcp -lpng -lz 
SRCS=pgs.f p1_1.f p2.f pgs2096x2338.f pgsc2096x2338.f pgs4080x3072.f pgsc4080x3072.f pgs500x320.f pgsc500x320.f rd.f
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

