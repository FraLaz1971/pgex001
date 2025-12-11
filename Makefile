CC=gcc
LD=f77
CFLAGS=-I$(PGPLOT_DIR)
FC=f77
FD=f77
OEXT=.o
EEXT=
RM=rm -rf
FFLAGS=
FDFLAGS=-static -L$(PGPLOT_DIR) -Wl,-rpath=$(PGPLOT_DIR)
LDFLAGS=$(FDFLAGS)LDFLAGS
LIBS=-lpgplot -lX11 -lxcb -lXau -lXdmcp -lpng -lz
SRCS=pgs.f p1_1.f p2.f pgs2096x2338.f pgsc2096x2338.f pgs4080x3072.f pgsc4080x3072.f pgs500x320.f \
pgsc500x320.f rd.f raasb.f waa.f wba.f m2pl.f test01.f test02.f pl2m.f ex1.f simple.f simple2.f \
asubarr.f waar.f waas.f
CSRCS=cpgsimple.c cpgarr2d.c
OBJS = $(SRCS:.f=$(OEXT))
COBJS = $(CSRCS:.c=$(OEXT))LDFLAGS

TARGETS = $(OBJS:$(OEXT)=$(EEXT)) $(COBJS:$(OEXT)=$(EEXT))
.PHONY: all clean

all: $(OBJS) $(TARGETS)

cpgsimple$(EEXT): cpgsimple$(OEXT)
	$(FD) -o $@ $^ $(FDFLAGS) -lcpgplot $(LIBS)

cpgarr2d$(EEXT): cpgarr2d$(OEXT)
	$(FD) -o $@ $^ $(FDFLAGS) -lcpgplot $(LIBS)

.f.o:
	$(FC) -c $(FFLAGS) -o $@ $<


.o$(EEXT):
	$(FD) -o $@ $^ $(FDFLAGS) $(LIBS)

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<


clean:
	$(RM) $(OBJS) $(TARGETS)

