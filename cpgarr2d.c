#include "cpgplot.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
int main(){
    static int nx = 40, ny = 40;
    int i,j,k,lw,ci,ls;
    float f[1600],fmin,fmax,alev;
    double x,y;
    static float tr[6] = {0.0,1.0,0.0,0.0,0.0,1.0};
    printf("Demonstration of PGPLOT contouring routines\n");
    /* Compute a suitable function. A C array is used to emulate a 2D fortran array f(nx,ny)
    */
    fmin=fmax=0.0;
    for(j=1;j<=ny;j++){
      for(i=1;i<=nx;i++){
        k=(j-1)*nx+i-1;
        x = tr[0]+tr[1]*i+tr[2]*j;
        y = tr[3]+tr[4]*i+tr[5]*j;
        f[k]=cos(0.3*sqrt(x*2)-0.13333*y)*cos(0.13333*x)+(x-y)/(double)nx;
        if(f[k]<fmin)fmin=f[k];
        if(f[k]>fmax)fmax=f[k];
      }
    }
    /*
     * Call PGBEG to initiate PGPLOT and open the output device;
     * PGBEG will prompt the user to supply the device name and type
     * PGBEG opens a graphical device or file and prepares it for
        subsequent plotting. A device must be opened with PGBEG or PGOPEN
        before any other calls to PGPLOT subroutines for the device.
        Returns PGBEG : a status return value. A value of 1 indicates
        successful completion, any other value indicates
        an error. In the event of error a message is written on the standard error unit.
        To test the return value, call
        PGBEG as a function, eg IER=PGBEG(...); note
        that PGBEG must be declared INTEGER in the calling program.
     * INTEGER FUNCTION PGBEG (UNIT, FILE, NXSUB, NYSUB)
        INTEGER UNIT
        CHARACTER*(*) FILE
        INTEGER NXSUB, NYSUB
        Arguments:
        UNIT (input): this argument is ignored by PGBEG (use zero).
        FILE (input): the "device specification" for the plot device.
        (For explanation, see description of PGOPEN.)
        NXSUB (input): the number of subdivisions of the view surface in X (>0 or <0).
        NYSUB (input): the number of subdivisions of the view surface in Y (>0).
        PGPLOT puts NXSUB x NYSUB graphs on each plot
        page or screen; when the view surface is sub-
        divided in this way, PGPAGE moves to the next
        panel, not the next physical page. If
        NXSUB > 0, PGPLOT uses the panels in row order;
        if <0, PGPLOT uses them in column order.
     */
    if(cpgbeg(0,"?",1,1)!=1)
        return EXIT_FAILURE;
    /* Clear the screen. Set up windows and viewport */
    cpgpage();
    cpgsvp(0.95,0.95,0.05,0.95);
    cpgswin(1.0,(float)nx,1.0,(float)ny);
    cpgbox("bcts",0.0,0,"bcts",0.0,0);
    cpgmtxt("t",1.0,0.0,0.0,"Contouring using PGCONT");
    /*Draw the map. PGCONT is called once for each contour, using
     different line attributes to distinguish contour levels.
     */
    cpgbbuf();
    for(i=1;i<21;i++){
      alev = fmin + i*(fmax-fmin)/20.0;
      lw=(i%5==0)?3:1;
      ci=(i<10)?2:3;
      ls=(i<10)?2:1;
      cpgslw(lw);
      cpgsci(ci);
      cpgsls(ls);
      cpgcont(f,nx,ny,1,nx,1,ny,&alev,-1,tr);
    }
    cpgslw(1);
    cpgsls(1);
    cpgsci(1);
    cpgebuf();
    /*
     * Finally call PGEND to terminate things properly
     */
    cpgend();
    return EXIT_SUCCESS;

}
