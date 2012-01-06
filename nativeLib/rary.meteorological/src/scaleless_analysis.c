
#define INTEGRATED_BUILD 1

#if INTEGRATED_BUILD
#include "meteoLib.h"
#endif

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static int sxy = 0;
static int n_swath = 0;
static int p_swath = 0;
static int m_swath = 0;
static int n_pts = 0;
static int n_oct = 0;

/* points within each octant run counterclockwise, i.e. right to left */
static short * ioct[8]; /* i coord list for octants */
static short * joct[8]; /* j coord list for octants */
static float * doct[8]; /* distance for each coord in octant */
static int * oct0[8];   /* start point of each octant */
static int * roct[8];   /* last right side point of each octant */
static int * moct[8];   /* mid point of each octant */
static int * loct[8];   /* first left point of each octant */
static char * eoct[8];  /* flag for whether edge points exist */

/* pointers to memory we actually allocate, we hand this out to the rest */
/* of the octant pointers. */
static int * octint = 0;
static short * octshort = 0;
static char * octchar = 0;
static float * octfloat = 0;

struct node
{
    struct node * prev;
    struct node * next;
    int base;
    int loc;
};

static struct node ** bases = 0;
static struct node * nodes = 0;
static int mbase = 0;
static int nbase = 0;

static int octwrapdat[] = {0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3, 4, 5, 6, 7};
static int * octwrap = octwrapdat+8;
static int octmagddat[] = {0, 1, 2, 3, 4, 3, 2, 1,
    0, 1, 2, 3, 4, 3, 2, 1,
    0, 1, 2, 3, 4, 3, 2, 1};
static int * octmagd = octmagddat+8;

#if 0
static int octdiffdat[] = {0, 1, 2, 3, 4, -3, -2, -1,
    0, 1, 2, 3, 4, -3, -2, -1,
    0, 1, 2, 3, 4, -3, -2, -1};
static int * octdiff = octdiffdat+8;
#endif

static int right = 1;
static int left = 2;
static int edgeno = 8;
static int edgeyes = 16;
static int full = 32;
static int allcon = -1;

#if 0
static int stoppt = -0x7FFFFFFF;
static int verify_nodes(int nver)
{
    struct node * node1 = 0;
    struct node * node2 = 0;
    int hi,c,n;

    n = 0;
    for (hi=0; hi<=nbase; hi++)
    {
        node1 = bases[hi];
        if (!node1) continue;
        c = 0;
        n++;
        if (node1->prev)
            fprintf(stderr,"*>non-null prev at %d %d %d\n",hi,c,node1-nodes);
        if (node1->loc!=node1-nodes)
            fprintf(stderr,"*>loc mismatch at %d %d %d >%d\n",
                    hi,c,node1-nodes,node1->loc);
        if (node1->base!=hi)
            fprintf(stderr,"*>base mismatch at %d %d %d >%d\n",
                    hi,c,node1-nodes,node1->base);
        while ((node2=node1->next))
        {
            n++;
            c++;
            if (node2->prev==0)
                fprintf(stderr,"*>missing prev at %d %d %d\n",
                        hi,c,node2-nodes);
            else if (node2->prev!=node1)
                fprintf(stderr,"*>chain error at %d %d %d >%d %d\n",
                        hi,c,node2-nodes,node2->prev-nodes,node1-nodes);
            if (node2->loc!=node2-nodes)
                fprintf(stderr,"*>loc mismatch at %d %d %d >%d\n",
                        hi,c,node2-nodes,node2->loc);
            if (node2->base!=hi)
                fprintf(stderr,"*>base mismatch at %d %d %d >%d\n",
                        hi,c,node2-nodes,node2->base);
            node1 = node2;
        }
    }
    if (nver>0 && n!=nver)
        fprintf(stderr,"*>total nodes mismatch %d >%d\n",nver,n);
    else if (nver==0)
        fprintf(stderr,"total nodes %d\n",n);

    return n;
}
#endif

static void init_distance_tables(int nx, int ny)
{
    int * kswath0;
    int * eswath0;
    int mswath0;
    int * kswath1;
    int * eswath1;
    int mswath1;
    int sxym,nb;
    int ii,jj,kk,bb,oo,oo0,ss;
    float sqrt2,dist;

    float * dlookup = 0;
    short * binidx = 0;
    char * pttype = 0;
    short *isp, *jsp, *sp0;
    int *ip0;
    float *rp;
    char *cp0;

    float rat, rat1, rat2;

    if (ny>nx) nx = ny;
    nx = (nx*3+1)/2;
    if (nx<10) nx = 10;
    if (nx<=sxy) return;
    if (sxy>0)
    {
        free(octint);
        free(octshort);
        free(octchar);
        free(octfloat);
    }
    rat1 = atan(1.0)/2;
    rat2 = rat1*3;
    rat1 = tan(rat1);
    rat2 = tan(rat2);
    /*fprintf(stderr,"rat1,rat2 %f %f\n",rat1,rat2);*/

    sxy = nx*3/2;
    /*fprintf(stderr,"sxy %d\n",sxy);*/
    sxym = sxy-1;
    n_swath = sxy*2;
    n_pts = sxy*sxy;
    sqrt2 = sqrt(2.0);

    dlookup = (float*)malloc(sizeof(float)*n_pts);
    binidx = (short*)malloc(sizeof(short)*n_pts);
    pttype = (char*)malloc(n_pts);

    /* Handle the special case of the nearest two rows. */
    pttype[0] = 0;
    pttype[sxy] = pttype[1] = -edgeyes;
    pttype[1+sxy] = edgeyes;
    if (0.5>rat1)
    {
        pttype[2*sxy] = -edgeyes;
        pttype[1+2*sxy] = edgeyes;
        pttype[2+2*sxy] = edgeno;
        pttype[2+sxy] = edgeyes;
        pttype[2] = -edgeyes;
    }
    else
    {
        pttype[2*sxy] = -edgeno;
        pttype[1+2*sxy] = -edgeyes;
        pttype[2+2*sxy] = edgeyes;
        pttype[2+sxy] = -edgeyes;
        pttype[2] = -edgeno;
    }

    /* Calculate the distances and bin indices. */
    for (jj=kk=0; jj<sxy; jj++)
        for (ii=0; ii<sxy; ii++,kk++)
        {
            if (kk==0)
            {
                dlookup[kk] = 0;
                binidx[kk] = -1;
                continue;
            }
            if (ii==jj)
            {
                bb = ii;
                dist = bb*sqrt2;
            }
            else
            {
                dist = sqrt(ii*ii+jj*jj);
                bb = (int)(0.99999+dist/sqrt2);
            }
            dlookup[kk] = dist;
            binidx[kk] = bb*2-2;
            if (ii==0)
                rat = (float)(jj)/0.1;
            else if (jj==0)
                rat = 0.1/(float)(ii);
            else
                rat = (float)(jj)/(float)(ii);
            if (ii<=2 && jj<=2) continue;
            if (rat>rat1 && rat<rat2)
                pttype[kk] = edgeno;
            else
                pttype[kk] = -edgeno;
        }

    /* Move points not at the very edge of their bin to the previous bin, */
    /* identify points adjacent to the other octant. */
    for (jj=kk=0; jj<sxy; jj++)
        for (ii=0; ii<sxy; ii++,kk++)
        {
            if (ii<=2 && jj<=2) continue;
            if (pttype[kk]>0)
            {
                if (ii==sxym || jj==sxym) continue;
                if (binidx[kk]>=binidx[kk+sxy+1]) binidx[kk]--;
                if (pttype[kk+sxym]<0 || pttype[kk-sxym]<0) pttype[kk] = edgeyes;
            }
            else if (jj>ii)
            {
                if (jj<sxym && binidx[kk]>=binidx[kk+sxy]) binidx[kk]--;
                if (pttype[kk+1]>0) pttype[kk] = -edgeyes;
            }
            else
            {
                if (ii<sxym && binidx[kk]>=binidx[kk+1]) binidx[kk]--;
                if (pttype[kk+sxy]>0) pttype[kk] = -edgeyes;
            }
        }

#if 0
    fprintf(stderr,"dlookup\n");
    for (jj=sxy-1; jj>=0; jj--) {
        for (ii=0,kk=jj*sxy; ii<sxy; ii++,kk++)
            fprintf(stderr,"%4.1f ",dlookup[kk]);
        fprintf(stderr,"\n"); }
        fprintf(stderr,"binidx\n");
        for (jj=sxy-1; jj>=0; jj--) {
            for (ii=0,kk=jj*sxy; ii<sxy; ii++,kk++)
                fprintf(stderr," %3d ",(int)(binidx[kk]));
            fprintf(stderr,"\n"); }
            fprintf(stderr,"pttype\n");
            for (jj=sxy-1; jj>=0; jj--) {
                for (ii=0,kk=jj*sxy; ii<sxy; ii++,kk++)
                    fprintf(stderr," %3d ",(int)(pttype[kk]));
                fprintf(stderr,"\n"); }
#endif

                /* set up the pointers to the info we will cache about each octant*/
                p_swath = n_swath+1;
                m_swath = n_swath-1;
                n_oct = 2*n_pts/3;
                octint = (int*)malloc(sizeof(int)*8*n_swath);
                memset(octint, -1, sizeof(int)*8*n_swath);
                octchar = (char*)malloc(2*n_swath);
                memset(octchar, 0, 2*n_swath);
                octfloat = (float*)malloc(sizeof(float)*n_oct*2);
                octshort = (short*)malloc(sizeof(short)*16*n_oct);
                isp = octshort;
                jsp = isp+8*n_oct;
                rp = octfloat;
                ip0 = octint;
                sp0 = octshort;
                cp0 = octchar;
                for (oo=0; oo<8; oo++)
                {
                    if ((oo%2)==0)
                    {
                        rp = octfloat;
                        ip0 = octint;
                        sp0 = octshort;
                        cp0 = octchar;
                    }
                    ioct[oo] = isp;
                    isp += n_oct;
                    joct[oo] = jsp;
                    jsp += n_oct;
                    oct0[oo] = ip0;
                    ip0 += n_swath;
                    loct[oo] = ip0;
                    ip0 += n_swath;
                    roct[oo] = ip0;
                    ip0 += n_swath;
                    moct[oo] = ip0;
                    ip0 += n_swath;
                    eoct[oo] = cp0;
                    cp0 += n_swath;
                    doct[oo] = rp;
                    rp += n_oct;
                }

                /* For each swath, create a list if indices in that references */
                /* the square arrays for quadrants 0 and 1. */
                nb = sxy*n_swath;
                kswath0 = (int*)malloc(sizeof(int)*nb);
                kswath1 = (int*)malloc(sizeof(int)*nb);
                eswath0 = (int*)malloc(sizeof(int)*n_swath);
                eswath1 = (int*)malloc(sizeof(int)*n_swath);
                mswath0 = mswath1 = 0;
                memset(kswath0, -1, sizeof(int)*nb);
                memset(kswath1, -1, sizeof(int)*nb);
                memset(eswath0, -1, sizeof(int)*n_swath);
                memset(eswath1, -1, sizeof(int)*n_swath);
                for (jj=kk=0; jj<sxy; jj++)
                    for (ii=0; ii<sxy; ii++,kk++)
                    {
                        bb = binidx[kk];
                        if (bb<0) continue;
                        if (pttype[kk]>0)
                        {
                            if (jj<ii) continue;
                            oo = bb*sxy+jj-ii;
                            kswath1[oo] = kk;
                            if (bb>mswath1) mswath1 = bb;
                            if (oo>eswath1[bb]) eswath1[bb] = oo;
                        }
                        else if (ii>jj)
                        {
                            oo = bb*sxy+jj;
                            kswath0[oo] = kk;
                            if (bb>mswath0) mswath0 = bb;
                            if (oo>eswath0[bb]) eswath0[bb] = oo;
                        }
                    }

#if 0
                fprintf(stderr,"mswath0 %d\n",mswath0);
                for (bb=0; bb<=mswath0; bb++) fprintf(stderr,"%d ",eswath0[bb]);
                fprintf(stderr,"\n");
                for (oo=bb=0; bb<n_swath; bb++) {
                    for (ii=0; ii<sxy; ii++,oo++)
                        fprintf(stderr,"%d ",kswath0[oo]);
                    fprintf(stderr,"\n"); }
                    fprintf(stderr,"mswath1 %d\n",mswath1);
                    for (bb=0; bb<=mswath1; bb++) fprintf(stderr,"%d ",eswath1[bb]);
                    fprintf(stderr,"\n");
                    for (oo=bb=0; bb<n_swath; bb++) {
                        for (ii=0; ii<sxy; ii++,oo++)
                            fprintf(stderr,"%d ",kswath1[oo]);
                        fprintf(stderr,"\n"); }
#endif

                        /* Set up all the info for octant 0 and like octants. */
                        for (ss=bb=0; bb<=mswath0; bb++)
                        {
                            oct0[0][bb] = ss;
                            if (eswath0[bb]<0) continue;
                            oo0 = bb*sxy;
                            /*fprintf(stderr,"bb,ss,oo0,eswath0 %d %d %d %d\n",bb,ss,oo0,eswath0[bb]);*/
                            for (oo=eswath0[bb]; oo>=oo0; oo--)
                            {
                                kk = kswath0[oo];
                                if (kk<0) continue;
                                jj = -kk/sxy;
                                ii = kk+jj*sxy;
                                /*fprintf(stderr,"oo,kk, ii,jj, ss  %d %d  %d %d  %d\n",oo,kk,ii,jj,ss);*/
                                doct[0][ss] = dlookup[kk];
                                ioct[0][ss] = ii;
                                joct[0][ss] = jj;
                                ioct[2][ss] = -jj;
                                joct[2][ss] = ii;
                                ioct[4][ss] = -ii;
                                joct[4][ss] = -jj;
                                ioct[6][ss] = jj;
                                joct[6][ss] = -ii;
                                if (jj==0) moct[0][bb] = loct[0][bb] = ss;
                                roct[0][bb] = ss;
                                if (pttype[kk]==-edgeyes) eoct[0][bb] = edgeyes;
                                ss++;
                            }
                            for (oo=oo0+1; oo<=eswath0[bb]; oo++)
                            {
                                kk = kswath0[oo];
                                if (kk<0) continue;
                                jj = kk/sxy;
                                ii = kk-jj*sxy;
                                /*fprintf(stderr,"oo,kk, ii,jj, ss  %d %d  %d %d  %d\n",oo,kk,ii,jj,ss);*/
                                if (loct[0][bb]<0) loct[0][bb] = ss;
                                doct[0][ss] = dlookup[kk];
                                ioct[0][ss] = ii;
                                joct[0][ss] = jj;
                                ioct[2][ss] = -jj;
                                joct[2][ss] = ii;
                                ioct[4][ss] = -ii;
                                joct[4][ss] = -jj;
                                ioct[6][ss] = jj;
                                joct[6][ss] = -ii;
                                ss++;
                            }
                            eoct[0][bb] |= 1;
                            if ((bb%2))
                            {
                                if (moct[0][bb]<0 || (eoct[0][bb]&edgeyes)==0) continue;
                                if ((ss-oct0[0][bb])*3>jj*2)
                                    eoct[0][bb] |= full;
                            }
                            else
                                eoct[0][bb] |= full;
                        }
                        oct0[0][bb] = ss;

                        /* Set up all the info for octant 1 and like octants. */
                        for (ii=ss=bb=0; bb<=mswath1; bb++)
                        {
                            oct0[1][bb] = ss;
                            if (eswath1[bb]<0) continue;
                            oo0 = bb*sxy;
                            for (oo=eswath1[bb]; oo>=oo0; oo--)
                            {
                                kk = kswath1[oo];
                                if (kk<0) continue;
                                ii = kk/sxy;
                                jj = kk-ii*sxy;
                                doct[1][ss] = dlookup[kk];
                                ioct[1][ss] = ii;
                                joct[1][ss] = jj;
                                ioct[3][ss] = -jj;
                                joct[3][ss] = ii;
                                ioct[5][ss] = -ii;
                                joct[5][ss] = -jj;
                                ioct[7][ss] = jj;
                                joct[7][ss] = -ii;
                                if (ii==jj) moct[1][bb] = loct[1][bb] = ss;
                                roct[1][bb] = ss;
                                if (pttype[kk]==edgeyes) eoct[1][bb] = edgeyes;
                                ss++;
                            }
                            for (oo=oo0+1; oo<=eswath1[bb]; oo++)
                            {
                                kk = kswath1[oo];
                                if (kk<0) continue;
                                jj = kk/sxy;
                                ii = kk-jj*sxy;
                                if (loct[1][bb]<0) loct[1][bb] = ss;
                                doct[1][ss] = dlookup[kk];
                                ioct[1][ss] = ii;
                                joct[1][ss] = jj;
                                ioct[3][ss] = -jj;
                                joct[3][ss] = ii;
                                ioct[5][ss] = -ii;
                                joct[5][ss] = -jj;
                                ioct[7][ss] = jj;
                                joct[7][ss] = -ii;
                                ss++;
                            }
                            eoct[1][bb] |= 1;
                            if ((bb%2))
                            {
                                if (moct[1][bb]<0 || (eoct[1][bb]&edgeyes)==0) continue;
                                if ((ss-oct0[1][bb])*3>(jj-ii)*2)
                                    eoct[1][bb] |= full;
                            }
                            else
                                eoct[1][bb] |= full;
                        }
                        oct0[1][bb] = ss;

#if 0
                        kswath0 = (int*)malloc(sizeof(int)*n_pts);
                        for (kk=0; kk<n_pts; kk++) kswath0[kk] = 0;
                        for (oo=0; oo<8; oo++)
                        {
                            fprintf(stderr,"dump octant %d",oo);
                            for (bb=kk=0; kk<n_swath; )
                            {
                                fprintf(stderr,"\n%d %d   %d   %d %d %d>> ",
                                        kk,eoct[oo][kk],oct0[oo][kk],roct[oo][kk],moct[oo][kk],loct[oo][kk]);
                                kk++;
                                for (; bb<oct0[oo][kk]; bb++) 
                                {
                                    ii = ioct[oo][bb];
                                    jj = joct[oo][bb];
                                    fprintf(stderr,"%d:%d ",ii,jj);
                                    if (ii>=0 && jj>=0) kswath0[ii+sxy*jj] = 1;
                                }
                            }
                            fprintf(stderr,"\n");
                        }
                        for (jj=sxy-1; jj>=0; jj--)
                        {
                            for (kk=jj*sxy,ii=0; ii<sxy; ii++,kk++)
                                fprintf(stderr,"%d ",(int)(kswath0[kk]));
                            fprintf(stderr,"\n");
                        }
#endif

                        free(dlookup);
                        free(binidx);
                        free(pttype);
                        free(kswath0);
                        free(kswath1);
                        free(eswath0);
                        free(eswath1);

}

#if INTEGRATED_BUILD
    int FTN_MANGLE (scaleless_analysis)
(float * xind, float * yind, float * values, 
 int * nv, int * nx, int * ny, float * grid) 
#else
    int scaleless_analysis
(float * xind, float * yind, float * values, 
 int * nv, int * nx, int * ny, float * grid) 
#endif
{

    float *rp0, *rpx, *rpy, *rpv, *rend;
    int   *ip;

    int i,j,ii,jj,iii,jjj,oo,ooo,nxto,osv,ss,sss,hi;
    int b,b1,b2,eb1,eb2,bb,nb;
    int kk,k0,kk0,kg,kkg;
    int di,dj;
    int cc,cco;
    int nvv, nxx, nyy, nx1, ny1, nnn, nxd, nyd, nnd;
    int ddd, ddx;
    int imin, imax, jmin, jmax;
    float xmin, xmax, ymin, ymax;
    float ddis,curdis;

    float * raw = 0;
    int * counts = 0;
    float * raw0;
    int * counts0;
    char * octant = 0;
    int * nearest = 0;
    char * octant0;
    int * nearest0;
    int * iout = 0;
    int * jout = 0;
    int *iout0, *jout0;
    short *iip, *jjp;
    int *oop;
    float *ddp;
    int ok;

    int islft, isrgt;
    int ivec,jvec,cross;

    float * dists = 0;
    float * dists0;
    short * obsrch[8];
    short * obsrchdat = 0;
    short * obsi = 0;
    short * obsj = 0;
    char * conflict[8];
    char * conflictdat = 0;
    short * leftcon[8];
    short * leftcondat = 0;
    short * rightcon[8];
    short * rightcondat = 0;
    int * srchlist = 0;
    int nsrch;
    int nsw;
    int nvs;
    int nund;
    int any;
    int nin,nset,rset,lset;

    float wgt,wtot,val;
    float qdis[8],qval[8];
    struct node * onenode = 0;

    /*int nc;*/

    nodes = 0;
    bases = 0;

    /*int step = 0;*/
    /*fprintf(stderr,"in scaleless_analysis\n");*/

    /* Verify input counts. */
    nvv = *nv;
    nxx = *nx;
    nyy = *ny;
    if (nvv<=0 || nxx<=0 || nyy<=0 || nxx>=1000 || nyy>=1000) return 0;
    /*fprintf(stderr,"nvv %d   nxx %d nyy %d\n",nvv,nxx,nyy);*/

    nx1 = nxx-1;
    ny1 = nyy-1;
    nnn = nxx*nyy;
    imin = -nxx/2;
    imax = nx1-imin;
    jmin = -nyy/2;
    jmax = ny1-jmin;
    /*fprintf(stderr,"imin,imax,jmin,jmax %d %d %d %d\n",imin,imax,jmin,jmax);*/

    /* Compute range of grid indicies over which we will scan for data. */
    xmin = ymin = 999999;
    xmax = ymax = -999999;
    for (rp0=xind,rend=xind+nvv; rp0<rend; rp0++)
        if (*rp0<xmin)
            xmin = *rp0;
        else if (*rp0>xmax)
            xmax = *rp0;
    if (xmax<0 || xmin>nx1) return 0;
    for (rp0=yind,rend=yind+nvv; rp0<rend; rp0++)
        if (*rp0<ymin)
            ymin = *rp0;
        else if (*rp0>ymax)
            ymax = *rp0;
    if (ymax<0 || ymin>ny1) return 0;
    /*fprintf(stderr,"xmin,xmax,ymin,ymax %f %f %f %f\n",xmin,xmax,ymin,ymax);*/

    if (xmin>-1)
        imin = -1;
    else if (xmin>imin)
        imin = -(int)(0.5-xmin);
    if (xmax<nxx)
        imax = nxx;
    else if (xmax<imax)
        imax = (int)(0.5+xmax);
    if (ymin>-1)
        jmin = -1;
    else if (ymin>jmin)
        jmin = -(int)(0.5-ymin);
    if (ymax<nyy)
        jmax = nyy;
    else if (ymax<jmax)
        jmax = (int)(0.5+ymax);
    /*fprintf(stderr,"imin,imax,jmin,jmax %d %d %d %d\n",imin,imax,jmin,jmax);*/

    nxd = 1+imax-imin;
    nyd = 1+jmax-jmin;
    nnd = nxd*nyd;
    ddd = jmin*nxd+imin;
    ddx = nxd-nxx;
    /*fprintf(stderr,"nxd %d nyd %d %d %d %d %d \n",nxd,nyd,imin,imax,jmin,jmax);*/

    init_distance_tables(nxx, nyy);

    /*fprintf(stderr,"have distance tables\n");*/

    /* Allocate and initialize all of our work grids. */
    raw = (float*)malloc(sizeof(float)*nnd);
    counts = (int*)malloc(sizeof(int)*nnd);
    memset(counts, 0, sizeof(int)*nnd);
    for (rp0=raw,rend=raw+nnd; rp0<rend; rp0++) *rp0 = 1e37;
    for (rp0=grid,rend=grid+nnn; rp0<rend; rp0++) *rp0 = 1e37;
    raw0 = raw-ddd;
    counts0 = counts-ddd;

    /* Assign the values to appropriate grid point in our data work grid. */
    rend = values + nvv;
    nvs = 0;
    for (rpx=xind,rpy=yind,rpv=values; rpv<rend; rpx++,rpy++,rpv++)
    {
        if (*rpv>1e36) continue;
        if (*rpx<0)
            ii = -(int)(0.5-*rpx);
        else
            ii = (int)(0.5+*rpx);
        if (*rpy<0)
            jj = -(int)(0.5-*rpy);
        else
            jj = (int)(0.5+*rpy);
        bb = 0;
        if (ii<imin)
        {
            bb -= (imin-ii);
            ii = imin;
        }
        else if (ii>imax)
        {
            bb -= (ii-imax);
            ii = imax;
        }
        else
            ii = (int)(*rpx+0.5);
        if (jj<jmin)
        {
            bb -= (jmin-jj);
            jj = jmin;
        }
        else if (jj>jmax)
        {
            bb -= (jj-jmax);
            jj = jmax;
        }
        else
            jj = (int)(*rpy+0.5);
        k0 = ii+nxd*jj;
        if (counts0[k0]==0) nvs++;
        if (bb<0)
        {
            if (counts0[k0]!=0 && bb<counts0[k0]) continue;
            raw0[k0] = *rpv;
            counts0[k0] = bb;
        }
        else if (counts0[k0]<=0)
        {
            raw0[k0] = *rpv;
            counts0[k0] = 1;
        }
        else
        {
            raw0[k0] += *rpv;
            counts0[k0]++;
        }
    }

    /*fprintf(stderr,"raw, counts set from obs\n");*/

    /* work arrays with info about each grid resolved observation */
    dists = (float*)malloc(sizeof(float)*nnd);
    for (rp0=dists,rend=dists+nnd; rp0<rend; rp0++) *rp0 = 1e37;
    dists0 = dists-ddd;
    octant = (char*)malloc(sizeof(char)*nnd);
    nearest = (int*)malloc(sizeof(int)*nnd);
    memset(octant, -1, nnd);
    memset(nearest, -1, sizeof(int)*nnd);
    octant0 = octant-ddd;
    nearest0 = nearest-ddd;
    obsi = (short*)malloc(sizeof(short)*nvs);
    obsj = (short*)malloc(sizeof(short)*nvs);
    srchlist = (int*)malloc(sizeof(int)*nvs);
    obsrch[0] = obsrchdat = (short*)malloc(sizeof(short)*nvs*8);
    memset(obsrchdat, 0, sizeof(short)*nvs*8);
    for (oo=1; oo<8; oo++)
        obsrch[oo] = obsrch[oo-1]+nvs;
    conflict[0] = conflictdat = (char*)malloc(sizeof(char)*nvs*8);
    memset(conflictdat, 0, sizeof(char)*nvs*8);
    for (oo=1; oo<8; oo++)
        conflict[oo] = conflict[oo-1]+nvs;
    leftcon[0] = leftcondat = (short*)malloc(sizeof(short)*nvs*8);
    memset(leftcondat, -1, sizeof(short)*nvs*8);
    for (oo=1; oo<8; oo++)
        leftcon[oo] = leftcon[oo-1]+nvs;
    rightcon[0] = rightcondat = (short*)malloc(sizeof(short)*nvs*8);
    memset(rightcondat, -1, sizeof(short)*nvs*8);
    for (oo=1; oo<8; oo++)
        rightcon[oo] = rightcon[oo-1]+nvs;

    /*fprintf(stderr,"have our memory initialized\n");*/

    /* finalize superobing of multiple points that resolve to one grid point*/
    nsrch = 0;
    nund = nnn;
    for (rp0=raw,ip=counts,jj=jmin; jj<=jmax; jj++)
        for (ii=imin; ii<=imax; ii++,rp0++,ip++)
        {
            if (*ip==0) continue;
            if (*ip>1) *rp0 /= *ip;
            srchlist[nsrch] = nsrch;
            kk = rp0-raw;
            dists[kk] = 0;
            nearest[kk] = nsrch;
            obsi[nsrch] = ii;
            obsj[nsrch] = jj;
            /*fprintf(stderr,"%d  %d %d %d  %d %f\n",nsrch,ii,jj,kk,*ip,*rp0);*/
            if (ii<0)
                kk = obsrch[3][nsrch] = 
                    obsrch[4][nsrch] = obsrch[5][nsrch] = -2;
            else if (ii>=nxx)
                kk = obsrch[0][nsrch] = obsrch[1][nsrch] = 
                    obsrch[7][nsrch] = -2;
            if (jj<0)
                kk = obsrch[5][nsrch] = 
                    obsrch[6][nsrch] = obsrch[7][nsrch] = -2;
            else if (jj>=nyy)
                kk = obsrch[1][nsrch] = 
                    obsrch[2][nsrch] = obsrch[3][nsrch] = -2;
            if (kk<0)
            {
                for (oo=0; oo<8; oo++)
                    conflict[oo][nsrch] = allcon;
                nsrch++;
                continue;
            }
            if (ii<=0)
                conflict[3][nsrch] = 
                    conflict[4][nsrch] = conflict[5][nsrch] = allcon;
            else if (ii>=nx1)
                conflict[0][nsrch] = conflict[1][nsrch] = 
                    conflict[7][nsrch] = allcon;
            if (jj<=0)
                conflict[5][nsrch] = 
                    conflict[6][nsrch] = conflict[7][nsrch] = allcon;
            else if (jj>=ny1)
                conflict[1][nsrch] = 
                    conflict[2][nsrch] = conflict[3][nsrch] = allcon;
            kkg = ii+jj*nxx;
            grid[kkg] = *rp0;
            nund--;
            nsrch++;
        }

#if 0
    fprintf(stderr,"nsrch,nvs %d %d\n",nsrch,nvs);
    for (ss=0; ss<nvs; ss++)
    {
        ii = obsi[ss];
        jj = obsj[ss];
        fprintf(stderr,"%d %d %d   ",ii,jj,srchlist[ss]);
        for (oo=0; oo<8; oo++) fprintf(stderr," %d",(int)(obsrch[oo][ss]));
        fprintf(stderr,"  ");
        for (oo=0; oo<8; oo++) fprintf(stderr," %d",(int)(conflict[oo][ss]));
        fprintf(stderr,"\n");
    }
#endif

    /* Make tables that will let us quickly decide whether we are */
    /* in our range of i and j values for raw data points */
    ii = imin-nxd;
    iout = (int*)malloc(sizeof(int)*3*nxd);
    iout0 = iout-ii;
    for (i=ii; i<3*nxd+ii; i++)
        if (i>=0 && i<nxx)
            iout0[i] = 0;
        else if (i>=imin && i<=imax)
            iout0[i] = -1;
        else
            iout0[i] = 1;
    jj = jmin-nyd;
    jout = (int*)malloc(sizeof(int)*3*nyd);
    jout0 = jout-jj;
    for (j=jj; j<3*nyd+jj; j++)
        if (j>=0 && j<nyy)
            jout0[j] = 0;
        else if (j>=jmin && j<=jmax)
            jout0[j] = -2;
        else
            jout0[j] = 2;

#if 0
    fprintf(stderr,"raw\n");
    for (jj=nyd-1; jj>=0; jj--)
    {
        kk = jj*nxd;
        for (i=0; i<nxd; i++,kk++)
            if (raw[kk]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",raw[kk]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");

    fprintf(stderr,"dists\n");
    for (jj=nyd-1; jj>=0; jj--)
    {
        kk = jj*nxd;
        for (i=0; i<nxd; i++,kk++)
            if (dists[kk]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",dists[kk]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
#endif

    /*main loop that establishes initiallly how far from the nearest ob */
    /*each grid point is*/
    bb = 0;
    while (nsrch>0)
    {
        nsw = 0;
        /*fprintf(stderr,"nsrch %d\n",nsrch);*/
        for (ss=0; ss<nsrch; ss++)
        {
            /*fprintf(stderr,"ss %d\n",ss);*/
            sss = srchlist[ss];
            /*fprintf(stderr,"sss %d\n",sss);*/
            ii = obsi[sss];
            jj = obsj[sss];
            /*fprintf(stderr,"ii,jj %d %d\n",ii,jj);*/
            any = 0;
            for (oo=0; oo<8; oo++)
            {
                osv = obsrch[oo][sss];
                /*fprintf(stderr,"oo,bb,osv %d %d %d\n",oo,bb,osv);*/
                if (osv<0) continue;
                if (eoct[oo][bb]<full)
                {
                    any = 1;
                    if (eoct[oo][bb]==0) continue;
                }
                if (osv==left)
                    b = loct[oo][bb];
                else
                    b = oct0[oo][bb];
                iip = ioct[oo]+b;
                jjp = joct[oo]+b;
                ddp = doct[oo]+b;
                if (osv==right)
                    nb = roct[oo][bb];
                else
                    nb = oct0[oo][bb+1]-1;
                nin = nset = lset = rset = 0;
                for (; b<=nb; b++, iip++, jjp++, ddp++)
                {
                    i = ii+*iip;
                    j = jj+*jjp;
                    if (iout0[i]>0 || jout0[j]>0) continue;
                    islft = b>=loct[oo][bb] ? 1 : 0;
                    isrgt = b<=roct[oo][bb] ? 1 : 0;
                    k0 = j*nxd+i;
                    cc = nearest0[k0];
                    cco = octant0[k0];
                    if (dists0[k0]==0)
                    {
                        conflict[oo][sss] = allcon;
                        conflict[octwrap[oo+3]][cc] = allcon;
                        conflict[octwrap[oo+4]][cc] = allcon;
                        conflict[octwrap[oo+5]][cc] = allcon;
                    }
                    else if (cc<0)
                    {
                        nearest0[k0] = sss;
                        octant0[k0] = oo;
                    }
                    else if (conflict[oo][sss]==allcon &&
                            conflict[cco][cc]==allcon)
                        ;
                    else if (octmagd[oo-cco]>=2)
                    {
                        conflict[oo][sss] = allcon;
                        conflict[cco][cc] = allcon;
                    }
                    else if (leftcon[oo][sss]!=cc &&
                            rightcon[oo][sss]!=cc)
                    {
                        ivec = obsi[cc]-ii;
                        jvec = obsj[cc]-jj;
                        cross = *iip*jvec - *jjp*ivec;
                        if (dists0[k0]<*ddp)
                        {
                            conflict[oo][sss] = allcon;
                            if (cross>0)
                            {
                                conflict[cco][cc] |= right;
                                rightcon[cco][cc] = sss;
                            }
                            else
                            {
                                conflict[cco][cc] |= left;
                                leftcon[cco][cc] = sss;
                            }
                        }
                        else if (dists0[k0]>*ddp)
                        {
                            conflict[cco][cc] = allcon;
                            if (cross>0)
                            {
                                conflict[oo][sss] |= left;
                                leftcon[oo][sss] = cc;
                            }
                            else
                            {
                                conflict[oo][sss] |= right;
                                rightcon[oo][sss] = cc;
                            }
                        }
                        else if (cross>0)
                        {
                            conflict[cco][cc] |= right;
                            rightcon[cco][cc] = sss;
                            conflict[oo][sss] |= left;
                            leftcon[oo][sss] = cc;
                        }
                        else
                        {
                            conflict[cco][cc] |= left;
                            leftcon[cco][cc] = sss;
                            conflict[oo][sss] |= right;
                            rightcon[oo][sss] = cc;
                        }
                    }
                    if (iout0[i]==jout0[j]) nin++;
                    if (dists0[k0]<*ddp) continue;
                    nset++;
                    lset += islft;
                    rset += isrgt;
                    dists0[k0] = *ddp;
                }
                if (nset==0)
                {
                    if (eoct[oo][bb]>=full) 
                        obsrch[oo][sss] = -1;
                    continue;
                }
                if (eoct[oo][bb]<full)
                    ;
                else if (lset==0)
                    obsrch[oo][sss] = right;
                else if (rset==0)
                    obsrch[oo][sss] = left;

                /* we want to be sure that if we are starting outside, we */
                /* get a chance to get inside before we end because we had */
                /* all points outside */
                ooo = octwrap[oo+4];
                if (nin==0)
                {
                    if (obsrch[ooo][sss]==-2)
                        any = 1;
                    else if (eoct[oo][bb]>=full)
                        obsrch[oo][sss] = -1;
                }
                else
                {
                    any = 1;
                    if (obsrch[ooo][sss]==-2)
                        obsrch[ooo][sss] = -1;
                }
            }

            if (any) srchlist[nsw++] = sss;
        }
        nsrch = nsw;
        bb++;
    }

#if 0
    fprintf(stderr,"nsrch,nvs %d %d\n",nsrch,nvs);
    for (ss=0; ss<nvs; ss++)
    {
        ii = obsi[ss];
        jj = obsj[ss];
        fprintf(stderr,"%d %d    ",ii,jj);
        for (oo=0; oo<8; oo++) fprintf(stderr," %d",conflict[oo][ss]);
        fprintf(stderr,"\n");
    }

    fprintf(stderr,"dists\n");
    for (jj=nyd-1; jj>=0; jj--)
    {
        kk = jj*nxd;
        for (i=0; i<nxd; i++,kk++)
            if (dists[kk]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",dists[kk]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
#endif

    /* Here is where we copy the original observation out to the boundary */
    /* where that makes sense. */
    for (ss=0; ss<nvs; ss++)
    {
        /* Find the first octant that we will fill some of */
        for (oo=7; oo>=0; oo--)
            if (conflict[oo][ss]!=allcon) break;
        if (oo<0) continue;
        while (oo>0 && conflict[oo-1][ss]!=allcon) oo--;

        /* Find the last octant that we will fill some of */
        ooo = oo;
        do
        {
            nxto = octwrap[ooo+1];
            if (conflict[nxto][ss]==allcon) break;
            ooo = nxto;
        }
        while (ooo!=oo);

        /* get some info about the obs point. */
        ii = obsi[ss];
        jj = obsj[ss];
        kk0 = ii+jj*nxd;
        val = raw0[kk0];
        /*fprintf(stderr,"fill %d  %d %d    %d %d %f\n",ss,oo,ooo,ii,jj,val);*/

        /* The first one, we fill and worry about adjacent distances */
        /* on the right and left, as appropriate. */
        any = 1;
        iip = ioct[oo];
        jjp = joct[oo];
        nxto = octwrap[oo+2];
        di = ioct[nxto][0];
        dj = joct[nxto][0];
        ddis = (oo%2) ? sqrt(2.0) : 1; 
        /*fprintf(stderr,"filling first %d %d\n",oo,(int)(conflict[oo][ss]));*/
        b1 = b2 = -1; /*swath points range, which should be defined immed*/
        eb1 = eb2 = -1; /*edge points, which can be undefined for part swath*/
        for (kk=b=bb=0; any; b++,iip++,jjp++)
        {
            if (b==oct0[oo][bb])
            {
                while ((kk=eoct[oo][bb])==0) bb++;
                if ((conflict[oo][ss]|right))
                    b1 = loct[oo][bb];
                else
                    b1 = b;
                if ((conflict[oo][ss]|left))
                    b2 = roct[oo][bb++];
                else
                    b2 = oct0[oo][++bb]-1;
                if (kk&edgeyes)
                {
                    eb1 = b1;
                    if (oo!=ooo) eb2 = b2;
                }
                if (kk&full) any = 0;
            }
            if (b<b1 || b>b2)  continue;
            i = ii+*iip;
            j = jj+*jjp;
            /*fprintf(stderr,"%d %d  %d %d  %d %d\n",ii,jj,(int)*iip,(int)*jjp,i,j);*/
            if (iout0[i]!=jout0[j]) continue;
            any = 1;
            kk0 = i+j*nxd;
            kkg = i+j*nxx;
            raw0[kk0] = grid[kkg] = val;
            dists0[kk0] = 0;

            /* check for right distance adjust. */
            if (b==eb1)
                for (iii=i,jjj=j,curdis=0; ; )
                {
                    iii -= di;
                    jjj -= dj;
                    if (iout0[iii]!=jout0[jjj]) break;
                    curdis += ddis;
                    kk0 = iii+jjj*nxd;
                    if (curdis>=dists0[kk0]) break;
                    dists0[kk0] = curdis;
                }

            /* check for left distance adjust. */
            else if (b==eb2)
                for (iii=i,jjj=j,curdis=0; ; )
                {
                    iii += di;
                    jjj += dj;
                    if (iout0[iii]!=jout0[jjj]) break;
                    curdis += ddis;
                    kk0 = iii+jjj*nxd;
                    if (curdis>=dists0[kk0]) break;
                    dists0[kk0] = curdis;
                }

        }
        if (oo==ooo) continue;

        /* The intermediate ones, we just fill the whole octant. */
        oo = octwrap[oo+1];
        while (oo!=ooo)
        {
            any = 1;
            iip = ioct[oo];
            jjp = joct[oo];
            /*fprintf(stderr,"filling mid %d\n",oo);*/
            for (b=bb=0; any; b++,iip++,jjp++)
            {
                if (b==oct0[oo][bb])
                {
                    bb++;
                    while (eoct[oo][bb]==0) bb++;
                    if (kk&full) any = 0;
                }
                i = ii+*iip;
                j = jj+*jjp;
                /*fprintf(stderr,"%d %d  %d %d  %d %d\n",ii,jj,(int)*iip,(int)*jjp,i,j);*/
                if (iout0[i]!=jout0[j]) continue;
                any = 1;
                kk0 = i+j*nxd;
                kkg = i+j*nxx;
                raw0[kk0] = grid[kkg] = val;
                dists0[kk0] = 0;
            }
            oo = octwrap[oo+1];
        }

        /* The last one, we fill and worry about adjacent distances */
        /* on the left only. */
        any = 1;
        iip = ioct[oo];
        jjp = joct[oo];
        nxto = octwrap[oo+2];
        di = ioct[nxto][0];
        dj = joct[nxto][0];
        ddis = (oo%2) ? sqrt(2.0) : 1; 
        /*fprintf(stderr,"filling last %d %d\n",oo,(int)(conflict[oo][ss]));*/
        eb2 = -1; /*left edge point, which can be undefined for part swath*/
        for (b=bb=0; any; b++,iip++,jjp++)
        {
            if (b==oct0[oo][bb])
            {
                while ((kk=eoct[oo][bb])==0) bb++;
                if ((conflict[oo][ss]|right))
                    b1 = loct[oo][bb];
                else
                    b1 = b;
                if ((conflict[oo][ss]|left))
                    b2 = roct[oo][bb++];
                else
                    b2 = oct0[oo][++bb]-1;
                if (kk&edgeyes) eb2 = b2;
                if (kk&full) any = 0;
            }
            if (b<b1 || b>b2) continue;
            i = ii+*iip;
            j = jj+*jjp;
            /*fprintf(stderr,"%d %d  %d %d  %d %d\n",ii,jj,(int)*iip,(int)*jjp,i,j);*/
            if (iout0[i]!=jout0[j]) continue;
            any = 1;
            kk0 = i+j*nxd;
            kkg = i+j*nxx;
            raw0[kk0] = grid[kkg] = val;
            dists0[kk0] = 0;

            /* check for left distance adjust. */
            if (b!=eb2) continue;
            for (iii=i,jjj=j,curdis=0; ; )
            {
                iii += di;
                jjj += dj;
                if (iout0[iii]!=jout0[jjj]) break;
                curdis += ddis;
                kk0 = iii+jjj*nxd;
                if (curdis>=dists0[kk0]) break;
                dists0[kk0] = curdis;
            }

        }

    }

#if 0
    fprintf(stderr,"raw\n");
    for (jj=nyd-1; jj>=0; jj--)
    {
        k = jj*nxd;
        for (i=0; i<nxd; i++,k++)
            if (raw[k]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",raw[k]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");

    fprintf(stderr,"dists\n");
    for (jj=nyd-1; jj>=0; jj--)
    {
        k = jj*nxd;
        for (i=0; i<nxd; i++,k++)
            if (dists[k]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",dists[k]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
#endif

    nbase = nsrch = 0;
    mbase = nxd;
    if (nyd>mbase) mbase = nyd;
    mbase *= 15;
    nodes = (struct node*)malloc(nnn*sizeof(struct node));
    bases = (struct node**)malloc(mbase*sizeof(struct node*));
    memset(nodes, 0, nnn*sizeof(struct node));
    memset(bases, 0, mbase*sizeof(struct node*));
    for (j=kg=k0=0; j<nyy; j++, k0+=ddx)
        for (i=0; i<nxx; i++,kg++,k0++)
        {
            grid[kg] = raw0[k0];
            onenode = nodes+kg;
            hi = (int)(dists0[k0]*10);
            if (hi<=0) continue;
            nsrch++;
            if (hi>nbase) nbase = hi;
            onenode->loc = kg;
            onenode->base = hi;
            onenode->next = bases[hi];
            if (onenode->next) onenode->next->prev = onenode;
            bases[hi] = onenode;
        }
    /*fprintf(stderr,"nbase nsrch %d %d\n",nbase,nsrch);*/
    /*verify_nodes(0);*/

    /*main loop that sets the value for the furthest grid point*/
    while (--nsrch>=0)
    {

        while (bases[nbase]==0 && nbase>0) nbase--;
        /*fprintf(stderr,"nbase nsrch %d %d\n",nbase,nsrch);*/
        if (nbase==0)
        {
            fprintf(stderr,"prematurely exhausted nodes\n");
            break;
        }
        onenode = bases[nbase];
        if (onenode->next) onenode->next->prev = 0;
        bases[nbase] = onenode->next;
        kg = onenode->loc;
        onenode->prev = onenode->next = 0;
        jj = kg/nxx;
        ii = kg-jj*nxx;
        k0 = ii+jj*nxd;
        /*fprintf(stderr,"<%d %d> %f\n",ii,jj,dists0[k0]);*/
        dists0[k0] = 0;

        for (oo=0; oo<8; oo++)
        {
            /*fprintf(stderr,"octant %d\n",oo);*/
            qdis[oo] = qval[oo] = 0;
            wtot = b = nb = 0;
            ok = 1;
            iip = ioct[oo];
            jjp = joct[oo];
            oop = oct0[oo];
            ddp = doct[oo];
            for (bb=0; bb<m_swath && nb==0 && ok; bb++)
            {
                oop++;
                if (eoct[oo][bb]==0) continue;
                if (eoct[oo][bb]&full) ok=0;
                for (; b<*oop; b++, iip++, jjp++)
                {
                    i = ii+*iip;
                    j = jj+*jjp;
                    if (iout0[i]>0 || jout0[j]>0) continue;
                    /*if (ok==0) fprintf(stderr,"** ");*/
                    /*fprintf(stderr,"|%d %d %d %d %d %d %d %d ",*/
                    /*ii,jj,iip-ioct[oo],jjp-joct[oo],*iip,*jjp,i,j);*/
                    ok = 1;
                    kk0 = j*nxd+i;
                    if (raw0[kk0]>1e36)
                    {
                        if (iout0[i]!=jout0[j]) continue;
                        if (ddp[b]>=dists0[kk0]) continue;
                        kkg = j*nxx+i;
                        onenode = nodes+kkg;
                        if (onenode->loc!=kkg)
                        {
                            fprintf(stderr,"node location mismatch %d %d\n",
                                    onenode->loc,kkg);
                            continue;
                        }
                        hi = (int)(ddp[b]*10);
                        if (hi>=onenode->base) continue;
                        /*fprintf(stderr,"  >>> %f %d  %f %d\n",dists0[kk0],onenode->base,ddp[b],hi);*/
                        /*nc = verify_nodes(-1);*/
                        if (onenode->next)
                            onenode->next->prev = onenode->prev;
                        if (onenode->prev)
                            onenode->prev->next = onenode->next;
                        else
                            bases[onenode->base] = onenode->next;
                        onenode->base = hi;
                        onenode->next = bases[hi];
                        if (onenode->next) onenode->next->prev = onenode;
                        onenode->prev = 0;
                        bases[hi] = onenode;
                        dists0[kk0] = ddp[b];
                        /*verify_nodes(nc);*/
                        continue;
                    }
                    /*fprintf(stderr,"%d  %d %d  %d %d  %d %d  %d %d  %f %f\n",*/
                    /*oo,ii,jj,iip-ioct[oo],jjp-joct[oo],*iip,*jjp,i,j,ddp[b],raw0[kk0]);*/
                    if (nb==0)
                    {
                        qdis[oo] = ddp[b];
                        qval[oo] = raw0[kk0];
                        wtot = 1/(ddp[b]*ddp[b]);
                    }
                    else
                    {
                        wgt = 1/ddp[b]*ddp[b];
                        qval[oo] = (raw0[kk0]*wgt+qval[oo]*wtot)/(wgt+wtot);
                        wtot = wgt+wtot;
                        if (ddp[b]<qdis[oo]) qdis[oo] = ddp[b];
                    }
                    nb++;
                }
            }
            /*if (nb<=1) fprintf(stderr,"\n");*/
            /*fprintf(stderr,"  %f %f\n",qdis[oo],qval[oo]);*/
        }


        /*fprintf(stderr,"finish\n");*/
        for (oo=0; oo<8; oo++)
        {
            if (qdis[oo]==0) continue;
            if (qdis[octwrap[oo+3]]>0 || qdis[octwrap[oo+5]]>0 ||
                    qdis[octwrap[oo+4]]>0) break;
        }
        ss = (oo<8);
        wtot = val = 0;
        for (oo=0; oo<8; oo++)
        {
            if (qdis[oo]==0) continue;
            /*fprintf(stderr,"  %f %f\n",qdis[oo],qval[oo]);*/
            wgt = qdis[oo];
            wgt = 1/(wgt*wgt);
            if (ss) wgt *= wgt;
            wtot += wgt;
            val += wgt*qval[oo];
        }
        raw0[k0] = grid[kg] = val/wtot;
        /*fprintf(stderr,"== %f %f %f\n",wtot,val,grid[kg]);*/

    }

    /*fprintf(stderr,"nbase,bases[nbase] %d %d\n\n",nbase,(int)(bases[nbase]));*/

#if 0
    fprintf(stderr,"analyzed\n");
    for (jj=nyy-1; jj>=0; jj--)
    {
        kg = jj*nxx;
        for (i=0; i<nxx; i++,kg++)
            if (grid[kg]>1e36)
                fprintf(stderr,"*** ");
            else
                fprintf(stderr,"%.1f ",grid[kg]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
#endif

    free(counts);
    free(raw);
    free(octant);
    free(nearest);
    free(iout);
    free(jout);
    free(dists);
    free(obsrchdat);
    free(conflictdat);
    free(leftcondat);
    free(rightcondat);
    free(srchlist);
    free(bases);
    free(nodes);

    return 1;
}

#if !INTEGRATED_BUILD
int main(int argc, char * argv[])
{
    int nx,ny,nv;
    int i,j,k,nn,s;

    float xind[5000], yind[5000], values[5000], grid[50000];
    char line[200];

    FILE * infile = stdin;
    FILE * fp;

    if (argc>1 && (fp=fopen(argv[1],"r"))) infile = fp;

    fgets(line, 200, infile);
    sscanf(line,"%d %d",&nx,&ny);
    nv = 0;
    while (fgets(line, 200, infile)!=NULL)
        if (sscanf(line,"%f %f %f",xind+nv,yind+nv,values+nv)==3)
            nv++;
    fprintf(stderr,"nv %d   ny %d ny %d\n",nv,nx,ny);
    scaleless_analysis
        (xind, yind, values, &nv, &nx, &ny, grid);

    for (j=ny-1; j>=0; j--)
    {
        k = j*nx;
        for (i=0; i<nx; i++,k++)
            if (grid[k]>1e36)
                fprintf(stderr,"*** ",grid[k]);
            else
                fprintf(stderr,"%.1f ",grid[k]);
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");

#if 0
    /*  printf("Enter dimension: "); */
    /*  scanf("%d",&nn); */
    /*  init_distance_tables(nn, nn); */
    fprintf(stderr,"\nsxy %d  n_pts %d  n_swath %d\n",sxy,n_pts,n_swath);
    for (j=sxy-1; j>=0; j--)
    {
        k = j*sxy;
        for (i=0; i<sxy; i++,k++)
            fprintf(stderr,"%.1f ",dlookup[k]);
        fprintf(stderr,"\n");
    }
    for (s=k=0; k<n_swath; )
    {
        fprintf(stderr,"swath %d: %d %d\n",k,swath0[k],swathm[k]); 
        k++;
        for (; s<swath0[k]; s++)
            fprintf(stderr,"%d %d %d  %.1f\n",
                    iswath[s],jswath[s],kswath[s],dswath[s]); 
    }
    fprintf(stderr,"end last swath %d\n",swath0[k]); 
#endif

}
#endif
