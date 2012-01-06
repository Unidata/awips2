/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.pointdata.vadriver;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Port of AWIPS I va_advanced driver.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         NWS         Original Code
 * Dec 10, 2007            brockwoo    Full port to Java
 * 
 * </pre>
 * 
 * The VA_Advanced class is a port of the National Weather Service code which
 * performs progressive disclosure on station data.
 * 
 * 
 * @author brockwoo
 * @version 1.0
 */
public class VA_Advanced {

    private static final int BREAKAXIS = 5;

    private static final int BREAKALL = 25;

    // private static final int BREAKAXIST = 8;
    // private static final int BREAKALLT = 64;

    private double aspect;

    private int min_literal;

    private int max_literal;

    private boolean dist_pass;

    private boolean recomp;

    private double[] xx, yy, zz;

    private byte[] assigned;

    private Coordinate[] latLons;

    private Double[] dist;

    private double[] gooddist;

    private Integer[] goodness;

    private double[] goodcomp;

    private double weighting;

    private boolean just_goodness;

    private double dtr;

    private double pio2;

    public VA_Advanced() {
        this.weighting = 0.0;
        this.just_goodness = false;
        this.aspect = 0;
        this.min_literal = 1;
        this.max_literal = 0;
        this.dist_pass = false;
        this.recomp = false;
    }

    /**
     * This routine allows the client to place more importance on either user
     * preferences or spatial uniformity in determining the visibility
     * thresholds. 0 means only consider user preferences, 1 means only consider
     * spatial uniformity. 0 is the default, values between 0 and 1 are
     * meaningful.
     * 
     * @param wgt
     */
    public void setVaWeighting(float wgt) {
        this.weighting = wgt;
    }

    /**
     * Normally, when selecting which station to include next, two things are
     * considered. First, spatial uniformity, which means use the station
     * furthest from any station currently included. Second, user preferences
     * which are usually expressed in terms of the distance to the nearest other
     * station with at least as great a goodness value. Expressing both in terms
     * of a distance means that these two considerations can easily be blended.
     * This routine allows one to just consider the goodness value as is for
     * determining the next station to include.
     * 
     * @param yes
     */
    public void setVaJustGoodness(boolean yes) {
        this.just_goodness = yes;
    }

    /**
     * Goodness values between these two numbers are taken literally as
     * progressive disclosure distances. By default no goodness values will be
     * treated this way.
     * 
     * @param mn
     * @param mx
     */
    public void setVaLiteral(int mn, int mx) {
        this.min_literal = mn;
        this.max_literal = mx;
    }

    /**
     * If this is set to true, then valid progressive disclosure distances in
     * the array input_dist will be preserved.
     * 
     * @param dp
     */
    public void setVaDistPass(boolean dp) {
        this.dist_pass = dp;
    }

    /**
     * If this is set to true, then valid progressive disclosure distances in
     * the array input_dist will be converted to goodness values.
     * 
     * @param rc
     */
    public void setVaRecomp(boolean rc) {
        this.recomp = rc;
    }

    /**
     * This routine performs the primary function of this class for the client.
     * The basic output of this module is a set of progressive disclosure
     * distances in the array `input_dist'. The basic input, beyond the lat/lon
     * locations of the points, is the goodness values in `input_goodness',
     * which are relative client preferences as to which station should show up
     * first when zooming. The goodness values are only hints; the routine makes
     * the final determination of the progressive disclosure distances based
     * primarily on two rules. The first rule is that once a point is revealed
     * by zooming, further zooming will not cause it to disappear. The second
     * rule is that stations do not overlap each other when plotted, which of
     * course it the main reason for the existence of this routine. The
     * progressive disclosure distances are the distance in kilometers to the
     * nearest other station which is at least as visible. In order to apply
     * these distances to a progressive disclosure strategy, the client needs
     * knowledge of the the physical size of the objects being plotted on the
     * screen.
     * 
     * @param latLon
     * @param input_goodness
     * @param dst
     * @return An array of progressive disclosure distances for the stations
     */
    public Double[] getVaAdvanced(Coordinate[] latLon,
            Integer[] input_goodness, Double[] dst) {

        int i, g, gg, k, kk, na;
        int ns;
        double lt, ln;
        double dx, dy, dz, d, dd;
        double d2, dd2;
        double minlat, minlon, maxlat, maxlon;
        int[] active;
        int[] unassigned;
        double big;
        double xxa, yya, zza;
        double xxn, yyn, zzn;
        double xxe, yye;
        double myaspect;

        /* No points, give up. */
        if (latLon.length <= 0) {
            return new Double[0];
        }

        this.dist = dst;

        /*
         * set internal variables for the input arguments, allocate any memory
         * we will need.
         */
        ns = latLon.length;
        this.latLons = latLon;
        this.goodness = input_goodness;
        this.xx = new double[ns];
        this.yy = new double[ns];
        this.zz = new double[ns];
        this.gooddist = new double[ns];
        assigned = new byte[ns];
        active = new int[ns];
        i = 0x7FFFFFFF;
        big = i;

        /*
         * goodcomp is double because it must have the precision of a float and
         * the range of integral values of an int. It is workspace within which
         * we convert goodness values to goodness distances.
         */
        goodcomp = new double[ns];

        /*
         * Here we calculate a unit vector on the earths surface which is used
         * to make quick relative comparisons of point to point distances. Here
         * we also figure out the entire range of lats and lons in the data set
         * and we handle whether input valid progressive disclosure distances
         * are preserved, ignored, or turned into goodness values. Stations with
         * no initial progressive disclosure distance get a relative distance of
         * 2.0, which when converted to a real distance is halfway round the
         * globe.
         */
        minlat = maxlat = this.latLons[0].y;
        minlon = maxlon = this.latLons[0].x;
        xxa = yya = zza = 0.0f;
        // this.dtr = Math.atan(1.0) / 45.0;
        pio2 = Math.acos(0.0);
        this.dtr = pio2 / 90.0;
        for (i = 0; i < ns; i++) {
            active[i] = i;
            lt = dtr * this.latLons[i].y;
            ln = dtr * this.latLons[i].x;
            xx[i] = Math.cos(lt) * Math.cos(ln);
            yy[i] = Math.cos(lt) * Math.sin(ln);
            zz[i] = Math.sin(lt);
            xxa += xx[i];
            yya += yy[i];
            zza += zz[i];
            if ((dist_pass || recomp) && dist[i] >= 0) {
                assigned[i] = 2;
                dist[i] = dist[i] / 12740.0;
                if (dist[i] >= pio2) {
                    dist[i] = 2.0;
                } else {
                    dist[i] = 2.0 * Math.sin(dist[i]);
                }
                goodcomp[i] = (1.0 + dist[i]) * big;
            } else if (goodness[i] >= min_literal && goodness[i] <= max_literal) {
                assigned[i] = 1;
                dist[i] = 2 * Math.sin(goodness[i] / 12740.0);
                goodcomp[i] = (1 + dist[i]) * big;
            } else {
                assigned[i] = 0;
                dist[i] = 2.0;
                goodcomp[i] = goodness[i];
            }/* endif */
            if (recomp) {
                assigned[i] = 0;
                dist[i] = 2.0;
            }
            gooddist[i] = 2.0;
            if (this.latLons[i].y < minlat) {
                minlat = this.latLons[i].y;
            } else if (this.latLons[i].y > maxlat) {
                maxlat = this.latLons[i].y;
            }
            if (this.latLons[i].x < minlon) {
                minlon = this.latLons[i].x;
            } else if (this.latLons[i].x > maxlon) {
                maxlon = this.latLons[i].x;
            }
        }/* end for */

        /*
         * try to account for the aspect ratio of what is being plotted if the
         * area of coverage is small.
         */
        myaspect = 1.0;
        while (aspect > 1) {

            /* unit vector of mean point */
            xxa /= ns;
            yya /= ns;
            zza /= ns;
            d = Math.sqrt(xxa * xxa + yya * yya + zza * zza);
            xxa /= d;
            yya /= d;
            zza /= d;

            /* east unit vector at mean point */
            xxe = -yya;
            yye = xxa;
            d = Math.sqrt(xxe * xxe + yye * yye);
            xxe /= d;
            yye /= d;

            /* get the greatest east-west separation from center */
            dx = 0;
            dd = 1.0;
            for (i = 0; i < ns; i++) {
                d = xxe * xx[i] + yye * yy[i];
                if (d < 0) {
                    d = -d;
                }
                if (d > dx) {
                    dx = d;
                }
                d = xxa * xx[i] + yya * yy[i] + zza * zz[i];
                if (d < dd) {
                    dd = d;
                }
            }/* end for */

            /* calculate working aspect based on the separation. */
            dx *= dx;
            dd = 1.0 - dd * dd;
            if (dd > dx) {
                dx = dd;
            }
            dd = 1.0 - 2 * dx;
            if (dd <= 0.0) {
                break;
            }
            myaspect = Math.pow(aspect, dd);

            /* north unit vector at mean point (AxE) */
            xxn = -zza * yye;
            yyn = zza * xxe;
            zzn = xxa * yye - yya * xxe;

            /* artificially sqeeze points in the east-west dimension */
            for (i = 0; i < ns; i++) {
                dx = (xxe * xx[i] + yye * yy[i]) / myaspect;
                dy = (xxn * xx[i] + yyn * yy[i] + zzn * zz[i]);
                dz = (xxa * xx[i] + yya * yy[i] + zza * zz[i]);
                xx[i] = dx;
                yy[i] = dy;
                zz[i] = dz;
            }/* end for */

            break;
        }/* end while */

        /*
         * This is the area within which we will work as we turn relative
         * goodness values into goodness distances.
         */
        d = (maxlat - minlat) / 200;
        minlat -= d;
        maxlat += d;
        d = (maxlon - minlon) / 200;
        minlon -= d;
        maxlon += d;

        /* Convert goodness values into goodness distances. */
        // if (just_goodness) {
        if (!just_goodness) {
            this.goodnessToDist(minlat, maxlat, minlon, maxlon, active, ns);
        }

        /*
         * Categorize stations as preassigned or not. Active becomes a map for
         * the order in which we consider stations for final resolution;
         * preassigned stations first, then all others.
         */
        unassigned = new int[ns];
        na = kk = 0;
        for (k = 0; k < ns; k++) {
            if (assigned[k] != 0) {
                active[na++] = k;
            } else {
                unassigned[kk++] = k;
            }
        }
        for (k = na, kk = 0; k < ns; k++, kk++) {
            active[k] = unassigned[kk];
        }

        /*
         * This loop is not technically through each station we need to assign,
         * but just for the number we need to assign.
         */
        for (k = 0; k < ns; k++) {

            if (k >= na) {

                /* Determine next non-preassigned station for final resolution. */
                dd2 = -big;
                g = -1;
                for (kk = na; kk < ns; kk++) {
                    gg = active[kk];
                    if (assigned[gg] != 0) {
                        continue;
                    }

                    /*
                     * Here is where we balance user preference versus spatial
                     * uniformity.
                     */
                    if (just_goodness) {
                        d2 = goodness[gg];
                    } else {
                        d2 = dist[gg] * weighting + (1 - weighting)
                                * gooddist[gg];
                    }
                    if (d2 <= dd2) {
                        continue;
                    }

                    g = gg;
                    dd2 = d2;
                }/* end for */
                if (g < 0) {
                    break;
                }

            } else {
                /* still working on preassigned stations. */
                g = active[k];
            }

            /*
             * Station g has been selected for final resolution. For all other
             * stations that have not yet been been resolved, reduce their
             * relative distance to the distance between it and station g,
             * unless its distance is already less.
             */
            if (assigned[g] == 0) {
                assigned[g] = 1;
            }
            for (kk = 0; kk < ns; kk++) {
                gg = active[kk];
                if (assigned[gg] != 0) {
                    continue;
                }
                dd = dist[gg];
                if (dist[g] < dd) {
                    dd = dist[g];
                }
                dx = xx[g] - xx[gg];
                if (dx >= dd || -dx >= dd) {
                    continue;
                }
                dy = yy[g] - yy[gg];
                if (dy >= dd || -dy >= dd) {
                    continue;
                }
                dz = zz[g] - zz[gg];
                if (dz >= dd || -dz >= dd) {
                    continue;
                }
                d = Math.sqrt(dx * dx + dy * dy + dz * dz);
                if (d >= dd) {
                    continue;
                }
                dist[gg] = d;
            }/* end for */

        }/* end for */

        /*
         * convert point to point distances in unit vector space to real
         * distances in kilometers.
         */
        for (i = 0; i < ns; i++) {
            // dist[i] = myaspect * 6370 * 2 * Math.asin(dist[i] / 2);
            dist[i] = 6370.0 * 2.0 * Math.asin(dist[i] / 2.0);
            if (assigned[i] != 2) {
                dist[i] *= myaspect;
            }
        }
        return this.dist;
    }

    /**
     * The job of this routine is to convert goodness values into a precursor to
     * progressive disclosure distances, namely the distance to the nearest
     * other station which has at least as great a goodness value.
     */
    private int goodnessToDist(double minlat, double maxlat, double minlon,
            double maxlon, int[] active, int na) {
        // System.out.println(minlat + " " + maxlat + " " + minlon + " " +
        // maxlon);
        double dis1;
        int best1;
        int subactive[][] = new int[BREAKALL][];
        int ns[] = new int[BREAKALL];
        double mnlts[] = new double[BREAKALL];
        double mxlts[] = new double[BREAKALL];
        double mnlns[] = new double[BREAKALL];
        double mxlns[] = new double[BREAKALL];
        int subim, subjm, submm;
        int nsim, nsjm, nsmm;
        int[] topactive;
        int nt;
        int i, j, k, kk, a, g, gg;
        double dlt, dln;
        double mnlt, mxlt, mnln, mxln;
        double dx, dy, dz, d, dd;
        int sub_used;

        if (na <= 0) {
            return -1;
        }

        if (na > BREAKALL * 2) {

            /*
             * We still have too many stations in this sub area for this
             * algorithm to work efficiently, divide this working area further
             * into subareas.
             */
            nt = 0;
            dlt = (maxlat - minlat) / (1.0 + BREAKAXIS);
            dln = (maxlon - minlon) / (1.0 + BREAKAXIS);
            mnlt = minlat;
            mxlt = mnlt + 2.0 * dlt;
            for (k = 0, j = 0; j < BREAKAXIS; j++) {
                mnln = minlon;
                mxln = mnln + 2.0 * dln;
                for (i = 0; i < BREAKAXIS; i++, k++) {
                    subactive[k] = new int[na];
                    ns[k] = 0;
                    mnlns[k] = mnln;
                    mxlns[k] = mxln;
                    mnlts[k] = mnlt;
                    mxlts[k] = mxlt;
                    mnln += dln;
                    mxln += dln;
                }/* end for i */
                mnlt += dlt;
                mxlt += dlt;
            }/* end for j */

            subim = -1;
            subjm = -BREAKAXIS;
            submm = subjm - 1;
            nsim = -1;
            nsjm = -BREAKAXIS;
            nsmm = nsjm - 1;

            /* Assign each station to its proper sub area. */
            for (a = 0; a < na; a++) {
                g = active[a];
                i = (int) ((this.latLons[g].x - minlon) / dln);
                if (i < 0) {
                    i = 0;
                }
                if (i > BREAKAXIS) {
                    i = BREAKAXIS;
                }
                j = (int) ((this.latLons[g].y - minlat) / dlt);
                if (j < 0) {
                    j = 0;
                }
                if (j > BREAKAXIS) {
                    j = BREAKAXIS;
                }
                k = j * BREAKAXIS + i;
                if (i <= 0) {
                    if (j > 0) {
                        subactive[k + subjm][ns[k + nsjm]++] = g;
                    }
                    if (j < BREAKAXIS) {
                        subactive[k][ns[k]++] = g;
                    }
                } else if (i >= BREAKAXIS) {
                    if (j > 0) {
                        subactive[k + submm][ns[k + nsmm]++] = g;
                    }
                    if (j < BREAKAXIS) {
                        subactive[k + subim][ns[k + nsim]++] = g;
                    }
                } else {
                    if (j > 0) {
                        subactive[k + subjm][ns[k + nsjm]++] = g;
                        subactive[k + submm][ns[k + nsmm]++] = g;
                    }/* endif */
                    if (j < BREAKAXIS) {
                        subactive[k][ns[k]++] = g;
                        subactive[k + subim][ns[k + nsim]++] = g;
                    }/* endif */
                }/* endif */
            }/* end for a */

            /*
             * Check if the stations have actually be divided into more than one
             * group
             */
            sub_used = 0;
            for (k = 0; k < BREAKALL; k++) {
                if (ns[k] != 0) {
                    sub_used++;
                }
            }
            if (sub_used > 1) {
                /*
                 * Do the goodness value to goodness distance conversions
                 * separately for the stations in each sub area. Accumulate the
                 * indices of the station with the highest goodness distance
                 * from each sub area.
                 */
                topactive = new int[BREAKALL];
                for (k = 0; k < BREAKALL; k++) {
                    kk = this.goodnessToDist(mnlts[k], mxlts[k], mnlns[k],
                            mxlns[k], subactive[k], ns[k]);
                    if (kk >= 0) {
                        topactive[nt++] = kk;
                    }
                }/* end for gg */
            } else {
                /*
                 * All of the stations have ended up in the same subset. Assign
                 * a gooddist of 0 to all stations unless there is one (and only
                 * one) with a maximal goodcomp. This is what the normal
                 * algorithm would do, but runs faster.
                 */

                dis1 = -1.0; /* This will track the maximal goodcomp value */
                best1 = -1;
                for (a = 0; a < na; a++) {
                    g = active[a];
                    if (goodcomp[g] > dis1) {
                        if (best1 != -1) {
                            gooddist[best1] = 0.0;
                        }
                        best1 = g;
                        dis1 = goodcomp[g];
                    } else if (goodcomp[g] == dis1) {
                        gooddist[best1] = gooddist[g] = 0.0;
                    } else {
                        gooddist[g] = 0.0;
                    }
                }

                /*
                 * If there was a station with a maximal goodcomp, return it.
                 * Otherwise, return the first one.
                 */
                if (best1 == -1) {
                    best1 = active[0];
                }
                return best1;
            }

        } else {

            nt = na;
            topactive = active;

        }/* endif */

        best1 = -1;
        dis1 = -1;

        /*
         * We are finding the distance to the nearest station that is at least
         * as desirable.
         */
        for (k = 0; k < nt - 1; k++) {
            for (kk = k + 1; kk < nt; kk++) {
                g = topactive[k];
                gg = topactive[kk];
                if (g == gg) {
                    continue;
                }
                if (goodcomp[g] < goodcomp[gg]) {
                    dd = gooddist[g];
                } else if (goodcomp[gg] < goodcomp[g]) {
                    dd = gooddist[gg];
                } else if (gooddist[g] > gooddist[gg]) {
                    dd = gooddist[g];
                } else {
                    dd = gooddist[gg];
                }
                dx = xx[g] - xx[gg];
                if (dx > dd || -dx > dd) {
                    continue;
                }
                dy = yy[g] - yy[gg];
                if (dy > dd || -dy > dd) {
                    continue;
                }
                dz = zz[g] - zz[gg];
                if (dz > dd || -dz > dd) {
                    continue;
                }
                d = Math.sqrt(dx * dx + dy * dy + dz * dz);
                if (d > dd) {
                    continue;
                }
                if (goodcomp[g] <= goodcomp[gg] && d < gooddist[g]) {
                    gooddist[g] = d;
                }
                if (goodcomp[gg] <= goodcomp[g] && d < gooddist[gg]) {
                    gooddist[gg] = d;
                }
            }/* end for kk */
        }/* end for k */

        /*
         * Report station with the furthest distance to the station that is at
         * least as desirable.
         */
        for (k = 0; k < nt; k++) {
            g = topactive[k];
            if (gooddist[g] < dis1) {
                continue;
            }
            dis1 = gooddist[g];
            best1 = g;
        }/* end for */
        return best1;
    }

    /**
     * @param aspect
     *            the aspect to set
     */
    public void setAspect(double aspect) {
        this.aspect = aspect;
    }

    /**
     * This routine performs the same basic function as va_advanced, except that
     * no user preferences are supplied. The resulting values for the
     * progressive disclosure distances are totally determined by the stochastic
     * distribution of the elements that are input
     * 
     * @param input_coords
     * @param input_dist
     */
    public Double[] getVaSimple(Coordinate[] input_coords, Double[] input_dist) {
        int i, g, gg, k, kk;
        int ns, np, na;
        double lt, ln, dd2;
        double dx, dy, dz, d, dd;
        int[] actdata;

        ns = input_coords.length;

        /* No points, give up */
        if (ns <= 0) {
            return new Double[0];
        }

        /*
         * set internal variables for the input arguments, allocate any memory
         * we will need.
         */
        this.latLons = input_coords;
        this.dist = input_dist;
        xx = new double[ns];
        yy = new double[ns];
        zz = new double[ns];
        this.assigned = new byte[ns];
        int active = 0;
        actdata = new int[ns];

        /*
         * Here we calculate a unit vector on the earths surface which is used
         * to make quick relative comparisons of point to point distances. Here
         * we also figure out the entire range of lats and lons in the data set
         * and we handle whether input valid progressive disclosure distances
         * are preserved, or ignored. Stations with no initial progressive
         * disclosure distance get a relative distance of 2.0, which when
         * converted to a real distance is halfway round the globe.
         */
        pio2 = Math.acos(0);
        dtr = pio2 / 90.0;
        np = na = 0;
        kk = ns;
        for (i = 0; i < ns; ++i) {
            lt = dtr * latLons[i].y;
            ln = dtr * latLons[i].x;
            xx[i] = Math.cos(lt) * Math.cos(ln);
            yy[i] = Math.cos(lt) * Math.sin(ln);
            zz[i] = Math.sin(lt);

            if (dist_pass && dist[i] >= 0) {
                assigned[i] = 2;
                dist[i] = dist[i] / 12740.0;
                if (dist[i] >= pio2) {
                    dist[i] = 2.0;
                } else {
                    dist[i] = 2 * Math.sin(dist[i]);
                }
            } else {
                assigned[i] = 0;
                dist[i] = 2.0;
            }
            if (recomp) {
                assigned[i] = 0;
                dist[i] = 2.0;
            }
            /*
             * Categorize stations as preassigned or not. Active becomes a map
             * for the order in which we consider stations for final resolution;
             * preassigned stations first, then all others.
             */
            if (assigned[i] != 0) {
                actdata[active + (np++)] = i;
            } else {
                actdata[active + (--kk)] = i;
            }
            na++;
        }

        /*
         * This loop is not technically through each station we need to assign,
         * but just for the number we need to assign.
         */
        if (np == 0) {
            np = 1;
        }

        while (na > 0) {
            na--;
            if (np == 0) {
                /* Determine next non-preassigned station for final resolution. */
                dd2 = -1;
                g = gg = -1;
                for (kk = 0; kk <= na; kk++) {
                    k = actdata[active + kk];
                    if (dist[k] <= dd2) {
                        continue;
                    }
                    g = k;
                    gg = kk;
                    dd2 = dist[k];
                }
                if (g < 0) {
                    break;
                }
                actdata[active + gg] = actdata[active + na];
            } else {
                /* still working on preassigned stations. */
                ++active;
                g = actdata[active];
                np--;
            }

            /*
             * Station g has been selected for final resolution. For all other
             * stations that have not yet been been resolved, reduce their
             * relative distance to the distance between it and station g,
             * unless its distance is already less.
             */
            for (kk = np; kk < na; kk++) {
                gg = actdata[active + kk];
                dd = dist[gg];
                if (dist[g] < dd) {
                    dd = dist[g];
                }
                dx = xx[g] - xx[gg];
                if (dx >= dd || -dx >= dd) {
                    continue;
                }
                dy = yy[g] - yy[gg];
                if (dy >= dd || -dy >= dd) {
                    continue;
                }
                dz = zz[g] - zz[gg];
                if (dz >= dd || -dz >= dd) {
                    continue;
                }
                d = Math.sqrt(dx * dx + dy * dy + dz * dz);
                if (d >= dd) {
                    continue;
                }
                dist[gg] = d;
            }
        }

        /*
         * convert point to point distances in unit vector space to real
         * distances in kilometers.
         */
        for (i = 0; i < ns; i++) {
            dist[i] = 6370 * 2 * Math.asin(dist[i] / 2);
        }
        return this.dist;
    }
}
