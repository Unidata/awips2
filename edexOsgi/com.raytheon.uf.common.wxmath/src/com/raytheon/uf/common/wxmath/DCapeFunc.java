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
package com.raytheon.uf.common.wxmath;

import static com.raytheon.uf.common.wxmath.AdiabeticTemperature.adiabatic_te;
import static com.raytheon.uf.common.wxmath.CalcTw.mytw;
import static com.raytheon.uf.common.wxmath.Constants.c0;
import static com.raytheon.uf.common.wxmath.Constants.c1;
import static com.raytheon.uf.common.wxmath.Constants.c2;
import static com.raytheon.uf.common.wxmath.Constants.c_1;
import static com.raytheon.uf.common.wxmath.Constants.c_2;
import static com.raytheon.uf.common.wxmath.Constants.kapa;
import static com.raytheon.uf.common.wxmath.Constants.kapa_1;
import static com.raytheon.uf.common.wxmath.TempOfTe.temp_of_te;
import static java.lang.Math.exp;
import static java.lang.Math.log;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

/**
 * We input theta and specific humidity for surface conditions because these are
 * things that can be arithemitically averaged for a mixed layer. In order for a
 * dcape to be calculated, there must be positive bouyancy somewhere in the
 * column based on the input surface conditions, neglecting any cap. Sinking
 * parcel starts from the minimum thetaE level that is above the condensation
 * pressure and below 400mb and the highest positive rising parcel bouyancy.
 * max_evap is the limit to how much water can be evaporated into the parcel as
 * it decends, in Kg/Kg. max_rh is the desired RH (%) as the parcel reaches the
 * surface. Will initially evaporate up to one-third of max_evap into the
 * sinking parcel at the start, afterward attempting to trend the RH toward
 * max_rh at the ground. If usetv=1, buoyancy is done with virtual temp, if
 * usetv=0 with temp.
 * 
 * There are a few small changes from the original C version. First all math is
 * done using doubles instead of floats since that is what the java.lang.Math
 * library uses and it gets more accurate results. Second all uses of 1e37 have
 * been replaces with Double.NaN to be more consistant with the rest of derived
 * parameters. Finally the loops have been reordered so that each grid cell is
 * iterated in the outermost loop which allows all intermediary calculations to
 * be stored as doubles instead of float[nx*ny] which dramatically reduces the
 * memory footprint.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2013 2043       bsteffen    Ported from meteolib C
 * Aug 13, 2013 2262       njensen     Moved from deriv params
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DCapeFunc {

    public static float[] dcapeFunc(float usetv, float[] p_dat, float[] t_dat,
            float[] td_dat, float[] p0, float[] th0, float[] sh0, int nx,
            int ny, int nz, float max_evap, float max_rh) {
        int n2 = nx * ny;
        int nzm = nz - 1;

        float[] dcape = new float[n2];

        for (int i = 0; i < n2; i += 1) {

            // Calculate the ascending parcel start equivalent temp, virtual
            // temp, and press at LCL, and the initial virtual temperature.
            // Initialize pm and wm, which now will be pressure at and min
            // environmetal virtual theta E.
            double tec = 0;
            double tvc = 0;
            double pc = 0;
            double tv = 0;
            double pp0 = p0[i];
            if (Double.isNaN(pp0) || Double.isNaN(th0[i])
                    || Double.isNaN(sh0[i])) {
                tec = tvc = pc = Double.NaN;
            } else {
                double t0 = th0[i] * pow(pp0 / 1000, kapa);
                tv = t0 * (1 + usetv * 0.000608 * sh0[i]);
                double b = c0 - log(pp0 / (622.0 / sh0[i] + 0.378));
                double td = (b - sqrt(b * b - c_1)) / c_2;
                td -= (t0 - td)
                        * (-0.37329638 + 41.178204 / t0 + 0.0015945203 * td);
                pc = pp0 * pow(td / t0, kapa_1);
                tec = adiabatic_te(td, pc);
                tvc = td * (1 + usetv * 0.000608 * sh0[i]);
            }

            // Now calculate the virtual temperature of the accending parcel at
            // the pressures in the input data.
            double[] tvp = new double[nz];

            for (int k = 0; k < nz; k++) {
                float pp = p_dat[k * n2 + i];
                if (Double.isNaN(pc) || Double.isNaN(tec) || Double.isNaN(tvc)
                        || Double.isNaN(pp)) {
                    tvp[k] = Double.NaN;
                } else if (pp > pc) {
                    tvp[k] = tvc * pow(pp / pc, kapa);
                } else {
                    double t0 = tec * pow(pp / pc, kapa);
                    t0 = temp_of_te(t0, pp);
                    tvp[k] = t0
                            * pp
                            / (pp - usetv
                                    * exp(25.687958917 - c1 * t0 - c2 / t0));
                }
            }

            // Calculate environment virtual temp, where we force the
            // environment to be no cooler than dry adiabatic from the ascending
            // parcel start. Find pressure of min environmetal virtual theta E
            // above condensation pressure...record temperature and dewpoint
            // there. Since we do not need the accending parcel temps to
            // complete the dcape calc, we will put the environmental virtual
            // temp into the that storage.
            double wm = Double.NaN;
            double pm = Double.NaN;
            double tm = 0;
            double tdm = 0;
            for (int k = nzm; k >= 0; k -= 1) {
                float pp = p_dat[k * n2 + i];
                float tt = t_dat[k * n2 + i];
                float td3 = td_dat[k * n2 + i];

                if (Double.isNaN(tvc) || Double.isNaN(pc) || Double.isNaN(pp)
                        || Double.isNaN(tvp[k]) || Double.isNaN(tt)
                        || Double.isNaN(td3)) {
                    tvp[k] = Double.NaN;
                    continue;
                }
                double t0 = tt;
                double eee = exp(26.186004814 - c1 * td3 - c2 / td3);
                double qd = eee / (pp - 0.60771703 * eee);
                eee = (1 + usetv * 0.608 * qd);
                double thve = t0 * eee;
                double pr = pow(pp / pc, kapa);
                if (thve < tvc * pr) {
                    thve = tvc * pr;
                    t0 = thve / eee;
                }
                if (tvp[k] <= thve && Double.isNaN(wm) || pp > pc
                        && (pm >= 400 || Double.isNaN(pm))) {
                    if (Double.isNaN(pm) && pp < pc) {
                        pm = pc;
                    }
                    tvp[k] = thve;
                    continue;
                }
                tvp[k] = thve;
                thve = (thve + 2529 * qd) * pow(1000 / (pp), kapa);
                if (thve > wm && pm >= 400) {
                    continue;
                }
                wm = thve;
                pm = pp;
                tm = t0;
                tdm = td3;
            }

            // Here we will reuse our condensation level storage for
            // the level above current. This loop performs the actual dcape
            // calculation.
            double rhm = 0;
            double qq = 0;
            double tve1 = tvc;
            double pp1 = pc;
            double tvp1 = tec;
            for (int k = nzm; k >= 0; k -= 1) {
                double tve = tvp[k];
                float pp = p_dat[k * n2 + i];

                if (k == nzm) {
                    dcape[i] = Float.NaN;
                    pp1 = tvp1 = tve1 = Double.NaN;
                }
                if (k == nzm) {
                    dcape[i] = Float.NaN;
                    pp1 = tvp1 = tve1 = Double.NaN;
                }
                if (Double.isNaN(pm) || Double.isNaN(pp0) || Double.isNaN(tv)) {
                    continue;
                } else if (Double.isNaN(pp0)) {
                    ;
                } else if (pp1 >= pp0) {
                    continue;
                } else if (Double.isNaN(tve1)) {
                    ;
                } else if (Double.isNaN(pp) || Double.isNaN(tve)) {
                    continue;
                } else if (pp <= pm) {
                    ;
                } else if (Double.isNaN(wm)) {
                    dcape[i] = 0;
                } else {

                    // Now we finally have the data we need for calculating
                    // the dcape contribution for this layer. If we have not
                    // made any dcape calculations to this point, initialize
                    // the decent parcel.
                    if (Double.isNaN(dcape[i])) {
                        dcape[i] = 0;
                        double eee = exp(26.186004814 - c1 * tdm - c2 / tdm);
                        double qd = eee / (pm - 0.60771703 * eee);
                        double qw = qd + max_evap / 3;
                        double t0 = tm - 2529 * max_evap / 3;
                        eee = exp(26.186004814 - c1 * t0 - c2 / t0);
                        double qs = eee / (pm - 0.60771703 * eee);
                        if (qs >= qw) {
                            wm = max_evap - max_evap / 3;
                            tm = t0;
                            rhm = qw / qs;
                            double b = c0 - log(qw * pm / (0.622 - 0.378 * qw));
                            tdm = (b - sqrt(b * b - c_1)) / c_2;
                        } else {
                            tm = tdm = mytw(tm, tdm, pm);
                            rhm = 1.0;
                            eee = exp(26.186004814 - c1 * tm - c2 / tm);
                            qw = eee / (pm - 0.60771703 * eee);
                            wm = max_evap - (qw - qd);
                        }
                        qq = qw;
                        tvp1 = tm * (1 + usetv * 0.608 * qw);
                        pp1 = pm;
                    }

                    // Deal with reaching the surface, add in top of layer part.
                    double pb, dlnp, thve;
                    if (pp > pp0) {
                        pb = pp0;
                        dlnp = log(pb / pp1);
                        thve = tv;
                    } else {
                        pb = pp;
                        dlnp = log(pb / pp1);
                        thve = tve;
                    }
                    double up = -dlnp * 287 * 0.5 * (tvp1 - tve1);
                    if (up < -dcape[i]) {
                        dcape[i] = 0;
                    } else {
                        dcape[i] += up;
                    }

                    // Deal with letting parcel fall to pb
                    double pr = pow(pb / pp1, kapa);
                    if (wm <= 0)
                        tvp1 *= pr;
                    else {
                        double rhmx = rhm + (pb - pp1) * (max_rh - rhm)
                                / (pp0 - pp1);
                        double t0 = tm * pr;
                        double eee = exp(26.186004814 - c1 * t0 - c2 / t0);
                        double qs = eee / (pb - 0.60771703 * eee);
                        if (qq / qs > rhmx) {
                            tm = t0;
                            double b = c0 - log(qq * pb / (0.622 - 0.378 * qq));
                            tdm = (b - sqrt(b * b - c_1)) / c_2;
                            tvp1 *= pr;
                            rhm = qq / qs;
                        } else {
                            double qd = (rhmx * qs - qq)
                                    / sqrt(1000 * (rhmx * qs + qq));
                            if (qd > wm) {
                                qd = wm;
                            }
                            double qw = qq + qd;
                            double td = t0 - 2529 * wm;
                            eee = exp(26.186004814 - c1 * td - c2 / td);
                            qs = eee / (pb - 0.60771703 * eee);
                            if (qs >= qw) {
                                tm = td;
                                rhm = qw / qs;
                                double b = c0
                                        - log(qw * pb / (0.622 - 0.378 * qw));
                                tdm = (b - sqrt(b * b - c_1)) / c_2;
                            } else {
                                double b = c0
                                        - log(qq * pb / (0.622 - 0.378 * qq));
                                tdm = (b - sqrt(b * b - c_1)) / c_2;
                                tm = tdm = mytw(t0, tdm, pb);
                                rhm = 1.0;
                                eee = exp(26.186004814 - c1 * tm - c2 / tm);
                                qw = eee / (pb - 0.60771703 * eee);
                                qd = qw - qq;
                            }
                            wm -= qd;
                            qq = qw;
                            tvp1 = tm * (1 + usetv * 0.608 * qw);
                        }
                    }

                    // Add contribution of bottom of layer.
                    double dn = -dlnp * 287 * 0.5 * (tvp1 - thve);
                    if (dn < -dcape[i])
                        dcape[i] = 0;
                    else
                        dcape[i] += dn;

                }

                // Make current layer top next layer bottom.
                tve1 = tve;
                pp1 = pp;
            }
        }
        return dcape;
    }

}
