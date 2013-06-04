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
package com.raytheon.uf.viz.derivparam.python.function;

import static com.raytheon.uf.viz.derivparam.python.function.AdiabeticTemperature.adiabatic_te;
import static com.raytheon.uf.viz.derivparam.python.function.TempOfTe.temp_of_te;
import static java.lang.Math.exp;
import static java.lang.Math.log;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;
import jep.INumpyable;

/**
 * We input theta and specific humidity for initial parcel because these are
 * things that can be arithemitically averaged for a mixed layer. If usetv=1,
 * buoyancy is done with virtual temp, if usetv=0 with temp. If usetv=0, must
 * supply temps in tve_dat.
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
 * Jun 3, 2013  2043       bsteffen    Ported from meteolib C
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CapeFunc {

    private static final float c0 = 26.66082f;

    private static final float c1 = 0.0091379024f;

    private static final float c2 = 6106.396f;

    private static final float c_1 = 223.1986f;

    private static final float c_2 = 0.0182758048f;

    private static final float kapa = 0.286f;

    private static final float kapa_1 = 3.498257f;

    public static CapeCinPair capeFunc(float usetv, float[] p_dat,
            float[] tve_dat, float[] p0, float[] th0, float[] sh0, int nx,
            int ny, int nz) {
        int n2 = nx * ny;

        float[] cin = new float[n2];
        float[] cap = new float[n2];

        // Calculate the parcel equivalent temp, virtual temp, and press at LCL.
        // Make working copy of sfc press, use as press below current 3d
        // pressure.
        for (int i = 0; i < n2; i += 1) {
            double tec = 0;
            double tvc = 0;
            double pc = 0;
            double pp0 = p0[i];
            double pp1 = pp0;
            if (Double.isNaN(pp0) || Double.isNaN(th0[i])
                    || Double.isNaN(sh0[i])
                    || sh0[i] < 0.0005) {
                tec = tvc = pc = Double.NaN;
            } else {
                double t0 = th0[i] * pow(pp0 / 1000, kapa);
                double b = c0 - log(pp0 / (622.0 / sh0[i] + 0.378));
                double td = (b - sqrt(b * b - c_1)) / c_2;
                double tdc = td - (t0 - td)
                        * (-0.37329638 + 41.178204 / t0 + 0.0015945203 * td);
                pc = pp0 * pow(tdc / t0, kapa_1);
                tec = adiabatic_te(tdc, pc);
                tvc = td * (1 + usetv * 0.000608 * sh0[i]);
            }

            // Initialize md and pmd, which will be pressure of and max Te
            // delta.
            double md = 0;
            double pmd = 0;

            // Now calculate the virtual temperature of the parcel at the
            // pressures in the input data. Then difference it from the
            // environmental temp, which has been tweaked to not be cooler than
            // dry adiabatic from the parcel start. Record the level of max
            // parcel difference.
            double[] tvp = new double[nz];

            for (int k = 0; k < nz; k += 1) {
                float pp = p_dat[k * n2 + i];
                float tve = tve_dat[k * n2 + i];
                if (Double.isNaN(pc) || Double.isNaN(pp) || Double.isNaN(tve)) {
                    tvp[k] = Double.NaN;
                } else {
                    double t0 = tvc * pow(pp / pc, kapa);
                    if (pp > pc) {
                        tvp[k] = t0;
                    } else {
                        double td = tec * pow(pp / pc, kapa);
                        tvp[k] = td = temp_of_te(td, pp);
                        if (usetv > 0) {
                            tvp[k] *= pp
                                    / (pp - exp(25.687958917 - c1 * td - c2
                                            / td));
                        }
                    }
                    if (tve < t0) {
                        tvp[k] -= t0;
                    } else {
                        tvp[k] -= tve;
                    }
                    if (pp > pc || tvp[k] < md) {
                        continue;
                    }
                    md = tvp[k];
                    pmd = pp;
                }
            }

            // This loop performs the actual cape and cin calculation. Here we
            // will reuse storage for virt temp, equiv temp, and max delta for
            // prev parcel temp, neg and pos. neg and pos are pending negative
            // and positive contributions we have not yet added into the cape
            // and cin yet.
            double neg = 0;
            double pos = 0;
            cin[i] = cap[i] = Float.NaN;

            double tvp1 = tvp[0];
            pp1 = p_dat[0];
            for (int k = 1; k < nz; k += 1) {
                float pp = p_dat[k * n2 + i];
                if (Double.isNaN(pp0)) {
                    continue;
                } else if (Double.isNaN(pp1) || Double.isNaN(tvp1)) {
                    ;
                } else if (pp >= pp1 || Double.isNaN(tvp[k])) {
                    continue;
                } else if (pp >= pp0) {
                    ;
                } else {
                    // Now we finally have the data we need for calculating
                    // the cape/cin contribution for this layer.
                    if (Double.isNaN(cap[i])) {
                        cap[i] = cin[i] = 0;
                    }
                    if (pmd == 0) {
                        continue; // No parcel delta>0, we're done.
                    }

                    // First deal with possibility of bottom lvl being below the
                    // initial parcel.
                    double dlnp;
                    double dn;
                    if (pp1 > pp0) {
                        dlnp = log(pp0 / pp);
                        dn = 0;
                    } else {
                        dlnp = log(pp1 / pp);
                        dn = dlnp * 287 * tvp1;
                    }

                    // Now deal with the fact that not allowing superadiabatic
                    // layers means no cape below condensation pressure.
                    double up;
                    if (pp1 >= pc) {
                        if (dn > 0) {
                            dn = 0;
                        }
                        if (tvp[k] <= 0) {
                            up = dlnp * 287 * tvp[k];
                        } else if (pp >= pc) {
                            up = 0;
                        } else {
                            up = log(pc / pp) * 287 * tvp[k];
                        }
                    } else {
                        up = dlnp * 287 * tvp[k];
                    }

                    // Deal with where the break point is.
                    double b = up * dn >= 0 ? 0.5 : up / (up - dn);
                    up *= b;
                    dn *= (1 - b);

                    // Now consider this layer's contribution, taking into
                    // account transitions between positive and negative
                    // acceleration.
                    if (up == 0 && dn == 0) {
                        ;
                        // Continuing deceleration.
                    } else if (up <= 0
                            && (dn < 0 || dn == 0 && (pp < pmd || pos == 0))) {
                        neg -= up + dn;

                        // Continuing upward acceleration.
                    } else if (up >= 0
                            && (dn > 0 || dn == 0 && (pp < pmd || neg == 0))) {
                        pos += up + dn;
                        if (pp > pmd && cap[i] + pos <= cin[i] + neg) {
                            ; // no net cape and below max delta
                        } else if (pp > pmd || cap[i] == 0) {
                            // below max delta or cape uninitialized
                            cap[i] += pos;
                            cin[i] += neg;
                            neg = pos = 0;
                        } else if (pos >= neg) {
                            // cape initialized and net positive contribution
                            cap[i] += (pos - neg);
                            neg = pos = 0;
                        }
                    } else if (up > 0 && dn <= 0) {
                        // Transition to upward acceleration.
                        neg += -dn;
                        if (pp1 <= pmd) {
                            // above max delta, only use net pos contribution
                            pos += up;
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                        } else if (pp <= pmd) {
                            // straddle max delta, force cape initialization
                            if (cap[i] == 0) {
                                cin[i] += neg;
                                cap[i] += pos;
                            } else if (neg > pos) {
                                cin[i] += neg - pos;
                            } else {
                                cap[i] += pos - neg;
                            }
                            cap[i] += up;
                            neg = pos = 0;
                        } else if (cap[i] + pos + up <= cin[i] + neg) {
                            // no net cape to this point
                            if (cap[i] + pos > 0) {
                                // reinitialize if there was cape before
                                cin[i] -= cap[i] + pos;
                                pos = cap[i] = 0;
                            }
                            cin[i] += neg;
                            pos += up;
                            neg = 0;
                        } else if (cap[i] == 0) { // initialize cape
                            cap[i] += pos + up;
                            cin[i] += neg;
                            neg = pos = 0;
                        } else { // what remains, only use net pos contribution
                            pos += up;
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                        }
                    } else {
                        // Transition to decceleration.
                        pos += dn;
                        if (pp1 <= pmd) {
                            // above max delta, only use net pos contribution
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                            neg += -up;
                        } else if (cap[i] + pos <= cin[i] + neg - up) {
                            // no net cape to this point
                            if (cap[i] > 0) {
                                // reinitialize if there was cape before
                                cin[i] -= cap[i] + pos;
                                pos = cap[i] = 0;
                            }
                            cin[i] += neg - up;
                            pos = neg = 0;
                        } else if (cap[i] == 0) { // initialize cape
                            cap[i] += pos;
                            cin[i] += neg - up;
                            neg = pos = 0;
                        } else { // what remains, only use net pos contribution
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                            neg += -up;
                        }
                    }
                }
                // Make current layer top next layer bottom.
                tvp1 = tvp[k];
                pp1 = pp;
            }
        }
        return new CapeCinPair(ny, nx, cap, cin);
    }

    // In this version we stop the computation at some arbitrary upper level,
    // ptop.
    public static CapeCinPair capeFuncTop(float usetv, float[] p_dat,
            float[] tve_dat, float[] p0, float[] th0, float[] sh0,
            float[] ptop, int nx, int ny, int nz) {
        int n2 = nx * ny;

        float[] cin = new float[n2];
        float[] cap = new float[n2];

        // Calculate the parcel equivalent temp, virtual temp, and press at LCL.
        // Make working copy of sfc press, use as press below current 3d
        // pressure.
        for (int i = 0; i < n2; i += 1) {
            double tec = 0;
            double tvc = 0;
            double pc = 0;
            double pp0 = p0[i];
            double pp1 = pp0;
            double pfin = ptop[i];
            if (Double.isNaN(pp0) || Double.isNaN(th0[i])
                    || Double.isNaN(sh0[i]) || sh0[i] < 0.0005
                    || pp0 < pfin) {
                tec = tvc = pc = Double.NaN;
            } else {
                double t0 = th0[i] * pow(pp0 / 1000, kapa);
                double b = c0 - log(pp0 / (622.0 / sh0[i] + 0.378));
                double td = (b - sqrt(b * b - c_1)) / c_2;
                double tdc = td - (t0 - td)
                        * (-0.37329638 + 41.178204 / t0 + 0.0015945203 * td);
                pc = pp0 * pow(tdc / t0, kapa_1);
                tec = adiabatic_te(tdc, pc);
                tvc = td * (1 + usetv * 0.000608 * sh0[i]);
            }

            // Initialize md and pmd, which will be pressure of and max Te
            // delta.
            double md = 0;
            double pmd = 0;

            // Now calculate the virtual temperature of the parcel at the
            // pressures in the input data. Then difference it from the
            // environmental temp, which has been tweaked to not be cooler than
            // dry adiabatic from the parcel start. Record the level of max
            // parcel difference.
            double[] tvp = new double[nz];

            for (int k = 0; k < nz; k += 1) {
                float pp = p_dat[k * n2 + i];
                float tve = tve_dat[k * n2 + i];
                if (Double.isNaN(pc) || Double.isNaN(pp) || Double.isNaN(tve)
                        || pp1 <= pfin) {
                    tvp[k] = Double.NaN;
                } else {
                    pp1 = pp;
                    double t0 = tvc * pow(pp / pc, kapa);
                    if (pp > pc) {
                        tvp[k] = t0;
                    } else {
                        double td = tec * pow(pp / pc, kapa);
                        tvp[k] = td = temp_of_te(td, pp);
                        if (usetv > 0) {
                            tvp[k] *= pp
                                    / (pp - exp(25.687958917 - c1 * td - c2
                                            / td));
                        }
                    }
                    if (tve < t0) {
                        tvp[k] -= t0;
                    } else {
                        tvp[k] -= tve;
                    }
                    if (pp > pc || pp < pfin || tvp[k] < md) {
                        continue;
                    }
                    md = tvp[k];
                    pmd = pp;
                }
            }

            // This loop performs the actual cape and cin calculation. Here we
            // will reuse storage for virt temp, equiv temp, and max delta for
            // prev parcel temp, neg and pos. neg and pos are pending negative
            // and positive contributions we have not yet added into the cape
            // and cin yet.
            double neg = 0;
            double pos = 0;
            cin[i] = cap[i] = Float.NaN;

            double tvp1 = tvp[0];
            pp1 = p_dat[i];
            for (int k = 1; k < nz; k += 1) {
                float pp = p_dat[k * n2 + i];
                if (Double.isNaN(pp0)) {
                    continue;
                } else if (Double.isNaN(pp1) || Double.isNaN(tvp1)) {
                    ;
                } else if (pp >= pp1 || Double.isNaN(tvp[k])) {
                    continue;
                } else if (pp >= pp0) {
                    ;
                } else {
                    // Now we finally have the data we need for calculating
                    // the cape/cin contribution for this layer.
                    if (Double.isNaN(cap[i])) {
                        cap[i] = cin[i] = 0;
                    }
                    if (pmd == 0) {
                        continue; // No parcel delta>0, we're done.
                    }

                    // First deal with possibility of bottom lvl being below the
                    // initial parcel and/or hitting the top of the computation.
                    double dlnp = 0;
                    double dn;
                    if (pp < pfin) {
                        double b = log(pp1 / pp);
                        dlnp = log(pp1 / pfin);
                        tvp[k] = tvp1 + (dlnp / b) * (tvp[k] - tvp1);
                    }
                    if (pp1 > pp0) {
                        if (pp < pfin) {
                            dlnp = log(pp0 / pfin);
                        } else {
                            dlnp = log(pp0 / pp);
                        }
                        dn = 0;
                    } else {
                        if (pp >= pfin) {
                            dlnp = log(pp1 / pp);
                        }
                        dn = dlnp * 287 * tvp1;
                    }

                    // Now deal with the fact that not allowing superadiabatic
                    // layers means no cape below condensation pressure.
                    double up;
                    if (pp1 >= pc) {
                        if (dn > 0) {
                            dn = 0;
                        }
                        if (tvp[k] <= 0) {
                            up = dlnp * 287 * tvp[k];
                        } else if (pp >= pc) {
                            up = 0;
                        } else if (pp < pfin) {
                            up = log(pc / pfin) * 287 * tvp[k];
                        } else {
                            up = log(pc / pp) * 287 * tvp[k];
                        }
                    } else {
                        up = dlnp * 287 * tvp[k];
                    }

                    // Deal with where the break point is.
                    double b = up * dn >= 0 ? 0.5 : up / (up - dn);
                    up *= b;
                    dn *= (1 - b);

                    // Now consider this layer's contribution, taking into
                    // account transitions between positive and negative
                    // acceleration.
                    if (up == 0 && dn == 0) {
                        ;
                        // Continuing deceleration.
                    } else if (up <= 0
                            && (dn < 0 || dn == 0 && (pp < pmd || pos == 0))) {
                        neg -= up + dn;

                        // Continuing upward acceleration.
                    } else if (up >= 0
                            && (dn > 0 || dn == 0 && (pp < pmd || neg == 0))) {
                        pos += up + dn;
                        if (pp > pmd && cap[i] + pos <= cin[i] + neg) {
                            ; // no net cape and below max delta
                        } else if (pp > pmd || cap[i] == 0) {
                            // below max delta or cape uninitialized
                            cap[i] += pos;
                            cin[i] += neg;
                            neg = pos = 0;
                        } else if (pos >= neg) {
                            // cape initialized and net positive contribution
                            cap[i] += (pos - neg);
                            neg = pos = 0;
                        }
                    } else if (up > 0 && dn <= 0) {
                        // Transition to upward acceleration.
                        neg += -dn;
                        if (pp1 <= pmd) {
                            // above max delta, only use net pos contribution
                            pos += up;
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                        } else if (pp <= pmd) {
                            // straddle max delta, force cape initialization
                            if (cap[i] == 0) {
                                cin[i] += neg;
                                cap[i] += pos;
                            } else if (neg > pos) {
                                cin[i] += neg - pos;
                            } else {
                                cap[i] += pos - neg;
                            }
                            cap[i] += up;
                            neg = pos = 0;
                        } else if (cap[i] + pos + up <= cin[i] + neg) {
                            // no net cape to this point
                            if (cap[i] + pos > 0) {
                                // reinitialize if there was cape before
                                cin[i] -= cap[i] + pos;
                                pos = cap[i] = 0;
                            }
                            cin[i] += neg;
                            pos += up;
                            neg = 0;
                        } else if (cap[i] == 0) { // initialize cape
                            cap[i] += pos + up;
                            cin[i] += neg;
                            neg = pos = 0;
                        } else { // what remains, only use net pos contribution
                            pos += up;
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                        }
                    } else {
                        // Transition to decceleration.
                        pos += dn;
                        if (pp1 <= pmd) {
                            // above max delta, only use net pos contribution
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                            neg += -up;
                        } else if (cap[i] + pos <= cin[i] + neg - up) {
                            // no net cape to this point
                            if (cap[i] > 0) {
                                // reinitialize if there was cape before
                                cin[i] -= cap[i] + pos;
                                pos = cap[i] = 0;
                            }
                            cin[i] += neg - up;
                            pos = neg = 0;
                        } else if (cap[i] == 0) { // initialize cape
                            cap[i] += pos;
                            cin[i] += neg - up;
                            neg = pos = 0;
                        } else { // what remains, only use net pos contribution
                            if (pos >= neg) {
                                cap[i] += (pos - neg);
                                neg = pos = 0;
                            }
                            neg += -up;
                        }
                    }
                }
                // Make current layer top next layer bottom.
                tvp1 = tvp[k];
                pp1 = pp;
            }
        }
        return new CapeCinPair(ny, nx, cap, cin);
    }

    public static class CapeCinPair implements INumpyable {

        private final int nx;

        private final int ny;

        private final float[] cape;

        private final float[] cin;

        public CapeCinPair(int nx, int ny, float[] cape, float[] cin) {
            this.nx = nx;
            this.ny = ny;
            this.cape = cape;
            this.cin = cin;
        }

        @Override
        public Object[] getNumPy() {
            return new Object[] { cape, cin };
        }

        @Override
        public int getNumpyX() {
            return nx;
        }

        @Override
        public int getNumpyY() {
            return ny;
        }

    }

}
