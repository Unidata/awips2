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

/**
 * Consolidated constants from various meteolib functions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 06, 2013  2043     bsteffen    Ported from meteolib C
 * Aug 13, 2013  2262     njensen     Moved from deriv params
 * Aug 13, 2013  2262     dgilling    Adding additional constants from
 *                                    calcrh.f, hgt2pres.f, ztopsa.f, ptozsa.f
 * Feb 27, 2014  2791     bsteffen    Move commonly used legacy NaN here.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class Constants {

    // from meteoLib calcrh.f
    public static final double b = 0.0091379024;

    // from meteoLib calcrh.f
    public static final double c = 6106.396;

    // from meteoLib capeFunc.c
    public static final double c0 = 26.66082;

    // from meteoLib capeFunc.c
    public static final double c1 = b;

    // from meteoLib capeFunc.c
    public static final double c2 = c;

    // from meteoLib capeFunc.c
    public static final double c_1 = 223.1986;

    // from meteoLib capeFunc.c
    public static final double c_2 = 0.0182758048;

    // from meteoLib calcrh.f
    public static final double k0 = 273.15;

    // from meteoLib capeFunc.c
    public static final double kapa = 0.286;

    // from meteoLib capeFunc.c
    public static final double kapa_1 = 3.498257;

    // from meteoLib calctw.f
    public static final double f = 0.0006355;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double T0 = 288.0;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double gamma = 0.0065;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double p0 = 1013.2;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double p11 = 226.0971;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double z11 = 11000.0;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double HGT_PRES_c1 = 5.256;

    // from meteoLib hgt2pres.f, ztopsa.f, ptozsa.f
    public static final double HGT_PRES_c2 = 14600.0;

    public static final float LEGACY_NAN = 1e37f;

    // Never allow this class to be directly instantiated
    private Constants() {
        throw new AssertionError();
    }
}
