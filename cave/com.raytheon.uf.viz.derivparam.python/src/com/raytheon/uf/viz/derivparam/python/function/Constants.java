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

/**
 * Consolidated constants from various meteolib functions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 06, 2013  2043       bsteffen    Ported from meteolib C
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class Constants {

    // from meteoLib capeFunc.c
    public static final double c0 = 26.66082;

    // from meteoLib capeFunc.c
    public static final double c1 = 0.0091379024;

    // from meteoLib capeFunc.c
    public static final double c2 = 6106.396;

    // from meteoLib capeFunc.c
    public static final double c_1 = 223.1986;

    // from meteoLib capeFunc.c
    public static final double c_2 = 0.0182758048;

    // from meteoLib capeFunc.c
    public static final double kapa = 0.286;

    // from meteoLib capeFunc.c
    public static final double kapa_1 = 3.498257;

    // from meteoLib calctw.f
    public static final double f = 0.0006355;
}
