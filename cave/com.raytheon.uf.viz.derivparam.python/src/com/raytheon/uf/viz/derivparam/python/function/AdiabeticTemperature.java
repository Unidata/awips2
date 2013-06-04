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

import static java.lang.Math.exp;

/**
 * This routine calculates the equivalent tempurature of a temperature and
 * pressure using the adiabatic definition, assuming saturation put a fudge
 * factor into L/cp to get agreement of moist adiabats with a published
 * thermodynamic diagram *
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

public class AdiabeticTemperature {

    public static double adiabatic_te(double temp, double press) {
        double e = exp(26.660820 - 0.0091379024 * temp - 6106.396 / temp);
        e = 0.622 * e / (press - e);
        return temp * exp(2740.0 * e / temp);
    }
}
