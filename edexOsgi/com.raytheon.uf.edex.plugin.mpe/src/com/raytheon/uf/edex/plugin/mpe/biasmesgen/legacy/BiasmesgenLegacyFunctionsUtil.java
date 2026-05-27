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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen.legacy;

import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Nearly direct ports of legacy functions in Biasmesgen. Reserved for methods
 * that perform activities so specialized and/or specific to MPE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2016 5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class BiasmesgenLegacyFunctionsUtil {

    protected BiasmesgenLegacyFunctionsUtil() {
    }

    /**
     * Calculates and returns the Bias Julian Date based on the specified
     * date/time. This is not a standard Julian Date algorithm. This is an
     * algorithm specific to how the Julian Date is recorded in a Bias Table -
     * the number of days since the epoch. Originally based on:
     * biasmesgen/TEXT/modif_julian.c
     * 
     * @param generationDateTime
     *            the specified date/time
     * @return the calculated bias Julian Date
     */
    public static int calculateBiasJulianDate(Calendar generationDateTime) {
        return (int) (generationDateTime.getTimeInMillis() / TimeUtil.MILLIS_PER_DAY) + 1;
    }

    /**
     * Calculates the number of seconds that have passed since midnight for the
     * specified date/time.
     * 
     * @param generationDateTime
     *            the specified date/time
     * @return the calculated number of seconds.
     */
    public static int biasSecondsAfterMidnight(Calendar generationDateTime) {
        final int hour = generationDateTime.get(Calendar.HOUR_OF_DAY);
        final int minute = generationDateTime.get(Calendar.MINUTE);
        final int second = generationDateTime.get(Calendar.SECOND);

        return (hour * TimeUtil.SECONDS_PER_HOUR)
                + (minute * TimeUtil.SECONDS_PER_MINUTE) + second;
    }

    /**
     * This function scales a real value by 1000 and converts to the nearest
     * integer. Based on: biasmesgen/TEXT/create_biastable_mesg.pgc - this
     * method has the same name as the legacy function.
     * 
     * @param value
     *            the value to convert
     * @return the converted value
     */
    public static int cnvrt1000(final double value) {
        return (int) (1000.0 * value + 0.5);
    }
}