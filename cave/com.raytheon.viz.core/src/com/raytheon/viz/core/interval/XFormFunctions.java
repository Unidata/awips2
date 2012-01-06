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

package com.raytheon.viz.core.interval;

import java.util.Scanner;

/**
 * Ported from D2D. Graph specific methods have been moved to GraphUtilPorted.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class XFormFunctions {

    // Resolve a logarithmic interval to either a multiple of one or the
    // inverse of a multiple of one. If isMin is true will return an
    // interval at least as great as interval, otherwise will return the
    // nearest value.
    public static float usableLogInterval(float interval, boolean isMin) {
        if (interval <= 0)
            return 1;
        if (interval == (int) interval)
            return interval;
        if (!isMin) {
            if (interval >= 1)
                return (int) (interval + 0.5);
            return (float) (1.0 / (int) (0.5 + 1 / interval));
        }
        if (interval < 1)
            return (float) (1.0 / (int) (1 / interval));
        return (int) (interval - 10000) + 10000;
    }

    public static float nextLogInterval(float interval) {
        if (interval <= 0)
            return 1;
        if (interval < 2 && interval >= 1)
            return 2;
        if (interval < 1 && interval >= 0.5)
            return 1;
        if (interval > 1)
            return 1 + (int) (interval);
        return (float) (1.0 / (int) (1 / interval - 0.999));
    }

    // -- global
    // -----------------------------------------------------------------
    // float newDataIntervalFromZoom(float defaultInterval,
    // float dataZoom,
    // bool specZoom = false,
    // char * labelFmt = 0,
    // int perDecade)
    //
    // Calculates a new contour or other interval for depicting data
    // based on a default interval and a zoom. If "specZoom" is true, then
    // certain intervals (like 30, typically used for heights) will be treated
    // differently. If something is actually passed in for "labelFmt", then
    // then that labeling format will be taken into consideration for
    // calculating the interval.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    public static float newDataIntervalFromZoom(float defaultInterval,
            float dataZoom, boolean specZoom, String labelFmt, int perDecade) {

        // Integerize the dataZoom. iZoom is for special
        // handling of 2x and 4x zoom and unzoom.
        int iZoom = 0;
        if (specZoom) {
            if (dataZoom > 1.5) {
                if (dataZoom > 4.5)
                    iZoom = 0;
                else if (dataZoom > 3)
                    iZoom = 4;
                else
                    iZoom = 2;
                dataZoom = (int) (0.5 + dataZoom);
            } else if (dataZoom < 0.75) {
                dataZoom = (float) (1.0 / dataZoom);
                if (dataZoom > 4.5)
                    iZoom = 0;
                else if (dataZoom > 3)
                    iZoom = -4;
                else
                    iZoom = -2;
                dataZoom = (int) (0.5 + dataZoom);
                dataZoom = (float) (1.0 / dataZoom);
            } else
                return defaultInterval;
        }// end if (specZoom)

        // see if we are using integer labels.
        boolean intLabels = false;
        if (labelFmt != null
                && (labelFmt.indexOf('i') > -1 || labelFmt.indexOf('I') > -1))
            intLabels = true;

        // find mantissa and exponent of scaled contour interval, check if
        // integer
        // labels mean we must use a contour interval of one.
        float temp = defaultInterval / dataZoom;
        if (intLabels && temp < 1.0)
            return 1.0f;
        float e = (float) Math.pow(10, (int) (Math.log10(temp) + 10.0) - 10);
        float m = temp / e;
        if (m > 9.99) {
            m = 1;
            e *= 10;
        }

        // find precision of contour labels
        int dec = -1;
        if (labelFmt != null)
            dec = labelFmt.indexOf('.');
        if (dec > -1) {
            int iprec;
            if (dec + 1 < labelFmt.length()) {
                Scanner scanner = new Scanner(labelFmt.substring(++dec));
                iprec = scanner.nextInt();
            } else {
                iprec = 0;
            }
            // if (sscanf(++dec,"%d",&iprec)!=1) iprec = 0;
            if (iprec != 1) {
                iprec = 0;
            }
            if (iprec < 0)
                iprec = 0;
            if (iprec > 4)
                iprec = 4;
            float fprec = (float) Math.pow(10, -iprec);
            if (temp < fprec)
                return fprec;
            if (temp == fprec)
                intLabels = true;
        }

        // special handling of 2x and 4x zoom and unzoom for integral mantissa
        // of
        // default contour intervals
        if (iZoom != 0) {

            // find mantissa and exponent of default contour interval, check if
            // mantissa is integral
            float tempd = defaultInterval;
            float ed = (float) Math.pow(10,
                    (int) (Math.log10(tempd) + 10.0) - 10);
            float md = tempd / ed;
            if (md > 9.99) {
                md = 1;
                ed *= 10;
            }
            float d = md - (int) md;
            if (d < 0)
                d = -d;
            if (d < 0.01)
                md = (int) md;
            if (md > 1.199 && md < 1.201)
                md = 1.2f;
            if (md > 1.499 && md < 1.501)
                md = 1.5f;
            if (md > 2.499 && md < 2.501)
                md = 2.5f;
            tempd = md * ed;

            if (md == 1) {
                if (iZoom == 2 && !intLabels)
                    return (float) (.5 * ed);
                if (iZoom == -2)
                    return 2 * ed;
                if (iZoom == -4)
                    return 4 * ed;
            } else if (md == 1.2) {
                if (iZoom == 4)
                    return 3 * ed;
                if (iZoom == 2)
                    return 6 * ed;
                if (iZoom == -2 && !intLabels)
                    return (float) (2.4 * ed);
            } else if (md == 1.5) {
                if (iZoom == -2)
                    return 3 * ed;
                if (iZoom == -4)
                    return 6 * ed;
            } else if (md == 2.5) {
                if (iZoom == 2)
                    return ed;
            } else if (md == 2) {
                if (iZoom == -2)
                    return 4 * ed;
                if (iZoom == -4)
                    return 8 * ed;
            } else if (md == 3) {
                if (iZoom == 2 && !intLabels)
                    return (float) (1.5 * ed);
                if (iZoom == -2)
                    return 6 * ed;
                if (iZoom == -4)
                    return 12 * ed;
            } else if (md == 4) {
                if (iZoom == -2)
                    return 8 * ed;
                if (iZoom == -4)
                    return 16 * ed;
            } else if (md == 5) {
                if (iZoom == 2 && !intLabels)
                    return (float) (2.5 * ed);
            } else if (md == 6) {
                if (iZoom == 4 && !intLabels)
                    return (float) (1.5 * ed);
                if (iZoom == 2)
                    return 3 * ed;
                if (iZoom == -2)
                    return 12 * ed;
                if (iZoom == -4)
                    return 24 * ed;
            } else if (md == 7) {
                if (iZoom == 2 && !intLabels)
                    return (float) (3.5 * ed);
                if (iZoom == -2)
                    return 14 * ed;
            } else if (md == 8) {
                if (iZoom == 2)
                    return 4 * ed;
                if (iZoom == -2)
                    return 16 * ed;
                if (iZoom == -4)
                    return 32 * ed;
            } else if (md == 9) {
                if (iZoom == 2 && !intLabels)
                    return (float) (4.5 * ed);
                if (iZoom == -2)
                    return 18 * ed;
            }
        }

        // All other intervals have specific mantissa based on number of values
        // per decade.
        switch (perDecade) {
        case 0:
        case 1:
            m = m <= 3 ? 1 : 10;
            break;
        case 2:
            m = m < 2 ? 1 : (m < 7 ? 3 : 10);
            break;
        case 3:
        case 4:
            m = m <= 3 ? (m < 1.5 ? 1 : 2) : (m < 7.5 ? 5 : 10);
            break;
        case 5:
        case 6:
        case 7:
            if (m >= 4)
                m = m < 6 ? 5 : (m < 8.5 ? 7 : 10);
            else if (intLabels)
                m = (int) (m + 0.5);
            else
                m = (float) (m < 1.75 ? (m < 1.25 ? 1 : 1.5)
                        : (m < 2.5 ? 2 : 3));
            break;
        default:
            if (m >= 9.5)
                m = 10;
            else if (m >= 5.5)
                m = m < 7.5 ? (m < 6.5 ? 6 : 7) : (m < 8.5 ? 8 : 9);
            else if (intLabels)
                m = (int) (m + 0.5);
            else if (m >= 2.25)
                m = (float) (m < 3.5 ? (m < 2.75 ? 2.5 : 3) : (m < 4.5 ? 4 : 5));
            else
                m = (float) (m < 1.35 ? (m < 1.1 ? 1 : 1.2) : (m < 1.75 ? 1.5
                        : 2));
        }

        return m * e;

    }
}
