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
package com.raytheon.viz.core.graphing.axis;

import java.text.DecimalFormat;
import java.util.HashMap;

import com.raytheon.viz.core.graphing.DataAxisInfo;
import com.raytheon.viz.core.graphing.util.GraphUtilPorted;
import com.raytheon.viz.core.style.graph.AxisScale;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class AxisUtil {

    // d2d is flawed with its formatting for axis labels so I'm altering
    // it to use these (see makeYaxis method)
    private static final DecimalFormat SUPER_SMALL_FORMAT = new DecimalFormat(
            "0.0000");

    private static final DecimalFormat SMALL_FORMAT = new DecimalFormat("0.000");

    private static final DecimalFormat MEDIUM_FORMAT = new DecimalFormat("0.00");

    private static final DecimalFormat LARGE_FORMAT = new DecimalFormat("0.0");

    // -- protected
    // --------------------------------------------------------------
    // void GridTSDepict::makeYaxis()
    //
    // Draws a y-axis on the time series based on the DataAxisInfo.
    // ---------------------------------------------------------------------------

    /**
     * Determines the format of the axis labels and the data sampling for a
     * graph. Ported from GridTSDepict's makeYaxis() method.
     * 
     * @param dataAxisInfo
     *            the info of the axis
     * @param nPanels
     *            somehow related to zoom // TODO better desc
     * @return the labeling for the axis
     */
    public static AxisLabeling makeYaxis(DataAxisInfo dataAxisInfo,
            int nPanels, boolean zooming) {
        if (dataAxisInfo == null)
            return null;

        String fmt = null;
        HashMap<Double, String> labels = new HashMap<Double, String>();

        // anything with the formatter is not ported, it's a custom improvement
        // added for awips-2
        DecimalFormat formatter = null;
        if (zooming) {
            double axisRange = dataAxisInfo.getDivMax()
                    - dataAxisInfo.getDivMin();
            if (dataAxisInfo.getStyle().getAxisScale().getScaleType() == AxisScale.Type.LOG) {
                if (axisRange <= .1) {
                    formatter = SUPER_SMALL_FORMAT;
                } else {
                    formatter = SMALL_FORMAT;
                }
            } else {
                if (axisRange <= .05) {
                    formatter = SUPER_SMALL_FORMAT;
                } else if (axisRange <= .5) {
                    formatter = SMALL_FORMAT;
                } else if (axisRange <= 10) {
                    formatter = MEDIUM_FORMAT;
                } else {
                    formatter = LARGE_FORMAT;
                }
            }
        }

        // Label the y axis
        // _painter->setPaintArea(CartDomain2D<float>());
        // _painter->setOffset(Painter::CENTER_RIGHT);
        float val;
        String wrkstr;
        float dd = 1;
        if (nPanels > 5)
            dd = 5;
        else if (nPanels > 2)
            dd = 2;
        float dend = GraphUtilPorted.N_GRAPH_DATA_DIVS + .1f;
        int eesamp = 99999;
        if (dataAxisInfo.getStyle().getAxisScale().getScaleType() != AxisScale.Type.LOG) {
            val = dataAxisInfo.getDivMax();
            if (-dataAxisInfo.getDivMin() > val)
                val = -dataAxisInfo.getDivMin();
            int eem = (int) (Math.log10(val) + 100) - 100;
            if (val / Math.pow(10.0, eem) > 9.5)
                eem++;
            else if (val / Math.pow(10.0, eem) < 0.95)
                eem--;
            int eei = (int) (Math.log10(dataAxisInfo.getInterval()) + 100) - 100;
            if (dataAxisInfo.getInterval() / Math.pow(10.0, eei) > 9.5)
                eei++;
            else if (dataAxisInfo.getInterval() / Math.pow(10.0, eei) < 0.95)
                eei--;
            float p = (float) (dataAxisInfo.getInterval() / Math.pow(10.0, eei));
            p = p - (int) p;
            if (p > 0.01 || p < -0.01)
                eei--;
            p = (float) Math.pow(10.0, eei - 1);
            float prec = 1.0f;
            if (eei == -3 && eem <= 1)
                fmt = "%.3f";
            else if (eei == -2 && eem <= 2)
                fmt = "%.2f";
            else if (eei == -1 && eem <= 3)
                fmt = "%.1f";
            else if (eem <= 5 && eem >= 0)
                fmt = "%.0f";
            else {
                prec = (float) Math.pow(10.0, eei);
                if (eem == eei)
                    fmt = "%.0f";
                else if (eem - eei == 1)
                    fmt = "%.1f";
                else if (eem - eei == 2)
                    fmt = "%.2f";
                else
                    fmt = "%.3f";
                // char cp = fmt+strlen(fmt);
                // sprintf(cp,"e%d",eei);
                eesamp = -99999;
            }
            int ee;
            for (float d = 0; d <= dend; d += dd) {
                val = GraphUtilPorted.valueOfAxisDivision(d, dataAxisInfo);
                // CartCoord2D<float> axisPt(_zoomedDataDomain.left(), d);
                // axisPt = _xform->forward(axisPt);
                if (val < p && val > -p) {
                    wrkstr = "0";
                } else if (prec == 1) {
                    wrkstr = String.format(fmt, val);
                    if (zooming) {
                        wrkstr = formatter.format(val);
                    }
                } else {
                    wrkstr = String.format(fmt, val / prec);
                    if (zooming) {
                        wrkstr = formatter.format(val / prec);
                    }
                }

                if (wrkstr.startsWith("0.") || wrkstr.startsWith("-0."))
                    wrkstr = wrkstr.replaceFirst("0", "");
                // if (strstr(wrkstr,"0.")==wrkstr)
                // strcpy(wrkstr,wrkstr+1);
                // else if (strstr(wrkstr,"-0.")==wrkstr)
                // strcpy(wrkstr+1,wrkstr+2);
                // _painter->makeText(wrkstr,axisPt);
                // labels.add(wrkstr);
                labels.put((double) val, wrkstr);
                if (eesamp == -99999)
                    continue;
                int cp = wrkstr.indexOf('.');
                // char * ep = wrkstr+(strlen(wrkstr)-1);
                // char ep = wrkstr.charAt(wrkstr.length() - 1);
                int ep = wrkstr.length() - 1;
                while (cp > -1 && wrkstr.charAt(ep) == '0')
                    ep--;
                if (cp > -1)
                    ee = cp - ep;
                else if (ep == '0')
                    ee = 1;
                else
                    ee = 0;
                if (ee < eesamp)
                    eesamp = ee;
            }
        } else {
            float m, vvv;
            int ee;
            for (float d = 0; d <= dend; d += dd) {
                val = GraphUtilPorted.valueOfAxisDivision(d, dataAxisInfo);
                // fprintf(stderr,"%d %f\n",i,val);
                // CartCoord2D<float> axisPt(_zoomedDataDomain.left(), d);
                // fprintf(stderr,"raw: %f %f ",axisPt.x,axisPt.y);
                // axisPt = _xform->forward(axisPt);
                // fprintf(stderr,"xformed: %f %f\n",axisPt.x,axisPt.y);
                if (val == 0) {
                    // _painter->makeText("0",axisPt);
                    continue;
                }
                ee = (int) (Math.log10(val > 0 ? val : -val) + 100) - 100;
                m = (float) (val / Math.pow(10.0, ee));
                if (val < 0)
                    m = -m;
                if (m > 9.95) {
                    m = 1.0f;
                    ee++;
                } else if (m < 1.05)
                    m = 1.0f;
                else
                    m = ((int) (10 * m + 0.5)) / 10.0f;
                if (val < 0)
                    m = -m;
                vvv = (float) (m * Math.pow(10.0, ee));
                if (ee > 4 || ee < -2) {
                    fmt = "%.1fe%d";
                    wrkstr = String.format(fmt, m, ee);
                    // sprintf(wrkstr,"%.1fe%d",m,ee);
                } else if (ee > 0) {
                    fmt = "%.0f";
                    wrkstr = String.format(fmt, vvv);
                    if (zooming) {
                        wrkstr = formatter.format(val);
                    }
                    // sprintf(wrkstr,"%.0f",vvv);
                } else if (ee == 0) {
                    fmt = "%.1f";
                    wrkstr = String.format(fmt, vvv);
                    if (zooming) {
                        wrkstr = formatter.format(val);
                    }
                    // sprintf(wrkstr,"%.1f",vvv);
                } else if (ee == -1) {
                    fmt = "%.2f";
                    wrkstr = String.format(fmt, vvv);
                    if (zooming) {
                        wrkstr = formatter.format(val);
                    }
                    // sprintf(wrkstr,"%.2f",vvv);
                } else {
                    fmt = "%.3f";
                    wrkstr = String.format(fmt, vvv);
                    if (zooming) {
                        wrkstr = formatter.format(val);
                    }
                    // sprintf(wrkstr,"%.3f",vvv);
                }
                // labels.add(wrkstr);
                labels.put((double) val, wrkstr);
                // _painter->makeText(wrkstr,axisPt);
                if (ee > 6 || ee < -2)
                    eesamp = -99999;
                if (eesamp == -99999)
                    continue;
                if (ee == 5 || ee == 6) {
                    fmt = "%.0f";
                    wrkstr = String.format(fmt, vvv);
                    // sprintf(wrkstr,"%.0f",vvv);
                }
                int cp = wrkstr.indexOf('.');
                // char ep = wrkstr.charAt(wrkstr.length() - 1);
                // int ep = Integer.valueOf(wrkstr.charAt(wrkstr.length() - 1));
                int ep = wrkstr.length() - 1;
                while (cp > -1 && wrkstr.charAt(ep) == '0')
                    ep--;
                if (cp > -1)
                    ee = cp - ep;
                else if (ep == '0')
                    ee = 1;
                else
                    ee = 0;
                if (ee < eesamp)
                    eesamp = ee;
            }
        }
        // _painter->flush();

        // Based on our minimum axis label precision, create the sampling
        // format.
        String smpFmt = null;
        if (eesamp == -99999)
            smpFmt = "%.2g";
        else if (eesamp >= 1)
            smpFmt = "%.0f";
        else if (eesamp == 0)
            smpFmt = "%.1f";
        else if (eesamp == -1)
            smpFmt = "%.2f";
        else if (eesamp == -2)
            smpFmt = "%.3f";
        else if (eesamp == -3)
            smpFmt = "%.4f";
        else
            smpFmt = "%.2g";

        AxisLabeling labeling = new AxisLabeling();
        labeling.setSampleFormat(smpFmt);
        labeling.setLabels(labels);
        return labeling;
    }

}
