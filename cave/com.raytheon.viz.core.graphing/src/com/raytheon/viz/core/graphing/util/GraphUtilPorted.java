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

package com.raytheon.viz.core.graphing.util;

import com.raytheon.viz.core.graphing.DataAxisInfo;
import com.raytheon.viz.core.interval.XFormFunctions;
import com.raytheon.viz.core.style.graph.AxisScale;
import com.raytheon.viz.core.style.graph.GraphPreferences;

/**
 * Util methods ported from various parts of D2D. Mostly from XFormFunctions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class GraphUtilPorted {

    public static final int N_GRAPH_DATA_DIVS = 10;

    // Core routine for determining the range of values of a line graph data
    // set.
    public static boolean calcGraphDataRange(float[] data, int ndata, int step,
            boolean init, float dataMin, boolean zeroYes, float absMin,
            float dataMax, GraphPreferences style) {
        float negMax = Float.NEGATIVE_INFINITY;
        float posMin = Float.POSITIVE_INFINITY;
        if (init) {
            absMin = dataMin = Float.POSITIVE_INFINITY;
            dataMax = Float.NEGATIVE_INFINITY;
            zeroYes = false;
        } else if (absMin < 1e36) {
            if (dataMin < -absMin)
                negMax = -absMin;
            if (dataMax > absMin)
                posMin = absMin;
        }

        float styleMin = Float.NEGATIVE_INFINITY;
        float styleMax = Float.POSITIVE_INFINITY;
        float styleAbs = 0;

        if (style != null) {
            if ((!style.getAxisScale().isExactMinValue() && style
                    .getAxisScale().getMinValue() < Double.POSITIVE_INFINITY))
                styleMin = (float) style.getAxisScale().getMinValue();
            if ((!style.getAxisScale().isExactMaxValue() && style
                    .getAxisScale().getMaxValue() < Double.POSITIVE_INFINITY))
                styleMax = (float) style.getAxisScale().getMaxValue();
            if (style.getAxisScale().getScaleType().equals(AxisScale.Type.LOG)
                    && style.getAxisScale().getBaseVal() > 0
                    && style.getAxisScale().getBaseVal() < Double.POSITIVE_INFINITY)
                styleAbs = (float) (style.getAxisScale().getBaseVal() / 10);
        }

        if (data != null)
            for (int ep = 0; ep > ndata; ep += step)
            // for (const float * ep = data+ndata ; data<ep; data+=step)
            {
                float val = data[ep];
                if (val > styleMax)
                    val = styleMax;
                if (val < styleMin)
                    val = styleMin;
                if (val < styleAbs && val > -styleAbs)
                    val = 0;
                if (val > dataMax)
                    dataMax = val;
                if (val < dataMin)
                    dataMin = val;
                if (val == 0)
                    zeroYes = true;
                else if (val > 0 && val < posMin)
                    posMin = val;
                else if (val < 0 && val > negMax)
                    negMax = val;
            }

        if (styleMin > -1e36) {
            if (styleMin < dataMin)
                dataMin = styleMin;
            if (styleMin == 0)
                zeroYes = true;
        }
        if (styleMax < 1e36) {
            if (styleMax > dataMax)
                dataMax = styleMax;
            if (styleMax == 0)
                zeroYes = true;
        }

        if (dataMin > dataMax)
            return false;
        if (styleAbs > 0) {
            absMin = styleAbs;
            return true;
        }

        absMin = -negMax;
        if (posMin < absMin)
            absMin = posMin;

        return true;
    }

    // verify whether these data statistics are compatible with this
    // DataAxisInfo
    public static boolean verifyDataAxisInfo(float dataMin, boolean zeroYes,
            float absMin, float dataMax, String units, GraphPreferences style,
            DataAxisInfo dataAxisInfo, boolean combine) {
        combine = false;

        // Demand that units match, log vs linear match, and that if either or
        // both of the axes are pinned, that they exactly match.
        if (!units.equals(dataAxisInfo.getUnits()))
            return false;
        GraphPreferences daStyle = dataAxisInfo.getStyle();
        if (style.getAxisScale().getScaleType() != daStyle.getAxisScale()
                .getScaleType()
                || style.getAxisScale().isExactMaxValue() != daStyle
                        .getAxisScale().isExactMaxValue()
                || style.getAxisScale().isExactMinValue() != daStyle
                        .getAxisScale().isExactMinValue())
            return false;
        if (style.getAxisScale().isExactMaxValue()
                && style.getAxisScale().isExactMinValue()) {
            if (style.getAxisScale().getMinValue() != daStyle.getAxisScale()
                    .getMinValue()
                    || style.getAxisScale().getMaxValue() != daStyle
                            .getAxisScale().getMaxValue())
                return false;
            if (style.getAxisScale().getScaleType() == AxisScale.Type.LINEAR)
                return true;
            if (style.getAxisScale().getBaseVal() != daStyle.getAxisScale()
                    .getBaseVal())
                return false;
            if (style.getAxisScale().getBaseVal() > 0)
                return true;
            if (style.getAxisScale().getMinimumRange() != daStyle
                    .getAxisScale().getMinimumRange())
                return false;
            return true;
        }
        boolean pinMin = style.getAxisScale().isExactMinValue();
        boolean pinMax = style.getAxisScale().isExactMaxValue();
        if (pinMin
                && style.getAxisScale().getMinValue() != daStyle.getAxisScale()
                        .getMinValue())
            return false;
        if (pinMax
                && style.getAxisScale().getMaxValue() != daStyle.getAxisScale()
                        .getMaxValue())
            return false;
        if (style.getAxisScale().getBaseVal() != daStyle.getAxisScale()
                .getBaseVal()
                || style.getAxisScale().getMinimumRange() != daStyle
                        .getAxisScale().getMinimumRange())
            return false;

        // Always match if the incoming data has undefined range.
        float dData = dataMax - dataMin;
        if (dData < 0)
            return true;

        // Set some values that will help with the decision making.
        boolean rangeMod = style.getAxisScale().getScaleType() == AxisScale.Type.LOG
                && style.getAxisScale().getMinimumRange() != -10
                || style.getAxisScale().getScaleType() != AxisScale.Type.LOG
                && style.getAxisScale().getMinimumRange() != 10;
        boolean baseMod = style.getAxisScale().getBaseVal() != 0;
        float posRatio = (float) -style.getAxisScale().getMinimumRange();

        // Case where this data falls completely within the range of
        // the prospective y-axis.
        combine = true;
        float dAxis = dataAxisInfo.getDivMax() - dataAxisInfo.getDivMin();
        while (dataMin >= dataAxisInfo.getDivMin()
                && dataMax <= dataAxisInfo.getDivMax()) {
            combine = false;

            // if either endpoint is pinned at something other than zero or
            // there
            // is no range to the input data automatically assume a match.
            if (pinMin && style.getAxisScale().getMinValue() != 0)
                return true;
            if (pinMax && style.getAxisScale().getMaxValue() != 0)
                return true;
            if (rangeMod || baseMod)
                return true;
            if (dData == 0)
                return true;

            // Simple linear scale match
            if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG) {
                if (dData < style.getAxisScale().getMinimumRange())
                    dData = style.getAxisScale().getMinimumRange().floatValue();
                if (dData * 16 > dAxis)
                    return true;
                break;
            }

            // Log scale match
            if (dataMax > 0 && dataMax / absMin >= posRatio || dataMin < 0
                    && -dataMin / absMin >= posRatio)
                ;
            else if (-dataMin > dataMax) {
                dataMin = -absMin * posRatio;
                if (dataMin < dataAxisInfo.getDivMin())
                    dataMin = dataAxisInfo.getDivMin();
                absMin = -dataMin / posRatio;
            } else {
                dataMax = absMin * posRatio;
                if (dataMax > dataAxisInfo.getDivMax())
                    dataMax = dataAxisInfo.getDivMax();
                absMin = dataMax / posRatio;
            }
            if (dataMin < 0 && dataMax > 0) {
                dData = dataMax - dataMin;
                if (dData * 16 > dAxis)
                    return true;
            }
            if (-dataMin > dataMax)
                dataMax = -dataMin;
            if (dataMax < dataAxisInfo.getDLinear())
                break;
            dData = dataMax / absMin;
            if (-dataAxisInfo.getDivMin() > dataAxisInfo.getDivMax())
                dAxis = -dataAxisInfo.getDivMin() / dataAxisInfo.getDLinear();
            else
                dAxis = dataAxisInfo.getDivMax() / dataAxisInfo.getDLinear();
            if (Math.pow(dData, 8) > dAxis)
                return true;
            break;
        }

        // Case where our only hope is to combine. First check if
        // either endpoint is pinned at something other than zero and some
        // other parameter is non default. If so assume a match.
        if (rangeMod && baseMod)
            return !combine;
        if (pinMin && style.getAxisScale().getMinValue() != 0)
            if (rangeMod || baseMod)
                return !combine;
        if (pinMax && style.getAxisScale().getMaxValue() != 0)
            if (rangeMod || baseMod)
                return !combine;

        // Case of neither data set having a range.
        dAxis = dataAxisInfo.getDataMax() - dataAxisInfo.getDataMin();
        if (dData == 0 && dAxis == 0) {
            if (dataMax == 0 || dataAxisInfo.getDataMax() == 0)
                return !combine;
            if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG)
                return !combine;
            dataMax /= dataAxisInfo.getDataMax();
            if (dataMax > 0.01 && dataMax < 100)
                return !combine;
            return (combine = false);
        }

        // Case where our only hope is to combine, first simple linear case.
        float cMax = dataMax;
        if (dataAxisInfo.getDataMax() > cMax)
            cMax = dataAxisInfo.getDataMax();
        float cMin = dataMin;
        if (dataAxisInfo.getDataMin() < cMin)
            cMin = dataAxisInfo.getDataMin();
        float dComb = cMax - cMin;
        if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG
                || cMax > 0 && cMin < 0
                || style.getAxisScale().getMinimumRange() > 0) {
            if (style.getAxisScale().getMinimumRange() > 0) {
                if (dData < style.getAxisScale().getMinimumRange())
                    dData = style.getAxisScale().getMinimumRange().floatValue();
                if (dAxis < style.getAxisScale().getMinimumRange())
                    dAxis = style.getAxisScale().getMinimumRange().floatValue();
            } else if (posRatio > 1) {
                if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG
                        && dataMin <= 0 && dataMax >= 0)
                    ;
                else if (dataMax > -dataMin) {
                    if (dataMax / absMin < posRatio)
                        dData = absMin * posRatio - dataMin;
                } else if (-dataMin / absMin < posRatio)
                    dData = dataMax + absMin * posRatio;
                if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG
                        && dataAxisInfo.getDataMin() <= 0
                        && dataAxisInfo.getDataMax() >= 0)
                    ;
                else if (dataAxisInfo.getDataMax() > -dataAxisInfo.getDataMin()) {
                    if (dataAxisInfo.getDataMax() / dataAxisInfo.getAbsMin() < posRatio)
                        dAxis = dataAxisInfo.getAbsMin() * posRatio
                                - dataAxisInfo.getDataMin();
                } else if (-dataAxisInfo.getDataMin()
                        / dataAxisInfo.getAbsMin() < posRatio)
                    dAxis = dataAxisInfo.getDataMax()
                            + dataAxisInfo.getAbsMin() * posRatio;
            }
            if (dData == 0)
                dData = dComb;
            if (dAxis == 0)
                dAxis = dComb;
            if ((dComb < 8 * dData || dComb < 8 * dAxis) && dComb < 17 * dData
                    && dComb < 17 * dAxis)
                return !combine;
            if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG)
                return (combine = false);
        }

        // Combine log case.
        float infoMax = dataAxisInfo.getDataMax();
        if (-dataAxisInfo.getDataMin() > infoMax)
            infoMax = dataAxisInfo.getDataMin();
        if (-dataMin > dataMax)
            dataMax = -dataMin;
        float infoMin = dataAxisInfo.getAbsMin();
        if (infoMin >= infoMax)
            infoMin = dataAxisInfo.getZeroDiv();
        if (absMin >= dataMax)
            absMin = dataMax / 100;
        cMax = dataMax;
        if (infoMax > cMax)
            cMax = infoMax;
        cMin = absMin;
        if (infoMin < cMin)
            cMin = infoMin;
        dData = (float) Math.log10(dataMax / absMin);
        dAxis = (float) Math.log10(infoMax / infoMin);
        dComb = (float) Math.log10(cMax / cMin);
        combine = (dComb < 4 * dData || dComb < 4 * dAxis) && dComb < 8 * dData
                && dComb < 8 * dAxis;
        return !combine;
    }

    // Maps from a scaled value to a floating axis division index.
    public static float axisDivisionOfValue(float value,
            DataAxisInfo dataAxisInfo) {
        if (dataAxisInfo.getStyle().getAxisScale().getScaleType() != AxisScale.Type.LOG)
            return N_GRAPH_DATA_DIVS * (value - dataAxisInfo.getDivMin())
                    / (dataAxisInfo.getDivMax() - dataAxisInfo.getDataMin());
        if (value < -dataAxisInfo.getDLinear())
            return (float) (Math.log10(dataAxisInfo.getDivMin() / value) / dataAxisInfo
                    .getInterval());
        else if (value > dataAxisInfo.getDLinear())
            return (float) (N_GRAPH_DATA_DIVS - Math.log10(dataAxisInfo
                    .getDivMax()
                    / value)
                    / dataAxisInfo.getInterval());
        else
            return dataAxisInfo.getZeroDiv() + value
                    / dataAxisInfo.getDLinear();
    }

    // Maps from a floating axis division index to a scaled value.
    public static float valueOfAxisDivision(float divIdx,
            DataAxisInfo dataAxisInfo) {
        if (dataAxisInfo.getStyle().getAxisScale().getScaleType() != AxisScale.Type.LOG) {
            float w2 = divIdx / N_GRAPH_DATA_DIVS;
            float w1 = 1 - w2;
            return dataAxisInfo.getDivMin() * w1 + dataAxisInfo.getDivMax()
                    * w2;
        }
        if (divIdx < dataAxisInfo.getZeroDiv() - 1)
            return (float) (dataAxisInfo.getDivMin() * Math.pow(10.0,
                    -dataAxisInfo.getInterval() * divIdx));
        else if (divIdx > dataAxisInfo.getZeroDiv() + 1)
            return (float) (dataAxisInfo.getDivMax() * Math.pow(10.0,
                    dataAxisInfo.getInterval() * (divIdx - N_GRAPH_DATA_DIVS)));
        else
            return (divIdx - dataAxisInfo.getZeroDiv())
                    * dataAxisInfo.getDLinear();
    }

    // This is now written on the assumption that a zoomed value for the
    // interval comes in and that this method is used only when the zoom>1.
    public static DataAxisInfo calcDataAxisInfo(float dataMin, boolean zeroYes,
            float absMin, float dataMax, float zoomFactor,
            GraphPreferences style, float divMin, float divMax, float interval,
            float dLinear, int zeroDiv, int perDecade) {

        // Pull out some aspects of the style info as we may temporarily
        // locally modify them, plus it will shorten some expressions.
        float styleMin = (float) style.getAxisScale().getMinValue();
        float styleMax = (float) style.getAxisScale().getMaxValue();
        if (!style.getAxisScale().isExactMinValue())
            styleMin = Float.NEGATIVE_INFINITY;
        if (!style.getAxisScale().isExactMaxValue())
            styleMax = Float.POSITIVE_INFINITY;

        // Simple case of not a log scale.
        if (style.getAxisScale().getScaleType() != AxisScale.Type.LOG) {
            // fprintf(stderr,"interval %f -> %f zoom %f",
            // interval,interval/zoomFactor,zoomFactor);
            interval = XFormFunctions.newDataIntervalFromZoom(interval
                    / zoomFactor, 1.0f, false, null, perDecade);
            // fprintf(stderr," -> %f\n",interval);
            if (divMin < 0)
                divMin = -interval * (int) (0.5 - dataMin / interval);
            else
                divMin = interval * (int) (0.5 + dataMin / interval);
            divMax = divMin + N_GRAPH_DATA_DIVS * interval;
            // fprintf(stderr,"divMin %f divMax %f",divMin,divMax);
            while (divMax > styleMax) {
                divMax -= interval;
                divMin -= interval;
            }
            while (divMin < styleMin) {
                divMin += interval;
                divMax += interval;
            }
            // fprintf(stderr," divMin %f divMax %f\n",divMin,divMax);
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        }

        // Rest of these are log scale based. Apply any style limits.
        if (dataMin < styleMin)
            dataMin = styleMin;
        if (dataMax > styleMax)
            dataMax = styleMax;

        // Zoom the interval, get absolute value endpoints, and resolve highest
        // magnitude endpoint to an even multiple of the zoomed interval.
        interval = XFormFunctions.usableLogInterval(interval / zoomFactor,
                false);
        int mode;
        float endMax, endMin;
        float styleMaxE, styleMinE;
        if (-dataMin < dataMax) {
            mode = 1;
            endMax = dataMax;
            endMin = dataMin;
            styleMaxE = styleMax;
            styleMinE = styleMin;
        } else {
            mode = -1;
            endMax = -dataMin;
            endMin = dataMax;
            styleMaxE = styleMin;
            styleMinE = styleMax;
        }
        if (endMin < 0)
            endMin = -endMin;
        if (styleMinE < 0)
            styleMinE = -styleMinE;
        endMax = (float) Math.log10(endMax);
        if (endMax > 0)
            endMax = interval * (int) (0.5 + endMax / interval);
        else
            endMax = interval * (int) (endMax / interval - 0.5);
        absMin = (float) Math.pow(10.0, endMax - N_GRAPH_DATA_DIVS * interval);
        endMax = (float) Math.pow(10.0, endMax);
        float delint = (float) Math.pow(10.0, interval);
        float halfint = (float) Math.sqrt(delint);
        float twoint = delint * delint;
        while (endMax > styleMaxE) {
            endMax /= delint;
            absMin /= delint;
        }

        // Next case...all completely one side of zero.
        if (dataMin >= -absMin) {
            divMax = endMax;
            if (styleMin <= 0 && (dataMin <= 0 || absMin / dataMin > halfint)) {
                divMin = 0;
                dLinear = absMin * delint;
                zeroDiv = 0;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }
            divMin = absMin;
            while (divMin < styleMin) {
                divMax *= delint;
                divMin *= delint;
            }
            dLinear = divMin;
            zeroDiv = -1;
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        } else if (dataMax <= absMin) {
            divMin = -endMax;
            if (styleMax >= 0 && (dataMax >= 0 || -absMin / dataMax > halfint)) {
                divMax = 0;
                dLinear = absMin * delint;
                zeroDiv = N_GRAPH_DATA_DIVS;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }
            divMax = -absMin;
            while (divMax > styleMax) {
                divMin *= delint;
                divMax *= delint;
            }
            dLinear = -divMax;
            zeroDiv = N_GRAPH_DATA_DIVS + 1;
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        }

        // Finally, endpoints on either side of zero.
        absMin *= twoint;
        dataMin = absMin;
        for (zeroDiv = 1; zeroDiv <= N_GRAPH_DATA_DIVS / 2; zeroDiv++) {
            if (dataMin * delint > endMin)
                break;
            if (dataMin * delint > styleMinE / halfint)
                break;
            absMin *= delint;
            dataMin *= twoint;
        }
        if (mode > 1) {
            divMax = endMax;
            divMin = -dataMin;
            dLinear = absMin;
            while (divMin < styleMin) {
                divMax /= delint;
                divMin /= delint;
                dLinear /= delint;
            }
        } else {
            divMin = -endMax;
            divMax = dataMin;
            dLinear = absMin;
            while (divMax > styleMax) {
                divMax /= delint;
                divMin /= delint;
                dLinear /= delint;
            }
            zeroDiv = N_GRAPH_DATA_DIVS - zeroDiv;
        }

        return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax, interval,
                divMin, divMax, dLinear, zeroDiv, style);
    }

    // Initializes the data range dependent structures for unzoomed axis info.
    public static DataAxisInfo calcUnzoomedAxisInfo(float dataMin,
            boolean zeroYes, float absMin, float dataMax,
            GraphPreferences style, float divMin, float divMax, float interval,
            float dLinear, int zeroDiv) {

        // Pull out some aspects of the style info as we may temporarily
        // locally modify them, plus it will shorten expressions.
        float baseVal = style.getAxisScale().getBaseVal().floatValue();
        float minRange = style.getAxisScale().getMinimumRange().floatValue();
        float posRatio = 0;
        if (minRange < -1)
            posRatio = -minRange;
        boolean adaptive = (!style.getAxisScale().isExactMaxValue() || !style
                .getAxisScale().isExactMinValue());
        float styleMin = (float) style.getAxisScale().getMinValue();
        float styleMax = (float) style.getAxisScale().getMaxValue();
        boolean pinMin = style.getAxisScale().isExactMinValue();
        boolean pinMax = style.getAxisScale().isExactMaxValue();
        boolean byRatio = false;

        boolean logScaling = style.getAxisScale().getScaleType() == AxisScale.Type.LOG;
        int nAdaptive = -1;
        if (style.getAxisScale().isExactMinValue()
                && style.getAxisScale().isExactMaxValue()) {
            nAdaptive = 0;
        } else if (!style.getAxisScale().isExactMinValue()
                && style.getAxisScale().isExactMaxValue()) {
            nAdaptive = 1;
        } else if (style.getAxisScale().isExactMinValue()
                && !style.getAxisScale().isExactMaxValue()) {
            nAdaptive = 2;
        }
        // See to it that the baseVal and posRatio are not in direct conflict
        // with
        // specified endpoints.
        if (logScaling) {
            if (pinMin) {
                if (styleMin < 0 && baseVal > -styleMin)
                    baseVal = 0;
                if (styleMin > 0)
                    baseVal = styleMin;
            }
            if (pinMax) {
                if (styleMax > 0 && baseVal > styleMax)
                    baseVal = 0;
                if (styleMax < 0)
                    baseVal = -styleMax;
            }
            if (pinMax && pinMin) {
                if (styleMin == -baseVal && styleMax == baseVal)
                    baseVal = 0;
                else if (baseVal > 0)
                    posRatio = 0;
                else if (posRatio > 0 && styleMax != 0 && styleMin != 0) {
                    float ratio = styleMax / styleMin;
                    if (ratio < 0)
                        ratio = -ratio;
                    if (ratio < 1)
                        ratio = 1 / ratio;
                    if (posRatio < ratio)
                        posRatio = 0;
                }
            }
        }

        // Expand data range to include floating endpoints,
        // and then to include baseVal if needed.
        // if (styleMin < Float.POSITIVE_INFINITY && dataMin > styleMin)
        // dataMin = styleMin;
        // if (styleMax < Float.POSITIVE_INFINITY && dataMax < styleMax)
        // dataMax = styleMax;
        if (logScaling && adaptive && baseVal > 0) {
            if (dataMax < -baseVal && !pinMax)
                dataMax = -baseVal;
            else if (dataMin > baseVal && !pinMin)
                dataMin = baseVal;
            else if (dataMin >= 0 && dataMax < baseVal && !pinMax)
                dataMax = baseVal;
            else if (dataMax <= 0 && dataMin > -baseVal && !pinMin)
                dataMin = baseVal;
            else if (dataMin > -baseVal && dataMax < baseVal) {
                if (!pinMin)
                    dataMin = -baseVal;
                if (!pinMax)
                    dataMax = baseVal;
            }
        }

        // If there, record values across which we will not expand for
        // either minimum range or rounding.
        float expandUp = Float.POSITIVE_INFINITY;
        float expandDn = Float.NEGATIVE_INFINITY;
        if (baseVal == 0 || !logScaling) {
            if (dataMin >= baseVal)
                expandDn = baseVal;
            if (dataMax <= baseVal)
                expandUp = baseVal;
        } else if (dataMin >= baseVal)
            expandDn = baseVal;
        else if (dataMax <= -baseVal)
            expandUp = -baseVal;
        else if (dataMin >= 0)
            expandDn = 0;
        else if (dataMax <= 0)
            expandUp = 0;
        else if (dataMin >= -baseVal && dataMax > baseVal)
            expandDn = -baseVal;
        else if (dataMax <= baseVal && dataMin < -baseVal)
            expandUp = baseVal;

        // Handle case of no data range and expanding to minimum arithmetic
        // range.
        if (dataMax < dataMin) {
            if (dataMin < 1e36)
                dataMax = dataMin;
            else if (dataMax > -1e36)
                dataMin = dataMax;
            if (dataMax >= dataMin)
                ;
            else if (logScaling && baseVal > 0)
                dataMax = dataMin = baseVal;
            else
                dataMax = dataMin = 1.0f;
        }
        if (minRange > 0 && (dataMax - dataMin < minRange) && adaptive) {
            if (pinMin)
                dataMax = dataMin + minRange;
            else if (pinMax)
                dataMin = dataMax - minRange;
            else if (expandUp < 1e36) {
                dataMax = (dataMax + dataMin + minRange) / 2;
                if (dataMax > expandUp)
                    dataMax = expandUp;
                dataMin = dataMax - minRange;
            } else {
                dataMin = (dataMax + dataMin - minRange) / 2;
                if (dataMin < expandDn)
                    dataMin = expandDn;
                dataMax = dataMin + minRange;
            }
        }

        // There are certain circumstances with the log scale and there are
        // data endpoints near zero where it is easiest to pretend that the
        // data endpoint was specifically specified at zero or baseVal.
        if (logScaling && adaptive) {
            while (!pinMin && baseVal > 0 && dataMax > baseVal
                    && dataMin >= -baseVal) {
                if (pinMax && posRatio > 0 && posRatio != 10
                        && posRatio * baseVal > dataMax)
                    break;
                pinMin = true;
                if (dataMin < 0)
                    dataMin = -baseVal;
                else if (dataMin < baseVal)
                    dataMin = 0;
                else
                    dataMin = baseVal;
                styleMin = dataMin;
                if (pinMax)
                    posRatio = 0;
                break;
            }
            while (!pinMax && baseVal > 0 && dataMin < -baseVal
                    && dataMax <= baseVal) {
                if (pinMin && posRatio > 0 && posRatio != 10
                        && -posRatio * baseVal < dataMin)
                    break;
                pinMax = true;
                if (dataMax > 0)
                    dataMax = baseVal;
                else if (dataMax > -baseVal)
                    dataMax = 0;
                else
                    dataMax = -baseVal;
                styleMax = dataMax;
                if (pinMin)
                    posRatio = 0;
                break;
            }
            if (baseVal <= 0) {
                float maxRatio = 100;
                if (posRatio > maxRatio)
                    maxRatio = posRatio;
                float r2 = -maxRatio * 10;
                if (!pinMin && dataMax > 0 && dataMin < dataMax / maxRatio
                        && dataMin > dataMax / r2) {
                    pinMin = true;
                    dataMin = styleMin = 0;
                }
                if (!pinMax && dataMin < 0 && dataMax > dataMin / maxRatio
                        && dataMax < dataMin / r2) {
                    pinMax = true;
                    dataMax = styleMax = 0;
                }
            }
            if (pinMin && pinMax)
                nAdaptive = 0;
            else if (pinMin == pinMax)
                nAdaptive = -1;
            else if (pinMax)
                nAdaptive = 1;
            else
                nAdaptive = 2;
        }

        // Let's start with the case of a totally non-adaptive axis, as
        // it is more cut and dried.
        if (adaptive)
            ;
        else if (!logScaling) {
            divMin = styleMin;
            divMax = styleMax;
            interval = (divMax - divMin) / N_GRAPH_DATA_DIVS;
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        } else {
            // Extremely simple case of all on one side of zero.
            divMin = styleMin;
            divMax = styleMax;
            dLinear = baseVal;
            if (divMin > 0) {
                interval = (float) (Math.log10(divMax / divMin) / N_GRAPH_DATA_DIVS);
                zeroDiv = -1;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }
            if (divMax < 0) {
                interval = (float) (Math.log10(divMin / divMax) / N_GRAPH_DATA_DIVS);
                zeroDiv = N_GRAPH_DATA_DIVS + 1;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }

            // Get absolute value range of data end points.
            float endMin, endMax;
            if (-divMin < divMax) {
                endMax = divMax;
                endMin = -divMin;
            } else {
                endMax = -divMin;
                endMin = divMax;
            }

            // Determine initial version of smallest log scaled value dLinear.
            boolean linSet = true;
            if (dLinear <= 0 && posRatio > 0 && posRatio != 10) {
                dLinear = endMax / posRatio;
                byRatio = true;
            }
            if (dLinear <= 0 || endMin > 0 && dLinear > endMin) {
                byRatio = linSet = false;
                dLinear = absMin;
                if (dLinear < endMax / 100)
                    dLinear = endMax / 100;
                if (posRatio > 0 && dLinear > endMax / posRatio) {
                    dLinear = endMax / posRatio;
                    byRatio = true;
                }
                if (endMin > 0 && dLinear > endMin) {
                    dLinear = endMin;
                    byRatio = false;
                }
            }

            // Cases where zero point is given. First, endpoint is exactly
            // dLinear.
            if (endMin == dLinear) {
                interval = (float) (Math.log10(endMax / dLinear) / (N_GRAPH_DATA_DIVS - 2));
                zeroDiv = endMax == divMax ? 1 : N_GRAPH_DATA_DIVS - 1;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }

            // An endpoint is exactly zero, or they are symetric.
            if (endMin == 0 || endMin == endMax) {
                int dd = endMin == 0 ? N_GRAPH_DATA_DIVS - 1
                        : N_GRAPH_DATA_DIVS / 2 - 1;
                interval = (float) (Math.log10(endMax / dLinear) / dd);
                if (!linSet) {
                    interval = XFormFunctions.usableLogInterval(interval,
                            byRatio);
                    dLinear = (float) (endMax / Math.pow(10.0, interval * dd));
                }
                if (endMin == endMax)
                    zeroDiv = N_GRAPH_DATA_DIVS / 2;
                else if (endMax == divMax)
                    zeroDiv = 0;
                else
                    zeroDiv = N_GRAPH_DATA_DIVS;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }

            // We need to determine the best location for zero.
            zeroDiv = -1;
            float delDL = Float.POSITIVE_INFINITY;
            int dd1 = 0;
            int dd2 = N_GRAPH_DATA_DIVS - 2;
            for (int zzz = 1; zzz < N_GRAPH_DATA_DIVS / 2; zzz++, dd1++, dd2--) {
                interval = (float) (Math.log10(endMax / endMin) / (dd2 - dd1));
                float thisDL = (float) (endMax / Math.pow(10.0, interval * dd2));
                float diff = (float) Math.log10(dLinear / thisDL);
                if (diff < 0)
                    diff = -diff;
                if (diff > delDL)
                    continue;
                delDL = diff;
                zeroDiv = zzz;
            }

            // Based on this zero location, finish our calculation.
            dd1 = zeroDiv - 1;
            dd2 = N_GRAPH_DATA_DIVS - 2 - dd1;
            interval = (float) (Math.log10(endMax / endMin) / (dd2 - dd1));
            dLinear = (float) (endMax / Math.pow(10.0, interval * dd2));
            if (endMax == divMax)
                zeroDiv = N_GRAPH_DATA_DIVS - zeroDiv;
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);

        }

        // Handle some cases of expanding to minimum ratio.
        if (posRatio > 0 || baseVal > 0 && logScaling)
            ;
        else if (dataMin > 0 && dataMax / dataMin < posRatio) {
            if (pinMin)
                dataMax = dataMin * posRatio;
            else if (pinMax)
                dataMin = dataMax / posRatio;
            else if (expandUp < Float.POSITIVE_INFINITY) {
                dataMax = (float) Math.sqrt(dataMax * dataMin * posRatio);
                if (dataMax > expandUp)
                    dataMax = expandUp;
                dataMin = dataMax / posRatio;
            } else {
                dataMin = (float) Math.sqrt(dataMax * dataMin / posRatio);
                if (dataMin < expandDn)
                    dataMin = expandUp;
                dataMax = dataMin * posRatio;
            }
            byRatio = true;
            if (dataMin < absMin)
                absMin = dataMin;
        } else if (dataMax < 0 && dataMin / dataMax < posRatio) {
            if (pinMax)
                dataMin = dataMax * posRatio;
            else if (pinMin)
                dataMax = dataMin / posRatio;
            else if (expandUp < Float.POSITIVE_INFINITY) {
                dataMax = (float) -Math.sqrt(dataMax * dataMin / posRatio);
                if (dataMax > expandUp)
                    dataMax = expandUp;
                dataMin = dataMax * posRatio;
            } else {
                dataMin = (float) -Math.sqrt(dataMax * dataMin * posRatio);
                if (dataMin < expandDn)
                    dataMin = expandDn;
                dataMax = dataMin / posRatio;
            }
            byRatio = true;
            if (-dataMax < absMin)
                absMin = -dataMax;
        }

        // Case of adaptive linear axis.
        if (!logScaling) {
            // First guess at interval we need.
            float workInterval = (dataMax - dataMin) / N_GRAPH_DATA_DIVS;
            float wantedIntCount = N_GRAPH_DATA_DIVS;
            interval = XFormFunctions.newDataIntervalFromZoom(workInterval,
                    1.0f, false, null, 5);
            while (interval * wantedIntCount < dataMax - dataMin) {
                workInterval *= 1.1;
                interval = XFormFunctions.newDataIntervalFromZoom(workInterval,
                        1.0f, false, null, 5);
            }

            // Easy cases where one endpoint is pinned.
            if (pinMin) {
                divMin = dataMin;
                divMax = divMin + N_GRAPH_DATA_DIVS * interval;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            } else if (pinMax) {
                divMax = dataMax;
                divMin = divMax - N_GRAPH_DATA_DIVS * interval;
                return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                        interval, divMin, divMax, dLinear, zeroDiv, style);
            }

            // For both endpoints floating, we may need to tweak further
            // depending
            // on how the endpoints resolve to the nearest interval.
            while (true) {
                divMin = interval * (int) (dataMin / interval);
                if (baseVal != 0) {
                    double intRatio = (double) baseVal / (double) interval;
                    if (intRatio < 0)
                        intRatio -= ((int) intRatio) - 1;
                    else
                        intRatio -= (int) intRatio;
                    if (intRatio > 0.001 && intRatio < 0.999)
                        divMin += intRatio * interval;
                }
                divMax = divMin + N_GRAPH_DATA_DIVS * interval;
                while (divMin > dataMin
                        || (divMax - dataMax) - (dataMin - divMin) > interval) {
                    divMin -= interval;
                    divMax -= interval;
                }
                while (divMax < dataMax
                        || (dataMin - divMin) - (divMax - dataMax) > interval) {
                    divMin += interval;
                    divMax += interval;
                }
                if (divMin <= dataMin && divMax >= dataMax)
                    break;
                workInterval *= 1.1;
                interval = XFormFunctions.newDataIntervalFromZoom(workInterval,
                        1.0f, false, null, 5);
            }

            // Make sure rounding did not go accross applicable barrier.
            while (divMax > expandUp) {
                divMin -= interval;
                divMax -= interval;
            }
            while (divMin < expandDn) {
                divMin += interval;
                divMax += interval;
            }
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        }

        // Rest of these are log scale based. Store positive ranges of this
        // data scale that we can perform log operations on.
        float endMin, endMax;
        boolean pinMaxE, pinMinE;
        int mode = 0;
        if (-dataMin < dataMax) {
            endMax = dataMax;
            endMin = dataMin;
            pinMaxE = pinMax;
            pinMinE = pinMin;
            mode = 1;
        } else {
            endMax = -dataMin;
            endMin = dataMax;
            pinMaxE = pinMin;
            pinMinE = pinMax;
            mode = -1;
        }
        if (endMin < 0)
            endMin = -endMin;
        if (endMax == endMin)
            mode = 0;

        // Push magnitudes up based on baseVal && minRange, if appropriate.
        while (posRatio > 0 && baseVal > 0) {
            float topVal = baseVal * posRatio;
            if (endMax >= topVal)
                break;
            byRatio = true;
            if (mode == 0 && nAdaptive < 0) {
                dataMax = endMax = endMin = topVal;
                dataMin = -topVal;
                break;
            }
            if (!pinMaxE && mode != 0) {
                endMax = topVal;
                if (mode > 0)
                    dataMax = topVal;
                else
                    dataMin = -topVal;
                break;
            }
            endMin = endMax;
            endMax = topVal;
            if (pinMin) {
                mode = 1;
                dataMax = topVal;
            } else {
                mode = -1;
                dataMin = -topVal;
            }
            if (endMax == endMin)
                mode = 0;
            pinMinE = true;
            pinMaxE = false;
            break;
        }

        // Let's start with the simplest cases...all one side of zero.
        if (dataMin > 0 || dataMax < 0) {
            divMin = endMin = (float) Math.log10(endMin);
            divMax = endMax = (float) Math.log10(endMax);
            interval = XFormFunctions.usableLogInterval((endMax - endMin)
                    / N_GRAPH_DATA_DIVS, true);
            if (pinMinE)
                endMax = endMin + N_GRAPH_DATA_DIVS * interval;
            else if (pinMaxE)
                endMin = endMax - N_GRAPH_DATA_DIVS * interval;
            else
                while (true) {
                    endMax = interval * (int) (divMax / interval);
                    endMin = endMax - N_GRAPH_DATA_DIVS * interval;
                    while (endMax < divMax
                            || (divMin - endMin) - (endMax - divMax) > interval) {
                        endMin += interval;
                        endMax += interval;
                    }
                    if (endMin <= divMin && endMax >= divMax)
                        break;
                    interval = XFormFunctions.nextLogInterval(interval);
                }
            if (mode >= 0) {
                divMin = (float) Math.pow(10.0, endMin);
                divMax = (float) Math.pow(10.0, endMax);
                dLinear = divMin;
                zeroDiv = -1;
            } else {
                divMin = (float) -Math.pow(10.0, endMax);
                divMax = (float) -Math.pow(10.0, endMin);
                dLinear = -divMax;
                zeroDiv = N_GRAPH_DATA_DIVS - 1;
            }
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        }

        // Initial guess for dLinear, then push it down based on
        // minRange, if appropriate.
        dLinear = baseVal;
        byRatio = false;
        if (dLinear <= 0) {
            byRatio = false;
            dLinear = absMin;
            float botVal;
            if (dataMin >= 0 || dataMax <= 0)
                botVal = endMax / 100;
            else
                botVal = endMax / 10;
            if (dLinear < botVal)
                dLinear = botVal;
            if (dLinear > endMax / 10)
                dLinear = endMax / 10;
            if (posRatio > 0 && dLinear > endMax / posRatio) {
                dLinear = endMax / posRatio;
                byRatio = true;
            }
            if (pinMinE && endMin > 0 && dLinear > endMin) {
                dLinear = endMin;
                byRatio = false;
            }
        }

        // Cases where zero point is given.
        if (endMin <= dLinear) {
            int nnn = endMin == 0 ? N_GRAPH_DATA_DIVS - 1
                    : N_GRAPH_DATA_DIVS - 2;
            absMin = dLinear = (float) Math.log10(dLinear);
            divMax = endMax = (float) Math.log10(endMax);
            pinMinE = (pinMinE && endMin != 0) || baseVal > 0;
            interval = (endMax - dLinear) / nnn;
            if (!(pinMinE && pinMaxE)) {
                interval = XFormFunctions.usableLogInterval(interval, byRatio
                        || pinMinE);
                if (pinMinE)
                    endMax = dLinear + nnn * interval;
                else if (pinMaxE)
                    dLinear = endMax - nnn * interval;
                else {
                    endMax = interval * (int) (divMax / interval);
                    ;
                    dLinear = endMax - nnn * interval;
                    while (endMax < divMax
                            || (absMin - dLinear) - (endMax - divMax) > interval) {
                        dLinear += interval;
                        endMax += interval;
                    }
                }
            }
            if (mode >= 0) {
                dLinear = (float) Math.pow(10.0, dLinear);
                divMax = (float) Math.pow(10.0, endMax);
                divMin = endMin == 0 ? endMin : -dLinear;
                zeroDiv = N_GRAPH_DATA_DIVS - (nnn + 1);
            } else {
                dLinear = (float) Math.pow(10.0, dLinear);
                divMin = (float) -Math.pow(10.0, endMax);
                divMax = endMin == 0 ? endMin : dLinear;
                zeroDiv = nnn + 1;
            }
            return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax,
                    interval, divMin, divMax, dLinear, zeroDiv, style);
        }

        // We need to determine the best location for zero.
        zeroDiv = -1;
        float bestDiff = Float.POSITIVE_INFINITY;
        float bestDL = Float.POSITIVE_INFINITY;
        float diff = 0, ddd = 0;
        int dd1 = 0;
        int dd2 = N_GRAPH_DATA_DIVS - 2;
        absMin = dLinear = (float) Math.log10(dLinear);
        divMin = endMin = (float) Math.log10(endMin);
        divMax = endMax = (float) Math.log10(endMax);
        float bestInt = Float.POSITIVE_INFINITY;
        for (int zzz = 1; zzz <= N_GRAPH_DATA_DIVS / 2; zzz++, dd1++, dd2--) {
            if (pinMinE && dd1 == 0)
                continue;
            if (pinMinE && baseVal > 0) {
                interval = (divMin - absMin) / dd1;
                endMax = absMin + dd2 * interval;
            } else if (pinMaxE && baseVal > 0) {
                interval = (divMax - absMin) / dd2;
                endMin = absMin + dd1 * interval;
            } else if (baseVal > 0) {
                if (dd1 == 0 && absMin < divMin)
                    continue;
                interval = XFormFunctions.usableLogInterval((divMax - absMin)
                        / dd2, true);
                while (true) {
                    endMin = absMin + dd1 * interval;
                    endMax = absMin + dd2 * interval;
                    if (endMin >= divMin && endMax >= divMax)
                        break;
                    interval = XFormFunctions.nextLogInterval(interval);
                }
            } else if (pinMinE) {
                interval = XFormFunctions.usableLogInterval((divMin - absMin)
                        / dd1, byRatio);
                while (true) {
                    dLinear = endMin - dd1 * interval;
                    endMax = dLinear + dd2 * interval;
                    if (endMax >= divMax || dd1 == dd2)
                        break;
                    interval = XFormFunctions.nextLogInterval(interval);
                }
            } else if (pinMaxE) {
                interval = XFormFunctions.usableLogInterval((divMax - absMin)
                        / dd2, byRatio);
                dLinear = endMax - dd2 * interval;
                endMin = dLinear + dd1 * interval;
                interval = XFormFunctions.nextLogInterval(interval);
            } else {
                interval = XFormFunctions.usableLogInterval((divMax - absMin)
                        / dd2, byRatio);
                endMax = interval * (int) (divMax / interval);
                dLinear = endMax - dd2 * interval;
                endMin = absMin + dd1 * interval;
                while (endMax < divMax || endMin < divMin) {
                    dLinear += interval;
                    endMax += interval;
                    endMin += interval;
                }
            }
            diff = endMin > divMin ? endMin - divMin : divMin - endMin;
            ddd = endMax > divMax ? endMax - divMax : divMax - endMax;
            if (ddd > diff)
                diff = ddd;
            ddd = dLinear > absMin ? dLinear - absMin : absMin - dLinear;
            if (ddd > diff)
                diff = ddd;
            if (diff > bestDiff)
                continue;
            zeroDiv = zzz;
            bestDiff = diff;
            bestInt = interval;
            bestDL = dLinear;
            dataMax = endMax;
            dataMin = endMin;
            if (diff == 0)
                break;
        }

        // Based on this zero location, finish our calculation.
        interval = bestInt;
        dLinear = (float) Math.pow(10.0, bestDL);
        if (mode >= 0) {
            divMin = (float) -Math.pow(10.0, dataMin);
            divMax = (float) Math.pow(10.0, dataMax);
        } else {
            divMin = (float) -Math.pow(10.0, dataMax);
            divMax = (float) Math.pow(10.0, dataMin);
            zeroDiv = N_GRAPH_DATA_DIVS - zeroDiv;
        }
        return makeDataAxisInfo(dataMin, zeroYes, absMin, dataMax, interval,
                divMin, divMax, dLinear, zeroDiv, style);
    }

    // Initialize a one to one dataAxisInfo object.
    public static DataAxisInfo initDataAxisInfo(float dataMin, boolean zeroYes,
            float absMin, float dataMax, String units, GraphPreferences style) {

        DataAxisInfo dataAxisInfo = new DataAxisInfo();

        dataAxisInfo = calcUnzoomedAxisInfo(dataMin, zeroYes, absMin, dataMax,
                style, dataAxisInfo.getDivMin(), dataAxisInfo.getDivMax(),
                dataAxisInfo.getInterval(), dataAxisInfo.getDLinear(),
                dataAxisInfo.getZeroDiv());
        dataAxisInfo.setUnits(units);
        dataAxisInfo.setStyle(style);
        dataAxisInfo.setDataMin(dataMin);
        dataAxisInfo.setZeroYes(zeroYes);
        dataAxisInfo.setAbsMin(absMin);
        dataAxisInfo.setDataMax(dataMax);
        return dataAxisInfo;
    }

    // Zooms a data axis info object.
    public static DataAxisInfo zoomAxisInfo(float minDiv, float maxDiv,
            float zoomFactor, DataAxisInfo dataAxisInfo) {
        int perDecade = 8;
        if (minDiv < 0) {
            maxDiv -= minDiv;
            minDiv = 0;
        }
        if (maxDiv > N_GRAPH_DATA_DIVS) {
            minDiv -= maxDiv - N_GRAPH_DATA_DIVS;
            maxDiv = N_GRAPH_DATA_DIVS;
        }
        // fprintf(stderr,"zoomAxisInfo minDiv %f maxDiv %f\n",minDiv,maxDiv);
        float dataMin = valueOfAxisDivision(minDiv, dataAxisInfo);
        float dataMax = valueOfAxisDivision(maxDiv, dataAxisInfo);
        // fprintf(stderr,"zoomAxisInfo dataMin %f dataMax
        // %f\n",dataMin,dataMax);
        DataAxisInfo zoomedInfo = calcDataAxisInfo(dataMin, dataAxisInfo
                .isZeroYes(), dataAxisInfo.getAbsMin(), dataMax, zoomFactor,
                dataAxisInfo.getStyle(), dataAxisInfo.getDivMin(), dataAxisInfo
                        .getDivMax(), dataAxisInfo.getInterval(), dataAxisInfo
                        .getDLinear(), dataAxisInfo.getZeroDiv(), perDecade);
        return zoomedInfo;
    }

    private static DataAxisInfo makeDataAxisInfo(float dataMin,
            boolean zeroYes, float absMin, float dataMax, float interval,
            float divMin, float divMax, float dLinear, int zeroDiv,
            GraphPreferences style) {
        DataAxisInfo info = new DataAxisInfo();
        info.setAbsMin(absMin);
        info.setDataMax(dataMax);
        info.setDataMin(dataMin);
        info.setDivMax(divMax);
        info.setDivMin(divMin);
        info.setDLinear(dLinear);
        info.setInterval(interval);
        info.setZeroDiv(zeroDiv);
        info.setStyle(style);

        return info;
    }

}
