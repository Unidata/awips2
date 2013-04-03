package com.raytheon.uf.common.dataplugin.ffmp;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Guidance Interpolation
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 Jan, 2010 3915         dhladky     Initial creation
 * 01/27/13     1478         dhladky     Added use of constants
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPGuidanceInterpolation {

    public String primarySource = null;

    public FFMPSourceConfigurationManager manager = null;

    public HashMap<String, Boolean> interpolationOffsets = null;

    // all hours
    public ArrayList<Double> orderedHours = new ArrayList<Double>();

    private ArrayList<String> interpolationSources = new ArrayList<String>();

    private double interpolationOffset = 0;

    private boolean isInterpolate = false;

    private ProductRunXML productRun;

    private ProductXML product;

    private String guidType;

    private String siteKey;

    public FFMPGuidanceInterpolation(FFMPSourceConfigurationManager manager,
            ProductXML product, ProductRunXML productRun, String primarySource,
            String ffgName, String siteKey) {

        this.manager = manager;
        this.primarySource = primarySource;
        this.productRun = productRun;
        this.product = product;
        this.guidType = ffgName;
        this.siteKey = siteKey;
        interpolationOffsets = new HashMap<String, Boolean>();

        for (SourceXML source : productRun.getGuidanceSources(product, ffgName)) {
            if (source.isInterpolatedGuidanceTransition()) {
                if (true) {
                    interpolationOffsets.put(source.getSourceName(),
                            source.getInterpolatedGuidanceDelay());
                    orderedHours.add(getHour(source.getSourceName()));
                }
            }
        }

        Collections.sort(orderedHours, new SortNumber());
    }

    public double getHour(String source) {
        SourceXML sourceXml = manager.getSource(source);
        return sourceXml.getDurationHour();
    }

    private String getSource(double i) {
        String rsource = null;
        ArrayList<SourceXML> sources = productRun.getGuidanceSources(product,
                guidType);
        for (SourceXML source : sources) {
            if (getHour(source.getSourceName()) == i) {
                rsource = source.getSourceName();
                break;
            }
        }
        return rsource;
    }

    /**
     * Sort by double
     * 
     * @author dhladky
     * 
     */
    public class SortNumber implements Comparator<Double> {

        @Override
        public int compare(Double o1, Double o2) {

            return o1.compareTo(o2);
        }
    }

    /**
     * Sets the source(s) used for getting guidance data
     * 
     * @param time
     */
    public void setInterpolationSources(double time) {

        Double previous = null;
        boolean skip = false;
        interpolationSources.clear();

        int index = 0;
        for (index = 0; index < orderedHours.size(); ++index) {
            Double i = orderedHours.get(index);
            if (time == i) {
                interpolationSources.add(getSource(i));
                // System.err.println("0: Time == " + i);
                skip = true;
                break;
            } else if (time < i) {
                if (previous == null) {
                    interpolationSources
                            .add(getSource(orderedHours.get(index)));
                    interpolationSources
                            .add(getSource(orderedHours.get(index)));
                    // System.err.println("1: " + time /
                    // orderedHours.get(index));
                    setInterpolationOffset(time / orderedHours.get(index));
                    isInterpolate = true;
                    skip = true;
                    break;
                } else {
                    interpolationSources.add(getSource(orderedHours
                            .get(index - 1)));
                    interpolationSources
                            .add(getSource(orderedHours.get(index)));
                    double totaldiff = orderedHours.get(index)
                            - orderedHours.get(index - 1);
                    double timediff = time - orderedHours.get(index - 1);
                    // System.err.println("2: " + timediff / totaldiff);
                    setInterpolationOffset(timediff / totaldiff);
                    isInterpolate = true;
                    skip = true;
                    break;
                }
            }
            previous = i;
        }
        // if the loop has exited and time is still > previous it is higher than
        // max
        if (!skip && (time > previous)) {
            interpolationSources.add(getSource(orderedHours.get(index - 1)));
            interpolationSources.add(getSource(orderedHours.get(index - 1)));
            // System.err.println("3: NaN");
            setInterpolationOffset(Double.NaN);
            isInterpolate = true;
        }
    }

    private void setInterpolationOffset(double interpolationOffset) {
        this.interpolationOffset = interpolationOffset;
    }

    /**
     * offset from the time
     * 
     * @return
     */
    public double getInterpolationOffset() {
        return interpolationOffset;
    }

    /**
     * Sources for interpolation / or not
     * 
     * @return
     */
    public ArrayList<String> getInterpolationSources() {
        return interpolationSources;
    }

    /**
     * Not interpolating
     * 
     * @return
     */
    public String getStandardSource() {
        return interpolationSources.get(0);
    }

    public String getSource1() {
        if (interpolationSources.size() > 0) {
            return interpolationSources.get(0);
        } else {
            return null;
        }
    }

    public String getSource2() {
        if (interpolationSources.size() > 1) {
            return interpolationSources.get(1);
        } else {
            return null;
        }
    }

    public boolean isInterpolate() {
        return isInterpolate;
    }

    public Float interpolateSourcePoint(String source, FFMPGuidanceBasin basin) {
        double hour = getHour(source);
        // System.err.println("interpolating for " + source);

        FFFGDataMgr dman = FFFGDataMgr.getInstance();
        Float lowVal = Float.NaN;
        double lowHour = 0;

        Float highVal = Float.NaN;
        double highHour = 0;

        boolean skip = false;

        for (int index = 0; index < orderedHours.size(); ++index) {
            Double thisHour = orderedHours.get(index);
            Float thisVal = basin
                    .getValue(
                            getSource(orderedHours.get(index)),
                            null,
                            manager.getSource(source).getExpirationMinutes(
                                    siteKey) * TimeUtil.MILLIS_PER_MINUTE);
            if (dman.isExpired() == false) {

                thisVal = dman.adjustValue(thisVal,
                        getSource(orderedHours.get(index)), basin.getPfaf(),
                        basin.getCountyFips());
            }

            if (thisVal.isNaN()) {
                break;
            }
            if ((hour > thisHour) && !thisVal.isNaN()) {
                lowVal = thisVal;
                lowHour = thisHour;
            } else if ((hour <= thisHour) && !thisVal.isNaN()) {
                skip = true;
                highVal = thisVal;
                highHour = thisHour;
                break;
            }
        }

        if (skip == false) {
            // this means there is no hour higher, return NaN;
            // System.err.println("\tNo higher hour");
            return Float.NaN;
        } else {
            // System.err.println("\tfrom " + lowHour + " to " + highHour);
            // calculate offset
            double totalDiff = highHour - lowHour;
            double timeDiff = hour - lowHour;
            double offset = timeDiff / totalDiff;
            // calculate value and return it
            return (float) (lowVal + ((highVal - lowVal) * offset));
        }
    }
}
