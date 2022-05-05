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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;

/**
 * Graph data object used to display 24 hour graph info
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ------------------
 * Apr 23, 2010  4494     D. Hladky  Initial release
 * Feb 01, 2013  1569     D. Hladky  Added constants
 * Oct 10, 2015  4756     dhladky    Max value method.
 * Jul 20, 2018  6642     randerso   Code cleanup.
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPGraphData {

    private Map<Double, Double> qpes = new HashMap<>();

    private Map<Double, Double> rates = new HashMap<>();

    private Map<Double, Double> ratios = new HashMap<>();

    private Map<Double, Double> diffs = new HashMap<>();

    private Map<Double, Double> guids = new HashMap<>();

    private Map<Double, Double> qpfs = new HashMap<>();

    private Map<Double, Double> virtuals = new HashMap<>();

    private List<Double> qpeTimes = new ArrayList<>();

    private List<Double> rateTimes = new ArrayList<>();

    private List<Double> guidanceTimes = new ArrayList<>();

    private List<Double> virtualTimes = new ArrayList<>();

    private List<Double> qpfTimes = new ArrayList<>();

    private Float qpfValue = Float.NaN;

    private Date date = null;

    private String county = null;

    private String streamName = null;

    private String state = null;

    private String pfaf = null;

    private FFMPBasin ffmpBasin = null;

    /**
     * Date matching the time on the FFMP time labels on the table dialog and
     * the basin trend graph dialog. Use for the x-axis on the basin trend
     * graph.
     */
    private Date displayDate;

    public FFMPGraphData(String pfaf, String county, String state,
            String streamName, Date date, FFMPBasin basin) {
        setPfaf(pfaf);
        setCounty(county);
        setState(state);
        setStreamName(streamName);
        setDate(date);
        setFfmpBasin(basin);
    }

    public List<Double> getQpeTimes() {
        return qpeTimes;
    }

    public void setQpeTimes(List<Double> qpeTimes) {
        this.qpeTimes = qpeTimes;
    }

    public List<Double> getQpfTimes() {
        return qpfTimes;
    }

    public void setQpfTimes(List<Double> qpfTimes) {
        this.qpfTimes = qpfTimes;
    }

    public List<Double> getGuidanceTimes() {
        return guidanceTimes;
    }

    public void setGuidanceTimes(List<Double> guidanceTimes) {
        this.guidanceTimes = guidanceTimes;
    }

    public List<Double> getVirtualTimes() {
        return virtualTimes;
    }

    public void setVirtualTimes(List<Double> virtualTimes) {
        this.virtualTimes = virtualTimes;
    }

    public Double getQpe(Double time) {
        return qpes.get(time);
    }

    public void setQpe(Double time, Double qpe) {
        qpes.put(time, qpe);
    }

    public Double getRate(Double time) {
        return rates.get(time);
    }

    public void setRateTimes(List<Double> rateTimes) {
        this.rateTimes = rateTimes;
    }

    public List<Double> getRateTimes() {
        return rateTimes;
    }

    public void setRate(Double time, Double rate) {
        rates.put(time, rate);
    }

    public Double getRatio(Double time) {
        return ratios.get(time);
    }

    public void setRatio(Double time, Double ratio) {
        ratios.put(time, ratio);
    }

    public void setDiff(Double time, Double diff) {
        diffs.put(time, diff);
    }

    public Double getDiff(Double time) {
        return diffs.get(time);
    }

    public void setGuid(Double time, Double guid) {
        guids.put(time, guid);
    }

    public Double getGuid(Double time) {
        return guids.get(time);
    }

    public Double getQpf(Double time) {
        return qpfs.get(time);
    }

    public Double getVirtual(Double time) {
        return virtuals.get(time);
    }

    public void setVirtual(Double time, Double virtual) {
        virtuals.put(time, virtual);
    }

    public void setQpf(Double time, Double qpf) {
        qpfs.put(time, qpf);
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public String getCounty() {
        return county;
    }

    public void setCounty(String county) {
        this.county = county;
    }

    public String getStreamName() {
        return streamName;
    }

    public void setStreamName(String streamName) {
        this.streamName = streamName;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getPfaf() {
        return pfaf;
    }

    public void setPfaf(String pfaf) {
        this.pfaf = pfaf;
    }

    public void setDisplayDate(Date date) {
        this.displayDate = date;
    }

    public Date getDisplayDate() {
        return this.displayDate;
    }

    public void printGraphData() {
        System.out.println("********* DIFF ******************");

        Set<Double> diffKeys = diffs.keySet();

        for (Double key : diffKeys) {
            System.out
                    .println("Key = " + key + "\t\tValue = " + diffs.get(key));
        }

        System.out.println("********* RATIO ******************");

        Set<Double> ratioKeys = ratios.keySet();

        for (Double key : ratioKeys) {
            System.out
                    .println("Key = " + key + "\t\tValue = " + ratios.get(key));
        }

        System.out.println("********* QPE ******************");

        Set<Double> qpeKeys = qpes.keySet();

        for (Double d : qpeKeys) {
            System.out.println("Key = " + d + "\t\tValue = " + qpes.get(d));
        }

        System.out.println("********* QPF ******************");

        Set<Double> qpfKeys = qpfs.keySet();

        for (Double d : qpfKeys) {
            System.out.println("Key = " + d + "\t\tValue = " + qpfs.get(d));
        }

        System.out.println("********* RATE ******************");

        Set<Double> rateKeys = rates.keySet();

        for (Double d : rateKeys) {
            System.out.println("Key = " + d + "\t\tValue = " + rates.get(d));
        }

        System.out.println("********* GUID ******************");

        Set<Double> guidKeys = guids.keySet();

        for (Double d : guidKeys) {
            System.out.println("Key = " + d + "\t\tValue = " + guids.get(d));
        }

        System.out.println("********* VIRTUAL ******************");

        Set<Double> virtKeys = virtuals.keySet();

        for (Double d : virtKeys) {
            System.out.println("Key = " + d + "\t\tValue = " + virtuals.get(d));
        }
    }

    /**
     * @param ffmpBasin
     *            the ffmpBasin to set
     */
    public void setFfmpBasin(FFMPBasin ffmpBasin) {
        this.ffmpBasin = ffmpBasin;
    }

    /**
     * @return the ffmpBasin
     */
    public FFMPBasin getFfmpBasin() {
        return ffmpBasin;
    }

    public Float getQpfValue() {
        return qpfValue;
    }

    public void setQpfValue(Float qpfValue) {
        this.qpfValue = qpfValue;
    }

    /**
     * Find the maximum value of the data for the graph
     *
     * @return Double
     */
    public Double getMaximumValue() {

        Double max = 0.0;

        for (Double d : qpes.values()) {
            if (d > max) {
                max = d;
            }
        }

        for (Double d : qpfs.values()) {
            if (d > max) {
                max = d;
            }
        }

        for (Double d : rates.values()) {
            if (d > max) {
                max = d;
            }
        }

        for (Double d : guids.values()) {
            if (d > max) {
                max = d;
            }
        }

        return max;
    }

}
