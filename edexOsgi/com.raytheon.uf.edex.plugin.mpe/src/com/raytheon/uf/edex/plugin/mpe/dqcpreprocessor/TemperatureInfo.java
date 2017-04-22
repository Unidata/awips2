package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Temperature Information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class TemperatureInfo {
    protected String lid;

    protected char source;

    protected char extremum;

    protected double lat = Double.NaN;

    protected double lon = Double.NaN;

    protected Map<Integer, List<Integer>> diffTime = new HashMap<>();

    protected Map<Integer, List<Double>> value = new HashMap<>();

    protected Map<Integer, List<Double>> maxHourlyValue = new HashMap<>();

    protected Map<Integer, List<Double>> minHourlyValue = new HashMap<>();

    public TemperatureInfo() {
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public char getSource() {
        return source;
    }

    public void setSource(char source) {
        this.source = source;
    }

    public char getExtremum() {
        return extremum;
    }

    public void setExtremum(char extremum) {
        this.extremum = extremum;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public Map<Integer, List<Integer>> getDiffTime() {
        return diffTime;
    }

    public void setDiffTime(Map<Integer, List<Integer>> diffTime) {
        this.diffTime = diffTime;
    }

    public Map<Integer, List<Double>> getValue() {
        return value;
    }

    public void setValue(Map<Integer, List<Double>> value) {
        this.value = value;
    }

    public Map<Integer, List<Double>> getMaxHourlyValue() {
        return maxHourlyValue;
    }

    public void setMaxHourlyValue(Map<Integer, List<Double>> maxHourlyValue) {
        this.maxHourlyValue = maxHourlyValue;
    }

    public Map<Integer, List<Double>> getMinHourlyValue() {
        return minHourlyValue;
    }

    public void setMinHourlyValue(Map<Integer, List<Double>> minHourlyValue) {
        this.minHourlyValue = minHourlyValue;
    }

    public String toString() {
        return lid;
    }

}
