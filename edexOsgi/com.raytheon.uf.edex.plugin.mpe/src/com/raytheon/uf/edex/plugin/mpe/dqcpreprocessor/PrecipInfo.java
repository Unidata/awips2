package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Precipitation Information.
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
public class PrecipInfo {
    protected String lid = null;

    protected char source;

    protected List<String> pPPQPE = new ArrayList<>();

    private double lat = Double.NaN;

    private double lon = Double.NaN;

    /* map day to value */
    protected Map<Integer, Double> pPPD = new HashMap<>();

    /* map day to list of */
    protected Map<Integer, List<Double>> pPPQ = new HashMap<>();

    public PrecipInfo() {
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

    public List<String> getpPPQPE() {
        return pPPQPE;
    }

    public void setpPPQPE(List<String> pPPQPE) {
        this.pPPQPE = pPPQPE;
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

    public Map<Integer, Double> getpPPD() {
        return pPPD;
    }

    public void setpPPD(Map<Integer, Double> pPPD) {
        this.pPPD = pPPD;
    }

    public Map<Integer, List<Double>> getpPPQ() {
        return pPPQ;
    }

    public void setpPPQ(Map<Integer, List<Double>> pPPQ) {
        this.pPPQ = pPPQ;
    }

    public String toString() {
        return getLid();
    }

}
