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
package com.raytheon.viz.hydro.pointdatacontrol.data;

import java.util.ArrayList;
import java.util.Calendar;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StationEntryDetails {
    private String lid = null;

    private String hsa = null;

    private String pe = null;

    private String shefDur = null;

    private String ts = null;

    private String ex = null;

    private double elevation;

    private double lon;

    private double lat;

    private boolean fcstPt;

    private String name = null;

    private boolean dcp;

    private boolean observer;

    private String telemType = null;

    private ArrayList<Double> valueArray = new ArrayList<Double>();

    private int valueCount;

    private Calendar StartTime;

    private int incrTime;

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the hsa
     */
    public String getHsa() {
        return hsa;
    }

    /**
     * @param hsa
     *            the hsa to set
     */
    public void setHsa(String hsa) {
        this.hsa = hsa;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the shefDur
     */
    public String getShefDur() {
        return shefDur;
    }

    /**
     * @param shefDur
     *            the shefDur to set
     */
    public void setShefDur(String shefDur) {
        this.shefDur = shefDur;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the ex
     */
    public String getEx() {
        return ex;
    }

    /**
     * @param ex
     *            the ex to set
     */
    public void setEx(String ex) {
        this.ex = ex;
    }

    /**
     * @return the elevation
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the lon
     */
    public double getLon() {
        return lon;
    }

    /**
     * @param lon
     *            the lon to set
     */
    public void setLon(double lon) {
        this.lon = lon;
    }

    /**
     * @return the lat
     */
    public double getLat() {
        return lat;
    }

    /**
     * @param lat
     *            the lat to set
     */
    public void setLat(double lat) {
        this.lat = lat;
    }

    /**
     * @return the fcstPt
     */
    public boolean getFcstPt() {
        return fcstPt;
    }

    /**
     * @param fcstPt
     *            the fcstPt to set (T or F)
     */
    public void setFcstPt(String fcstPt) {
        if (fcstPt.equalsIgnoreCase("T")) {
            this.fcstPt = true;
        } else {
            this.fcstPt = false;
        }
    }
    
    /**
     * @param fcstPt the fcstPt to set
     */
    public void setFcstPt(boolean fcstPt) {
        this.fcstPt = fcstPt;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the dcp
     */
    public boolean getDcp() {
        return dcp;
    }

    /**
     * @param dcp
     *            the dcp to set (T or F)
     */
    public void setDcp(String dcp) {
        if (dcp.equalsIgnoreCase("T")) {
            this.dcp = true;
        } else {
            this.dcp = false;
        }
    }

    /**
     * @param dcp
     *            the dcp to set
     */
    public void setDcp(boolean dcp) {
            this.dcp = dcp;
    }
    
    /**
     * @return the observer
     */
    public boolean getObserver() {
        return observer;
    }

    /**
     * @param observer
     *            the observer to set
     */
    public void setObserver(boolean observer) {
        this.observer = observer;
    }

    /**
     * @param observer
     *            the observer to set (T or F)
     */
    public void setObserver(String observer) {
        if (observer.equalsIgnoreCase("T")) {
            this.observer = true;
        } else {
            this.observer = false;
        }
    }

    /**
     * @return the telemType
     */
    public String getTelemType() {
        return telemType;
    }

    /**
     * @param telemType
     *            the telemType to set
     */
    public void setTelemType(String telemType) {
        this.telemType = telemType;
    }

    /**
     * @return the valueArray
     */
    public ArrayList<Double> getValueArray() {
        return valueArray;
    }

    /**
     * @param valueArray
     *            the valueArray to set
     */
    public void setValueArray(ArrayList<Double> valueArray) {
        this.valueArray = valueArray;
    }
    
    /**
     * Add a value to the valueArray ArrayList.
     * @param value
     *    The value to add to the array
     */
    public void addValue(double value) {
        valueArray.add(value);
    }
    
    /**
     * Reset the valueArray ArrayList.
     */
    public void resetValueArray() {
        valueArray = new ArrayList<Double>();
    }

    /**
     * @return the valueCount
     */
    public int getValueCount() {
        return valueCount;
    }

    /**
     * @param valueCount
     *            the valueCount to set
     */
    public void setValueCount(int valueCount) {
        this.valueCount = valueCount;
    }

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return StartTime;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Calendar startTime) {
        StartTime = startTime;
    }

    /**
     * @return the incrTime in seconds
     */
    public int getIncrTime() {
        return incrTime;
    }

    /**
     * @param incrTime
     *            the incrTime to set in seconds
     */
    public void setIncrTime(int incrTime) {
        this.incrTime = incrTime;
    }

    /**
     * Return the contents of this object as a String.
     * 
     * @see java.lang.Object#toString()
     * @return String
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("\n****** Station Entry Details ******");
        sb.append("\nLID:  " + getLid());
        sb.append("\nDCP:  " + getDcp());
        sb.append("\nElevation:  " + getElevation());
        sb.append("\nEx:  " + getEx());
        sb.append("\nHSA:  " + getHsa());
        sb.append("\nIncr Time:  " + getIncrTime());
        sb.append("\nLatitude:  " + getLat());
        sb.append("\nLongitude:  " + getLon());
        sb.append("\nName:  " + getName());
        sb.append("\nPE:  " + getPe());
        sb.append("\nSHEF Dur:  " + getShefDur());
        sb.append("\nTelm Type:  " + getTelemType());
        sb.append("\nTS:  " + getTs());
        
        return sb.toString();
    }

    
}
