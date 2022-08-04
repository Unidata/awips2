package com.raytheon.viz.hydro.timeseries.util;

/**
 * Metadata container for a time series graph trace
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jun 27, 2018  6748        randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public class TraceInfo {

    private String name = null;

    private String lid = null;

    private String pe = null;

    private int dur = 0;

    private String ts = null;

    private String extremum = null;

    private String cdur = null;

    private String colorName = null;

    private boolean forecast = false;

    /**
     * Default Constructor
     */
    public TraceInfo() {
    }

    /**
     * Copy Constructor
     *
     * @param other
     *            TraceInfo object to be copied
     */
    public TraceInfo(TraceInfo other) {
        this.name = other.name;
        this.lid = other.lid;
        this.pe = other.pe;
        this.dur = other.dur;
        this.ts = other.ts;
        this.extremum = other.extremum;
        this.cdur = other.cdur;
        this.colorName = other.colorName;
        this.forecast = other.forecast;
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
     * @return the dur
     */
    public int getDur() {
        return dur;
    }

    /**
     * @param dur
     *            the dur to set
     */
    public void setDur(int dur) {
        this.dur = dur;
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
     * @return the extremum
     */
    public String getExtremum() {
        return extremum;
    }

    /**
     * @param extremum
     *            the extremum to set
     */
    public void setExtremum(String extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the cdur
     */
    public String getCdur() {
        return cdur;
    }

    /**
     * @param cdur
     *            the cdur to set
     */
    public void setCdur(String cdur) {
        this.cdur = cdur;
    }

    /**
     * @return the colorName
     */
    public String getColorName() {
        return colorName;
    }

    /**
     * @param colorName
     *            the colorName to set
     */
    public void setColorName(String colorName) {
        this.colorName = colorName;
    }

    /**
     * @return the forecast
     */
    public boolean isForecast() {
        return forecast;
    }

    /**
     * @param forecast
     *            the forecast to set
     */
    public void setForecast(boolean forecast) {
        this.forecast = forecast;
    }

    /**
     * Set the PEDTSE values from a single string
     *
     * @param pc
     *            the PEDTSE values as a String
     */
    public void setPc(String pc) {
        setPe(pc.substring(0, 2));
        setDur(TimeSeriesUtil.convertDur2Int(pc.charAt(2)));
        setTs(pc.substring(3, 5));
        setExtremum(pc.substring(5));

        // Set forecast if ts is F or C
        this.setForecast(false);
        if (getTs().startsWith("F") || getTs().startsWith("f")
                || getTs().startsWith("C") || getTs().startsWith("c")) {
            this.setForecast(true);
        }
    }

    /**
     * @return the pe dur ts andextremum formatted as a string
     */
    public String getPEDTSE() {
        return String.format("%s %d %s %s", getPe(), getDur(), getTs(),
                getExtremum()).toUpperCase();
    }

}
