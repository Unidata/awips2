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

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2008            mpduff      Initial creation
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LocPDC {
    /**
     * Location ID.
     */
    private String lid = null;

    /**
     * Name.
     */
    private String name = null;

    /**
     * Latitude.
     */
    private double lat;

    /**
     * Longitude.
     */
    private double lon;

    /**
     * HSA.
     */
    private String hsa = null;

    private long post;

    /**
     * Elevation.
     */
    private double elev;

    /**
     * Primary Physical Element.
     */
    private String primaryPe = null;

    /**
     * Flood Stage.
     */
    private double fs;

    /**
     * Flood Flow.
     */
    private double fq;

    /**
     * Display Class.
     */
    private String dispClass = null;

    /**
     * 
     */
    private boolean dcp = false;

    private boolean observer = false;

    /**
     * Telemetry Type.
     */
    private String telemType = null;

    public LocPDC() {

    }

    public LocPDC(Object[] data) {
        int i = 0;
        if (data[i] != null) { // LID
            setLid((String) data[i]);
        }

        if (data[++i] != null) { // name
            setName((String) data[i]);
        }

        if (data[++i] != null) { // lat
            setLat((Double) data[i]);
        }

        if (data[++i] != null) { // lon
            setLon((Double) data[i]);
        }

        if (data[++i] != null) { // hsa
            setHsa((String) data[i]);
        }

        if (data[++i] != null) { // post
            setPost(((Number) data[i]).intValue());
        }

        if (data[++i] != null) { // elev
            setElev((Double) data[i]);
        }

        if (data[++i] != null) { // primary_pe
            setPrimaryPe((String) data[i]);
        }

        if (data[++i] != null) { // fs
            setFs((Double) data[i]);
        }

        if (data[++i] != null) { // fq
            setFq((Double) data[i]);
        }
        // disp_class, is_dcp, is_observer, telem_type

        if (data[++i] != null) { // disp_class
            setDispClass((String) data[i]);
        }

        if (data[++i] != null) {
            if (((String) data[i]).equalsIgnoreCase("F")) {
                setDcp(false);
            } else {
                setDcp(true);
            }
        }

        if (data[++i] != null) {
            if (((String) data[i]).equalsIgnoreCase("F")) {
                setObserver(false);
            } else {
                setObserver(true);
            }
        }

        if (data[++i] != null) { // Telem_type
            setTelemType((String) data[i]);
        }
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
     * @return the post
     */
    public long getPost() {
        return post;
    }

    /**
     * @param post
     *            the post to set
     */
    public void setPost(long post) {
        this.post = post;
    }

    /**
     * @return the elev
     */
    public double getElev() {
        return elev;
    }

    /**
     * @param elev
     *            the elev to set
     */
    public void setElev(double elev) {
        this.elev = elev;
    }

    /**
     * @return the primaryPe
     */
    public String getPrimaryPe() {
        return primaryPe;
    }

    /**
     * @param primaryPe
     *            the primaryPe to set
     */
    public void setPrimaryPe(String primaryPe) {
        this.primaryPe = primaryPe;
    }

    /**
     * @return the fs
     */
    public double getFs() {
        return fs;
    }

    /**
     * @param fs
     *            the fs to set
     */
    public void setFs(double fs) {
        this.fs = fs;
    }

    /**
     * @return the fq
     */
    public double getFq() {
        return fq;
    }

    /**
     * @param fq
     *            the fq to set
     */
    public void setFq(double fq) {
        this.fq = fq;
    }

    /**
     * @return the dispClass
     */
    public String getDispClass() {
        return dispClass;
    }

    /**
     * @param dispClass
     *            the dispClass to set
     */
    public void setDispClass(String dispClass) {
        this.dispClass = dispClass;
    }

    /**
     * @return the dcp
     */
    public boolean isDcp() {
        return dcp;
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
    public boolean isObserver() {
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
}
