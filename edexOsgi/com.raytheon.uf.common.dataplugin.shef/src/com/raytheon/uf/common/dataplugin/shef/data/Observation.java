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
package com.raytheon.uf.common.dataplugin.shef.data;

import java.util.Date;

/**
 * Generic POJO that multiple IHFS tables can be mapped to.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2008            mpduff      Initial creation
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * May 23, 2016 5590       bkowal      Relocated to a common plugin for use
 *                                     in both EDEX/CAVE. Cleanup.
 * Jun 24, 2016 5699       bkowal      Removed version tag.
 * Jun 29, 2016 5699       bkowal      Added {@link #producttime}.
 * </pre>
 * 
 * @author mpduff
 */

public class Observation {

    /**
     * Location ID.
     */
    private String lid;

    /**
     * Physical Element
     */
    private String pe;

    /**
     * Duration
     */
    private int dur;

    /**
     * Type Source
     */
    private String ts;

    /**
     * Extremum
     */
    private String extremum;

    /**
     * Obstime
     */
    private Date obstime;

    /**
     * Basistime
     */
    private Date basisTime;

    /**
     * Data value
     */
    private Double value;

    /**
     * Shef Quality Code
     */
    private String shefQualCode;

    /**
     * Quality Code
     */
    private Integer qualityCode;

    /**
     * Revision
     */
    private int revision;

    /**
     * Product ID
     */
    private String productId;

    /**
     * The Product Time. Note, this field will not be set by the
     * {@link Observation#Observation(Object[])} constructor.
     */
    private Date producttime;

    public Observation() {
    }

    public Observation(Object[] oa) {
        setLid((String) oa[0]);
        setPe((String) oa[1]);
        setDur(((Number) oa[2]).intValue());
        setTs((String) oa[3]);
        setExtremum((String) oa[4]);
        setObstime((Date) oa[5]);
        setValue((Double) oa[6]);
        if (oa[7] != null) {
            setShefQualCode((String) oa[7]);
        }
        if (oa[8] != null) {
            setQualityCode(((Number) oa[8]).intValue());
        }
        if (oa[9] != null) {
            setRevision(((Number) oa[9]).intValue());
        }
        if (oa[10] != null) {
            setProductId((String) oa[10]);
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
     * @return the obstime
     */
    public Date getObstime() {
        return obstime;
    }

    /**
     * @param obstime
     *            the obstime to set
     */
    public void setObstime(Date obstime) {
        this.obstime = obstime;
    }

    /**
     * @return the value
     */
    public Double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(Double value) {
        this.value = value;
    }

    /**
     * @return the shefQualCode
     */
    public String getShefQualCode() {
        return shefQualCode;
    }

    /**
     * @param shefQualCode
     *            the shefQualCode to set
     */
    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

    /**
     * @return the qualityCode
     */
    public Integer getQualityCode() {
        return qualityCode;
    }

    /**
     * @param qualityCode
     *            the qualityCode to set
     */
    public void setQualityCode(Integer qualityCode) {
        this.qualityCode = qualityCode;
    }

    /**
     * @return the revision
     */
    public int getRevision() {
        return revision;
    }

    /**
     * @param revision
     *            the revision to set
     */
    public void setRevision(int revision) {
        this.revision = revision;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    public Date getProducttime() {
        return producttime;
    }

    public void setProducttime(Date producttime) {
        this.producttime = producttime;
    }

    /**
     * @return the basisTime
     */
    public Date getBasisTime() {
        return basisTime;
    }

    /**
     * @param basisTime
     *            the basisTime to set
     */
    public void setBasisTime(Date basisTime) {
        this.basisTime = basisTime;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Observation [");
        sb.append("lid=").append(lid);
        sb.append(", pe=").append(pe);
        sb.append(", dur=").append(dur);
        sb.append(", ts=").append(ts);
        sb.append(", extremum=").append(extremum);
        sb.append(", obstime=").append(obstime);
        sb.append(", basisTime=").append(basisTime);
        sb.append(", value=").append(value);
        sb.append(", shefQualCode=").append(shefQualCode);
        sb.append(", qualityCode=").append(qualityCode);
        sb.append(", revision=").append(revision);
        sb.append(", productId=").append(productId);
        if (producttime != null) {
            sb.append(", producttime=").append(producttime);
        }
        sb.append("]");
        return sb.toString();
    }
}