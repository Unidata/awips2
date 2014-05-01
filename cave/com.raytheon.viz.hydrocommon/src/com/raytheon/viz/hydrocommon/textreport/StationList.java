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
package com.raytheon.viz.hydrocommon.textreport;

/**
 * Data object for the Station List Report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2009  2260       mpduff       Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StationList {
    private String lid = null;

    private String name = null;

    private String county = null;

    private String rb = null;

    private String wfo = null;

    private String lastname = null;

    private String hPhone = null;

    private String oPhone = null;

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
     * @return the county
     */
    public String getCounty() {
        return county;
    }

    /**
     * @param county
     *            the county to set
     */
    public void setCounty(String county) {
        this.county = county;
    }

    /**
     * @return the rb
     */
    public String getRb() {
        return rb;
    }

    /**
     * @param rb
     *            the rb to set
     */
    public void setRb(String rb) {
        this.rb = rb;
    }

    /**
     * @return the wfo
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * @param wfo
     *            the wfo to set
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * @return the lastname
     */
    public String getLastname() {
        return lastname;
    }

    /**
     * @param lastname
     *            the lastname to set
     */
    public void setLastname(String lastname) {
        this.lastname = lastname;
    }

    /**
     * @return the hPhone
     */
    public String getHPhone() {
        return hPhone;
    }

    /**
     * @param phone
     *            the hPhone to set
     */
    public void setHPhone(String phone) {
        hPhone = phone;
    }

    /**
     * @return the oPhone
     */
    public String getOPhone() {
        return oPhone;
    }

    /**
     * @param phone
     *            the oPhone to set
     */
    public void setOPhone(String phone) {
        oPhone = phone;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append("Station List Data:\n");
        sb.append("  LID = " + getLid());
        sb.append("\n  Location = " + getName());
        sb.append("\n  County = " + getCounty());
        sb.append("\n  Basin = " + getRb());
        sb.append("\n  WFO = " + getWfo());
        sb.append("\n  Observer = " + getLastname());
        sb.append("\n  Home Phone = " + getHPhone());
        sb.append("\n  Office Phone = " + getOPhone());
        sb.append("\n\n");
        
        return sb.toString();
    }
}
