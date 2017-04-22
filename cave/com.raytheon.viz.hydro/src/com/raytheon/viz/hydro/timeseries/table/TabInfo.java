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
package com.raytheon.viz.hydro.timeseries.table;

import java.util.ArrayList;
import java.util.Calendar;

/**
 * Table data record object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2008 #1520      mpduff      Initial creation
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TabInfo {
    /** Number of items */
    private int nitems;
    
    /** The selected position */
    private int selectedPos;

    /** The beginning time */
    private Calendar beginTime;
    
    /** The ending time */
    private Calendar endTime;
    
    /** Location ID */
    private String lid = null;

    /** List of location names */
    private ArrayList<String> buffer = new ArrayList<String>();
    
    /** List of SiteInfo objects */
    private ArrayList<SiteInfo> infoList = new ArrayList<SiteInfo>();

    /**
     * @return the nitems
     */
    public int getNitems() {
        return nitems;
    }

    /**
     * @param nitems
     *            the nitems to set
     */
    public void setNitems(int nitems) {
        this.nitems = nitems;
    }

    /**
     * @return the selectedPos
     */
    public int isSelectedPos() {
        return selectedPos;
    }

    /**
     * @param selectedPos the selectedPos to set
     */
    public void setSelectedPos(int selectedPos) {
        this.selectedPos = selectedPos;
    }

    /**
     * @return the beginTime
     */
    public Calendar getBeginTime() {
        return beginTime;
    }

    /**
     * @param beginTime
     *            the beginTime to set
     */
    public void setBeginTime(Calendar beginTime) {
        this.beginTime = beginTime;
    }

    /**
     * @return the endTime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * @param endTime
     *            the endTime to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the buffer
     */
    public String[] getBuffer() {
        return buffer.toArray(new String[buffer.size()]);
    }

    /**
     * @param buffer
     *            the buffer to set
     */
    public void addBuffer(String buffer) {
        this.buffer.add(buffer);;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the infoList
     */
    public ArrayList<SiteInfo> getInfoList() {
        return infoList;
    }

    /**
     * @param infoList the infoList to set
     */
    public void setInfoList(ArrayList<SiteInfo> infoList) {
        this.infoList = infoList;
    }
    
    /**
     * @param siteInfo the site information to add
     */
    public void addSiteInfo(SiteInfo info) {
        infoList.add(info);
    }
    
    /**
     * @param siteInfo the site information to add
     */
    public SiteInfo getSiteInfo(int index) {
        return infoList.get(index);
    }    
}
