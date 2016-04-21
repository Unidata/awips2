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
import java.util.HashMap;
import java.util.Map;

/**
 * Data object holding a site's precip data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SitePrecipData {
    /** List of TS available for this site */
    private ArrayList<String> tsList = new ArrayList<String>();

    /** Map containing the data for each TS */
    private Map<String, ArrayList<PrecipRecord>> dataMap = new HashMap<String, ArrayList<PrecipRecord>>();

    /** Location ID */
    private String lid = null;

    /** Physical Element */
    private String pe = null;

    public SitePrecipData() {

    }

    public SitePrecipData(String lid) {
        this.lid = lid;
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
     * @return the tsList
     */
    public ArrayList<String> getTsList() {
        return tsList;
    }

    /**
     * Add a TS.
     * 
     * @param ts
     * @param count
     */
    public void addTs(String ts) {
        tsList.add(ts);
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
     * Get the number if TS in the object.
     * 
     * @return int number of TS items in the object
     */
    public int getTsCount() {
        return tsList.size();
    }
    
    /**
     * Add a data set.
     * 
     * @param ts
     *      The type source
     * @param data
     *      The data
     */
    public void addData(String ts, ArrayList<PrecipRecord> data) {
        dataMap.put(ts, data);
    }
    
    /**
     * Get the list of data for the TS passed in.
     * 
     * @param ts
     *      The Type Source
     * @return
     *      The data for the Type Source
     */
    public ArrayList<PrecipRecord> getData(String ts) {
        return dataMap.get(ts);
    }
}
