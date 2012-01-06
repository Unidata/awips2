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
package com.raytheon.viz.hydro.timeseries.util;

/**
 * Bean to store Physical Element and Duration value.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2008            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class LIDData {
    private String pe = null;
    private String dur = null;
    
    /**
     * Default constructor
     */
    public LIDData() {
        
    }
    
    public LIDData(String pe, String dur) {
        this.pe = pe;
        this.dur = dur;
    }
    
    public void setData(String dataString) {
        String[] data = dataString.split("\\s+");
        pe = data[0];
        dur = data[3];
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the dur
     */
    public String getDur() {
        return dur;
    }

    /**
     * @param dur the dur to set
     */
    public void setDur(String dur) {
        this.dur = dur;
    }
}
