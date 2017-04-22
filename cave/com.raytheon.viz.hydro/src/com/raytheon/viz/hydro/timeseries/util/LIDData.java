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
 * Aug 05, 2008            mpduff      Initial creation
 * Mar 17, 2016  #5483     randerso    Updated for use in reworked TimeSeriesDlg
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

    /**
     * Constructor
     * 
     * @param pe
     * @param dur
     */
    public LIDData(String pe, String dur) {
        this.pe = pe;
        this.dur = dur;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @return the dur
     */
    public String getDur() {
        return dur;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LIDData other = (LIDData) obj;
        if (dur == null) {
            if (other.dur != null) {
                return false;
            }
        } else if (!dur.equalsIgnoreCase(other.dur)) {
            return false;
        }
        if (pe == null) {
            if (other.pe != null) {
                return false;
            }
        } else if (!pe.equalsIgnoreCase(other.pe)) {
            return false;
        }
        return true;
    }

}
