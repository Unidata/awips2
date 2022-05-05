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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------------
 * Aug 05, 2008           mpduff    Initial creation
 * Mar 17, 2016  5483     randerso  Updated for use in reworked TimeSeriesDlg
 * Apr 12, 2018  6619     randerso  Changed dur to int
 *
 * </pre>
 *
 * @author mpduff
 */

public class LIDData {
    private String pe = null;

    private int dur;

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
    public LIDData(String pe, int dur) {
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
    public int getDur() {
        return dur;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + dur;
        result = prime * result + ((pe == null) ? 0 : pe.hashCode());
        return result;
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
        if (dur != other.dur) {
            return false;
        }
        if (pe == null) {
            if (other.pe != null) {
                return false;
            }
        } else if (!pe.equals(other.pe)) {
            return false;
        }
        return true;
    }

}
