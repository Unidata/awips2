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
package com.raytheon.viz.warngen.gis;

import java.util.Date;
import java.util.List;

/**
 * Simple POJO for a watch. The phenSig, action, etn, start time, and end time
 * make each watch unique similar to the VTEC.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2014 3419       jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class Watch {

    private String phenSig;

    private String action;

    private String etn;

    private Date startTime;

    private Date endTime;

    private List<String> areas;

    private String state;

    private List<String> partOfState;

    public Watch(String state, String action, String phenSig, String etn,
            Date startTime, Date endTime) {
        this.state = state;
        this.action = action;
        this.phenSig = phenSig;
        this.etn = etn;
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public String getPhenSig() {
        return phenSig;
    }

    public void setPhenSig(String phenSig) {
        this.phenSig = phenSig;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public List<String> getAreas() {
        return areas;
    }

    public void setAreas(List<String> areas) {
        this.areas = areas;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public List<String> getPartOfState() {
        return partOfState;
    }

    public void setPartOfState(List<String> partOfState) {
        this.partOfState = partOfState;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getEtn() {
        return etn;
    }

    public void setEtn(String etn) {
        this.etn = etn;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((action == null) ? 0 : action.hashCode());
        result = prime * result + ((endTime == null) ? 0 : endTime.hashCode());
        result = prime * result + ((etn == null) ? 0 : etn.hashCode());
        result = prime * result + ((phenSig == null) ? 0 : phenSig.hashCode());
        result = prime * result
                + ((startTime == null) ? 0 : startTime.hashCode());
        result = prime * result + ((state == null) ? 0 : state.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Watch other = (Watch) obj;
        if (action == null) {
            if (other.action != null)
                return false;
        } else if (!action.equals(other.action))
            return false;
        if (endTime == null) {
            if (other.endTime != null)
                return false;
        } else if (!endTime.equals(other.endTime))
            return false;
        if (etn == null) {
            if (other.etn != null)
                return false;
        } else if (!etn.equals(other.etn))
            return false;
        if (phenSig == null) {
            if (other.phenSig != null)
                return false;
        } else if (!phenSig.equals(other.phenSig))
            return false;
        if (startTime == null) {
            if (other.startTime != null)
                return false;
        } else if (!startTime.equals(other.startTime))
            return false;
        if (state == null) {
            if (other.state != null)
                return false;
        } else if (!state.equals(other.state))
            return false;
        return true;
    }

}
