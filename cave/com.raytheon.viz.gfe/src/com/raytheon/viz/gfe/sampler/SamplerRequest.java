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

package com.raytheon.viz.gfe.sampler;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Contains the request for getting data from the samplers
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 3, 2008	1167		mnash	    Initial creation
 * Sep 3, 2008  1283        njensen     Fixed issues
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 * 
 */
public class SamplerRequest implements Comparable<SamplerRequest> {

    private TimeRange _timeRange;

    private ReferenceID _areaID;

    private ReferenceData _area;

    private ParmID _parmID;

    /**
     * Default Constructor
     */
    public SamplerRequest() {
    }

    /**
     * Constructor taking the parmID, area, and time range stores all this data
     * 
     * @param parmID
     * @param area
     * @param timeRange
     */
    public SamplerRequest(ParmID parmID, ReferenceData area, TimeRange timeRange) {
        _parmID = parmID;
        _area = area;
        _timeRange = timeRange;
    }

    /**
     * copy Constructor storing the data
     * 
     * @param rhs
     */
    public SamplerRequest(final SamplerRequest rhs) {
        _parmID = rhs._parmID;
        _area = rhs._area;
        _areaID = rhs._areaID;
        _timeRange = rhs._timeRange;
    }

    /**
     * Constructor taking the parmID, areadID, and time range storing the data
     * 
     * @param parmID
     * @param areaID
     * @param timeRange
     */
    public SamplerRequest(ParmID parmID, ReferenceID areaID, TimeRange timeRange) {
        _parmID = parmID;
        _areaID = areaID;
        _timeRange = timeRange;
    }

    /**
     * Returns the reference data associated with this request, _area
     * 
     * @return
     */
    public final ReferenceData area() {
        return _area;
    }

    /**
     * Returns the reference id associated with this request, _areaID
     * 
     * @return
     */
    public final ReferenceID areaID() {
        return _areaID;
    }

    /**
     * Returns the time range associated with this request, _timeRange
     * 
     * @return
     */
    public final TimeRange timeRange() {
        return _timeRange;
    }

    /**
     * Returns the parameter identifier for this SamplerRequest
     * 
     * @return
     */
    public final ParmID parmID() {
        return _parmID;
    }

    public final void setParmID(ParmID parm) {
        _parmID = parm;
    }

    /**
     * Returns true if the area specified is a ReferenceID
     * 
     * @return
     */
    public boolean isRefID() {
        if (_areaID != null && _areaID.getName().length() > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns true if the area specified is ReferenceData
     * 
     * @return
     */
    public boolean isRefArea() {
        if (_area.getId().getName().length() > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Outputs the class information
     * 
     * @param o
     */
    public String printOn() {
        return "(" + _parmID + "," + _timeRange + "," + _areaID + "," + _area
                + ")";
    }

    @Override
    public int compareTo(SamplerRequest samp) {
        int pid = _parmID.compareTo(samp._parmID);
        if (pid == -1) {
            return -1;
        } else if (pid == 1) {
            return 1;
        } else if (pid == 0) {
            int tr = _timeRange.compareTo(samp._timeRange);
            if (tr == -1) {
                return -1;
            } else if (tr == 1) {
                return 1;
            } else if (tr == 0) {
                int rid = _area.getId().getName().compareTo(
                        samp._area.getId().getName());
                if (rid == -1) {
                    return -1;
                } else if (rid == 1) {
                    return 1;
                } else if (rid == 0) {
                    return 0;
                }
            }
        }
        return 0; // should never reach this
    }

    public String toString() {
        String s = "SamplerRequest: " + _parmID.getParmName() + " "
                + _timeRange.toString() + " ";
        if (_area != null) {
            s += _area.getId().getName();
        } else if (_areaID != null) {
            s += _areaID.getName();
        }
        return s;
    }
}
