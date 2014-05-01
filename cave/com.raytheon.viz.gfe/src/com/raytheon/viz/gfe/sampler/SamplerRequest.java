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
 * Aug 9, 2012  #1036       dgilling    Fixed NullPointerException
 *                                      in compareTo().
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
        if ((_areaID != null) && (!_areaID.getName().isEmpty())) {
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
        if ((_area != null) && (!_area.getId().getName().isEmpty())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(SamplerRequest samp) {
        // 1st level - parmID
        int pid = _parmID.compareTo(samp._parmID);
        if (pid != 0) {
            return pid;
        } else {
            // 2nd level, time range
            int tr = _timeRange.compareTo(samp._timeRange);
            if (tr != 0) {
                return tr;
            } else {
                // 3rd level, reference id
                String left = (_area != null) ? _area.getId().getName()
                        : _areaID.getName();
                String right = (samp._area != null) ? samp._area.getId()
                        .getName() : samp._areaID.getName();
                return left.compareTo(right);
            }
        }
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
        SamplerRequest other = (SamplerRequest) obj;
        if (_area == null) {
            if (other._area != null) {
                return false;
            }
        } else if (!_area.equals(other._area)) {
            return false;
        }
        if (_areaID == null) {
            if (other._areaID != null) {
                return false;
            }
        } else if (!_areaID.equals(other._areaID)) {
            return false;
        }
        if (_parmID == null) {
            if (other._parmID != null) {
                return false;
            }
        } else if (!_parmID.equals(other._parmID)) {
            return false;
        }
        if (_timeRange == null) {
            if (other._timeRange != null) {
                return false;
            }
        } else if (!_timeRange.equals(other._timeRange)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SamplerRequest (");
        builder.append(_parmID);
        builder.append(", ");
        builder.append(_timeRange);
        builder.append(", ");
        if (_areaID != null) {
            builder.append(_areaID.getName());
        } else {
            builder.append("null");
        }
        builder.append(", ");
        if (_area != null) {
            builder.append(_area.getId().getName());
        } else {
            builder.append("null");
        }
        builder.append(")");
        return builder.toString();
    }
}
