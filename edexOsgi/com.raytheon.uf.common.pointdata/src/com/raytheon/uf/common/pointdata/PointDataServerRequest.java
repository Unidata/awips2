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

package com.raytheon.uf.common.pointdata;

import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A message for querying pointdata
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 16, 2011  8070     ekladstrup  Initial creation
 * Nov 26, 2013  2537     bsteffen    Move common constants here.
 * 
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
@DynamicSerialize
public class PointDataServerRequest implements IServerRequest {

    public static final String REQUEST_PARAMETERS_KEY = "requestedParameters";

    public static final String REQUEST_MODE_KEY = "mode";

    public static final String REQUEST_MODE_2D = "select2d";

    public static final String REQUEST_MODE_PARAMETERS = "getParameters";

    // the information needed for a PointDataQuery object
    @DynamicSerializeElement
    private Map<String, RequestConstraint> rcMap;

    public PointDataServerRequest() {

    }

    public PointDataServerRequest(Map<String, RequestConstraint> map) {
        rcMap = map;
    }

    public void setRcMap(Map<String, RequestConstraint> map) {
        rcMap = map;
    }

    public Map<String, RequestConstraint> getRcMap() {
        return rcMap;
    }

    @Override
    public String toString() {
        return "PointDataServerRequest [rcMap=" + rcMap + "]";
    }

}
