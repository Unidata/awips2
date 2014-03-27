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
package com.raytheon.uf.common.dataquery.requests;

import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * IServerRequest object for making a sql or hql request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 16, 2011  8070     ekladstrup  Initial creation
 * Dec 18, 2013  2579     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
@DynamicSerialize
public class QlServerRequest implements IServerRequest {

    @DynamicSerializeElement
    private Map<String, RequestConstraint> rcMap;

    public QlServerRequest() {

    }

    public QlServerRequest(Map<String, RequestConstraint> rcMap) {
        this.rcMap = rcMap;
    }

    public Map<String, RequestConstraint> getRcMap() {
        return rcMap;
    }

    public void setRcMap(Map<String, RequestConstraint> map) {
        rcMap = map;
    }

    @Override
    public String toString() {
        return "QlServerRequest [rcMap=" + rcMap + "]";
    }

}
