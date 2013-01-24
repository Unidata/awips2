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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractBaseDataNode extends AbstractRequestableNode {

    public AbstractBaseDataNode() {
        super();
    }

    public AbstractBaseDataNode(Level level) {
        super(level);
    }

    public AbstractBaseDataNode(LevelNode that) {
        super(that);
    }

    /**
     * Optional method to provide a request to the AvailabilityContainer so that
     * it can bulk all requests to edex to save time. The result of the request
     * will be passed to getAvailability. If the return value is null then the
     * response passed to getAvailability will be null also.
     * 
     * @param originalConstraints
     *            the original constraints passed to the data cube container
     * 
     * @return
     */
    public abstract DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints);

    /**
     * Optional method to provide a request to the DataContainer so that it can
     * bulk all requests to edex to save time. The result of the request will be
     * passed to getData. If the return value is null then the response passed
     * to getData will be null also.
     * 
     * 
     * @param originalConstraints
     *            the original constraints passed to the data cube container
     * @param availability
     *            the TimeAndSpace objects where data is being requested
     * @return
     */
    public abstract DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> availability);

    /**
     * Determine where and when data is available for this node.
     * 
     * @param originalConstraints
     *            the original constraints passed to the data cube container
     * @param response
     *            if getAvailabilityRequest() returned a request then this will
     *            have the response to that request.
     * @return the TimeAndSpace when this node has available data.
     * @throws VizException
     */
    public abstract Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws VizException;

    /**
     * Get data from this node.
     * 
     * @param originalConstraints
     *            the original constraints passed to the data cube container
     * @param availability
     *            the TimeAndSpace objects where data is being requested
     * @param response
     *            response to the server request returned by getDataRequest.
     * @return
     */
    public abstract Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws VizException;

    @Override
    public boolean isConstant() {
        return false;
    }

}
