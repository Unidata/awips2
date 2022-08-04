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

package com.raytheon.uf.edex.registry.ebxml.services.lifecycle;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

/**
 *
 * Delegate class to redirect queued calls to the actual implementation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------------------------------------
 * Jul 18  2018  7239     ksunil    Initial implementation
 *
 * </pre>
 *
 * @author ksunil
 */

@Transactional(propagation = Propagation.REQUIRED)
public class LifecycleManagerQueueDelegate {

    private final LifecycleManager lcm;

    /** The logger */
    private static final Logger statusHandler = LoggerFactory
            .getLogger(LifecycleManagerQueueDelegate.class);

    public LifecycleManagerQueueDelegate(LifecycleManager lifecycleManager) {
        if (lifecycleManager == null) {
            statusHandler.error("Found NULL LifecycleManager object");
            throw new RuntimeException(
                    "Found NULL LifecycleManager object in constructor");
        }

        this.lcm = lifecycleManager;
    }

    /**
     * The call to process request in an asynchronous/queued fashion is
     * delegated to this bean by the route. This code, then calls the existing
     * code in LifecycleManager.
     *
     * @param request
     * @return void
     *
     * @throws MsgRegistryException
     */
    public void processObjectsQueued(RegistryRequestType request)
            throws MsgRegistryException {

        long t0 = TimeUtil.currentTimeMillis();
        int size = 0;

        if (request instanceof RemoveObjectsRequest) {
            RemoveObjectsRequest rRequest = (RemoveObjectsRequest) request;

            if (rRequest.getObjectRefList() != null
                    && !CollectionUtil.isNullOrEmpty(
                            rRequest.getObjectRefList().getObjectRef())) {
                size = rRequest.getObjectRefList().getObjectRef().size();
            }

            lcm.removeObjects(rRequest);
        } else if (request instanceof SubmitObjectsRequest) {
            SubmitObjectsRequest sRequest = (SubmitObjectsRequest) request;

            List<RegistryObjectType> objs = sRequest.getRegistryObjectList()
                    .getRegistryObject();
            size = objs.size();

            lcm.submitObjects(sRequest);
        }

        long t1 = TimeUtil.currentTimeMillis();
        statusHandler.info("{} registry objects processed in queued mode in {}",
                size, TimeUtil.prettyDuration(t1 - t0));
    }

}
