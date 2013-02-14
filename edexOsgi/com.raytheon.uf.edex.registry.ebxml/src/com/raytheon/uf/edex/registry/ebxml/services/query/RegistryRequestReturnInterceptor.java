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
package com.raytheon.uf.edex.registry.ebxml.services.query;

import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.services.util.RegistrySessionManager;

/**
 * 
 * This code intercepts the response returning from the server so the Hibernate
 * session can be closed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RegistryRequestReturnInterceptor extends
        AbstractPhaseInterceptor<Message> {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryRequestReturnInterceptor.class);

    /**
     * Creates a new RegistryRequestReturnInterceptor inserted after the
     * response marshalling has taken place
     */
    public RegistryRequestReturnInterceptor() {
        super(Phase.POST_MARSHAL);
    }

    @Override
    public void handleMessage(Message arg0) throws Fault {
        RegistrySessionManager.closeSession();
        statusHandler.info("Hibernate Transactional Session Closed");
    }

}
