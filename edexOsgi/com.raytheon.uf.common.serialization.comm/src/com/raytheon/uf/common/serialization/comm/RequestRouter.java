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
package com.raytheon.uf.common.serialization.comm;

import java.util.Iterator;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class RequestRouter {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RequestRouter.class);
    private static final IRequestRouter requestRouter;

    static {
        // lookup router via service loader
        ServiceLoader<IRequestRouter> loader = ServiceLoader.load(
                IRequestRouter.class, RequestRouter.class.getClassLoader());

        Iterator<IRequestRouter> iter = loader.iterator();
        IRequestRouter tmp = null;
        try {
            if (iter.hasNext()) {
                tmp = iter.next();
            }
        } catch (ServiceConfigurationError e) {
            statusHandler.handle(
                    Priority.FATAL,
                    "No Service Handler found for "
                            + IRequestRouter.class.getName()
                            + ".  Unable to route requests to server.");
            throw e;
        }

        requestRouter = tmp;
    }

    public static Object route(IServerRequest request) throws Exception {
        return requestRouter.route(request);
    }
}
