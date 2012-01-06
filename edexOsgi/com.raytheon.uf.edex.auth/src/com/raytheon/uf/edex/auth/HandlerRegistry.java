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
package com.raytheon.uf.edex.auth;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.util.registry.GenericRegistry;

/**
 * Class used for registering request handlers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class HandlerRegistry extends GenericRegistry<String, Object> {

    private static HandlerRegistry instance = new HandlerRegistry();

    static class DefaultHandler implements IRequestHandler<IServerRequest> {
        @Override
        public Object handleRequest(IServerRequest request) throws Exception {
            throw new ClassNotFoundException("No handler registered for type: "
                    + request.getClass().getCanonicalName());
        }
    }

    private DefaultHandler defaultHandler;

    public static HandlerRegistry getInstance() {
        return instance;
    }

    private HandlerRegistry() {
        super();
        defaultHandler = new DefaultHandler();
    }

    public IRequestHandler<?> getRequestHandler(String object) {
        Object obj = super.getRegisteredObject(object);
        if (obj instanceof IRequestHandler<?>) {
            return (IRequestHandler<?>) obj;
        }
        return defaultHandler;
    }

}
