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
package com.raytheon.uf.edex.management.handler;

import com.raytheon.uf.common.management.request.ChangeContextRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IContextAdmin;

/**
 * Adjust a context as requested
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ChangeContextHandler implements
        IRequestHandler<ChangeContextRequest> {

    @Override
    public Object handleRequest(ChangeContextRequest request) throws Exception {
        IContextAdmin admin = EDEXUtil.getContextAdmin();
        String name = request.getContextName();
        switch (request.getAction()) {
        case RESTART:
            admin.stopContext(name);
            admin.startContext(name);
            break;
        case START:
            admin.startContext(name);
            break;
        case STOP:
            admin.stopContext(name);
            break;
        }

        return null; // TODO
    }

}
