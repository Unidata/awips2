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
package com.raytheon.uf.edex.dataaccess.handler;

import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.request.GetRequiredIdentifiersRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for <code>GetRequiredIdentifiersRequest</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2014 3185       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetRequiredIdentifiersHandler implements
        IRequestHandler<GetRequiredIdentifiersRequest> {

    @Override
    public String[] handleRequest(GetRequiredIdentifiersRequest request)
            throws Exception {
        return DataAccessLayer.getRequiredIdentifiers(request.getDatatype());
    }

}
