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
package com.raytheon.uf.edex.plugin.text.dbsrv.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.request.GetPartialAfosIdRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.AfosToAwipsLookup;

/**
 * Request handler for GetPartialAfosIdRequests. Forwards request to
 * AfosToAwipsDao.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 19, 2009           avarani   Initial creation
 * Dec 09, 2015  5166     kbisanz   Update logging to use SLF4J.
 * Jan 18, 2016  4562     tjensen   Moved from edex.plugin.text to
 *                                  edex.plugin.text.dbsrv
 * Aug 09, 2016  5801     tgurney   Use AfosToAwipsLookup
 * 
 * </pre>
 * 
 * @author avarani
 */

public class GetPartialAfosIdHandler implements
        IRequestHandler<GetPartialAfosIdRequest> {

    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    @Override
    public AfosWmoIdDataContainer handleRequest(GetPartialAfosIdRequest request) {
        return AfosToAwipsLookup.lookupAfosId(request.getCccc(),
                request.getNnn(), request.getXxx());
    }
}
