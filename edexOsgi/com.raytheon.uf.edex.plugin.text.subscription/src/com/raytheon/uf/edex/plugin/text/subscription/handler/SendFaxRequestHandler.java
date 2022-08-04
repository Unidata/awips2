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
package com.raytheon.uf.edex.plugin.text.subscription.handler;

import com.raytheon.uf.common.dataplugin.text.subscription.request.SendFaxRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.subscription.fax.FaxSender;

/**
 * Request handler for SendFaxRequests. Sends FAX message using
 * {@link FaxSender}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2010            bfarmer     Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class SendFaxRequestHandler implements IRequestHandler<SendFaxRequest> {

    @Override
    public Object handleRequest(SendFaxRequest request) throws Exception {
        return FaxSender.sendFax(request.getFaxCompany(),
                request.getFaxNumber(), request.getFaxRecipient(),
                request.getFaxText(), request.getFaxTitle());
    }

}
