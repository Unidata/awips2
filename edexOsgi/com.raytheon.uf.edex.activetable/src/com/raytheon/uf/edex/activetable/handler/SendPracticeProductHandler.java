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
package com.raytheon.uf.edex.activetable.handler;

import com.raytheon.uf.common.activetable.SendPracticeProductRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Send Practice VTEC Product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2010            njensen     Initial creation
 * Nov 14, 2014  4953      randerso    Changed to use sendAsyncThriftUri so headers
 *                                     are not lost
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SendPracticeProductHandler implements
        IRequestHandler<SendPracticeProductRequest> {

    @Override
    public Object handleRequest(SendPracticeProductRequest request)
            throws Exception {
        EDEXUtil.getMessageProducer().sendAsyncThriftUri(
                "jms-generic:queue:practiceVtec", request);
        return null;
    }

}
