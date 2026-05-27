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
package com.raytheon.uf.edex.alertviz.handler;

import com.raytheon.uf.common.alertviz.InitializeAlertMonitorsRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Handler for InitializeAlertMonitorsRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 14, 2018  6670     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class InitializeAlertMonitorsHandler
        implements IRequestHandler<InitializeAlertMonitorsRequest> {

    @Override
    public Object handleRequest(InitializeAlertMonitorsRequest request)
            throws Exception {

        EDEXUtil.getMessageProducer().sendAsyncThriftUri(
                "jms-generic:topic:initializeAlertMonitors?timeToLive=60000",
                request);

        return null;
    }

}
