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
package com.raytheon.uf.edex.backupsvc.handlers;

import com.raytheon.uf.common.backupsvc.request.GetEDEXVersionRequest;
import com.raytheon.uf.common.backupsvc.response.GetEDEXVersionResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.common.util.app.AppInfo;

/**
 * Handler for GetEDEXVersionRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2016 5937       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class GetEDEXVersionHandler
        implements IRequestHandler<GetEDEXVersionRequest> {

    @Override
    public Object handleRequest(GetEDEXVersionRequest request)
            throws Exception {
        GetEDEXVersionResponse response = new GetEDEXVersionResponse();
        response.setEdexVersion(GetEDEXVersionResponse.UNDEFINED);
        response.setRespondingHost(SystemUtil.getHostName());
        String edexVersion = AppInfo.getInstance().getVersion();
        if (edexVersion != null) {
            response.setEdexVersion(edexVersion);
        }
        return response;
    }

}
