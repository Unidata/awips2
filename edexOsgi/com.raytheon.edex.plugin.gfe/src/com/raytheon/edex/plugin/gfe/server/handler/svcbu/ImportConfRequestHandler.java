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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import org.apache.commons.lang.BooleanUtils;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.ImportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Mar 20, 2013   1447     dgilling     Support troubleshooting mode
 *                                      added to match A1 DR 21404.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ImportConfRequestHandler implements
        IRequestHandler<ImportConfRequest> {

    @Override
    public Object handleRequest(ImportConfRequest request) throws Exception {

        ServerResponse<String> sr = new ServerResponse<String>();
        SvcBackupUtil.execute("request_configuration", request.getPrimarySite()
                .toLowerCase(), request.getFailedSite().toLowerCase(), Integer
                .toString(BooleanUtils.toInteger(request.isTrMode())));
        return sr;
    }
}
