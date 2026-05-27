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

import com.raytheon.uf.common.backupsvc.request.GetBackupServiceCapabilitiesRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceCapabilityManager;

/**
 * Handler for {@link GetBackupServiceCapabilitiesRequest}
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2017 6352       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class GetBackupServiceCapabilitiesHandler
        implements IRequestHandler<GetBackupServiceCapabilitiesRequest> {

    @Override
    public Object handleRequest(GetBackupServiceCapabilitiesRequest request)
            throws Exception {
        return BackupServiceCapabilityManager.getInstance().getCapabilities();
    }
}
