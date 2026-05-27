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
package com.raytheon.uf.common.backupsvc.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Request to update or retrieve outgoing Backup Jobs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2021  91325      lsingh          Initial creation
 * Jul 01, 2021  93517     Amanuel Challa  Added new Constructor with filterQueryStr param
 *
 * </pre>
 *
 * @author lsingh
 */
@DynamicSerialize
public class OutgoingBackupJobsRequest extends IncomingOutgoingJobsRequest {

    public OutgoingBackupJobsRequest() {
        super();
    }

    public OutgoingBackupJobsRequest(
            IncomingOutgoingJobsRequest.RequestType type, String site,
            List<Long> jobIDs) {
        super(type, site, jobIDs);
    }

    public OutgoingBackupJobsRequest(RequestType type, String site) {
        super(type, site);
    }

    public OutgoingBackupJobsRequest(RequestType type, String site,
            String filterQueryStr) {
        super(type, site, filterQueryStr);
    }

}