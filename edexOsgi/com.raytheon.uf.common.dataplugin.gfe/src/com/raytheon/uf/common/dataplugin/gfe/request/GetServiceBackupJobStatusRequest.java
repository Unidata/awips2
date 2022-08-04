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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to get service backup status by job/task for the specified site. If
 * no site is specified, status for all sites currently in service backup mode
 * are returned.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * Feb 24, 2015  #4103     dgilling     Allow requestor to specify site IDs.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class GetServiceBackupJobStatusRequest implements IServerRequest {

    @DynamicSerializeElement
    private Collection<String> requestedSiteIds;

    public GetServiceBackupJobStatusRequest() {
        this.requestedSiteIds = Collections.emptyList();
    }

    public GetServiceBackupJobStatusRequest(String siteId,
            String... moreSiteIds) {
        this.requestedSiteIds = new HashSet<>(Arrays.asList(moreSiteIds));
        this.requestedSiteIds.add(siteId);
    }

    public Collection<String> getRequestedSiteIds() {
        return requestedSiteIds;
    }

    public void setRequestedSiteIds(Collection<String> requestedSiteIds) {
        this.requestedSiteIds = requestedSiteIds;
    }
}
