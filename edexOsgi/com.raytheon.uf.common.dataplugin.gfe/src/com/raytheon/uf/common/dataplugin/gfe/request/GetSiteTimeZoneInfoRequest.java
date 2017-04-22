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

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to retrieve the configured time zone for the specified 3-character
 * site identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2011            dgilling     Initial creation
 * Nov 17, 2015  #5129     dgilling     Accept multiple site IDs to better match A1.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GetSiteTimeZoneInfoRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private List<String> requestedSiteIDs;

    public GetSiteTimeZoneInfoRequest() {
    }

    public GetSiteTimeZoneInfoRequest(List<String> siteIDs) {
        this.requestedSiteIDs = siteIDs;
    }

    public List<String> getRequestedSiteIDs() {
        return requestedSiteIDs;
    }

    public void setRequestedSiteIDs(List<String> requestedSiteIDs) {
        this.requestedSiteIDs = requestedSiteIDs;
    }
}
