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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3058       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class GetGridDataRequest extends AbstractGfeRequest {
    @DynamicSerializeElement
    private List<GetGridRequest> requests = new ArrayList<GetGridRequest>();

    public void addRequest(GetGridRequest aRequest) {
        requests.add(aRequest);
    }

    /**
     * @return the requests
     */
    public List<GetGridRequest> getRequests() {
        return requests;
    }

    /**
     * @param requests
     *            the requests to set
     */
    public void setRequests(List<GetGridRequest> requests) {
        this.requests = requests;
    }
}
