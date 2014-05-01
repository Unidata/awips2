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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request object for getting the latest database ID for a given model name and
 * site ID.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010            dgilling     Initial creation
 * May 22, 2013  2025      dgilling     Add DynamicSerialize support.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GetLatestModelDbIdRequest extends AbstractGfeRequest {

    /**
     * The model name to perform the request for.
     */
    @DynamicSerializeElement
    private String modelName;

    public GetLatestModelDbIdRequest() {
        // no-op
    }

    /**
     * Creates a new GetLatestModelDbIdRequest object given a model name and
     * site identifier.
     * 
     * @param siteId
     *            The site identifier to search for.
     * @param modelName
     *            The name of the model to search for.
     */
    public GetLatestModelDbIdRequest(String siteId, String modelName) {
        super();
        this.modelName = modelName;
        this.siteID = siteId;
    }

    public String getSiteId() {
        return getSiteID();
    }

    public void setSiteId(String siteId) {
        setSiteID(siteId);
    }

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }
}
