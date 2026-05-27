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
package com.raytheon.uf.common.damagepath.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to send the given Damage Path Tool data (in GeoJSON format) to LDAD.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 08, 2015  #4355     dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@DynamicSerialize
public class ExportToLdadRequest implements IServerRequest {

    @DynamicSerializeElement
    private String siteId;

    @DynamicSerializeElement
    private byte[] data;

    /**
     * Default constructor--should only be used by DynamicSerialize.
     */
    public ExportToLdadRequest() {
    }

    /**
     * Construct a new request for the given site identifier with the given
     * damage path data.
     * 
     * @param siteId
     *            3-character site identifier for the site making the request.
     * @param data
     *            GeoJSON data for the damage path stored as bytes.
     */
    public ExportToLdadRequest(String siteId, byte[] data) {
        this.siteId = siteId;
        this.data = data;
    }

    @Override
    public String toString() {
        return "ExportToLdadRequest [siteId=" + siteId + "]";
    }

    public String getSiteId() {
        return siteId;
    }

    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] data) {
        this.data = data;
    }
}
