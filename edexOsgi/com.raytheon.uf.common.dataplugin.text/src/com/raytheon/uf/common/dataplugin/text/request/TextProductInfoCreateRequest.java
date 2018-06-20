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

package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Standard Text Product Info Server Request class to be used in a thrift
 * service. Register request and handler beans in text-request.xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07Jun2010    5004       cjeanbap    Initial development.
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
@DynamicSerialize
public class TextProductInfoCreateRequest implements IServerRequest {

    @DynamicSerializeElement
    private String cccid;

    @DynamicSerializeElement
    private String nnnid;

    @DynamicSerializeElement
    private String xxxid;

    @DynamicSerializeElement
    private boolean operationalMode = true;

    public TextProductInfoCreateRequest() {

    }

    public TextProductInfoCreateRequest(String cccid, String nnnid, String xxxid) {
        this.cccid = cccid;
        this.nnnid = nnnid;
        this.xxxid = xxxid;
    }

    public String getCccid() {
        return cccid;
    }

    public void setCccid(String cccid) {
        this.cccid = cccid;
    }

    public String getNnnid() {
        return nnnid;
    }

    public void setNnnid(String nnnid) {
        this.nnnid = nnnid;
    }

    public String getXxxid() {
        return xxxid;
    }

    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    public boolean getOperationalMode() {
        return operationalMode;
    }

    public void setOperationalMode(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }
}
