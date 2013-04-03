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
package com.raytheon.uf.common.plugin.nwsauth;

import java.util.Map;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A request to retrieve, or submit, the NWS role date.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 09, 2013 1412       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class NwsRoleDataRequest extends AbstractPrivilegedRequest {
    // Ticket #1315 should use this class to actually retrieve/submit role data
    // changes

    public static enum NwsRoleDataRequestType {
        REQUEST, SUBMIT;
    }

    @DynamicSerializeElement
    private NwsRoleDataRequestType type;

    @DynamicSerializeElement
    private Map<String, NwsRoleData> roleDataMap;

    /**
     * @return the type
     */
    public NwsRoleDataRequestType getType() {
        return type;
    }

    /**
     * @param request
     */
    public void setType(NwsRoleDataRequestType type) {
        this.type = type;
    }

    /**
     * @return
     */
    public Map<String, NwsRoleData> getRoleDataMap() {
        return roleDataMap;
    }

    /**
     * @param roleDataMap2
     */
    public void setRoleDataMap(Map<String, NwsRoleData> roleDataMap2) {
        this.roleDataMap = roleDataMap2;
    }
}
