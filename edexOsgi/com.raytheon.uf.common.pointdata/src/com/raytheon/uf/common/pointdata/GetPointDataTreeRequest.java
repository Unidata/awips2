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
package com.raytheon.uf.common.pointdata;

import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
public class GetPointDataTreeRequest implements IServerRequest {

    @DynamicSerializeElement
    private Map<String, String> pluginTypeKeyMap;

    public GetPointDataTreeRequest() {
        super();
    }

    public GetPointDataTreeRequest(Map<String, String> pluginTypeKeyMap) {
        this();
        this.pluginTypeKeyMap = pluginTypeKeyMap;
    }

    /**
     * @return the pluginTypeKeyMap
     */
    public Map<String, String> getPluginTypeKeyMap() {
        return pluginTypeKeyMap;
    }

    /**
     * @param pluginTypeKeyMap
     *            the pluginTypeKeyMap to set
     */
    public void setPluginTypeKeyMap(Map<String, String> pluginTypeKeyMap) {
        this.pluginTypeKeyMap = pluginTypeKeyMap;
    }

}
