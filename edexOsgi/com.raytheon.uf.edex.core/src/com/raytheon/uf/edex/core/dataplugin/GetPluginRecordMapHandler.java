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
package com.raytheon.uf.edex.core.dataplugin;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.request.GetPluginRecordMapRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GetPluginRecordMapHandler implements
        IRequestHandler<GetPluginRecordMapRequest> {

    @Override
    public Map<String, String> handleRequest(GetPluginRecordMapRequest request)
            throws Exception {
        PluginRegistry reg = PluginRegistry.getInstance();

        Set<String> plugins = reg.getRegisteredObjects();
        Map<String, String> rval = new HashMap<String, String>(plugins.size());
        for (String plugin : plugins) {
            PluginProperties props = reg.getRegisteredObject(plugin);
            if (props != null) {
                Class<PluginDataObject> clazz = props.getRecord();
                String clazzName = null;
                if (clazz != null) {
                    clazzName = clazz.getName();
                }
                rval.put(plugin, clazzName);
            }
        }

        return rval;
    }
}
