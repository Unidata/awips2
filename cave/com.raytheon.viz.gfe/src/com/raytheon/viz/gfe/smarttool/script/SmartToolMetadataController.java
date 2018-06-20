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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Smart tool interpreter used primarily to determine list of smart tools and
 * attributes on those tools. This class should only be used for retrieving
 * information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            njensen     Initial creation
 * Mar 01, 2012  #346      dgilling    Use identity-based ListenerLists.
 * Jul 23, 2015  #4263     dgilling    Renamed from SmartToolUIController,
 *                                     refactor to support python concurrent 
 *                                     execution framework.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolMetadataController extends SmartToolController {

    public SmartToolMetadataController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager);
    }

    @SuppressWarnings("unchecked")
    public Map<String, SmartToolMetadata> getToolMetadata() throws JepException {
        Map<String, Object> args = new HashMap<>(1, 1f);
        args.put("dataMgr", dataMgr);
        return (Map<String, SmartToolMetadata>) execute("getScripts",
                INTERFACE, args);
    }

    public void updateScriptInventory(final FileUpdatedMessage notification) {
        super.fileUpdated(notification);
        processFileUpdates();
    }
}
