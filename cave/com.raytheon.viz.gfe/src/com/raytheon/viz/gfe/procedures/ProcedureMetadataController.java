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
package com.raytheon.viz.gfe.procedures;

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Procedure interpreter to be used for gathering metadata on all procedures in
 * the current inventory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2010            njensen      Initial creation
 * Jul 24, 2015  #4263     dgilling     Renamed from ProcedureUIController,
 *                                      remove unnecessary methods.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public final class ProcedureMetadataController extends ProcedureController {

    public ProcedureMetadataController(String scriptPath,
            String buildIncludePath, ClassLoader classLoader,
            DataManager dataMgr) throws JepException {
        super(scriptPath, buildIncludePath, classLoader, dataMgr);
    }

    @SuppressWarnings("unchecked")
    public Map<String, ProcedureMetadata> getMetadata() throws JepException {
        Map<String, Object> args = new HashMap<>(1, 1f);
        args.put("dataMgr", dataMgr);
        return (Map<String, ProcedureMetadata>) execute("getScripts",
                INTERFACE, args);
    }

    public void updateScriptInventory(final FileUpdatedMessage notification) {
        super.fileUpdated(notification);
        processFileUpdates();
    }
}
