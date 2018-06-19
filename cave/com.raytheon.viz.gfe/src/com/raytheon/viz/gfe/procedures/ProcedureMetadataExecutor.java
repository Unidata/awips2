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

import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;

/**
 * Retrieves the current procedure inventory along with the metadata (menu
 * items, varDict, etc.) for each procedure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ProcedureMetadataExecutor
        implements
        IPythonExecutor<ProcedureMetadataController, Map<String, ProcedureMetadata>> {

    private final FileUpdatedMessage localizationNotification;

    public ProcedureMetadataExecutor() {
        this(null);
    }

    public ProcedureMetadataExecutor(FileUpdatedMessage message) {
        this.localizationNotification = message;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.IPythonExecutor#execute(com.
     * raytheon.uf.common.python.PythonInterpreter)
     */
    @Override
    public Map<String, ProcedureMetadata> execute(
            ProcedureMetadataController script) throws JepException {
        if (localizationNotification != null) {
            script.updateScriptInventory(localizationNotification);
        }
        return script.getMetadata();
    }
}
