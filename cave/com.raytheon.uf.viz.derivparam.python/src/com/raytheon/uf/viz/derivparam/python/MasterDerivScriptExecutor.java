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
package com.raytheon.uf.viz.derivparam.python;

import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;

/**
 * Executor for calling executeFunction on a MasterDerivScript
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013 2041       bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MasterDerivScriptExecutor implements
        IPythonExecutor<MasterDerivScript, List<IDataRecord>> {

    private final String name;

    private final List<Object> arguments;

    public MasterDerivScriptExecutor(String name, List<Object> arguments) {
        this.name = name;
        this.arguments = arguments;
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<IDataRecord> execute(MasterDerivScript script)
            throws JepException {
        return (List<IDataRecord>) script.executeFunction(name, arguments);
    }

}
