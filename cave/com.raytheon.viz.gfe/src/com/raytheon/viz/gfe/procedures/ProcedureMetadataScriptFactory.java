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

import jep.JepException;

import com.raytheon.viz.gfe.core.DataManager;

/**
 * Script factory for {@code ProcedureMetadataController} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2015  #4263     dgilling     Initial creation
 * Dec 14, 2015  #4816     dgilling     Support refactored PythonJobCoordinator API.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ProcedureMetadataScriptFactory extends
        ProcedureFactory<ProcedureMetadataController> {

    public ProcedureMetadataScriptFactory(final DataManager dataMgr) {
        super(dataMgr);
    }

    @Override
    public ProcedureMetadataController createPythonScript() throws JepException {
        return new ProcedureMetadataController(buildScriptPath(),
                buildIncludePath(), getClass().getClassLoader(), dataMgr);
    }
}
