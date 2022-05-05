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
package com.raytheon.edex.plugin.gfe.isc;

import jep.JepException;

import com.raytheon.uf.common.python.concurrent.PythonInterpreterFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Instantiates a {@link IscScript} on a separate thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2013            dgilling     Initial creation
 * Dec 14, 2015  #4816     dgilling     Support refactored PythonJobCoordinator API.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscScriptFactory implements PythonInterpreterFactory<IscScript> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscScriptFactory.class);

    private static final String SCRIPT_EXTENSION = ".py";

    private final String name;

    /**
     * Construct a new script factory for the python module with the specified
     * name.
     * 
     * @param name
     *            The name of the python module (minus the .py extension) that
     *            this factory will build {@code IscScript} instances for.
     */
    public IscScriptFactory(String name) {
        this.name = name;
    }

    @Override
    public IscScript createPythonScript() {
        try {
            return new IscScript(name + SCRIPT_EXTENSION);
        } catch (JepException e) {
            statusHandler
                    .error(String.format(
                            "Unable to create GFE ISC script [%s]", name), e);
        }
        return null;
    }
}
