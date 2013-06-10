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

import com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscScriptFactory extends AbstractPythonScriptFactory<IscScript> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscScriptFactory.class);

    private static final String SCRIPT_EXTENSION = ".py";

    /**
     * @param name
     * @param maxThreads
     */
    public IscScriptFactory(String name, int maxThreads) {
        super(name, maxThreads);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory#
     * createPythonScript()
     */
    @Override
    public IscScript createPythonScript() {
        try {
            return new IscScript(getName() + SCRIPT_EXTENSION);
        } catch (JepException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to create GFE ISC script [" + getName() + "]", e);
        }
        return null;
    }
}
