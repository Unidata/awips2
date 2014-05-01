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
package com.raytheon.uf.common.python.concurrent;

import jep.JepException;

import com.raytheon.uf.common.python.PythonInterpreter;

/**
 * Must be extended by all classes that want to run Python on different threads.
 * Defines the thread pool, gets the {@link PythonInterpreter}, and tells how to
 * execute.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 05, 2013            mnash       Initial creation
 * Jun 04, 2013 2041       bsteffen    Improve exception handling for concurrent
 *                                     python.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public abstract class AbstractPythonScriptFactory<P extends PythonInterpreter> {

    private final int maxThreads;

    private final String name;

    /**
     * This method will be called on the Python thread and will instantiate the
     * PythonInterpreter that is going to be used.
     * 
     * @return
     */
    public abstract P createPythonScript() throws JepException;

    public AbstractPythonScriptFactory(String name, int maxThreads) {
        this.name = name;
        this.maxThreads = maxThreads;
    }

    /**
     * @return the maxThreads
     */
    public final int getMaxThreads() {
        return maxThreads;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }
}
