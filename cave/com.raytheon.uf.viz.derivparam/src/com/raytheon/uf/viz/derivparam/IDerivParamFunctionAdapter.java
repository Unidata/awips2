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
package com.raytheon.uf.viz.derivparam;

import java.util.List;
import java.util.concurrent.ExecutionException;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.derivparam.DerivParamFunctionType.FunctionArgument;

/**
 * Derived parameter function adapter, used to execution functions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2010            mschenke    Initial creation
 * Jun 04, 2013 2041       bsteffen    Switch derived parameters to use
 *                                     concurrent python for threading.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IDerivParamFunctionAdapter {

    /**
     * Create a new function for the derived paramter type this adapter is meant
     * for
     * 
     * @param functionName
     *            name of the function
     * @param arguments
     *            function arguments
     * @return
     */
    public String createNewFunction(String functionName,
            FunctionArgument[] arguments);

    /**
     * Get a list of all available argument types
     * 
     * @return the available types
     */
    public String[] getArgumentTypes();

    /**
     * Initialize the adapter
     */
    public void init();

    /**
     * Execute the derived parameter request
     * 
     * @param name
     *            name of function to execture
     * @param arguments
     *            arguments to pass into function
     */
    public List<IDataRecord> executeFunction(String name, List<Object> arguments)
            throws ExecutionException;

    /**
     * Stop the adapter and any child threads.
     */
    public void shutdown();
}
