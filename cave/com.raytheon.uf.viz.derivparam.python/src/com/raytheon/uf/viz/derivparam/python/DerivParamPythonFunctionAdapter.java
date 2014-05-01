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

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutionException;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.derivparam.DerivParamFunctionType.FunctionArgument;
import com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;

/**
 * Python derived parameter adapter
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

public class DerivParamPythonFunctionAdapter implements
        IDerivParamFunctionAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DerivParamPythonFunctionAdapter.class);

    private static final String PYTHON = "python";

    private static final String TEMPLATE_FILE = DerivedParameterGenerator.DERIV_PARAM_DIR
            + File.separator + PYTHON + File.separator + "functionTemplate.txt";

    private PythonJobCoordinator<MasterDerivScript> coordinator;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#createNewFunction
     * (java.lang.String,
     * com.raytheon.uf.viz.derivparam.DerivParamFunctionType.FunctionArgument[])
     */
    @Override
    public String createNewFunction(String functionName,
            FunctionArgument[] arguments) {
        File template = PathManagerFactory.getPathManager().getStaticFile(
                TEMPLATE_FILE);
        try {
            String templateText = new String(FileUtil.file2bytes(template));
            SimpleDateFormat dateFormat = new SimpleDateFormat("MMM dd, yyyy");
            String date = dateFormat.format(new Date());
            String argList = "";
            boolean first = true;
            for (FunctionArgument arg : arguments) {
                if (!first) {
                    argList += ",";
                }
                first = false;
                argList += arg.name;
            }
            return String.format(templateText, date, argList);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading in python template file");
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#getArgumentTypes
     * ()
     */
    @Override
    public String[] getArgumentTypes() {
        return new String[] { "Parameter" };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#init()
     */
    @Override
    public void init() {
        if (coordinator != null) {
            coordinator.shutdown();
        }
        coordinator = PythonJobCoordinator
                .newInstance(new MasterDerivScriptFactory());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#executeFunction
     * (java.lang.String, java.util.List)
     */
    @Override
    public List<IDataRecord> executeFunction(String name, List<Object> arguments)
            throws ExecutionException {
        try {
            return coordinator.submitSyncJob(new MasterDerivScriptExecutor(
                    name, arguments));
        } catch (InterruptedException e) {
            throw new ExecutionException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#shutdown()
     */
    @Override
    public void shutdown() {
        if (coordinator != null) {
            coordinator.shutdown();
        }
    }

}
