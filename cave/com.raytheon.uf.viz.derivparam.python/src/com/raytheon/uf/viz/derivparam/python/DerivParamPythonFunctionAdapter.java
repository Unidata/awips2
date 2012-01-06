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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
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
 * Dec 16, 2010            mschenke     Initial creation
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

    private static final String INTERFACE_SCRIPT = DerivedParameterGenerator.DERIV_PARAM_DIR
            + File.separator
            + PYTHON
            + File.separator
            + "DerivParamImporter.py";

    private static final String TEMPLATE_FILE = DerivedParameterGenerator.DERIV_PARAM_DIR
            + File.separator + PYTHON + File.separator + "functionTemplate.txt";

    private File baseDir = null;

    private MasterDerivScript masterScript;

    private List<IDataRecord> results;

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
        IPathManager pm = PathManagerFactory.getPathManager();
        if (baseDir == null) {
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
            context.setContextName(Activator.getDefault().getBundle()
                    .getSymbolicName());
            baseDir = pm.getFile(context,
                    DerivedParameterGenerator.FUNCTIONS_DIR);
            if (baseDir == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not find base directory for "
                                + DerivedParameterGenerator.FUNCTIONS_DIR);
            }
        }

        File script = pm.getStaticFile(INTERFACE_SCRIPT);
        File siteDir = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE),
                DerivedParameterGenerator.FUNCTIONS_DIR);
        File userDir = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER),
                DerivedParameterGenerator.FUNCTIONS_DIR);

        if (baseDir.exists() == false) {
            baseDir.mkdirs();
        }
        if (siteDir.exists() == false) {
            siteDir.mkdirs();
        }
        if (userDir.exists() == false) {
            userDir.mkdirs();
        }

        String PATH = PyUtil.buildJepIncludePath(script.getParent(),
                userDir.getAbsolutePath(), siteDir.getAbsolutePath(),
                baseDir.getAbsolutePath());

        List<String> preEvals = new ArrayList<String>(2);
        preEvals.add("import DerivParamImporter");
        StringBuilder cmd = new StringBuilder(200);
        cmd.append("sys.meta_path.append(DerivParamImporter.DerivParamImporter(");
        cmd.append("'").append(userDir.getPath()).append("', ");
        cmd.append("'").append(siteDir.getPath()).append("', ");
        cmd.append("'").append(siteDir.getPath()).append("'))");
        preEvals.add(cmd.toString());

        try {
            if (masterScript != null) {
                masterScript.dispose();
            }
            masterScript = new MasterDerivScript(PATH,
                    MasterDerivScript.class.getClassLoader(), preEvals);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to load derived parameters", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#executeFunction
     * (java.lang.String, java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void executeFunction(String name, List<Object> arguments) {
        try {
            results = (List<IDataRecord>) masterScript.executeFunction(name,
                    arguments);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error executing derived parameter request", e);
            results = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#getRequestResults
     * ()
     */
    @Override
    public List<IDataRecord> getRequestResults() {
        return results;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter#
     * getRequestIdentifierResults()
     */
    @Override
    public Object getRequestIdentifierResults() {
        // TODO: Not supported yet
        return null;
    }

}
