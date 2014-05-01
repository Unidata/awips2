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
package com.raytheon.viz.aviation.climatedata;

import java.io.File;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.climatology.ClimatePython;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            avarani     Initial creation
 * Feb 16, 2011 7878       rferrel     Modifications for create ident/site.
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class ClimateDataPython {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimateDataPython.class);

    private static PythonClimateDataProcess initializePython()
            throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();
        File runner = pm.getStaticFile("aviation/python/ClimateDataEntry.py");
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil
                .getPointDataDir(), AvnPyUtil.getCommonPythonDir());
        PythonClimateDataProcess python = new PythonClimateDataProcess(
                filePath, includePath, ClimatePython.class.getClassLoader());
        return python;
    }

    public static synchronized PythonClimateDataProcess getClimateInterpreter() {
        PythonClimateDataProcess python = null;
        try {
            python = initializePython();
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error initializing climate python interpreter", e);
        }
        return python;
    }

    /**
     * Obtain path name of the directory with the ISH files and verify the files
     * exist.
     * 
     * @return dataDir
     * @throws VizException
     *             when unable to find ISH files
     */
    public static String getIshFilePath() throws VizException {
        String dataDirStr = VizApp.getDataDir();
        File dataDir = new File(dataDirStr + "/aviation");
        if (dataDir == null || !dataDir.exists()) {
            throw new VizException(String.format(
                    "Directory: \"%s\" does not exist.", dataDir.getPath()));
        }

        File histFile = new File(dataDir, "ish-history.txt");
        if (histFile == null || !histFile.exists()) {
            throw new VizException(String.format(
                    "ISH history file: \"%s\" does not exist.",
                    histFile.getPath()));
        }

        File invFile = new File(dataDir, "ish-inventory.txt");
        if (invFile == null || !invFile.exists()) {
            throw new VizException(String.format(
                    "ISH inventory file: \"%s\" does not exist.",
                    invFile.getPath()));
        }

        return dataDir.getPath();
    }

    public static String getClimateFilePath(String site) throws VizException {
        String dataDir = VizApp.getDataDir();
        File file = new File(dataDir + "/aviation/" + site + ".hd5");
        String climateFile = file.getPath();
        return climateFile;
    }
}
