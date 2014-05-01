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
package com.raytheon.viz.aviation.climatology;

import java.io.File;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.multiprocessing.PythonProcess;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

/**
 * Reference to a single interpreter for running climate python methods
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 15, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ClimatePython {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimatePython.class);

    private static PythonProcess initializePython() throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();
        File runner = pm.getStaticFile("aviation/python/ClimateEntry.py");
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil
                .getPointDataDir(), AvnPyUtil.getCommonPythonDir());
        PythonProcess python = new PythonProcess(filePath, includePath,
                ClimatePython.class.getClassLoader());
        return python;
    }

    public static synchronized PythonProcess getClimateInterpreter() {
        PythonProcess python = null;
        try {
            python = initializePython();
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error initializing climate python interpreter", e);
        }
        return python;
    }

    public static String getClimateFilePath(String site) throws VizException {
        String dataDir = VizApp.getDataDir();
        File file = new File(dataDir + "/aviation/" + site + ".hd5");
        if (file == null || !file.exists()) {
            throw new VizException("Climate hdf5 file " + file.getPath()
                    + " does not exist.");
        }
        String climateFile = file.getPath();
        return climateFile;
    }

}
