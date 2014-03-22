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
package com.raytheon.viz.pointdata.python;

import java.io.File;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory;
import com.raytheon.viz.pointdata.PlotModelFactory2;

/**
 * Builds a plot delegate python script based on the script text extracted from
 * a plot model SVG file. The name should be the name of the plot model SVG file
 * to optimize reusing the interpreters across different resources that display
 * the same plot model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2014 2868       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotPythonScriptFactory extends
        AbstractPythonScriptFactory<PlotPythonScript> {

    private static String includePath;

    private static String baseFilePath;

    protected String scriptText;

    protected String plotDelegateName;

    /**
     * Constructor
     * 
     * @param plotSvgName
     *            the name of the svg file that the python code was extracted
     *            from
     * @param scriptText
     *            the python code that was extracted from the svg file
     * @param plotDelegateName
     *            the name of the instance of the plot delegate inside the
     *            python code
     */
    public PlotPythonScriptFactory(String plotSvgName, String scriptText,
            String plotDelegateName) {
        super(plotSvgName, 1);
        this.scriptText = scriptText;
        this.plotDelegateName = plotDelegateName;
    }

    @Override
    public PlotPythonScript createPythonScript() throws JepException {
        synchronized (PlotPythonScriptFactory.class) {
            if (includePath == null) {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationFile[] files = pm.listFiles(pm
                        .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC),
                        PlotModelFactory2.PLOT_MODEL_DIR, null, false, false);
                StringBuilder includeBuilder = new StringBuilder();
                for (LocalizationFile lf : files) {
                    if (lf.exists() && lf.isDirectory()) {
                        if (includeBuilder.length() > 0) {
                            includeBuilder.append(File.pathSeparator);
                        }
                        includeBuilder.append(lf.getFile().getAbsolutePath());
                    }
                }
                includePath = includeBuilder.toString();
            }

            if (baseFilePath == null) {
                File baseFile = PathManagerFactory.getPathManager()
                        .getStaticFile(
                                PlotModelFactory2.PLOT_MODEL_DIR
                                        + IPathManager.SEPARATOR
                                        + "PlotModelInterface.py");
                baseFilePath = baseFile.getAbsolutePath();
            }
        }

        return new PlotPythonScript(baseFilePath, includePath, scriptText,
                plotDelegateName, this.getName());
    }
}
