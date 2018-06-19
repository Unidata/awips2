package com.raytheon.uf.viz.alertviz.python;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.python.concurrent.PythonInterpreterFactory;

import jep.JepException;

/**
 * This is the main class for the alert visualization.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date        Ticket#    Engineer      Description
 * ----------  ---------  ------------  --------------------------
 * 2017-03-24  DR 16985   D. Friedman   Restore Python script functionality
 *
 * </pre>
 *
 */
public class AlertVizPythonScriptFactory implements PythonInterpreterFactory<PythonScript> {

    private static String ALERTVIZ_PYTHON_SCRIPT_DIR = "alertViz/python";

    private static String buildScriptPath() {
        File baseFile = PathManagerFactory.getPathManager()
                .getStaticFile(
                        ALERTVIZ_PYTHON_SCRIPT_DIR
                                + IPathManager.SEPARATOR
                                + "MasterProcessor.py");
        return baseFile.getAbsolutePath();
    }

    private static String buildIncludePath() {
        StringBuilder includeBuilder = new StringBuilder();
        IPathManager pm = PathManagerFactory.getPathManager();
        for (LocalizationContext lc : pm.getLocalSearchHierarchy(LocalizationType.CAVE_STATIC)) {
            LocalizationFile lf = pm.getLocalizationFile(lc, ALERTVIZ_PYTHON_SCRIPT_DIR);
            if (lf.exists()) {
                if (includeBuilder.length() > 0) {
                    includeBuilder.append(File.pathSeparator);
                }
                includeBuilder.append(lf.getFile().getAbsolutePath());
            }
        }
        return includeBuilder.toString();
    }

    @Override
    public PythonScript createPythonScript() throws JepException {
        return new PythonScript(buildScriptPath(), buildIncludePath(),
                getClass().getClassLoader());
    }

}
