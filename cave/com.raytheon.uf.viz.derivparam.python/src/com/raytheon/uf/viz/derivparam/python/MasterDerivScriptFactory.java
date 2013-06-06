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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;

/**
 * Factory for creating and initializing MasterDerivScript.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013 2041       bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MasterDerivScriptFactory extends
        AbstractPythonScriptFactory<MasterDerivScript> {

    private static final int MAX_THREADS = 2;

    public static final String NAME = "DerivedParameterPython";

    private static final String INTERFACE_SCRIPT = DerivedParameterGenerator.DERIV_PARAM_DIR
            + File.separator
            + "python"
            + File.separator
            + "DerivParamImporter.py";

    public MasterDerivScriptFactory() {
        super(NAME, MAX_THREADS);
    }

    @Override
    public MasterDerivScript createPythonScript() throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();

        File script = pm.getStaticFile(INTERFACE_SCRIPT);

        // Get list of all files for search hierarch of CAVE_STATIC
        LocalizationFile[] derivParamFiles = pm.listFiles(
                pm.getLocalSearchHierarchy(LocalizationType.CAVE_STATIC),
                DerivedParameterGenerator.DERIV_PARAM_DIR, null, false, false);
        List<String> functionDirs = new ArrayList<String>(
                derivParamFiles.length);
        functionDirs.add(script.getParent());

        Arrays.sort(derivParamFiles);

        for (LocalizationFile file : derivParamFiles) {
            if (file.isDirectory()
                    && DerivedParameterGenerator.FUNCTIONS
                            .equals(LocalizationUtil.extractName(file.getName()))) {
                // If it is a derived parameters functions directory, add to
                // search list
                functionDirs.add(file.getFile().getAbsolutePath());
            }
        }

        // Create path from function dir list
        String PATH = PyUtil.buildJepIncludePath(functionDirs
                .toArray(new String[functionDirs.size()]));

        List<String> preEvals = new ArrayList<String>(2);
        preEvals.add("import DerivParamImporter");
        StringBuilder cmd = new StringBuilder(200);
        cmd.append("sys.meta_path.append(DerivParamImporter.DerivParamImporter(");
        // Pass in directories to search based on function directories
        int size = functionDirs.size() - 1;
        for (int i = size; i > 0; --i) {
            if (i < size) {
                cmd.append(", ");
            }
            cmd.append("'").append(functionDirs.get(i)).append("'");
        }
        cmd.append("))");
        preEvals.add(cmd.toString());
        return new MasterDerivScript(PATH,
                MasterDerivScript.class.getClassLoader(), preEvals);
    }

}
