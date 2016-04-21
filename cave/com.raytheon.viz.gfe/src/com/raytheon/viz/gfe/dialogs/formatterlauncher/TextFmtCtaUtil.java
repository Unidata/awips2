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
//package com.raytheon.viz.gfe.textformatter;
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Call to actions generator.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 JAN 2010  DR3463      rtran         Initial creation
 * </pre>
 * 
 * @author rtran
 * @version 1.0
 */
public class TextFmtCtaUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextFmtCtaUtil.class);

    private static PythonScript getScript() throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();

        File scriptFile = pm.getStaticFile("gfe" + File.separator
                + "userPython" + File.separator + "textUtilities"
                + File.separator + "regular" + File.separator
                + "CallToActions.py");

        // The 3 functions below used to be defined in CallToActions.py
        // directly. However, this required all sites porting their AWIPS1 files
        // to also define these new functions. To make life simpler, we'll
        // define them here as pre-evals.
        // Additionally, the \n at the end of each function def, fixed a python
        // SyntaxError about an unexpected EOF. Guess it's a parser bug...
        List<String> preEvals = new ArrayList<String>(4);
        preEvals.add("import JUtil");
        preEvals.add("def javaGenericCTAs(): return JUtil.pyValToJavaObj(obj.genericCTAs())\n");
        preEvals.add("def jallCTAs(phensig): return JUtil.pyValToJavaObj(obj.allCTAs(phensig))\n");
        preEvals.add("def javapilCTAs(pil): return JUtil.pyValToJavaObj(obj.pilCTAs(pil))\n");

        PythonScript script = new PythonScript(scriptFile.getPath(),
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                TextFmtCtaUtil.class.getClassLoader(), preEvals);
        script.instantiatePythonClass("obj", "CallToActions", null);

        return script;
    }

    @SuppressWarnings("unchecked")
    private static List<Object> runMethod(String method,
            Map<String, Object> args) {
        List<Object> result;
        PythonScript script = null;

        try {
            script = getScript();
            result = (List<Object>) script.execute(method, args);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            result = new ArrayList<Object>();
        } finally {
            if (script != null) {
                script.dispose();
            }
        }
        return result;
    }

    public static List<Object> ctaText() {
        return runMethod("javaGenericCTAs", null);
    }

    public static List<Object> hazText(String phensig) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("phensig", phensig);

        return runMethod("jallCTAs", args);
    }

    public static List<Object> prodsText(String pil) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("pil", pil);

        return runMethod("javapilCTAs", args);
    }
}
