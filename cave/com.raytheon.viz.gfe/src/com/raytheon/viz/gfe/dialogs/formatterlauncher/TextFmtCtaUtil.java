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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import jep.JepConfig;
import jep.JepException;

/**
 * Call to actions generator.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 JAN 2010  DR3463      rtran         Initial creation
 * 13 Jul 2017  #6346       dgilling      Fix include path.
 * 20 Feb 2018  #6602       dgilling      Update for new text utilities path.
 * Jun 03, 2019 7852        dgilling      Update code for jep 3.8.
 * </pre>
 *
 * @author rtran
 */
public class TextFmtCtaUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextFmtCtaUtil.class);

    private static PythonScript getScript() throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();

        File scriptFile = pm.getStaticFile(LocalizationUtil
                .join(GfePyIncludeUtil.TEXT_UTILITIES, "CallToActions.py"));

        /*
         * The 3 functions below used to be defined in CallToActions.py
         * directly. However, this required all sites porting their AWIPS1 files
         * to also define these new functions. To make life simpler, we'll
         * define them here as pre-evals. Additionally, the \n at the end of
         * each function def, fixed a python SyntaxError about an unexpected
         * EOF. Guess it's a parser bug...
         */
        List<String> preEvals = new ArrayList<>(4);
        preEvals.add("import JUtil");
        preEvals.add("def javaGenericCTAs(): return JUtil.pyValToJavaObj(obj.genericCTAs())\n");
        preEvals.add("def jallCTAs(phensig): return JUtil.pyValToJavaObj(obj.allCTAs(phensig))\n");
        preEvals.add("def javapilCTAs(pil): return JUtil.pyValToJavaObj(obj.pilCTAs(pil))\n");

        JepConfig jepConfig = new JepConfig();
        jepConfig
                .addIncludePaths(GfePyIncludeUtil.getTextUtilitiesIncludePath(),
                        GfePyIncludeUtil.getCommonPythonIncludePath());
        jepConfig.setClassLoader(TextFmtCtaUtil.class.getClassLoader());

        PythonScript script = new PythonScript(jepConfig, scriptFile.getPath(),
                preEvals);
        script.instantiatePythonClass("obj", "CallToActions", null);

        return script;
    }

    @SuppressWarnings("unchecked")
    private static List<Object> runMethod(String method,
            Map<String, Object> args) {
        try (PythonScript script = getScript()) {
            return (List<Object>) script.execute(method, args);
        } catch (JepException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        return Collections.emptyList();
    }

    public static List<Object> ctaText() {
        return runMethod("javaGenericCTAs", null);
    }

    public static List<Object> hazText(String phensig) {
        HashMap<String, Object> args = new HashMap<>();
        args.put("phensig", phensig);

        return runMethod("jallCTAs", args);
    }

    public static List<Object> prodsText(String pil) {
        HashMap<String, Object> args = new HashMap<>();
        args.put("pil", pil);

        return runMethod("javapilCTAs", args);
    }
}
