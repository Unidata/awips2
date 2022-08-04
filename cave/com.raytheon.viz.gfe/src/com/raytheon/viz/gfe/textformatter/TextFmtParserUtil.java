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
package com.raytheon.viz.gfe.textformatter;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;

import jep.JepConfig;
import jep.JepException;

/**
 * Parser for the text formatter.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09 Dec 2008             lvenable    Initial creation
 * 10 Nov 2010             njensen     Cache python
 * 29 Aug 2013   #2250     dgilling    Fix PythonScript construction to use
 *                                     proper ClassLoader, return JepExceptions
 *                                     to caller.
 * 21 Nov 2016    5959     njensen     Cleanup
 * Aug 16, 2019   7914     tgurney     Stop caching the python interpreter
 * </pre>
 *
 * @author lvenable
 */
public class TextFmtParserUtil {

    /**
     * Private constructor, since all methods are static.
     */
    private TextFmtParserUtil() {
        throw new AssertionError();
    }

    @SuppressWarnings("unchecked")
    public static Map<String, Object> parseText(String text)
            throws JepException {

        Map<String, Object> parsedText = null;

        Map<String, Object> map = new HashMap<>(1);

        map.put("text", text);
        try (PythonScript py = getPython()) {
            Object com = py.execute("parseFromJava", "parser", map);
            parsedText = (Map<String, Object>) com;
        }

        return parsedText;
    }

    private static PythonScript getPython() throws JepException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext baseContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
        File baseFile = pm.getFile(baseContext,
                "gfe" + File.separator + "userPython" + File.separator
                        + "utilities" + File.separator + "ProductParser.py");
        JepConfig jepConfig = new JepConfig();
        jepConfig.setClassLoader(TextFmtParserUtil.class.getClassLoader());
        jepConfig.setIncludePath(GfePyIncludeUtil.getCommonPythonIncludePath());
        PythonScript python = new PythonScript(jepConfig, baseFile.getPath());
        python.instantiatePythonClass("parser", "ProductParser", null);
        return python;
    }
}
