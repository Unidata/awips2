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

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;

/**
 * Parser for the text formatter.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09 Dec 2008             lvenable    Initial creation
 * 10 Nov 2010             njensen     Cache python
 * 07 Mar 2013  15717      jzeng       Change CAVE_STATIC to COMMON_STATIC
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TextFmtParserUtil {

    private static PythonScript python;

    @SuppressWarnings("unchecked")
    public static HashMap<String, Object> parseText(String text) {

        HashMap<String, Object> parsedText = null;

        HashMap<String, Object> map = new HashMap<String, Object>(1);

        try {
            PythonScript py = getPython();
            map.put("text", text);
            Object com = py.execute("parseFromJava", "parser", map);
            parsedText = (HashMap<String, Object>) com;
        } catch (JepException e) {
            e.printStackTrace();
        }
        return parsedText;
    }

    private static synchronized PythonScript getPython() throws JepException {
        if (python == null) {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext baseContext = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            File baseFile = pm.getFile(baseContext, "gfe" + File.separator
                    + "userPython" + File.separator + "utilities"
                    + File.separator + "ProductParser.py");
            python = new PythonScript(baseFile.getPath(),
                    GfePyIncludeUtil.getCommonPythonIncludePath());
            python.instantiatePythonClass("parser", "ProductParser", null);
        }
        return python;
    }
}
