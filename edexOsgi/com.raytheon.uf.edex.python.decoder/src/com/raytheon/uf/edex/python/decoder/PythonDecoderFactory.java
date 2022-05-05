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
package com.raytheon.uf.edex.python.decoder;

import java.io.File;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.edex.core.EDEXUtil;

import jep.JepException;

/**
 * Factory class for getting pooled Python decoder instances
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009             njensen     Initial creation
 * Jul 10, 2014 2914       garmendariz Remove EnvProperties
 * Aug 04, 2014 3427       bclement    decoder interface now takes full path to jar
 * Dec 17, 2015 5166       kbisanz     Update logging to use SLF4J
 * Nov 22, 2016 5959       njensen     Improved jar lookup
 * Mar 10, 2017 6171       bsteffen    Fix improved jar lookup.
 * Apr 23, 2019 7182       rblum       Added preEval to fix _strptime threading issues.
 * 
 * </pre>
 * 
 * @author njensen
 */

public class PythonDecoderFactory {

    protected static transient Logger logger = LoggerFactory
            .getLogger(PythonDecoderFactory.class);

    /** number of times to retry instantiating the decoder if it fails */
    private static final int MAX_RETRIES = 3;

    /** python statements to be ran by the interpreter */
    private static final List<String> PRE_EVALS = Collections
            .unmodifiableList(Arrays.asList("import _strptime"));

    /** The standard Python decoder interface file */
    private static String decoderInterface;

    /** The Python module include path */
    private static String includePath;

    /** The directory containing the EDEX plugins */
    public static final String pluginDir;

    /*
     * Initializes the static variables
     */
    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        decoderInterface = pathMgr
                .getFile(commonCx,
                        "python" + File.separator + "DecoderInterface.py")
                .getPath();
        includePath = pathMgr.getFile(commonCx, "python").getPath();
        pluginDir = EDEXUtil.getEdexPlugins();
    }

    /**
     * Creates the interpreter and also loads the specific decoder into the
     * python module. Synchronized as zipimport has race condition that only
     * allows one interpreter to load a given jar file at a time.
     * 
     * @param pluginFQN
     * @param moduleName
     * @return
     * @throws Exception
     */
    public static synchronized PythonScript makePythonDecoder(String pluginFQN,
            String moduleName) throws Exception {
        String jarpath = Paths.get(pluginDir, pluginFQN + ".jar").toString();
        File jarFile = new File(jarpath);
        if (!jarFile.exists()) {
            /* check for any jar files of the format pluginFQN_version.jar */
            Pattern p = Pattern
                    .compile("^" + Pattern.quote(pluginFQN) + "_.*\\.jar$");
            File pluginDirectory = new File(pluginDir);
            for (File f : pluginDirectory.listFiles()) {
                if (p.matcher(f.getName()).find()) {
                    jarpath = f.getPath();
                    break;
                }
            }
        }

        int retryCount = 0;
        PythonScript py = null;
        // we try multiple times cause once in a blue moon we get an error on
        // zlib that is an extremely rare fluke
        while (py == null && retryCount < MAX_RETRIES) {
            retryCount++;
            try {

                py = new PythonScript(decoderInterface, includePath,
                        PythonDecoder.class.getClassLoader(), PRE_EVALS);
                Map<String, Object> argMap = new HashMap<>();
                argMap.put("jarpath", jarpath);
                argMap.put("moduleName", moduleName);
                py.execute("loadModule", argMap);
            } catch (JepException e) {
                logger.error("Error instantiating python decoder " + moduleName
                        + " from jar " + jarpath, e);
                if (py != null) {
                    py.dispose();
                    py = null;
                }
            }
        }
        if (py == null) {
            String msg = "Cannot instantiate " + pluginFQN + " decoder, "
                    + moduleName + " products will not be decoded!";
            logger.error(msg);
            throw new DecoderException(msg);
        }
        return py;
    }
}
