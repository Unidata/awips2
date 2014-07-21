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
import java.util.HashMap;

import jep.JepException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.edex.core.EDEXUtil;

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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonDecoderFactory {

    protected static transient Log logger = LogFactory
            .getLog(PythonDecoderFactory.class);

    /** number of times to retry instantiating the decoder if it fails */
    private static final int MAX_RETRIES = 3;

    /** The standard Python decoder interface file */
    private static String decoderInterface;

    /** The Python module include path */
    private static String includePath;

    /** The directory containing the EDEX plugins */
    public static String pluginDir;

    /*
     * Initializes the static variables
     */
    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        decoderInterface = pathMgr.getFile(commonCx,
                "python" + File.separator + "DecoderInterface.py").getPath();
        includePath = pathMgr.getFile(commonCx, "python").getPath();
        pluginDir = EDEXUtil.getEdexPlugins();
    }

    /**
     * Creates the interpreter and also loads the specific decoder into the
     * python module. Synchronized as zipimport has race condition that only
     * allows one interpreter to load a given jar file at a time.
     * 
     * @param pluginName
     * @return
     * @throws Exception
     */
    public static synchronized PythonScript makePythonDecoder(String pluginFQN,
            String moduleName) throws Exception {
        PythonScript py = null;
        int retryCount = 0;
        // we try multiple times cause once in a blue moon we get an error on
        // zlib that is an extremely rare fluke
        while (py == null && retryCount < MAX_RETRIES) {
            retryCount++;
            try {
                py = new PythonScript(decoderInterface, includePath,
                        PythonDecoder.class.getClassLoader());
                HashMap<String, Object> argMap = new HashMap<String, Object>();
                argMap.put("pluginDir", pluginDir);
                argMap.put("pluginFQN", pluginFQN);
                argMap.put("moduleName", moduleName);
                py.execute("loadModule", argMap);
            } catch (JepException e) {
                logger.error(
                        "Error instantiating python decoder " + moduleName, e);
                if (py != null) {
                    py.dispose();
                    py = null;
                }
            }
        }
        if (py == null) {
            String msg = "Cannot instantiate " + pluginFQN + " decoder, "
                    + moduleName + " products will not be decoded!";
            logger.fatal(msg);
            throw new DecoderException(msg);
        }
        return py;
    }
}
