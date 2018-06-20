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
package com.raytheon.edex.productsrv;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class ThreadLocalPython extends ThreadLocal<PythonEval> {

    //
    // Python include path setup ( global )
    //
    private static String includePath;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String edexPython = pathMgr.getFile(edexStaticBase, "python").getPath();
        String commonPython = pathMgr.getFile(commonStaticBase, "python")
                .getPath();
        includePath = PyUtil.buildJepIncludePath(commonPython, edexPython);
    }

    // called internally to get the initial value
    protected synchronized PythonEval initialValue() {
        // System.err.println("Creating PythonEval in Thread "
        // + Thread.currentThread().getId());
        PythonEval jepInstance = null;
        try {
            jepInstance = createJepInstance();
        } catch (JepException e) {
            throw new JepRuntimeException(e);
        }
        return jepInstance;
    }

    private PythonEval createJepInstance() throws JepException {
        PythonEval eval = new PythonEval(includePath,
                ThreadLocalPython.class.getClassLoader());
        eval.eval("import CatalogQuery");
        return eval;
    }
}
