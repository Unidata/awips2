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

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;

/**
 * Executor object to retrieve text product configuration and metadata from
 * python into Java.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductConfigDataExecutor implements
        IPythonExecutor<FormatterScript, TextProductConfigData> {

    private final boolean getVtecCodes;

    /**
     * 
     * @param handleVtecModes
     *            Retrieve default VTEC mode configuration data
     */
    public TextProductConfigDataExecutor(boolean getVtecCodes) {
        this.getVtecCodes = getVtecCodes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.IPythonExecutor#execute(com.
     * raytheon.uf.common.python.PythonInterpreter)
     */
    @Override
    public TextProductConfigData execute(FormatterScript script)
            throws JepException {
        Map<String, Object> map = new HashMap<String, Object>(2, 1f);
        map.put("paths", GfePyIncludeUtil.getTextProductsIncludePath());
        map.put("getVtecCodes", getVtecCodes);
        TextProductConfigData data = (TextProductConfigData) script.execute(
                "getScripts", map);

        return data;
    }
}
