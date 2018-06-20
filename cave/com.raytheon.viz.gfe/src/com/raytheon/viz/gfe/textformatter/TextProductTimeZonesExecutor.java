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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.concurrent.IPythonExecutor;

/**
 * Executor object to get the necessary time zones for a text product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductTimeZonesExecutor implements
        IPythonExecutor<FormatterScript, Collection<String>> {

    private final Collection<String> zones;

    private final String officeTimeZone;

    /**
     * @param zones
     * @param officeTimeZone
     */
    public TextProductTimeZonesExecutor(Collection<String> zones,
            String officeTimeZone) {
        this.zones = zones;
        this.officeTimeZone = officeTimeZone;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.IPythonExecutor#execute(com.
     * raytheon.uf.common.python.PythonInterpreter)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Collection<String> execute(FormatterScript script)
            throws JepException {
        Map<String, Object> map = new HashMap<String, Object>(2, 1f);
        map.put("zones", zones);
        map.put("officeTZ", officeTimeZone);
        Collection<String> timeZones = (Collection<String>) script.execute(
                "getTimeZones", map);
        return timeZones;
    }
}
