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
package com.raytheon.edex.meteoLib;

import jep.JepException;

import com.raytheon.uf.common.python.PythonScript;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2008            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MeteoLibPython extends PythonScript {

    public MeteoLibPython(String name, String path) throws JepException {
        super(name, path);
    }

    @Override
    public void evaluateArgument(String name, Object value) throws JepException {
        if (value instanceof float[]) {
            float[] val = (float[]) value;
            jep.setNumeric(name, val, 1, val.length);
        } else {
            super.evaluateArgument(name, value);
        }
    }

    protected Object getExecutionResult() throws JepException {
        if ((Boolean) jep.getValue("type(" + RESULT + ") is tuple")) {
            int size = (Integer) jep.getValue("len(" + RESULT + ")");
            Object[] result = new Object[size];
            for (int i = 0; i < size; i++) {
                result[i] = jep.getValue(RESULT + "[" + i + "]");
            }
            return result;
        } else {
            return super.getExecutionResult();
        }

    }
}
