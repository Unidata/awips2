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
package com.raytheon.viz.aviation.climatedata;

import java.util.ArrayList;

import jep.JepException;

import com.raytheon.uf.common.python.multiprocessing.PythonProcess;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2010            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class PythonClimateDataProcess extends PythonProcess {

    /**
     * @param filePath
     * @param anIncludePath
     * @param classLoader
     * @throws JepException
     */
    public PythonClimateDataProcess(String filePath, String anIncludePath,
            ClassLoader classLoader) throws JepException {
        super(filePath, anIncludePath, classLoader);
    }

    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof ArrayList) {
            ArrayList<?> list = (ArrayList<?>) argValue;
            int size = list.size();
            String cmd = argName + " = ['";

            for (int i = 0; i < size; i++) {
                cmd += list.get(i);

                if (i < (size - 1)) {
                    cmd += "', '";
                }
            }

            cmd += "']";
            jep.eval(cmd);
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }
}
