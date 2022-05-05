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
import java.util.List;
import java.util.stream.Collectors;

import com.raytheon.uf.common.python.PythonScript;

import jep.JepConfig;
import jep.JepException;

/**
 * PythonScript used to execute climate python files
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2019  7878      tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class ClimatePythonScript extends PythonScript {

    public ClimatePythonScript(JepConfig config) throws JepException {
        super(config);
    }

    public ClimatePythonScript(JepConfig config, String filePath)
            throws JepException {
        super(config, filePath);
    }

    public ClimatePythonScript(JepConfig config, List<String> preEvals)
            throws JepException {
        super(config, preEvals);
    }

    public ClimatePythonScript(JepConfig config, String filePath,
            List<String> preEvals) throws JepException {
        super(config, filePath, preEvals);
    }

    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof ArrayList) {
            ArrayList<?> list = (ArrayList<?>) argValue;
            String prefix = argName + " = ['";
            String suffix = "']";
            String cmd = list.stream().map(Object::toString)
                    .collect(Collectors.joining("', '", prefix, suffix));
            jep.eval(cmd);
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }

}
