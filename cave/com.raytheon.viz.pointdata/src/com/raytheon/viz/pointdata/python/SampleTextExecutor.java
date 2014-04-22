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
package com.raytheon.viz.pointdata.python;

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.viz.pointdata.PlotData;

/**
 * Task to get the sample data text if the plot data is valid. Uses the python
 * code of the plot delegate's isValid() and getSampleText() methods to get the
 * sample text.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2014 2868       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SampleTextExecutor extends AbstractPlotDelegateExecutor implements
        IPythonExecutor<PlotPythonScript, String> {

    public SampleTextExecutor(PlotData data) {
        super(data);
    }

    @Override
    public String execute(PlotPythonScript script) throws JepException {
        String result = null;
        Map<String, Object> args = new HashMap<String, Object>();
        args.put("rec", plotData);
        Boolean isValid = (Boolean) script.execute("isValid", args);
        if (isValid.booleanValue()) {
            result = (String) script.execute("getSampleText", args);
        }

        return result;
    }

}
