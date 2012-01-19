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
package com.raytheon.edex.uengine.runners;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jep.Jep;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.jep.JepFactory;
import com.raytheon.edex.uengine.jep.JepPool;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;

/**
 * A &mu;Engine script runner for Python scripts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Nov2008    1709       MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class PyMicroEngine extends AMicroEngine {
    /**
     * The JEPP pool used to obtain JEPP instances.
     */
    private static JepPool pool;
    static {
        pool = new JepPool(new JepFactory());
    }

    /**
     * The JEPP instance used to execute the script.
     */
    private Jep jep = null;

    /**
     * The thread ID - used by the JEPP pool.
     */
    private long id;

    /**
     * Constructor.
     */
    public PyMicroEngine() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.AMicroEngine#execute()
     */
    @Override
    public void execute() throws MicroEngineException {
        String wrapper = wrapper(this.script);
        try {
            jep.eval(wrapper);
            jep.eval("result = executeScript()");
            Object result = jep.getValue("result");
            this.result = generateScriptResponse(result);
        } catch (Throwable e) {
            throw new MicroEngineException("Error executing uEngine script.", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.AMicroEngine#initialize()
     */
    @Override
    public void initialize() throws MicroEngineException {
        this.id = Thread.currentThread().getId();
        this.jep = (Jep) pool.borrowObject(id);
        if (this.jep == null) {
            throw new MicroEngineException(
                    "Unable to initialize uEngine script runner, "
                            + "unable to obtain JEPP object from pool");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.AMicroEngine#release()
     */
    @Override
    public void release() throws MicroEngineException {
        pool.returnObject(this.id, this.jep);
    }

    /**
     * Converts the result of executing the script into a <code>
     * List<AbstractResponseMessage></code>
     * . Ensures that unexpected results are wrapped correctly.
     * 
     * @param result
     *            the script result to convert.
     * 
     * @return the converted result
     */
    @SuppressWarnings("unchecked")
    private List<AbstractResponseMessage> generateScriptResponse(Object result) {
        List<AbstractResponseMessage> retVal = null;
        if (result instanceof ArrayList) {
            retVal = (ArrayList<AbstractResponseMessage>) result;
        } else if (result instanceof AbstractResponseMessage) {
            retVal = new ArrayList<AbstractResponseMessage>();
            retVal.add((AbstractResponseMessage) result);
        } else if (result == null) {
            retVal = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError empty = ResponseMessageError
                    .generateErrorResponse(
                            "Script returned empty response list.", null);
            retVal.add(empty);
        } else {
            retVal = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError empty = ResponseMessageError
                    .generateErrorResponse(
                            "Unable to process script response. "
                                    + "Python scripts must return AbstractResponseMessages or ArrayList<AbstractResponseMessage>.",
                            null);
            retVal.add(empty);
        }

        return retVal;
    }

    /**
     * Converts the submitted Python script to one having a single method
     * <code>executeScript()</code> which contains the original script.
     * 
     * @param script
     *            the script to convert
     * 
     * @return the converted script
     */
    private static String wrapper(String script) {
        String wrapper = "def executeScript():\n    "
                + script.replaceAll("\n", "\n    ") + "\n";

        if (wrapper.indexOf("'''") > -1) {
            Pattern pattern = Pattern.compile("'''.*'''", Pattern.DOTALL);
            Matcher matcher = pattern.matcher(wrapper);
            if (matcher.find()) {
                wrapper = wrapper.substring(0, wrapper.indexOf("'''"))
                        + matcher.group().replaceAll("\n    ", "\n")
                        + wrapper.substring(wrapper.indexOf("'''", wrapper
                                .indexOf("'''") + 3) + 3);
            }
        }
        return wrapper;
    }

}
