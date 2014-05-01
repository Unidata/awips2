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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.utility.StreamHandler;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;

/**
 * A &mu;Engine script runner for system scripts. This script runner assumes the
 * script provided is a path to a valid executable file on the file system. The
 * script may include command line arguments.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 17Nov2008    1709       MW Fegan    Initial creation.
 * 19Sep2011    10955      rferrel     make sure process destroy is called.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SysMicroEngine extends AMicroEngine {

    /**
     * 
     */
    public SysMicroEngine() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.AMicroEngine#execute()
     */
    @Override
    public void execute() throws MicroEngineException {
        List<String> command = new ArrayList<String>();
        /*
         * break the "script" into parts. elements[0] will be the process to
         * execute.
         */
        logger.debug("Execution: script is " + script);
        String[] elements = this.script.split(" ");
        for (String item : elements) {
            command.add(item);
        }
        if (!validProcess(elements[0])) {
            throw new MicroEngineException("Unable to execute script, ["
                    + elements[0] + "] is not a valid process");
        }

        /* run the script */
        ProcessBuilder procBld = null;
        Process proc = null;
        StreamHandler output = null;
        int exitCode;
        try {
            /* create and start process w/ merged output */
            procBld = new ProcessBuilder(command);
            procBld.redirectErrorStream(true);
            proc = procBld.start();
            /* monitor the output */
            output = new StreamHandler(proc.getInputStream());
            output.start();

            /* wait for the process to terminate */
            exitCode = proc.waitFor();
            if (exitCode != 0) {
                throw new MicroEngineException("Program [" + elements[0]
                        + "] terminated abnormally");
            }
            this.result = generateScriptResponse(output.getOutput());
        } catch (Exception e) {
            throw new MicroEngineException(
                    "Encountered errors executing script", e);
        } finally {
            // DR #10995
            if (proc != null) {
                proc.destroy();
            }
        }

    }

    /**
     * checks to see if the specified process is a valid process.
     * 
     * @param process
     *            the path to the process to check
     * @return true if the process is valid
     */
    private boolean validProcess(String process) {
        try {
            File file = new File(process);
            return file.canExecute();
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Wraps the result of the process execution into a list for return to the
     * caller.
     * 
     * @param result
     *            the value to wrap
     * 
     * @return the wrapped value
     */
    private List<AbstractResponseMessage> generateScriptResponse(String result) {
        AbstractResponseMessage msg = new ResponseMessageGeneric(result);
        List<AbstractResponseMessage> retVal = new ArrayList<AbstractResponseMessage>();
        retVal.add(msg);
        return retVal;
    }
}
