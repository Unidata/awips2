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

package com.raytheon.edex.adapterSrv;

import java.util.ArrayList;

import com.raytheon.edex.msg.Command;
import com.raytheon.edex.msg.ProgramOutput;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.utility.StreamHandler;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Receives a canonical XML message from a mule endpoint containing a command
 * and associated arguments necessary for running an external command line
 * application.
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09August2006 17          Phillippe   Initial Creation
 * 10August2006 17          Phillippe   Output is now persisted as XML in
 *                                      ..\opt\data\output  
 * 1Sept2006    17          Phillippe   Modified to use JiBX to create XML output
 * 12Feb2007       TO5      MW Fegan    Modified to return program output as XML.
 * 26Nov2008                chammack    Camel refactor
 * 19Sep2011    10955       rferrel     make sure process destroy is called.
 * </pre>
 * 
 * 
 * @author bphillip
 * 
 */
public class AdapterSrv {

    protected String process(String msg) throws EdexException {
        Command command = null;
        ArrayList<String> execCommand = new ArrayList<String>();
        ProgramOutput progOut = null;
        ProcessBuilder procBuilder;
        Process proc = null;
        StreamHandler output;
        int exitCode;

        try {
            long executionTime = System.currentTimeMillis() / 1000;

            /* get the Command object and extract the command line */
            command = (Command) SerializationUtil.unmarshalFromXml(msg);
            execCommand.add(command.getName());
            String[] args = command.getArguments();
            if (args != null && args.length != 0) {
                for (String arg : command.getArguments()) {
                    execCommand.add(arg);
                }
            }

            // Creates and starts a new process with the given command/args.
            // Also merges error stream with the output stream.
            // logger.debug("process() executing " + execCommand.get(0));
            procBuilder = new ProcessBuilder(execCommand);
            procBuilder.redirectErrorStream(true);
            proc = procBuilder.start();

            // Monitors output stream of created process
            output = new StreamHandler(proc.getInputStream());
            output.start();

            // Waits for child process to terminate
            exitCode = proc.waitFor();
            if (exitCode != 0) {
                throw new EdexException("Program: " + execCommand.get(0)
                        + " Terminated Abnormally");
            }

            progOut = new ProgramOutput(executionTime, execCommand,
                    output.getOutput());
            // logger.debug("process() execution complete");

        } catch (Exception e) {
            throw new EdexException(
                    "AdapterSrv: Error running external executable: "
                            + Util.printString(command), e);
        } finally {
            // #DR 10955
            if (proc != null) {
                proc.destroy();
            }
        }
        return progOut.toXML();
    }

}
