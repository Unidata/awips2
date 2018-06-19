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
package com.raytheon.viz.gfe.tasks;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;

/**
 * GFE Product Script Task
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2011            randerso     Initial creation
 * Sep 19,2011  10955     rferrel      make sure process destroy is called.
 * May 28,2014  #2841     randerso     Fix null pointer when script is cancelled.
 * Aug 20, 2015 #4749     dgilling     Add cleanUp.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ScriptTask extends AbstractGfeTask {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScriptTask.class);

    private String command;

    private ProcessBuilder processBuilder;

    private Process process;

    public ScriptTask(String name, String command) {
        super(name);
        this.command = command;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.tasks.AbstractGfeTask#start()
     */
    @Override
    public synchronized void start() {
        ProcessBuilder pb = new ProcessBuilder("sh", "-c", getCommand());
        pb.redirectErrorStream(true);
        setProcessBuilder(pb);
        super.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#run()
     */
    @Override
    public void doRun() {
        BufferedWriter log = null;
        try {
            log = new BufferedWriter(new FileWriter(getLogFile()));
            process = processBuilder.start();

            BufferedReader input = new BufferedReader(new InputStreamReader(
                    process.getInputStream()));
            String line = null;

            while ((line = input.readLine()) != null) {
                log.write(line);
                log.write('\n');
                log.flush();
            }

            StringBuilder msg = new StringBuilder("Job ");
            msg.append(getDisplayName());
            Priority priority = Priority.INFO;
            if (process != null) {
                int exitVal = process.waitFor();
                log.write("Exited with error code " + exitVal + "\n");
                if (exitVal != 0) {
                    priority = Priority.PROBLEM;
                }
                msg.append(" exited with code: ").append(exitVal);
            } else {
                msg.append(" was cancelled.");
            }
            statusHandler.handle(priority, msg.toString());
            msg.append("\n");
            log.write(msg.toString());
        } catch (Exception e) {
            statusHandler.handle(
                    Priority.PROBLEM,

                    "Error running job " + getDisplayName() + ": "
                            + e.getLocalizedMessage(), e);

        } finally {
            // DR #10955
            if (process != null) {
                process.destroy();
                process = null;
            }
            if (log != null) {
                try {
                    log.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error closing log file for" + getDisplayName()
                                    + ": " + e.getLocalizedMessage(), e);
                }
            }
        }
    }

    @Override
    public void cancel() {
        this.status = TaskStatus.CANCELED;

        // if process was started
        if (this.process != null) {
            // kill the process and let the normal scheduling process update it.
            process.destroy();
            process = null;
        } else {
            // mark the task finished and let the scheduler know it was canceled
            this.finishedTime = SimulatedTime.getSystemTime().getTime();
            taskCanceled();
        }
    }

    /**
     * @return the command
     */
    @Override
    public String getCommand() {
        return command;
    }

    /**
     * @param processBuilder
     *            the processBuilder to set
     */
    protected void setProcessBuilder(ProcessBuilder processBuilder) {
        this.processBuilder = processBuilder;
    }

    @Override
    public void cleanUp() {
        super.cleanUp();
        processBuilder = null;
    }
}
