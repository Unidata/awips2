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
package com.raytheon.uf.edex.ohd;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Abstract Main method class provides supporting functions for the main methods
 * found in the ohd library.
 * <p>
 * After calling the constructor and before calling any of the process related
 * functions, the start() method should be called to begin the process. If the
 * start() method isn't called, a ProcessNotStarted() exception will be thrown.
 * <p>
 * This class is somewhat of a hybrid ProcessBuilder & Process class. It has all
 * the same methods as a Process class in addition to the start() method which
 * works the same as the ProcessBuilder start() method.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2008            jelkins     Initial creation
 * Oct 19, 2012  #1274     bgonzale    Set AppContext on the process builder in ctor.
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class MainMethod extends Process {

    protected Log log;

    private ProcessBuilder processBuilder;

    private Process process;

    private BufferedReader fromStdOut;

    private BufferedReader fromStdErr;

    /**
     * Contains the initial command used to initialize the process. This field
     * is useful to use when overriding the start method like in the RocChecker
     * and RpfBatch classes.
     */
    protected ArrayList<String> initialCommand;

    /**
     * Thrown when the process has not yet been started.
     * <p>
     * Make sure to call start()!
     */
    public class ProcessNotStartedException extends RuntimeException {

        private static final long serialVersionUID = 5620316335774758460L;

    }

    /**
     * Run a program
     * 
     * @param args
     *            The first argument in the string list _must_ be only the
     *            command without any arguments. The arguments to the command
     *            are given by proceeding elements in the argument string list.
     * @return The return value of the program.
     */
    public static int runProgram(String... args) {
        MainMethod program = new MainMethod(new ProcessBuilder(args));
        return program.execute();
    }

    /**
     * Default constructor
     * <P>
     * Constructs a main method process from the given class.
     * 
     * @param arguments
     *            the string of arguments to pass to the main process
     */
    public MainMethod(ProcessBuilder builder) {

        this.processBuilder = builder;
        this.log = LogFactory.getLog(processBuilder.getClass());

        try {
            processBuilder.environment().put(
                    "apps_dir",
                    new File(PropertiesFactory.getInstance().getEnvProperties()
                            .getEnvValue("SHAREDIR")
                            + File.separator + "hydroapps").getCanonicalPath());
            AppsDefaults.getInstance().setAppContext(processBuilder);
        } catch (IOException e) {
            log.error("Unable to get apps_dir", e);
        }

        this.initialCommand = new ArrayList<String>(getProcessBuilder()
                .command());

    }

    /**
     * Execute the method.
     * <p>
     * 
     * @return the exit value of the method. Usually 0 if nothing went wrong.
     */
    public int execute() {
        int exitValue = 0;
        // Start the main method
        this.start();
        try {
            this.waitFor();
        } catch (InterruptedException e) {
            log.error("Native process interrupted!!", e);
        } finally {
            exitValue = this.exitValue();
            this.closeStreams();
            this.destroy();
        }

        if (exitValue != 0) {
            StringBuffer error = new StringBuffer();
            for (int i = 0; i < processBuilder.command().size(); i++) {
                error.append(processBuilder.command().get(i)).append(" ");
            }
            error.append("failed with exit code " + exitValue);
            log.error(error);
        }

        return exitValue;
    }

    /**
     * Get the processBuilder object of this process
     * <p>
     * 
     * @return the processBuilder
     */
    public ProcessBuilder getProcessBuilder() {
        return processBuilder;
    }

    /**
     * Start the process.
     * <p>
     * Output that would normally be written to stdout and stderror are
     * redirected to the log.
     */
    private void start() {
        try {
            process = processBuilder.start();

            // TODO seperate the stream readers into threads

            // redirect the normal output to log as info
            fromStdOut = new BufferedReader(new InputStreamReader(
                    process.getInputStream()));
            // redirect the normal error to log as error
            fromStdErr = new BufferedReader(new InputStreamReader(
                    process.getErrorStream()));

        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
    }

    /**
     * @param br
     * @return
     */
    private String read(BufferedReader br) {
        StringBuffer returnString = new StringBuffer();
        String line;
        try {
            while ((line = br.readLine()) != null) {
                returnString.append(line).append("\n");
            }
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
        return returnString.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#destroy()
     */
    @Override
    public void destroy() {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        process.destroy();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#exitValue()
     */
    @Override
    public int exitValue() {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        return process.exitValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#getErrorStream()
     */
    @Override
    public InputStream getErrorStream() {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        return process.getErrorStream();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#getInputStream()
     */
    @Override
    public InputStream getInputStream() {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        return process.getInputStream();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#getOutputStream()
     */
    @Override
    public OutputStream getOutputStream() {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        return process.getOutputStream();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Process#waitFor()
     */
    @Override
    public int waitFor() throws InterruptedException {
        if (process == null) {
            throw new ProcessNotStartedException();
        }
        int retVal = process.waitFor();
        String stdOut = read(fromStdOut);
        String stdErr = read(fromStdErr);
        if (!stdOut.isEmpty()) {
            log.info(stdOut);
        }
        if (!stdErr.isEmpty()) {
            log.error(stdErr);
        }
        return retVal;
    }

    public void closeStreams() {
        try {
            if (this.getInputStream() != null) {
                this.getInputStream().close();
            }
        } catch (IOException e) {
            log.error("Unable to close input stream! Resource leak possible.",
                    e);
        }
        try {
            if (this.getOutputStream() != null) {
                this.getOutputStream().close();
            }
        } catch (IOException e) {
            log.error("Unable to close output stream! Resource leak possible",
                    e);
        }

        try {
            if (this.getErrorStream() != null) {
                this.getErrorStream().close();
            }
        } catch (IOException e) {
            log.error("Unable to close error stream! Resource leadk possible",
                    e);
        }
    }

}
