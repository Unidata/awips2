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

import java.io.File;
import java.util.Map;

import org.quartz.CronExpression;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Legacy script wrapper.
 * <p>
 * The script service is intended to provide a central java interface into
 * legacy scripts run from a cron.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2008            jelkins     Initial creation
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class ScriptService implements ServiceInterface {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ScriptService.class);

    /** A Cron expression representation of the cron line in the legacy system */
    private CronExpression cronExpression;

    /** The script file to execute when running the service */
    private File script;

    /** Additional environment variables to set before running the script */
    private Map<String, String> environmentVariables;

    /** The last portion of the scriptLocation */
    private final String scriptName;

    /**
     * Execute the script service.
     * <p>
     * Adds the environmentVariables into the environment and executes the
     * script service.
     * 
     * @return the exit code returned from the script. Usually 0 if nothing went
     *         wrong.
     * @throws EdexException
     */
    @Override
    public void execute() throws EdexException {

        MainMethod m = new MainMethod(new ProcessBuilder(
                script.getAbsolutePath()));

        if (environmentVariables != null) {
            m.getProcessBuilder().environment().putAll(environmentVariables);
        }

        int exitValue = m.execute();

        if (exitValue == 0) {
            logger.info(scriptName + " execution successful");
        } else {
            throw new EdexException(scriptName
                    + " process terminated abnormally with exit code: "
                    + exitValue);
        }
    }

    /**
     * Construct a new ScriptService
     * <p>
     * 
     * @param scriptLocation
     *            The location of the script
     * @throws EdexException
     */
    public ScriptService(String scriptLocation) throws EdexException {
        this.scriptName = new File(scriptLocation).getName();
        setScriptLocation(scriptLocation);
    }

    /**
     * @return the scriptLocation
     */
    public String getScriptLocation() {
        return script.getAbsolutePath();
    }

    /**
     * @param scriptLocation
     *            the scriptLocation to set
     * @throws EdexException
     */
    public void setScriptLocation(String scriptLocation) throws EdexException {
        this.script = new File(scriptLocation);

        // TODO renabled the following check when it can be made sure that the
        // hydroapps file system exits before any ScriptServices are initialized

        // if (!script.canExecute()) {
        // throw new EdexException(scriptName + " is not executable");
        // }
    }

    /**
     * @return the cronExpression
     */
    public CronExpression getCronExpression() {
        return cronExpression;
    }

    /**
     * @param cronExpression
     *            the cronExpression to set
     */
    public void setCronExpression(CronExpression cronExpression) {
        this.cronExpression = cronExpression;
    }

    /**
     * @return the environmentVariables
     */
    public Map<String, String> getEnvironmentVariables() {
        return environmentVariables;
    }

    /**
     * @param environmentVariables
     *            the environmentVariables to set
     */
    public void setEnvironmentVariables(Map<String, String> environmentVariables) {
        this.environmentVariables = environmentVariables;
    }

}
