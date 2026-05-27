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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.Arrays;
import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * GFE Client Request
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------
 * Dec 06, 2016  6092     randerso  Initial creation
 * Feb 08, 2017  6092     randerso  Cleaned up toString() output
 *
 * </pre>
 *
 * @author randerso
 */

@DynamicSerialize
public class GfeClientRequest extends AbstractGfeRequest {
    public static final String DEFAULT_CONFIG = "gfeConfig";

    public static final String SHUTDOWN_CMD = "ShutDown";

    @DynamicSerializeElement
    private String script;

    @DynamicSerializeElement
    private String configFile;

    @DynamicSerializeElement
    private String user;

    @DynamicSerializeElement
    private Date time;

    @DynamicSerializeElement
    private String[] args;

    /**
     * Default constructor
     */
    public GfeClientRequest() {
        this.script = SHUTDOWN_CMD;
        this.configFile = DEFAULT_CONFIG;
        this.args = new String[0];
    }

    /**
     * Constructor
     *
     * @param script
     *            script to be run
     * @param site
     *            3-letter site id
     * @param configFile
     *            GFE config file
     * @param user
     *            the user name to be used for localization
     * @param args
     *            arguments for script
     */
    public GfeClientRequest(String script, String site, String configFile,
            String user, String[] args) {
        super();
        this.siteID = site;
        this.script = script;
        this.args = (args == null ? new String[0] : args);
        this.configFile = configFile;
        this.user = user;
        this.time = null;
    }

    /**
     * @return the script
     */
    public String getScript() {
        return script;
    }

    /**
     * @param script
     *            the script to set
     */
    public void setScript(String script) {
        this.script = script;
    }

    /**
     * @return the args
     */
    public String[] getArgs() {
        return args;
    }

    /**
     * @param args
     *            the args to set
     */
    public void setArgs(String[] args) {
        this.args = args;
    }

    /**
     * @return the configFile
     */
    public String getConfigFile() {
        return configFile;
    }

    /**
     * @param configFile
     *            the configFile to set
     */
    public void setConfigFile(String configFile) {
        this.configFile = configFile;
    }

    /**
     * @return the user
     */
    public String getUser() {
        return user;
    }

    /**
     * @param user
     *            the user to set
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * @return the time
     */
    public Date getTime() {
        return time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setTime(Date time) {
        this.time = time;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(this.getClass().getSimpleName());
        sb.append("(");
        sb.append("siteID:").append(siteID).append(", ");
        sb.append("script:").append(script).append(", ");
        sb.append("configFile:").append(configFile).append(", ");
        sb.append("user:").append(user).append(", ");
        if (time != null) {
            sb.append("time:").append(TimeUtil.formatDate(time)).append(", ");
        }
        sb.append("args:").append(Arrays.toString(args));
        sb.append(")");

        return sb.toString();
    }
}
