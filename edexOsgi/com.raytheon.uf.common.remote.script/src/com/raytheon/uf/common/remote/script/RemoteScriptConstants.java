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
package com.raytheon.uf.common.remote.script;

/**
 * Useful constants for remote scripts.
 * <p>
 * The resource defaults noted here are the ones defined in this class and are
 * used when the a property value is not defined. These values can be overridden
 * for the EDEX server by defining a property value in the EDEX resources file:
 * com.raytheon.uf.edex.remote.script.properties.
 * <p>
 * Except where noted a given run request may override a value. See
 * {@link RemoteScriptRunRequest#putProperty(String, String)}.
 * 
 * <pre>
 * Resource properties:
 * 
 * KEY                        VALUES DESCRIPTION
 * ======================================================
 * remote.script.directory    Localized directories to search for scripts. (default
 *                             remoteScripts). Cannot be overridden in a request.
 * remote.script.timeout      Kill script process if it doesn't finish by this
 *                             number of seconds (default 30).
 * remote.script.use.stderr   When true separate standard out and standard error
 *                             (default false).
 * remote.script.setup.error  Exit error to use when unable to run a script
 *                             (default 99).
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2014 #2742      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class RemoteScriptConstants {
    /**
     * Resource key to obtain script directory. Cannot be overridden in script
     * properties. See
     * {@link RemoteScriptRunRequest#putProperty(String, String)}. .
     */
    public static final String scriptDirectoryKey = "remote.script.directory";

    /**
     * Resource/Property key to obtain execution timeout in seconds. See @{link
     * {@link RemoteScriptRunRequest#putProperty(String, String)}.
     */
    public static final String scriptTimeoutKey = "remote.script.timeout";

    /**
     * Resource/Property key to obtain boolean to separate standard error from
     * the standard out stream. See
     * {@link RemoteScriptRunRequest#putProperty(String, String)}.
     */
    public static final String scriptUseStdErrKey = "remote.script.use.stderr";

    /**
     * Resource/Property key to obtain exit value to use when unable to run the
     * script.
     */
    public static final String scriptSetupErrrorKey = "remote.script.setup.error";

    /**
     * Default common static directory for remote scripts. See
     * {@link RemoteScriptRunRequest#putProperty(String, String)}.
     */
    public static final String scriptDirectoryDefault = "remoteScripts";

    /**
     * Default time out value in seconds. See
     * {@link RemoteScriptRunRequest#putProperty(String, String)}.
     */
    public static final String scriptTimeoutDefault = "30";

    /**
     * Default flag to separate standard error from the standard out stream. See
     * {@link RemoteScriptRunRequest#putProperty(String, String)}.
     */
    public static final String scriptUseStdErrDefault = "false";

    /**
     * Error exit value to use when unable to run the script.
     */
    public static final String scriptSetUpErrorDefault = "99";

    private RemoteScriptConstants() {
    }
}
