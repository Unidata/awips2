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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This contains the results from running a remote script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014 2743       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@DynamicSerialize
public class RemoteScriptRunResponse {
    /**
     * The exit status for the script.
     */
    @DynamicSerializeElement
    private Integer exitStatus;

    /**
     * The scripts output either just stdout or a combination of stadout and
     * stderr. See {@link RemoteScriptConstants}.
     */
    @DynamicSerializeElement
    private String output = "";

    /**
     * When requested a separate sting with stderr. See
     * {@link RemoteScriptConstants}.
     */
    @DynamicSerializeElement
    private String error = "";

    /**
     * When true script process timed out and was killed otherwise false. See
     * {@link RemoteScriptConstants}.
     */
    @DynamicSerializeElement
    private boolean timedOut = false;

    /**
     * Default Constructor.
     */
    public RemoteScriptRunResponse() {
    }

    /**
     * Get the exit status for the process running the script.
     * 
     * @return exitStatus
     */
    public Integer getExitStatus() {
        return exitStatus;
    }

    /**
     * Set the exit status for script's process. Should only be used by the
     * handler and serialization.
     * 
     * @param exitStatus
     */
    public void setExitStatus(Integer exitStatus) {
        this.exitStatus = exitStatus;
    }

    /**
     * Get the script's output. Based on the request properties this is either
     * just stdout or a combination of stdout and stderr. See
     * {@link RemoteScriptConstants}.
     * 
     * @return output - never null
     */
    public String getOutput() {
        return output;
    }

    /**
     * Set the script's output. Based on the request properties this is either
     * just stdout or a combination of stdout and stderr. Should only be used by
     * the handler and serialization. See {@link RemoteScriptConstants}.
     * 
     * @param output
     */
    public void setOutput(String output) {
        this.output = (output == null) ? "" : output;
    }

    /**
     * Get script's stderr when not placed in output.
     * 
     * @return error - never null
     */
    public String getError() {
        return error;
    }

    /**
     * Set script's stderr when not place in output. Should only be used by the
     * handler and serialization.
     * 
     * @param error
     */
    public void setError(String error) {
        this.error = (error == null) ? "" : error;
    }

    /**
     * Flag to indicate script did not finish in the desired number of seconds.
     * See {@link RemoteScriptConstants}.
     * 
     * @return true when script process is killed otherwise false
     */
    public boolean isTimedOut() {
        return timedOut;
    }

    /**
     * Set the timed out flag. Should only be used by the handler and
     * serialization. See {@link RemoteScriptConstants}.
     * 
     * @param timeOut
     */
    public void setTimedOut(boolean timedOut) {
        this.timedOut = timedOut;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    public String toString() {
        StringBuilder sb = new StringBuilder(this.getClass().getName());
        sb.append("[");
        sb.append("exitStatus: ").append(getExitStatus());
        sb.append(", timedOut: ").append(isTimedOut());
        sb.append(", error: \"").append(getError()).append("\"");
        sb.append(", output: \"").append(getOutput()).append("\"");
        sb.append("]");
        return sb.toString();
    }
}
