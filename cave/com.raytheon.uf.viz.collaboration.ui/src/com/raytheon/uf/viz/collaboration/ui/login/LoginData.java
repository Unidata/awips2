package com.raytheon.uf.viz.collaboration.ui.login;

import com.raytheon.uf.viz.collaboration.data.DataUser;

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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class LoginData {
    private String user;

    private String server;

    transient private String password;

    private DataUser.StatusType status;

    private String statusMessage;

    public LoginData(final String user, final String server,
            final String password, final DataUser.StatusType status,
            final String statusMessage) {
        this.user = user;
        this.server = server;
        this.password = password;
        this.status = status;
        this.statusMessage = statusMessage;
    }

    public String getUser() {
        return user;
    }

    public String getPassword() {
        return password;
    }

    public void clearPassword() {
        password = null;
    }

    public DataUser.StatusType getStatus() {
        return status;
    }

    public String getServer() {
        return server;
    }

    public String getMessage() {
        return statusMessage;
    }

    public String getAccount() {
        return user + "@" + server;
    }

    public String toString() {
        return "userId: \"" + user + "\", server \"" + server + "\", status:\""
                + status.value() + "\", statusMessage: \"" + statusMessage
                + "\", pw: "
                + ((password == null) ? "null" : password.length());
    }

    // public void addErrorMessage(String error) {
    // errorMessages.add(error);
    // }
    //
    // public List<String> getErrorMessages() {
    // return errorMessages;
    // }
}
