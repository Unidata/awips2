package com.raytheon.uf.viz.collaboration.ui.login;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

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
 * Data class that provides log on information. All but the password may be
 * saved in a localized file.
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
@XmlRootElement(name = "CollaborationLogon")
@XmlAccessorType(XmlAccessType.NONE)
public class LoginData implements ISerializableObject {
    @XmlElement(name = "user")
    private String user;

    @XmlElement(name = "server")
    private String server;

    private transient String password;

    @XmlElement(name = "mode")
    private IPresence.Mode mode;

    @XmlElement(name = "modeMessage")
    private String modeMessage;

    public LoginData() {
        this.user = "";
        this.server = "";
        this.password = "";
        this.mode = IPresence.Mode.AVAILABLE;
        this.modeMessage = "";
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

    public IPresence.Mode getMode() {
        return mode;
    }

    public String getServer() {
        return server;
    }

    public String getModeMessage() {
        return modeMessage;
    }

    public UserId getAccount() {
        return new UserId(user, server);
    }

    public void setUser(String user) {
        this.user = user;
    }

    public void setServer(String server) {
        this.server = server;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setMode(IPresence.Mode mode) {
        this.mode = mode;
    }

    public void setModeMessage(String statusMessage) {
        this.modeMessage = statusMessage;
    }

    public String toString() {
        return "userId: \"" + user + "\", server \"" + server + "\", mode:\""
                + mode.toString() + "\", modeMessage: \"" + modeMessage
                + "\", pw: "
                + ((password == null) ? "null" : password.length());
    }
}
