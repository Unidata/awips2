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
package com.raytheon.uf.viz.collaboration.ui.notifier;

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Notifier Task. Holds a list of {@link Notifier} actions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2014    2632    mpduff      Initial creation.
 * Mar 27, 2014    2632    mpduff      Implemented toString()
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NotifierTask {

    /** Set of Notifiers for this task */
    @XmlElements({ @XmlElement(name = "notifier") })
    private Set<Notifier> notifierList;

    /** User to whom this task applies */
    @XmlElement
    private String userName;

    /** Path to the sound file */
    @XmlElement
    private String soundFilePath;

    /** Recurring pounce flag */
    @XmlElement
    private boolean recurring;

    /**
     * Default Constructor.
     */
    public NotifierTask() {
        notifierList = new HashSet<Notifier>();
    }

    /**
     * Create with the provided list of Notifiers.
     * 
     * @param notifiers
     *            List of notifiers
     */
    public NotifierTask(Notifier... notifiers) {
        for (Notifier notifier : notifiers) {
            notifierList.add(notifier);
        }
    }

    /**
     * Add a notifier.
     * 
     * @param notifier
     *            The notifier to add
     */
    public void addNotifier(Notifier notifier) {
        notifierList.add(notifier);
    }

    /**
     * Get the notifiers.
     * 
     * @return The Set of Notifiers
     */
    public Set<Notifier> getNotifiers() {
        return notifierList;
    }

    /**
     * 
     * @return the userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * @param userName
     *            the userName to set
     */
    public void setUserName(String userName) {
        this.userName = userName;
    }

    /**
     * @return the notifierList
     */
    public Set<Notifier> getNotifierList() {
        return notifierList;
    }

    /**
     * @param notifierList
     *            the notifierList to set
     */
    public void setNotifierList(Set<Notifier> notifierList) {
        this.notifierList = notifierList;
    }

    /**
     * @return the soundFilePath
     */
    public String getSoundFilePath() {
        return soundFilePath;
    }

    /**
     * @param soundFilePath
     *            the soundFilePath to set
     */
    public void setSoundFilePath(String soundFilePath) {
        this.soundFilePath = soundFilePath;
    }

    /**
     * @return the recurring
     */
    public boolean isRecurring() {
        return recurring;
    }

    /**
     * @param recurring
     *            the recurring to set
     */
    public void setRecurring(boolean recurring) {
        this.recurring = recurring;
    }

    public boolean isSoundValid() {
        return soundFilePath != null && soundFilePath.length() > 0;
    }

    public boolean containsSendMessage() {
        return notifierList.contains(Notifier.SendMessage);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((notifierList == null) ? 0 : notifierList.hashCode());
        result = prime * result + (recurring ? 1231 : 1237);
        result = prime * result
                + ((soundFilePath == null) ? 0 : soundFilePath.hashCode());
        result = prime * result
                + ((userName == null) ? 0 : userName.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof NotifierTask)) {
            return false;
        }
        NotifierTask other = (NotifierTask) obj;
        if (notifierList == null) {
            if (other.notifierList != null) {
                return false;
            }
        } else if (!notifierList.equals(other.notifierList)) {
            return false;
        }
        if (recurring != other.recurring) {
            return false;
        }
        if (soundFilePath == null) {
            if (other.soundFilePath != null) {
                return false;
            }
        } else if (!soundFilePath.equals(other.soundFilePath)) {
            return false;
        }
        if (userName == null) {
            if (other.userName != null) {
                return false;
            }
        } else if (!userName.equals(other.userName)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String nl = StringUtil.NEWLINE;
        StringBuilder sb = new StringBuilder();
        sb.append("User:  " + this.userName).append(nl);
        sb.append("Sound: " + this.soundFilePath).append(nl);
        for (Notifier notifier : this.notifierList) {
            sb.append(notifier.getDescription()).append("    ");
        }

        return sb.toString();
    }
}
