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
package com.raytheon.uf.common.backupsvc.notification;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This class generates the backup service notifications to be published or
 * subscribed
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * Jun 28, 2021 84643      Gang Chen    Initial creation
 *
 * </pre>
 *
 * @author gchen
 * @version 1.0
 */
@DynamicSerialize
public class BackupServiceNotification {

    public static final String TOPIC = "edex.alerts.backupsvc";

    /**
     * INCOMING: notification type for incoming backup service jobs; OUTGOING:
     * notification type for outgoing backup service jobs; ALL: notification
     * type for both incoming & outgoing backup service jobs
     */
    public enum NotificationType {
        INCOMING, OUTGOING, ALL;
    }

    @DynamicSerializeElement
    private NotificationType type;

    /**
     * The practice (true) or operational (false) mode flag
     */
    @DynamicSerializeElement
    private boolean practiceMode;

    /**
     * Used only for serialization
     */
    public BackupServiceNotification() {
    }

    public BackupServiceNotification(NotificationType type, boolean practice) {
        this.type = type;
        this.practiceMode = practice;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(" Mode: ");
        builder.append(practiceMode);
        builder.append(" Type: ");
        builder.append(type);
        return builder.toString();
    }

    /**
     * @return the type
     */
    public NotificationType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(NotificationType type) {
        this.type = type;
    }

    public boolean isPracticeMode() {
        return practiceMode;
    }

    public void setPracticeMode(boolean practiceMode) {
        this.practiceMode = practiceMode;
    }
}