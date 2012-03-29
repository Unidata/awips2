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
package com.raytheon.uf.viz.collaboration.comm.provider.info;

import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;

import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class InfoAdapter {
    /**
     * TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 1, 2012            jkorman     Initial creation
     * 
     * </pre>
     * 
     * @author jkorman
     * @version 1.0
     */
    /**
     * TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 1, 2012            jkorman     Initial creation
     * 
     * </pre>
     * 
     * @author jkorman
     * @version 1.0
     */
    private static class MutableVenueInfo implements IVenueInfo {

        private String description;

        private String name;

        private String subject;

        private String id;

        private int participantCount;

        private boolean isModerated = false;

        private boolean isPersistent = false;

        private boolean requiresPassword = false;

        /**
         * 
         * @param description
         */
        public void setVenueDescription(String description) {
            this.description = description;
        }

        /**
         * 
         * @param name
         */
        public void setVenueName(String name) {
            this.name = name;
        }

        /**
         * 
         * @param subject
         */
        public void setVenueSubject(String subject) {
            this.subject = subject;
        }

        /**
         * 
         * @param id
         */
        public void setVenueID(String id) {
            this.id = id;
        }

        /**
         * 
         * @param count
         */
        public void setParticipantCount(int count) {
            participantCount = count;
        }

        /**
         * 
         * @param moderated
         */
        public void setModerated(boolean moderated) {
            isModerated = moderated;
        }

        /**
         * 
         * @param persistent
         */
        public void setPersistent(boolean persistent) {
            isPersistent = persistent;
        }

        public void setRequiresPassword(boolean requiresPassword) {
            this.requiresPassword = requiresPassword;
        }

        /**
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#getVenueDescription()
         */
        @Override
        public String getVenueDescription() {
            return description;
        }

        /**
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#getVenueName()
         */
        @Override
        public String getVenueName() {
            return name;
        }

        /**
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#getVenueSubject()
         */
        @Override
        public String getVenueSubject() {
            return subject;
        }

        /**
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#getVenueID()
         */
        @Override
        public String getVenueID() {
            return id;
        }

        /**
         * 
         */
        @Override
        public int getParticipantCount() {
            return participantCount;
        }

        /**
         * 
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#isModerated()
         */
        @Override
        public boolean isModerated() {
            return isModerated;
        }

        /**
         * 
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#isPersistent()
         */
        @Override
        public boolean isPersistent() {
            return isPersistent;
        }

        /**
         * 
         * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo#requiresPassword()
         */
        @Override
        public boolean requiresPassword() {
            return requiresPassword;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("[%s]:", name));
            sb.append(String.format("[%s]:", id));
            sb.append(String.format("mod[%s]:", (isModerated) ? "T" : "F"));
            sb.append(String.format("pers[%s]:", (isPersistent) ? "T" : "F"));
            sb.append(String
                    .format("pass[%s]:", (requiresPassword) ? "T" : "F"));
            sb.append(String.format("\n   subject     : %s", subject));
            sb.append(String.format("\n   description : %s", description));

            return sb.toString();
        }

    }

    /**
     * Convert ECF room into to a VenueInfo instance.
     * 
     * @param info
     * @return
     */
    public static IVenueInfo createVenueInfo(IChatRoomInfo info) {
        MutableVenueInfo venue = null;
        if (info != null) {

            venue = new MutableVenueInfo();
            venue.setVenueDescription(info.getDescription());
            venue.setVenueName(info.getName());
            venue.setVenueSubject(info.getSubject());
            venue.setVenueID(info.getRoomID().toExternalForm());
            venue.setParticipantCount(info.getParticipantsCount());

            venue.setModerated(info.isModerated());
            venue.setPersistent(info.isPersistent());
            venue.setRequiresPassword(info.requiresPassword());
        }
        return venue;
    }
}
