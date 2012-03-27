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
package com.raytheon.uf.viz.collaboration.comm.identity;

/**
 * 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */

public interface IPresence extends IPropertied {

    public static enum Mode {
        AVAILABLE("Available"), AWAY("Away"), CHAT("chat"), DND("Do Not Disturb"), EXTENDED_AWAY(
                "Extended Away"), INVISIBLE("Invisible");

        private final String mode;

        private Mode(String mode) {
            this.mode = mode;
        }

        public String getMode() {
            return mode;
        }
    };

    public static enum Type {
        AVAILABLE("available"), ERROR("error"), SUBSCRIBE("subscribe"), SUBSCRIBED(
                "subscribed"), UNAVAILABLE("unavailable"), UNKNOWN("unknown"), UNSUBSCRIBE(
                "unsubscribe"), UNSUBSCRIBED("unsubscribed");

        private final String type;

        /**
         * 
         * @param type
         */
        private Type(String type) {
            this.type = type;
        }

        /**
         * 
         * @return
         */
        public String getType() {
            return type;
        }
    };

    
    /**
     * 
     * @return
     */
    Mode getMode();

    /**
     * 
     * @return
     */
    void setMode(Mode mode);

    /**
     * 
     * @return
     */
    Type getType();

    /**
     * 
     * @return
     */
    void setType(Type type);
    
    /**
     * Get the status message for this presence.
     * @return The status message.
     */
    String getStatusMessage();
    
    /**
     * Set the status message for this presence. 
     * @param statusMessage The status message.
     */
    void setStatusMessage(String statusMessage);
    
}
