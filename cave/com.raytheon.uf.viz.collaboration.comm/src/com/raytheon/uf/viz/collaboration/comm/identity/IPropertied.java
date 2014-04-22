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

import java.util.Collection;

/**
 * Property interface for collaboration messages
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

public interface IPropertied {

    /**
     * Simple key/value type for collaboration
     * 
     */
    public static class Property {
        private String value;
        
        private String key;

        /**
         * 
         * @param key
         * @param value
         */
        public Property(String key, String value) {
            this.key = key;
            this.value = value;
        }

        /**
         * @return the value
         */
        public String getValue() {
            return value;
        }

        /**
         * @param value the value to set
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * @return the key
         */
        public String getKey() {
            return key;
        }

        /**
         * @param key the key to set
         */
        public void setKey(String key) {
            this.key = key;
        }
        
        @Override
        public String toString() {
            return String.format("[%s{%s}]", key, value);
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((key == null) ? 0 : key.hashCode());
            result = prime * result + ((value == null) ? 0 : value.hashCode());
            return result;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Property other = (Property) obj;
            if (key == null) {
                if (other.key != null)
                    return false;
            } else if (!key.equals(other.key))
                return false;
            if (value == null) {
                if (other.value != null)
                    return false;
            } else if (!value.equals(other.value))
                return false;
            return true;
        }
    }
    
    /**
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public void setProperty(String key, String value);
    
    /**
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public String getProperty(String key, String defaultValue);
    
    /**
     * Gets the message properties as a collection of key, value
     * pairs. Always returns a not-null value.
     * @return
     */
    public Collection<Property> getProperties();
    

}
