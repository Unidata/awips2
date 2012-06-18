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
package com.raytheon.uf.viz.collaboration.ui.role;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class UserInformation {
    @XmlElement
    private List<Info> info;

    /**
     * @return the infos
     */
    public List<Info> getInfo() {
        return info;
    }

    /**
     * @param infos
     *            the infos to set
     */
    public void setInfo(List<Info> info) {
        this.info = info;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class Info {

        @XmlAttribute
        private String name;

        @XmlElement
        private String[] attibuteOptions;

        @XmlAttribute
        private boolean allowMultiple;

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

        /**
         * @return the attibuteOptions
         */
        public String[] getAttibuteOptions() {
            return attibuteOptions;
        }

        /**
         * @param attibuteOptions
         *            the attibuteOptions to set
         */
        public void setAttibuteOptions(String[] attibuteOptions) {
            this.attibuteOptions = attibuteOptions;
        }

        /**
         * @param allowMultiple
         *            the allowMultiple to set
         */
        public void setAllowMultiple(boolean allowMultiple) {
            this.allowMultiple = allowMultiple;
        }

        /**
         * @return the allowMultiple
         */
        public boolean isAllowMultiple() {
            return allowMultiple;
        }
    }
}