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
package com.raytheon.edex.plugin.shef.alarms;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

public enum CmdlineOptionId {

    DB_NAME("Database Name") {

        public String toString() {
            return "d";
        }

    },

    PRODUCT_ID("Product Id") {

        public String toString() {
            return "p";
        }

    },

    REPORT_MODE("Report mode") {
        public String toString() {
            return "r";
        }

    },

    MINUTES("minutes") {

        public String toString() {
            return "m";
        }
    },

    FILE_SUFFIX("File Suffix") {
        public String toString() {
            return "s";
        }

    },

    FLAGS("Include Flags") {
        public String toString() {
            return "f";
        }

    },

    PE("Physical Element") {
        public String toString() {
            return "e";
        }

    };

    CmdlineOptionId(String desc) {
        this.desc = desc;
    }

    public String description() {
        return this.desc;
    }

    private String desc;
}
