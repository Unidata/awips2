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
package com.raytheon.uf.viz.datadelivery.system;

/**
 * OpsNet field name enumeration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012   730       jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public enum OpsNetFieldNames {
        /** Dataset Name */
        NAME("Dataset Name"),
        /** DataType*/
        TYPE("Datatype"),
        /** Dataset Size */
        SIZE("Dataset Size"),
        /** Dataset Frequency */
        FREQUENCY("Dataset Frequency");

        /** Dataset field name*/
        private final String fieldName;

        private OpsNetFieldNames(String fieldName) {
            this.fieldName = fieldName;
        }

        /**
         * Get field name.
         * 
         * @return Field Name
         */
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public String toString() {
            return fieldName;
        }
    }
