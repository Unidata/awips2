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
package com.raytheon.edex.plugin.shef.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.shef.data.QC;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2008 387        M. Duff     Initial Creation.
 * Dec 16, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */

public class BitUtils {
    /** The logger */
    private static final Logger log = LoggerFactory
            .getLogger("com.raytheon.edex.plugin.shef.util");

    /**
     * Check if the specified bit of the quality code is set.
     * 
     * @param bitPosition
     *            position of the bit to check
     * @param qualityCode
     *            the code to check
     * @return true if bit is set
     */
    public static boolean checkQcBit(long bitPosition, long qualityCode) {
        boolean returnVal = false;

        /*
         * Variable used to set a specific bit in bit string to 1; initialized
         * as 0000000000000000 0000000000000001
         */
        int mask = 1;
        long bitwise_AND_result;

        /*
         * The mask is employed to set a specific bit to the value of 1 in a
         * 32-bit string while the value of the other bits. The mask is
         * leftwardly shifted to the bit position of the bit being checked.
         */
        mask = mask << bitPosition;

        /*
         * To check the setting of the specified bit, the mask variable is
         * compared to the bit string of quality control variable. The end
         * result yields an integer value.
         */
        bitwise_AND_result = qualityCode & mask;

        /*
         * The following conditional block checks the setting of the specified
         * bit. The check determined by the return value of the bitwise AND
         * operation. If the return value is greater than zero (0), the
         * specified bit is set.
         */

        if (bitwise_AND_result > 0) {
            returnVal = true;
        } else {
            returnVal = false;
        }

        return returnVal;
    }

    /**
     * Sets the QC bits
     * 
     * @param qc
     *            - The QC object
     * @return Status - -1 for invalid, 1 for valid
     */
    public static long setQcBit(QC qc) {
        /*
         * Variable used to set a specific bit in bit string to 1; initialized
         * as 0000000000000000 0000000000000001
         */
        long mask = 1;
        long bitwise_inclusive_OR_result;
        long bitwise_AND_result;
        long status;

        /* Ensure that a valid bit position is requested. */

        if (qc.getBitPosition() < ShefConstants.SIGN_QC) {
            status = ShefConstants.VALID_QC_REQUEST;

            /* if setting the bit ON */

            if (qc.getSetting() == 1) {

                /*
                 * The mask is employed to set a specific bit to the value of 1
                 * in a 32-bit string while hiding the value of the other bits.
                 * The mask is leftwardly shifted to the bit position of the bit
                 * being referenced.
                 */

                mask = mask << qc.getBitPosition();

                /*
                 * The bitwise inclusive OR operation is used to set the
                 * specified bit. Upon completion, the bit is written to
                 * qualityCode memory location.
                 */

                bitwise_inclusive_OR_result = qc.getQualityCode() | mask;

                qc.setQualityCode(bitwise_inclusive_OR_result);
            } else {
                /*
                 * if setting the bit OFF. first build a mask that has all ones
                 * except for the bit in question, then AND the mask with the
                 * existing value to turn off the single bit.
                 */

                mask = ShefConstants.ALL_ONES ^ (mask << qc.getBitPosition());

                bitwise_AND_result = qc.getQualityCode() & mask;

                qc.setQualityCode(bitwise_AND_result);
            }
        } else {
            log.warn("Invalid request made in setQcBit() Method.");
            status = ShefConstants.INVALID_QC_REQUEST;
        }
        return status;
    }
}
