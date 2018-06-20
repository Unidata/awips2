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
package com.raytheon.viz.hydrocommon.util;

/**
 * Hydro QC utilities.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2008            mpduff      Initial creation.
 * 11/18/2008   1662       grichard    Made private consts public.
 * 1/4/2009                mpduff      Added buildQcWhere method.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class QualityCodeUtil {
    public static final int QC_PASSED = 101;

    public static final int QC_QUESTIONABLE = 102;

    public static final int QC_FAILED = 103;

    public static final int QC_NOT_PASSED = 104;

    public static final int QC_NOT_FAILED = 105;

    public static final int QC_DEFAULT = 110;

    public static final int SIGN_QC = 31;

    public static final int CERTAINTY_QC = 30;

    public static final int NOTQUEST_QC = 29;

    public static final int EXTERN_QC = 23;

    public static final int MANUAL_QC = 22;

    public static final int GROSSRANGE_QC = 21;

    public static final int EXTERN_NOTQUEST_QC = 20;

    public static final int REASONRANGE_QC = 19;

    public static final int ROC_QC = 18;

    public static final int OUTLIER_QC = 17;

    public static final int SCC_QC = 16;

    public static final int MSC_QC = 15;

    public static final int QC_GROSSRANGE_FAILED = 120;

    public static final int QC_MANUAL_PASSED = 121;

    public static final int QC_MANUAL_QUEST = 122;

    public static final int QC_MANUAL_FAILED = 123;

    public static final int QC_MANUAL_NEW = 124;

    public static final int QC_EXTERN_QUEST = 125;

    public static final int QC_EXTERN_FAILED = 126;

    public static final int QC_REASONRANGE_FAILED = 127;

    public static final int QC_ROC_PASSED = 128;

    public static final int QC_ROC_FAILED = 129;

    public static final int QC_OUTLIER_PASSED = 130;

    public static final int QC_OUTLIER_FAILED = 131;

    public static final int QC_SCC_PASSED = 132;

    public static final int QC_SCC_FAILED = 133;

    public static final int QC_MSC_PASSED = 134;

    public static final int QC_MSC_FAILED = 135;

    public static final int FALSE_QC = 0;

    public static final int TRUE_QC = 1;

    public static final int INVALID_QC_REQUEST = -1;

    public static final int VALID_QC_REQUEST = 1;

    /**
     * Bit pattern 0110111111111111 1111111111111111 yields 1,879,048,191.
     */
    public static final int DEFAULT_QC_VALUE = 1879048191;

    /**
     * Bit pattern 0110000000000000 0000000000000000 yields 1,610,612,736.
     */
    public static final int GOOD_QUESTIONABLE_THRESHOLD = 1610612736;

    /**
     * Bit pattern 0100000000000000 0000000000000000 yields 1,073,741,824.
     */
    public static final int QUESTIONABLE_BAD_THRESHOLD = 1073741824;

    /**
     * Bit pattern 0111111111111111 1111111111111111 yields 2,147,483,647. =
     * 2**31 - 1
     */
    public static final int ALL_ONES = 2147483647;

    /**
     * Check if the specified bit of the quality code is set.
     * 
     * @param bitPosition
     *            The bit position
     * @param qualityCode
     *            The quality code
     */
    public static boolean checkQcBit(int bitPosition, long qualityCode) {

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
            return true;
        } else {
            return false;
        }
    }

    public static long setQcCode(int bitGrpDescr) {
        long qualityCode = 0;
        switch (bitGrpDescr) {
        case QC_DEFAULT:
            /* Set the quality control code to the default value. */
            qualityCode = DEFAULT_QC_VALUE;
            break;
        case QC_GROSSRANGE_FAILED:
            /*
             * Set the quality control code so that the specific bit for gross
             * range check indicates it failed, and therefore set the summary
             * certainty bit to indicated the failure.
             */
            qualityCode = setQcBit(GROSSRANGE_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_REASONRANGE_FAILED:
            /*
             * Set the quality control code so that the specific bit for
             * reasonableness range check indicates failure, and therefore set
             * the summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(REASONRANGE_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_ROC_FAILED:
            /*
             * Set the quality control code so that the specific bit for roc
             * check indicates it failed, and therefore set the summary
             * questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(ROC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_ROC_PASSED:
            /*
             * Set the quality control code so that the specific bit for roc
             * check indicates that it passed, and therefore set the summary
             * questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(ROC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_OUTLIER_FAILED:
            /*
             * Set the quality control code so that the specific bit for outlier
             * check indicates it failed, and therefore set the summary
             * questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(OUTLIER_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            /*
             * Set the quality control code so that the specific bit for outlier
             * check indicates that it passed, and therefore set the summary
             * questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(OUTLIER_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_SCC_FAILED:
            /*
             * Set the quality control code so that the specific bit for spatial
             * consistency check indicates that it failed, and therefore set the
             * summary questionable bit to indicate the failure.
             */

            qualityCode = setQcBit(SCC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_SCC_PASSED:
            /*
             * Set the quality control code so that the specific bit for spatial
             * consistency check indicates that it passed, and therefore set the
             * summary questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(SCC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_MSC_FAILED:
            /*
             * Set the quality control code so that the specific bit for multi
             * sensor check indicates that it failed, and therefore set the
             * summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(MSC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_MSC_PASSED:
            /*
             * Set the quality control code so that the specific bit for multi
             * sensor check indicates that it passed, and therefore set the
             * summary questionable bit to indicate the passing.
             */

            qualityCode = setQcBit(MSC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_EXTERN_FAILED:
            /*
             * Set the quality control code so that the specific bit for
             * external check indicates it failed, and therefore set the summary
             * certainty bit to indicate the failure.
             */
            qualityCode = setQcBit(EXTERN_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_EXTERN_QUEST:
            /*
             * Set the quality control code so that the specific bit for
             * external check indicates it is questioable, and therefore set the
             * summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(EXTERN_NOTQUEST_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_MANUAL_PASSED:
            /*
             * When manually updating a value, assume that the value is set to a
             * good value, in which case we should set it to the default.
             */
            qualityCode = setQcCode(QC_DEFAULT);
            break;
        case QC_MANUAL_QUEST:
            /*
             * Set the quality control code so that the specific bit for manual
             * check indicates it is questionable, and therefore set the summary
             * certainty bit to indicate the questionable state.
             */
            qualityCode = setQcBit(MANUAL_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 1, qualityCode);
            break;
        case QC_MANUAL_FAILED:
            /*
             * Set the quality control code so that the specific bit for manual
             * check indicates it failed, and therefore set the summary
             * certainty bit to indicate the failure.
             */
            qualityCode = setQcBit(MANUAL_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_MANUAL_NEW:
            /*
             * When manually inserting a value, assume that the value is set to
             * a good value, in which case we should set it to the default.
             */
            qualityCode = setQcCode(QC_DEFAULT);
            break;
        default:
            /*
             * An invalid bit value was processed. The quality control code
             * remains unchanged.
             */
            // TODO log the error message
            // fprintf(stderr, "%s",
            // "Invalid request made in set_qccode() function.\n");
            break;
        }

        return qualityCode;
    }

    /**
     * Sets the specified bit of the quality code to the specified value.
     * 
     * @param bitPosition
     *            The bit position
     * @param setting
     *            The setting
     * @param qualityCode
     *            The quality code
     * @return true if valid request, false if invalid request
     */
    public static long setQcBit(int bitPosition, int setting, long qualityCode) {

        /*
         * Variable used to set a specific bit in bit string to 1; initialized
         * as 0000000000000000 0000000000000001
         */
        int mask = 1;
        long bitwise_inclusive_OR_result;
        long bitwise_AND_result;

        /* Ensure that a valid bit position is requested. */
        if (bitPosition < SIGN_QC) {

            /* if setting the bit ON */
            if (setting == 1) {

                /*
                 * The mask is employed to set a specific bit to the value of 1
                 * in a 32-bit string while hiding the value of the other bits.
                 * The mask is leftwardly shifted to the bit position of the bit
                 * being referenced.
                 */
                mask = mask << bitPosition;

                /*
                 * The bitwise inclusive OR operation is used to set the
                 * specified bit. Upon completion, the bit is written to
                 * quality_code memory location.
                 */
                bitwise_inclusive_OR_result = qualityCode | mask;
                qualityCode = bitwise_inclusive_OR_result;
            } else {
                /*
                 * if setting the bit OFF. first build a mask that has all ones
                 * except for the bit in question, then AND the mask with the
                 * existing value to turn off the single bit.
                 */

                mask = ALL_ONES ^ (mask << bitPosition);

                bitwise_AND_result = qualityCode & mask;
                qualityCode = bitwise_AND_result;
            }
        } else {
            // TODO log this error
            // fprintf(stderr, "%s", "Invalid request made in set_qcbit()
            // function.\n");
        }

        return qualityCode;
    }

    /**
     * Set's the notquest_qc bit according to whether it passed the following
     * tests: External QC Not-QUESTIONABLE Indicator (BIT20) Reasonable Range
     * Test (BIT19) Rate-of-change Test (BIT18) Outlier Test (BIT17) Spatial
     * Consistency Test (BIT16) Multi Sensor Test (BIT15)
     */
    public static long setQcNotQuestSummary(long qualityCode) {

        if ((checkQcBit(EXTERN_NOTQUEST_QC, qualityCode) == false)
                || (checkQcBit(REASONRANGE_QC, qualityCode) == false)
                || (checkQcBit(ROC_QC, qualityCode) == false)
                || (checkQcBit(OUTLIER_QC, qualityCode) == false)
                || (checkQcBit(SCC_QC, qualityCode) == false)
                || (checkQcBit(MSC_QC, qualityCode) == false)) {
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
        } else {
            qualityCode = setQcBit(NOTQUEST_QC, 1, qualityCode);
        }

        return qualityCode;
    }

    public static String buildQcSymbol(long qualityCode) {
        String returnVal = "";
        if (checkQcCode(QC_PASSED, qualityCode) == true) {
            returnVal = "G";
        } else if (checkQcCode(QC_QUESTIONABLE, qualityCode) == true) {
            returnVal = "Q";
        } else if (checkQcCode(QC_FAILED, qualityCode) == true) {
            returnVal = "B";
        } else {
            returnVal = "?";
        }

        return returnVal;
    }

    public static boolean checkQcCode(int bitGroupDescription, long qualityCode) {
        boolean returnValue = false;
        switch (bitGroupDescription) {
        case QC_DEFAULT:
            if (qualityCode == DEFAULT_QC_VALUE) {
                returnValue = true;
            }
            break;
        case QC_PASSED:
            if (qualityCode > GOOD_QUESTIONABLE_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_QUESTIONABLE:
            if ((qualityCode >= QUESTIONABLE_BAD_THRESHOLD)
                    && (qualityCode < GOOD_QUESTIONABLE_THRESHOLD)) {
                returnValue = true;
            }
            break;
        case QC_ROC_PASSED:
            returnValue = checkQcBit(ROC_QC, qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            returnValue = checkQcBit(OUTLIER_QC, qualityCode);
            break;
        case QC_SCC_PASSED:
            returnValue = checkQcBit(SCC_QC, qualityCode);
            break;
        case QC_MSC_PASSED:
            returnValue = checkQcBit(MSC_QC, qualityCode);
            break;
        case QC_FAILED:
            if (qualityCode < QUESTIONABLE_BAD_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_NOT_FAILED:
            if (qualityCode >= QUESTIONABLE_BAD_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_NOT_PASSED:
            if ((qualityCode <= GOOD_QUESTIONABLE_THRESHOLD)) {
                returnValue = true;
            }
            break;
        default:
            // TODO log error message
            // "Invalid request made in check_qc_code() function.\n");
        }

        return returnValue;
    }

    public static String buildQcDescr(long qualityCode) {
        StringBuffer rval = new StringBuffer();

        boolean passedCertainty;
        boolean passedQuestionable;
        int num_failed = 0;

        /* for convenience, determine some bit settings here for use later */

        passedCertainty = checkQcBit(CERTAINTY_QC, qualityCode);
        passedQuestionable = checkQcBit(NOTQUEST_QC, qualityCode);

        /* first check whether the quality is fully successfull */

        if (passedCertainty) {
            if (passedQuestionable) {
                rval.append("Passed All Tests");
            } else { /* now check whether the data is considered questionable */
                rval.append("Questionable: Failed");

                if (!checkQcBit(EXTERN_NOTQUEST_QC, qualityCode)) {
                    rval.append(" External Test");
                    num_failed++;
                }

                if (!checkQcBit(MANUAL_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Manual Review");
                    num_failed++;
                }

                if (!checkQcBit(REASONRANGE_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Reasonable Range Test");
                    num_failed++;
                }

                if (!checkQcBit(ROC_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Rate-of-Change Test");
                    num_failed++;
                }

                if (!checkQcBit(OUTLIER_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Outlier Test");
                    num_failed++;
                }

                if (!checkQcBit(SCC_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Spatial Consistency Test");
                    num_failed++;
                }

                if (!checkQcBit(MSC_QC, qualityCode)) {
                    if (num_failed > 0)
                        rval.append(",");
                    rval.append(" Multi Sensor Test");
                    num_failed++;
                }
            }
        }

        /* check if the data is bad */

        else {
            rval.append("Bad: Failed");

            if (!checkQcBit(EXTERN_QC, qualityCode)) {
                rval.append(" External Test");
                num_failed++;
            }

            if (!checkQcBit(MANUAL_QC, qualityCode)) {
                if (num_failed > 0)
                    rval.append(",");
                rval.append(" Manual Review");
                num_failed++;
            }

            if (!checkQcBit(GROSSRANGE_QC, qualityCode)) {
                if (num_failed > 0)
                    rval.append(",");
                rval.append(" Gross Range Test");
                num_failed++;
            }
        }

        return rval.toString();
    }
    
    public static String buildQcWhere(int request) {
        StringBuilder where = new StringBuilder();
        
        if (request == QC_PASSED) {
            /* The data value is valid as per all checks. */
            
            where.append(String.format("%s%d", "quality_code >= ", GOOD_QUESTIONABLE_THRESHOLD));            
        } else if (request == QC_NOT_PASSED) {
            /* The gross range certainty check did not pass. The data is not usable. */
            
            where.append(String.format("%s%d", "quality_code < ", 
                    GOOD_QUESTIONABLE_THRESHOLD));
        } else if (request == QC_FAILED) {
            /* The data value is invalid because it failed at least one
            certainty check */
            
            where.append(String.format("%s%d", "quality_code < ", 
                    QUESTIONABLE_BAD_THRESHOLD));
        } else if (request == QC_QUESTIONABLE) {
            /* The data value is questionable.  It passed all certainty checks  
            but failed at least one quality check. */
            
            where.append(String.format("quality_code BETWEEN %d AND %d",
                    QUESTIONABLE_BAD_THRESHOLD, 
                    (GOOD_QUESTIONABLE_THRESHOLD - 1)));
        } else if (request == QC_NOT_FAILED) {
            /* The data is deemed full valid or is questionable, but it is 
            not deemed bad with certainty. */
            
            where.append(String.format("%s%d", "quality_code >= ",
                    QUESTIONABLE_BAD_THRESHOLD));
        } else {
            /* Processing of an invalid request submitted by the calling function. */
            
            //TODO Log statement here
            where.append("INVALID REQUEST");
        }
        
        return where.toString();
    }
}
