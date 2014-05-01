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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

/**
 * 
 * 
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface ISynoptic {

    public static final String GENERAL_GROUP = "[0-9/]{5}";

    public static final String YYGGI_SUB_W = "([012]\\d{1}|3[01])[0-5]\\d{1}[/0134]";

    public static final String SEC_1_IRIXHVV = "[0-4][0-7][0-9/](\\d{2}|//)";

    public static final String SEC_1_NDDFF = "([/0-9])(//|([012]\\d)|(3[0-6]))(//|\\d{2})";

    public static final String SEC_1_TEMPDEW = "[0128](\\d{4} | ////)";

    public static final String SEC_5_72_CTEMP = "1[01][0-9/]{2}";

    public static final String SEC_5_72_CMAXMIN = "[01][0-9/]{2}[01][0-9/]{2}";

    public static final String SEC_2_LEAD = "222[0-9/]{2}";

    public static final String SEC_3_LEAD = "333";

    public static final String SEC_4_LEAD = "444";

    public static final String SEC_5_LEAD = "555";

}
