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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

/**
 * Common constants used by the Gage Table dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 2476       mpduff      Initial creation
 * Mar 01, 2017 6158       mpduff      Changed default width to 80.
 * May 12, 2017 6283       bkowal      Added {@link #EDIT_GAGE_VALUE}.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class GageTableConstants {
    public static final String EDIT_GAGE_VALUE = "Edit Gage Value";

    public static final String[] BASE_COLUMNS = { "LID", "Gage Value",
            EDIT_GAGE_VALUE, "Diff (Gage-Grid)", "Radar ID" };

    /** Default column width */
    public static final int DEFAULT_WIDTH = 80;

}
