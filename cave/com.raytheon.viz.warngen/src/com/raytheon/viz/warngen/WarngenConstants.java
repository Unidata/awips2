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
package com.raytheon.viz.warngen;


/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            bwoodle     Initial creation
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class WarngenConstants {

    public static final int INSTRUCTIONS_HEIGHT = 40;

    public static final String INSTRUCTION_DRAG_STORM = "Move Centroid to Storm in any Frame";

    public static final String INSTRUCTION_NO_SHADED_AREA = "Move Storm Polygon to CWA";

    /**
     * Pattern is localization site (OAX, ABR, etc) concatenated with WRK
     * concatenated with a 1-3 char id set by workstation (W4, WG4, etc)
     */
    public static final String AFOS_ID_PATTERN = "%sWRK%s";

}
