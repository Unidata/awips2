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
package com.raytheon.viz.core.contours.util;

/**
 * Configuration parameters for calculating stream lines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2013  #1999     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class StrmPakConfig {

    public float asize;

    public float minspc;

    public float maxspc;

    public float badlo;

    public float badhi;

    /**
     * Creates a new configuration to run StrmPak.
     * 
     * @param asize
     *            Size of arrows in MVI.
     * @param minspc
     *            If greater than one, no two streamlines will approach any
     *            closer than this number of cells. If less than one, a
     *            streamline will terminate if it runs through 1/minspc
     *            consecutive already occupied cells.
     * @param maxspc
     *            No streamline will be started any closer than this number of
     *            cells to an existing streamline.
     * @param badlo
     * @param badhi
     */
    public StrmPakConfig(float asize, float minspc, float maxspc, float badlo,
            float badhi) {
        this.asize = asize;
        this.minspc = minspc;
        this.maxspc = maxspc;
        this.badlo = badlo;
        this.badhi = badhi;
    }
}
