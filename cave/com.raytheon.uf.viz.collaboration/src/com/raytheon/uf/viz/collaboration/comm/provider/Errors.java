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
package com.raytheon.uf.viz.collaboration.comm.provider;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class Errors {

    public static final int NO_ERROR = 0;

    public static final int CANNOT_CONNECT = -50;

    public static final int ALREADY_CONNECTED = -51;

    public static final int BAD_NAME = -52;

    // Error - An attempt to use a Venue that has been disposed.
    public static final int VENUE_DISPOSED = -100;

    // Error - Venue exists when attempting to create a new venue.
    public static final int VENUE_EXISTS = -101;

    // Error - Venue not found when attempting to join an existing venue.
    public static final int VENUE_NOT_FOUND = -102;

}
