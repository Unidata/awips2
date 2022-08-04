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
package com.raytheon.uf.edex.plugin.mpe;

/**
 * Common sizes of C++ data types.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class CPlusPlusSizeConstants {

    public static final int SIZE_OF_CHAR = 1;

    public static final int SIZE_OF_SHORT = 2;

    public static final int SIZE_OF_INT = 4;

    public static final int SIZE_OF_FLOAT = 4;

    public static final int SIZE_OF_DOUBLE = 8;

    protected CPlusPlusSizeConstants() {
    }
}