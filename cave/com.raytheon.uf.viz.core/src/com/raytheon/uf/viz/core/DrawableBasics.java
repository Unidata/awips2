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
package com.raytheon.uf.viz.core;

import org.eclipse.swt.graphics.RGB;

/**
 * Basic drawable information, color, alpha, location and settings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawableBasics {

    public RGB color;

    /** x coordinate to draw at */
    public double x = 0.0;

    /** x coordinate to draw at */
    public double y = 0.0;

    /** x coordinate to draw at */
    public double z = 0.0;

    /** Alpha to use for text */
    public float alpha = 1.0f;

    /**
     * This boolean specifies that the color of the object will be xor-ed with
     * the color that was rendered before the drawing of this string
     */
    public boolean xOrColors = false;

}
