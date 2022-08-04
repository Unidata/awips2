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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.gif;

import java.awt.Color;

/**
 * Identifies the beginning of a data range that will be rendered a specific
 * {@link Color}. Used when defining legend parameters.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DataLevel {

    public static final double COLOR_FACTOR = 0.255;

    private final double begin;

    private final Color legendColor;

    public DataLevel(final double begin, final int r, final int g,
            final int b) {
        this.begin = begin;
        this.legendColor = new Color((int) (r * COLOR_FACTOR),
                (int) (g * COLOR_FACTOR), (int) (b * COLOR_FACTOR));
    }

    public double getBegin() {
        return begin;
    }

    public Color getLegendColor() {
        return legendColor;
    }
}