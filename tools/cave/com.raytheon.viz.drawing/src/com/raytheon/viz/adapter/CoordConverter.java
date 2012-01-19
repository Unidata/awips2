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

package com.raytheon.viz.adapter;

import java.awt.Point;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Translation layer that converts points from screen space to world space.
 * 
 * Used as a bridge between CAVE and FSL code.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Oct 30, 2006         66  chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CoordConverter {

    private IDisplayPaneContainer container;

    public CoordConverter(IDisplayPaneContainer container) {
        this.container = container;
    }

    public Point convert(Coordinate c) {
        double[] x = container.translateInverseClick(c);
        return new Point((int) x[0], (int) x[1]);
    }

    public Coordinate convert(Point p) {
        Coordinate c = container.translateClick(p.x, p.y);
        return c;

    }

}
