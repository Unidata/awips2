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
package com.raytheon.viz.core.graphing;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.viz.core.graphing.axis.IAxis;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for graphs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IGraph extends IRenderable {

    public IAxis getDomainAxis();

    public IAxis getRangeAxis();

    public boolean isShown();

    public void setShown(boolean aShown);

    public void updateLabeling(int numberOfGraphs);

    public boolean zoomGraph(double x, double y, double zoomLevel);

    public double[] translateCoordToValue(Coordinate aCoordinate);

    public IExtent getWorldExtent();

    public void setWorldExtent(IExtent anExtent);

}
