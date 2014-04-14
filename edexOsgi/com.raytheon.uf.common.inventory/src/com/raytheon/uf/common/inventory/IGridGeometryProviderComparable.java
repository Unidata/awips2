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
package com.raytheon.uf.common.inventory;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.geospatial.IGridGeometryProvider;

/**
 * Interace for {@link IGridGeometryProvider} that can compare itself to other
 * IGridGeometryProviders and provide an intersecting IGridGeometryProvider.
 * This method can be used by the {@link TimeAndSpaceMatcher} to match different
 * spaces.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 11, 2014           bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface IGridGeometryProviderComparable extends IGridGeometryProvider {

    /**
     * Compare another IGridGeometryProvider to this one. If the two are
     * compatible return a provider that will generate a {@link GridGeometry2D}
     * representing the intersection. If the two are incompatible or
     * nonintersecting null should be returned to indicate no space matching is
     * possible.
     * 
     * @param other
     * @return
     */
    public IGridGeometryProvider compare(IGridGeometryProvider other);

}
