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
package com.raytheon.uf.common.dataaccess.grid;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IData;
import com.raytheon.uf.common.geospatial.interpolation.data.DataDestination;
import com.raytheon.uf.common.geospatial.interpolation.data.UnitConvertingDataDestination;

/**
 * An IGridData represents data that is gridded, ie rectangular (when not
 * projected) with a set x and y size.
 * 
 * To get the data values out of the IGridData, use
 * populateData(DataDestination) with a DataDestination in the format you are
 * looking for. You can also use a {@link UnitConvertingDataDestination} to
 * obtain the data in a specific unit.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IGridData extends IData {    
    /**
     * Gets the parameter of the data
     * 
     * @return the parameter of the data
     */
    public String getParameter();

    /**
     * Gets the GridGeometry of the data
     * 
     * @return the grid geometry of the data
     */
    public GridGeometry2D getGridGeometry();

    /**
     * Gets the unit of the raw data. This may differ from the unit of a data
     * destination.
     * 
     * @return the unit of the data
     */
    public Unit<?> getUnit();

    /**
     * Populates the DataDestination argument with the raw data converted to the
     * type to match the DataDestination. The destination must not be null. If
     * unit conversions are desired, use the
     * {@link UnitConvertingDataDestination} to specify what unit conversion
     * should be applied to the data.
     * 
     * @param destination
     *            the destination to fill with data
     * @return the data destination that was passed in, with the data populated
     */
    public <DD extends DataDestination> DD populateData(DD destination);

}
