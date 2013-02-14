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

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IDataFactory;

/**
 * IDataFactory interface for two dimensional gridded data. Note that IGridData
 * has the populateData() methods, therefore the implementations of this
 * interface can choose to either retrieve the raw data when factory.getData()
 * is called, or have the implementation of IGridData retrieve the raw data when
 * populateData() is called.
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

public interface IGridDataFactory extends IDataFactory<IGridRequest, IGridData> {

    /**
     * Gets the GridGeometry2D that matches the request. Useful for determining
     * the area before requesting the data.
     * 
     * @param request
     *            the request to get the geometry for
     * @return the grid geometry of the data that would be returned from this
     *         request
     */
    public GridGeometry2D getGeometry(IGridRequest request);

}
