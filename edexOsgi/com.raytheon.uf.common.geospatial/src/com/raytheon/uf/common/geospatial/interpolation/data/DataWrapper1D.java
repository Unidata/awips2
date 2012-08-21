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
package com.raytheon.uf.common.geospatial.interpolation.data;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * 
 * converts a 2D x,y index pair into an index value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class DataWrapper1D extends AbstractDataWrapper {

    public DataWrapper1D(GeneralGridGeometry geometry) {
        super(geometry);
    }

    public DataWrapper1D(int nx, int ny) {
        super(nx, ny);
    }

    @Override
    protected double getDataValueInternal(int x, int y) {
        return getDataValueInternal(x + y * nx);
    }

    @Override
    public void setDataValueInternal(double dataValue, int x, int y) {
        setDataValueInternal(dataValue, x + nx * y);
    }

    protected abstract double getDataValueInternal(int index);

    protected abstract void setDataValueInternal(double dataValue, int index);

}