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
package com.raytheon.uf.common.geospatial.interpolation;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Data Source that reprojects from another grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 28, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridReprojectionDataSource implements DataSource {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridReprojectionDataSource.class);

    private final GridReprojection reprojection;

    private final GridSampler sampler;

    public GridReprojectionDataSource(GridReprojection reprojection,
            GridSampler sampler) {
        super();
        this.reprojection = reprojection;
        this.sampler = sampler;
    }

    @Override
    public double getDataValue(int x, int y) {
        try {
            return reprojection.reprojectedGridCell(sampler, x, y);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.DEBUG, e.getLocalizedMessage(), e);
            return Double.NaN;
        } catch (TransformException e) {
            statusHandler.handle(Priority.DEBUG, e.getLocalizedMessage(), e);
            return Double.NaN;
        }
    }

}
