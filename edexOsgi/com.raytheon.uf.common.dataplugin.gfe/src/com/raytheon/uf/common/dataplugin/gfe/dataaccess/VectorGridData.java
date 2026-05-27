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
package com.raytheon.uf.common.dataplugin.gfe.dataaccess;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;

/**
 * 
 * Specialized {@link IGridData} that can be used by {@link GFEGridFactory} for
 * vector parameters. This gridData does not do anything different from the
 * superclass except for exposing the internal {@link VectorDataSource} so that
 * the factory can separate the magnitude and direction to be returned as
 * separate grids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 23, 2016  5637     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class VectorGridData extends DefaultGridData {

    public VectorGridData(VectorDataSource data, GridGeometry2D gridGeometry) {
        super(data, gridGeometry);
    }

    public VectorDataSource getData() {
        return (VectorDataSource) data;
    }

}
