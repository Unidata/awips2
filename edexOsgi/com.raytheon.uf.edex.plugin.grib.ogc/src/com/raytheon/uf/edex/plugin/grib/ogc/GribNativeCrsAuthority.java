/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.List;

import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.Projection;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.spatial.NativeCrsAuthority;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class GribNativeCrsAuthority implements NativeCrsAuthority {

    public static final String ID = "grib";

    private final CoreDao covDao = new CoreDao(
            DaoConfig.forClass(GridCoverage.class));

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.spatial.NativeCrsAuthority#lookup(java.lang.String)
     */
    @Override
    public CoordinateReferenceSystem lookup(String urn) throws OgcException {
        if ( urn == null){
            return null;
        }
        String local = urn.substring(NATIVE_CRS_PREFIX.length());
        String[] parts = local.split(":");
        try {
            List<?> res = covDao.queryBySingleCriteria("name", parts[0]);
            if (res == null || res.isEmpty()) {
                return null;
            }
            GridCoverage cov = (GridCoverage) res.get(0);
            return cov.getCrs();
        } catch (DataAccessLayerException e) {
            log.error("Unable to lookup coverage", e);
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /**
     * @param cov
     * @return Native CRS URN for coverage
     */
    public static String createURN(GridCoverage cov) {
        String name = cov.getName();
        // TODO assumption that all native grid systems are projected
        ProjectedCRS crs = (ProjectedCRS) cov.getCrs();
        return createURN(name, crs);
    }

    /**
     * @param coverageName
     * @param crs
     * @return Native CRS URN for coverage
     */
    public static String createURN(String coverageName, ProjectedCRS crs) {
        Projection conv = crs.getConversionFromBase();
        ParameterValueGroup params = conv.getParameterValues();
        String projName = params.getDescriptor().getName().getCode();
        return NATIVE_CRS_PREFIX + coverageName + ":" + projName;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.spatial.NativeCrsAuthority#getId()
     */
    @Override
    public String getId() {
        return ID;
    }

}
