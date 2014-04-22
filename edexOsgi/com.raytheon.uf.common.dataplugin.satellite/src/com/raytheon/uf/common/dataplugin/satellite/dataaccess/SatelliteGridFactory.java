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
package com.raytheon.uf.common.dataplugin.satellite.dataaccess;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IDataFactory;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * A data factory for getting satellite data from the metadata database. There
 * are currently not any required identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 02, 2012           bkowal      Initial creation
 * Jan 22, 2012           bsteffen    Extract common functionality to AbstractGridDataPluginFactory
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Feb 04, 2014  2672     bsteffen    Enable requesting subgrids.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class SatelliteGridFactory extends AbstractGridDataPluginFactory
        implements IDataFactory {

    private static final String FIELD_PYHSICAL_ELEMENT = "physicalElement";

    private static final String FIELD_SECTOR_ID = "sectorID";

    private static final String[] VALID_IDENTIFIERS = { "source",
            "creatingEntity", FIELD_SECTOR_ID, FIELD_PYHSICAL_ELEMENT };

    public SatelliteGridFactory() {
        SatelliteUnits.register();
    }

    @Override
    public String[] getValidIdentifiers() {
        return VALID_IDENTIFIERS;
    }

    protected DefaultGridData constructGridDataResponse(IDataRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            DataSource dataSource) {
        if (pdo instanceof SatelliteRecord == false) {
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + pdo.getClass().getSimpleName());
        }

        SatelliteRecord satelliteRecord = (SatelliteRecord) pdo;
        DefaultGridData defaultGridData = new DefaultGridData(dataSource,
                gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        defaultGridData.setParameter(satelliteRecord.getPhysicalElement());
        defaultGridData.setLevel(null);
        defaultGridData.setLocationName(satelliteRecord.getSectorID());
        // unit
        Unit<?> unit = null;
        if ((satelliteRecord.getUnits() == null) == false) {
            try {
                unit = UnitFormat.getUCUMInstance().parseSingleUnit(
                        satelliteRecord.getUnits(), new ParsePosition(0));
            } catch (ParseException e) {
                throw new DataRetrievalException("Failed to parse the Unit: "
                        + satelliteRecord.getUnits(), e);
            }
        }
        defaultGridData.setUnit(unit);
        defaultGridData.setAttributes(request.getIdentifiers());

        return defaultGridData;
    }

    /**
     * Builds the base constraint map based on the supplied grid request
     * 
     * @param request
     *            the original grid request
     * @return the base constraint map
     */
    /*
     * This method is a candidate for relocation into a utility or superclass if
     * multiple factories will be building a base constraint map using the same
     * technique
     */
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        if ((request.getIdentifiers() == null) == false) {
            Iterator<String> identifiersIterator = request.getIdentifiers()
                    .keySet().iterator();

            while (identifiersIterator.hasNext()) {
                String identifier = identifiersIterator.next();

                constraints.put(identifier, new RequestConstraint(request
                        .getIdentifiers().get(identifier).toString()));
            }
        }
        String[] parameters = request.getParameters();
        if (parameters != null && parameters.length > 0) {
            RequestConstraint rc = new RequestConstraint(parameters);
            constraints.put(FIELD_PYHSICAL_ELEMENT, rc);
        }

        String[] locations = request.getLocationNames();
        if (locations != null && locations.length > 0) {
            RequestConstraint rc = new RequestConstraint(locations);
            constraints.put(FIELD_SECTOR_ID, rc);
        }

        return constraints;
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        return getAvailableLocationNames(request, FIELD_SECTOR_ID);
    }

}