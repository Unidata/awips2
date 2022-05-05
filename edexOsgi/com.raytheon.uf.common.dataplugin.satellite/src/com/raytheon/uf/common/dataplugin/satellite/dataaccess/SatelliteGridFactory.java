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

import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.format.ParserException;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnitsUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.units.UnitConv;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

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
 * Jul 30, 2014  3184     njensen     Renamed valid identifiers to optional
 * Sep 29, 2014  3596     nabowle     Always put creatingEntity in attributes.
 * Feb 13, 2015  4124     mapeters    Overrode getAvailableParameters(), inherits IDataFactory.
 * Feb 27, 2015  4179     mapeters    Use AbstractDataPluginFactory.getAvailableValues().
 * Apr 13, 2016  5379     tgurney     Add getIdentifierValues() implementation
 * Jun 07, 2016  5587     tgurney     Change get*Identifiers() to take
 *                                    IDataRequest
 * Jun 07, 2016  5574     tgurney     Add advanced query support
 * Aug 01, 2016  2416     tgurney     Add dataURI as optional identifier
 * Mar 06, 2017  6142     bsteffen    Remove dataURI as optional identifier
 * Aug 29, 2017  6389     bsteffen    Ensure location names are unique.
 * Jul 31, 2018  6389     mapeters    Override getDataSource() to handle unit
 *                                    conversion for GOES-R
 * Apr 15, 2019  7596     lsingh      Updated units framework to JSR-363.
 *                                    Handled unit conversion
 *
 * </pre>
 *
 * @author bkowal
 */
public class SatelliteGridFactory extends AbstractGridDataPluginFactory {

    private static final String FIELD_CREATING_ENTITY = "creatingEntity";

    private static final String FIELD_PHYSICAL_ELEMENT = "physicalElement";

    private static final String FIELD_SECTOR_ID = "sectorID";

    private static final String FIELD_SOURCE = "source";

    private static final String[] OPTIONAL_IDENTIFIERS = { FIELD_SOURCE,
            FIELD_CREATING_ENTITY, FIELD_SECTOR_ID, FIELD_PHYSICAL_ELEMENT };

    public SatelliteGridFactory() {
        SatelliteUnits.register();
    }

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return OPTIONAL_IDENTIFIERS;
    }

    @Override
    protected DefaultGridData constructGridDataResponse(IDataRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            DataSource dataSource) {
        if (!(pdo instanceof SatelliteRecord)) {
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + pdo.getClass().getSimpleName());
        }

        SatelliteRecord satelliteRecord = (SatelliteRecord) pdo;
        DefaultGridData defaultGridData = new DefaultGridData(dataSource,
                gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        defaultGridData.setParameter(satelliteRecord.getPhysicalElement());
        defaultGridData.setLevel(null);
        defaultGridData.setLocationName(generateLocationName(satelliteRecord));
        // unit
        Unit<?> unit = null;
        if (satelliteRecord.getUnits() != null) {
            try {
                unit = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).parseSingleUnit(
                        satelliteRecord.getUnits(), new ParsePosition(0));
            } catch (ParserException e) {
                throw new DataRetrievalException("Failed to parse the Unit: "
                        + satelliteRecord.getUnits(), e);
            }
        }
        defaultGridData.setUnit(unit);

        Map<String, Object> attributes = new HashMap<>();
        attributes.put(FIELD_CREATING_ENTITY,
                satelliteRecord.getCreatingEntity());

        // each allowed identifier should be explicitly checked for below

        if (request.getIdentifiers().get(FIELD_SECTOR_ID) != null) {
            attributes.put(FIELD_SECTOR_ID, satelliteRecord.getSectorID());
        }
        if (request.getIdentifiers().get(FIELD_PHYSICAL_ELEMENT) != null) {
            attributes.put(FIELD_PHYSICAL_ELEMENT,
                    satelliteRecord.getPhysicalElement());
        }
        if (request.getIdentifiers().get(FIELD_SOURCE) != null) {
            attributes.put(FIELD_SOURCE, satelliteRecord.getSource());
        }
        defaultGridData.setAttributes(attributes);

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
    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> constraints = new HashMap<>();
        if (request.getIdentifiers() != null) {
            for (Entry<String, Object> entry : request.getIdentifiers()
                    .entrySet()) {
                Object value = entry.getValue();
                if (value instanceof RequestConstraint) {
                    constraints.put(entry.getKey(), (RequestConstraint) value);
                } else {
                    constraints.put(entry.getKey(),
                            new RequestConstraint(value.toString()));
                }
            }
        }
        String[] parameters = request.getParameters();
        if (parameters != null && parameters.length > 0) {
            RequestConstraint rc = new RequestConstraint(parameters);
            constraints.put(FIELD_PHYSICAL_ELEMENT, rc);
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
        return getAvailableValues(request, FIELD_SECTOR_ID, String.class);
    }

    /**
     * Get the available parameters.
     */
    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        return getAvailableValues(request, FIELD_PHYSICAL_ELEMENT,
                String.class);
    }

    @Override
    public String[] getIdentifierValues(IDataRequest request,
            String identifierKey) {
        if (!Arrays.asList(getRequiredIdentifiers(request))
                .contains(identifierKey)
                && !Arrays.asList(getOptionalIdentifiers(request))
                        .contains(identifierKey)) {
            throw new InvalidIdentifiersException(request.getDatatype(), null,
                    Arrays.asList(new String[] { identifierKey }));
        }
        Object[] idValues = getAvailableValues(request, identifierKey,
                Object.class);
        List<String> idValStrings = new ArrayList<>(idValues.length);
        for (Object idValue : idValues) {
            idValStrings.add(idValue.toString());
        }
        return idValStrings.toArray(new String[idValues.length]);
    }

    @Override
    protected DataSource getDataSource(PluginDataObject pdo,
            SubGridGeometryCalculator subGrid) {
        IDataRecord dataRecord = getDataRecord(pdo, subGrid);
        if (dataRecord == null) {
            return null;
        }

        // Get converter from data unit to record unit
        Unit<?> recordUnit = SatelliteUnitsUtil
                .getRecordUnit((SatelliteRecord) pdo);
        if (recordUnit == null) {
            recordUnit = AbstractUnit.ONE;
        }
        Unit<?> dataUnit = SatelliteUnitsUtil.getDataUnit(recordUnit,
                dataRecord);
        UnitConverter converter = UnitConv.getConverterToUnchecked(dataUnit,
                recordUnit);

        // Construct unit-converting data source
        DataSource dataSource = DataWrapperUtil
                .constructArrayWrapper(dataRecord, false);
        dataSource = UnitConvertingDataFilter.apply(dataSource, converter);
        return dataSource;

    }

    protected static String generateLocationName(
            SatelliteRecord satelliteRecord) {
        StringBuilder locationName = new StringBuilder(32);
        locationName.append(satelliteRecord.getSectorID());
        locationName.append("_");
        locationName.append(satelliteRecord.getCoverage().getMinX());
        locationName.append("_");
        locationName.append(satelliteRecord.getCoverage().getMinY());
        return locationName.toString();
    }
}