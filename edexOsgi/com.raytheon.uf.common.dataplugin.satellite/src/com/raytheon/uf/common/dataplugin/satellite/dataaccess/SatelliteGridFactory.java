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

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.grid.IGridDataFactory;
import com.raytheon.uf.common.dataaccess.grid.IGridRequest;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;

/**
 * A data factory for getting satellite data from the metadata database. There
 * are currently not any required identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 02, 2012            bkowal      Initial creation
 * Jan 22, 2012            bsteffen    Extract common functionality to AbstractGridDataPluginFactory
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class SatelliteGridFactory extends AbstractGridDataPluginFactory
        implements IGridDataFactory {

    private static final String FIELD_PYHSICAL_ELEMENT = "physicalElement";

    private static final String FIELD_SECTOR_ID = "sectorID";

    private Map<String, GridGeometry2D> sectorGeometryMapCache;

    public SatelliteGridFactory() {
        this.sectorGeometryMapCache = new HashMap<String, GridGeometry2D>();
        SatelliteUnits.register();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.grid.IGridDataFactory#getGeometry(com
     * .raytheon.uf.common.dataaccess.grid.IGridRequest)
     */
    @Override
    public GridGeometry2D getGeometry(IGridRequest request) {
        String satelliteSectorID = this.retrieveSectorID(request);

        if (satelliteSectorID == null) {
            return null;
        }

        GridGeometry2D geometry = null;

        /*
         * Has the Geometry for the sector id already been cached?
         */
        synchronized (this.sectorGeometryMapCache) {
            if (this.sectorGeometryMapCache.containsKey(satelliteSectorID)) {
                /*
                 * Return the Geometry from cache.
                 */
                geometry = this.sectorGeometryMapCache.get(satelliteSectorID);
            }
        }

        if (geometry == null) {
            /*
             * Retrieve the Geometry.
             */
            IGridData[] records = this.getData(request, new DataTime[] {});
            if (records.length <= 0) {
                // No records were found
                return null;
            }
            geometry = records[0].getGridGeometry();

            /*
             * Cache the Geometry.
             */
            synchronized (this.sectorGeometryMapCache) {
                this.sectorGeometryMapCache.put(satelliteSectorID, geometry);
            }
        }

        /*
         * Return the Geometry.
         */
        return trimGridGeometryToRequest(geometry, request.getStorageRequest());
    }

    /**
     * Will either extract the satellite sector id from the request if it has
     * been provided or execute a database query utilizing the information in
     * the request to retrieve the sector id.
     * 
     * @param request
     *            the original grid request
     * @return the satellite sector id
     */
    private String retrieveSectorID(IGridRequest request) {
        /*
         * Determine if the sector id has been included in the request.
         */
        if (request.getIdentifiers().containsKey(FIELD_SECTOR_ID)) {
            return request.getIdentifiers().get(FIELD_SECTOR_ID).toString();
        }

        /*
         * First, retrieve the unique sector id(s) associated with the request.
         * Ideally, there will only be one.
         */
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        dbQueryRequest.setDistinct(Boolean.TRUE);
        dbQueryRequest.addRequestField(FIELD_SECTOR_ID);

        DbQueryResponse dbQueryResponse = this.executeDbQueryRequest(
                dbQueryRequest, request.toString());

        // Check for no results returned?

        /*
         * Verify that only one sector id has been returned.
         */
        if (dbQueryResponse.getResults().size() > 1) {
            throw new DataRetrievalException(
                    "The provided request parameters refer to more than one geographical location.");
        }

        /*
         * Retrieve the sector id from the results.
         */
        return dbQueryResponse.getResults().get(0).get(FIELD_SECTOR_ID)
                .toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getRequiredIdentifiers()
     */
    @Override
    public String[] getRequiredIdentifiers() {
        return null;
    }

    protected DefaultGridData constructGridDataResponse(IGridRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            IDataRecord dataRecord)  {
        if(pdo instanceof SatelliteRecord == false){
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + pdo.getClass().getSimpleName());
        }
        
        SatelliteRecord satelliteRecord = (SatelliteRecord) pdo;
        DefaultGridData defaultGridData = new DefaultGridData(
                DataWrapperUtil.constructArrayWrapper(dataRecord), gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        defaultGridData.setParameter(satelliteRecord.getPhysicalElement());
        defaultGridData.setLevel(null);
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
            IGridRequest request) {
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
        if ((request.getParameters() == null) == false) {
            RequestConstraint requestConstraint = new RequestConstraint();
            requestConstraint.setConstraintType(ConstraintType.IN);
            requestConstraint.setConstraintValueList(request.getParameters());
            constraints.put(FIELD_PYHSICAL_ELEMENT, requestConstraint);
        }

        return constraints;
    }
}