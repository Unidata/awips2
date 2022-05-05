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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.INotificationFilter;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.reference.GroupID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceMgr;
import com.raytheon.uf.common.dataplugin.gfe.request.GetActiveSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GridLocRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * A data factory for retrieving GFE edit area data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 09, 2017 6298       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */

public class GFEEditAreaGeometryFactory extends AbstractDataFactory {

    private static final String SITE_ID = "siteId";

    private static final String GROUP = "group";

    @Override
    public String[] getRequiredIdentifiers(IDataRequest request) {
        return new String[] { SITE_ID };
    }

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return new String[] { GROUP };
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        this.validateRequest(request);

        // Determine sites to get edit area names for
        String[] sites = getSiteIdValues(request);

        // Add each site's edit area names to set to get union
        Set<String> locsSet = new HashSet<>();
        for (String siteId : sites) {
            ReferenceMgr refMgr = new ReferenceMgr(getGridLocation(siteId));
            ServerResponse<List<ReferenceID>> sr = refMgr.getInventory();
            if (sr.isOkay()) {
                for (ReferenceID refId : sr.getPayload()) {
                    locsSet.add(refId.getName());
                }
            } else {
                throw new DataRetrievalException(sr.message());
            }
        }

        // Sort and return results
        String[] locsArray = locsSet.toArray(new String[0]);
        Arrays.sort(locsArray);
        return locsArray;
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        return getData(request);
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        return getData(request);
    }

    /**
     * Get the edit area geometry data for the given request.
     *
     * @param request
     * @return the edit area geometry data
     */
    protected IGeometryData[] getData(IDataRequest request) {
        validateRequest(request);

        String[] sites = getSiteIdValues(request);
        Envelope envelope = request.getEnvelope();
        Geometry envGeom = null;
        if (envelope != null) {
            /*
             * Convert to geometry to be able to accurately check if it
             * intersects the geometries
             */
            envGeom = new GeometryFactory().toGeometry(envelope);
        }

        String[] groups = getGroupValues(request);
        // Groups and location names can't be specified together
        if (groups != null) {
            return getGroupsData(groups, sites, envGeom);
        } else {
            String[] editAreas = request.getLocationNames();
            if (editAreas.length == 0) {
                // Default to all edit areas
                editAreas = getAvailableLocationNames(request);
            }
            return getLocationNamesData(editAreas, sites, envGeom);
        }
    }

    /**
     * Get edit area geometry data for the given groups.
     *
     * @param groups
     * @param sites
     * @param envGeom
     * @return edit area geometries
     */
    private IGeometryData[] getGroupsData(String[] groups, String[] sites,
            Geometry envGeom) {
        // Convert groups to GroupID set
        Set<GroupID> groupIds = new HashSet<>(groups.length);
        for (String group : groups) {
            groupIds.add(new GroupID(group));
        }

        // Retrieve edit area data and build geometries
        List<IGeometryData> geoms = new ArrayList<>();
        for (String siteId : sites) {
            // Get group data from reference manager
            ReferenceMgr refMgr = new ReferenceMgr(getGridLocation(siteId));
            ServerResponse<Map<String, List<ReferenceData>>> sr = refMgr
                    .getGroupData(groupIds, true, false);
            if (!sr.isOkay()) {
                throw new DataRetrievalException(sr.message());
            }

            // Convert to TreeMap to sort groupName keys
            Map<String, List<ReferenceData>> groupDataMap = new TreeMap<>(
                    sr.getPayload());
            for (Entry<String, List<ReferenceData>> entry : groupDataMap
                    .entrySet()) {
                // Build geometries for each group and add to geoms list
                String groupName = entry.getKey();
                List<ReferenceData> groupRefData = entry.getValue();
                groupRefData.sort((ReferenceData rd1, ReferenceData rd2) -> rd1
                        .getId().getName().compareTo(rd2.getId().getName()));
                for (ReferenceData rd : groupRefData) {
                    addRefDataGeomToList(geoms, rd, envGeom, siteId, groupName);
                }
            }
        }

        return geoms.toArray(new IGeometryData[0]);
    }

    /**
     * Get edit area geometry data for the given location names.
     *
     * @param locNames
     * @param sites
     * @param envGeom
     * @return edit area geometries
     */
    private IGeometryData[] getLocationNamesData(String[] locNames,
            String[] sites, Geometry envGeom) {
        // Convert locNames to ReferenceID set
        Set<ReferenceID> editAreaIds = new HashSet<>(locNames.length);
        for (String loc : locNames) {
            editAreaIds.add(new ReferenceID(loc));
        }

        // Retrieve edit area data and build geometries
        List<IGeometryData> geoms = new ArrayList<>();
        for (String siteId : sites) {
            // Get edit area data from reference manager
            ReferenceMgr refMgr = new ReferenceMgr(getGridLocation(siteId));
            ServerResponse<List<ReferenceData>> sr = refMgr.getData(editAreaIds,
                    true, false);
            if (!sr.isOkay()) {
                throw new DataRetrievalException(sr.message());
            }

            // Build geometries and add to geoms list
            List<ReferenceData> refData = sr.getPayload();
            refData.sort((ReferenceData rd1, ReferenceData rd2) -> rd1.getId()
                    .getName().compareTo(rd2.getId().getName()));
            for (ReferenceData rd : refData) {
                addRefDataGeomToList(geoms, rd, envGeom, siteId, null);
            }
        }

        return geoms.toArray(new IGeometryData[0]);
    }

    /**
     * Build the geometry data for the given reference
     * @param geomList
     *            list that the geometry will be added to
     * @param refData
     * @param envGeom
     *            envelope on the request as a Geometry, may be null
     * @param siteId
     * @param groupName
     *            group that refData is a part of, may be null
     */
    private void addRefDataGeomToList(List<IGeometryData> geomList,
            ReferenceData refData, Geometry envGeom, String siteId,
            String groupName) {
        Geometry geom = refData.getPolygons(CoordinateType.LATLON);
        if (geom == null || envGeom != null && !envGeom.intersects(geom)) {
            /*
             * Skip edit areas that have null geometry or are outside of
             * request's envelope. Geometry will be null if edit area file
             * contained invalid polygon data.
             */
            return;
        }

        // Build geom data and add to list
        DefaultGeometryData geomData = new DefaultGeometryData();
        geomData.setGeometry(geom);
        geomData.setLocationName(refData.getId().getName());
        geomData.addAttribute(SITE_ID, siteId);
        if (groupName != null) {
            geomData.addAttribute(GROUP, groupName);
        }
        geomList.add(geomData);
    }

    @Override
    public String[] getIdentifierValues(IDataRequest request,
            String identifierKey) {
        if (SITE_ID.equals(identifierKey)) {
            return getActiveSiteIds();
        } else if (GROUP.equals(identifierKey)) {
            return getAvailableGroups(request);
        } else {
            throw new InvalidIdentifiersException(request.getDatatype(), null,
                    Arrays.asList(new String[] { identifierKey }));
        }
    }

    private String[] getAvailableGroups(IDataRequest request) {
        Set<String> groupsSet = new HashSet<>();
        String[] sites = getSiteIdValues(request);
        if (sites == null) {
            // None specified, get groups for all
            sites = getActiveSiteIds();
        }
        for (String siteId : sites) {
            // Get inventory from each site's reference manager
            ReferenceMgr refMgr = new ReferenceMgr(getGridLocation(siteId));
            ServerResponse<List<GroupID>> sr = refMgr.getGroupsInventory();
            if (sr.isOkay()) {
                for (GroupID groupId : sr.getPayload()) {
                    groupsSet.add(groupId.getName());
                }
            } else {
                throw new DataRetrievalException(sr.message());
            }
        }

        // Sort and return the results
        String[] groups = groupsSet.toArray(new String[0]);
        Arrays.sort(groups);
        return groups;
    }

    private String[] getActiveSiteIds() {
        // Build and route request for active sites
        GetActiveSitesRequest activeSitesReq = new GetActiveSitesRequest();
        String errorMsg = "Failed to retrieve active sites";
        Set<String> activeSiteIdsSet = routeRequest(activeSitesReq, errorMsg);

        // Sort and return the results
        String[] activeSiteIds = activeSiteIdsSet.toArray(new String[0]);
        Arrays.sort(activeSiteIds);
        return activeSiteIds;
    }

    private GridLocation getGridLocation(String siteId) {
        // Build and route request for site's GridLocation
        GridLocRequest gridLocReq = new GridLocRequest();
        gridLocReq.setSiteID(siteId);
        String errorMsg = "Failed to retrieve grid location for site " + siteId;
        return routeRequest(gridLocReq, errorMsg);
    }

    /**
     * Route the given server request and return the server response's payload.
     * Handles any errors that occur.
     *
     * @param serverRequest
     * @param errorMsg
     * @return the server response's payload
     */
    private <T> T routeRequest(IServerRequest serverRequest, String errorMsg) {
        // Route request and get server response
        ServerResponse<T> sr;
        try {
            sr = (ServerResponse<T>) RequestRouter.route(serverRequest);
        } catch (Exception e) {
            throw new DataRetrievalException(errorMsg, e);
        }

        // Check if errors occurred
        if (sr.isOkay()) {
            return sr.getPayload();
        } else {
            throw new DataRetrievalException(sr.message());
        }
    }

    /**
     * Get the list of siteIds that satisfy the identifier constraint on the
     * given request.
     *
     * @param request
     * @return the siteIds
     */
    private String[] getSiteIdValues(IDataRequest request) {
        return getValues(request, SITE_ID);
    }

    /**
     * Get the list of edit area groups that satisfy the identifier constraint
     * on the given request.
     *
     * @param request
     * @return the groups
     */
    private String[] getGroupValues(IDataRequest request) {
        return getValues(request, GROUP);
    }

    /**
     * Get the identifier values that satisfy the constraint on the given
     * request.
     *
     * @param request
     * @param idKey
     *            the identifier to get values for
     * @return the constrained identifier values
     */
    private String[] getValues(IDataRequest request, String idKey) {
        Object valueObj = request.getIdentifiers().get(idKey);
        if (valueObj == null) {
            return null;
        } else if (valueObj instanceof String) {
            String value = ((String) valueObj).trim();
            return new String[] { value };
        } else if (valueObj instanceof RequestConstraint) {
            RequestConstraint constraint = (RequestConstraint) valueObj;
            // If EQUALS or IN, just return specified values.
            if (constraint.getConstraintType() == ConstraintType.EQUALS) {
                return new String[] { constraint.getConstraintValue().trim() };
            } else if (constraint.getConstraintType() == ConstraintType.IN) {
                String[] values = constraint.getConstraintValue().split(",");
                return Arrays.stream(values).map(String::trim)
                        .toArray(unused -> values);
            } else {
                // Otherwise, evaluate available values to build list
                String[] availableValues = getIdentifierValues(request, idKey);
                return Arrays.stream(availableValues)
                        .filter(constraint::evaluate).toArray(String[]::new);
            }
        } else {
            throw new IncompatibleRequestException(
                    "Only string and RequestConstraint identifier values are valid for '"
                            + idKey + "'");
        }
    }

    @Override
    public void validateRequest(IDataRequest request) {
        // GFE edit area requests don't have parameters
        validateRequest(request, false);
    }

    @Override
    public void validateRequest(IDataRequest request,
            boolean validateParameters) {
        super.validateRequest(request, validateParameters);

        if (request.getIdentifiers().containsKey(GROUP)
                && request.getLocationNames().length > 0) {
            throw new IncompatibleRequestException("The '" + GROUP
                    + "' identifier and location names cannot be specified on the same request");
        }
    }

    // Unsupported methods.

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            boolean refTimeOnly) throws TimeAgnosticDataException {
        throw new TimeAgnosticDataException(request.getDatatype()
                + " data requests do not support getting available times.");
    }

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        throw new TimeAgnosticDataException(request.getDatatype()
                + " data requests do not support getting available times.");
    }

    @Override
    public INotificationFilter getNotificationFilter(IDataRequest request) {
        throw new IncompatibleRequestException(
                "Cannot listen for updates to GFE edit area data.");
    }
}
