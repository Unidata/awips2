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
package com.raytheon.uf.viz.npp.crimss;

import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.npp.crimss.CrimssRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpDescriptor;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpDisplay;
import com.raytheon.uf.viz.npp.crimss.map.CrimssMapResourceData;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.viz.ui.UiUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CrimssDataDefinition
        extends
        AbstractRequestableProductBrowserDataDefinition<AbstractRequestableResourceData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrimssDataDefinition.class);

    private static final String MAP_RESOURCE = "map_resource";

    private static final String POINT = "point";

    private static final double DISTANCE = 0.8;

    public CrimssDataDefinition() {
        productName = "crimss";
        displayName = "CrIMSS";
        order = new String[] { POINT };
        order = getOrder();
        loadProperties = new LoadProperties();
        loadProperties.setResourceType(getResourceType());
    }

    @Override
    protected String[] queryData(String param,
            HashMap<String, RequestConstraint> queryList) {
        if (param.equals(POINT)) {
            // TODO depending on how much data we have this might be way too
            // data to request.
            List<String> points = new ArrayList<String>();
            PointsDataManager pdm = PointsDataManager.getInstance();
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(CrimssRecord.class.getName());
            request.setDistinct(true);
            request.addRequestField(CrimssRecord.LATITUDE);
            request.addRequestField(CrimssRecord.LONGITUDE);
            try {
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                for (Map<String, Object> result : response.getResults()) {
                    float lat = ((Number) result.get(CrimssRecord.LATITUDE))
                            .floatValue();
                    float lon = ((Number) result.get(CrimssRecord.LONGITUDE))
                            .floatValue();
                    Coordinate c = new Coordinate(lon, lat);
                    for (String point : pdm.getPointNames()) {
                        if (points.contains(point)) {
                            continue;
                        }
                        Coordinate p = pdm.getCoordinate(point);
                        if (p.distance(c) < DISTANCE) {
                            points.add(point);
                        }
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            points.add(MAP_RESOURCE);
            return points.toArray(new String[0]);
        }
        return super.queryData(param, queryList);
    }

    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        if (param.equals(POINT)) {
            Arrays.sort(parameters);
            List<ProductBrowserLabel> temp = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < parameters.length; i++) {
                if (parameters[i].equals(MAP_RESOURCE)) {
                    temp.add(0, new ProductBrowserLabel("CrIMSS Availability",
                            parameters[i]));
                } else {
                    temp.add(new ProductBrowserLabel("Point " + parameters[i],
                            parameters[i]));
                }
            }
            return temp;
        }
        return super.formatData(param, parameters);
    }

    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(order[0], new RequestConstraint(productName));
        for (int i = 1; i < selection.length; i++) {
            if (order[i].equals(POINT)) {
                if (!selection[i].equals("map_resource")) {
                    Coordinate coord = PointsDataManager.getInstance()
                            .getCoordinate(selection[i]);
                    RequestConstraint rc = new RequestConstraint(null,
                            ConstraintType.BETWEEN);
                    rc.setBetweenValueList(new String[] {
                            String.valueOf(coord.x - DISTANCE),
                            String.valueOf(coord.x + DISTANCE) });
                    queryList.put(CrimssRecord.LONGITUDE, rc);
                    rc = new RequestConstraint(null, ConstraintType.BETWEEN);
                    rc.setBetweenValueList(new String[] {
                            String.valueOf(coord.y - DISTANCE),
                            String.valueOf(coord.y + DISTANCE) });
                    queryList.put(CrimssRecord.LATITUDE, rc);
                    if (resourceData != null) {
                        ((CrimssNSharpResourceData) resourceData)
                                .setCoordinate(coord);
                        ((CrimssNSharpResourceData) resourceData)
                                .setPointName("CrIMSS-Pnt" + selection[i]);
                    }
                }
            } else {
                queryList.put(order[i], new RequestConstraint(selection[i]));
            }
        }

        return queryList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition#getDescriptorClass()
     */
    @Override
    protected Class<? extends IDescriptor> getDescriptorClass() {
        if (resourceData instanceof CrimssNSharpResourceData) {
            return D2DNSharpDescriptor.class;
        } else {
            return super.getDescriptorClass();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition
     * #openNewEditor(java.lang.String)
     */
    @Override
    protected IDisplayPaneContainer openNewEditor(String editorId) {
        if (NsharpSkewTEditor.EDITOR_ID.equals(editorId)) {
            return UiUtil.createEditor(editorId, new D2DNSharpDisplay());
        } else {
            return super.openNewEditor(editorId);
        }
    }

    @Override
    public AbstractRequestableResourceData getResourceData() {
        return resourceData;
    }

    @Override
    public void constructResource(String[] selection, ResourceType type) {
        resourceData = null;
        for (int i = 0; i < selection.length; i++) {
            if (order[i].equals(POINT)) {
                if (selection[i].equals(MAP_RESOURCE)) {
                    resourceData = new CrimssMapResourceData();
                }
            }
        }
        if (resourceData == null) {
            resourceData = new CrimssNSharpResourceData();
        }
        super.constructResource(selection, type);
    }

}
