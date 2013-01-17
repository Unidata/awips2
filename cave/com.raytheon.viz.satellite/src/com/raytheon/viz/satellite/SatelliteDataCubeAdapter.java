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
package com.raytheon.viz.satellite;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.media.jai.Interpolation;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequestSet;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.uf.viz.derivparam.library.IDerivParamField;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            jsanchez    Initial creation
 * Nov 21, 2009 #3576      rjpeter     Refactored use of DerivParamDesc.
 * - AWIPS2 Baseline Repository --------
 * 08/03/2012          798 jkorman     Explicitly set interpolationLevels
 *                                     from "source" record.    
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SatelliteDataCubeAdapter implements IDataCubeAdapter {

    private static final String PE = "physicalElement";

    public static final String DERIVED = "DERIVED";

    private Map<String, DerivParamDesc> derParLibrary;

    @Override
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException {
        if (!property.getEntryQueryParameters(false).containsKey("DERIVED")) {
            String scriptToExecute = ScriptCreator.createScript(property);
            return Loader
                    .loadScripts(new String[] { scriptToExecute }, timeOut);
        }
        String requestedParam;
        ArrayList<Object> initResponses = new ArrayList<Object>();
        HashMap<String, RequestConstraint> originalQuery = property
                .getEntryQueryParameters(false);
        ArrayList<DerivedParameterRequest> listOfRequests = new ArrayList<DerivedParameterRequest>();
        HashMap<String, RequestConstraint> modifiedQuery = property
                .getEntryQueryParameters(false);
        modifiedQuery.remove(DERIVED);
        modifiedQuery.remove(PE);

        if (!originalQuery.containsKey(PE)
                || !originalQuery.containsKey("pluginName")) {
            throw new VizException("getData method requires more query "
                    + "information for plugin : "
                    + originalQuery.get("pluginName").getConstraintValue());
        } else {
            String[] physicalElements = originalQuery.get(PE)
                    .getConstraintValue().split(",");
            requestedParam = physicalElements[physicalElements.length - 1];
        }

        List<String> baseParams = new ArrayList<String>(
                Arrays.asList(CatalogQuery.performQuery(PE, modifiedQuery)));

        if (!baseParams.contains(requestedParam)
                && derParLibrary.containsKey(requestedParam)) {
            DerivParamDesc desc = derParLibrary.get(requestedParam);
            DerivParamMethod method = desc.getMethods().get(0);
            boolean requestInitialized = false;

            for (IDerivParamField ifield : method.getFields()) {
                DerivParamField field = (DerivParamField) ifield;
                Map<String, RequestConstraint> query = modifyQuery(
                        modifiedQuery, field);

                property.setEntryQueryParameters(query, false);
                String scriptToExecute = ScriptCreator.createScript(property);
                List<Object> responses = Loader.loadScripts(
                        new String[] { scriptToExecute }, timeOut);
                for (int i = 0; i < responses.size(); i++) {
                    SatelliteRecord record = (SatelliteRecord) responses.get(i);
                    if (requestInitialized) {
                        for (DerivedParameterRequest definedRequest : listOfRequests) {
                            if (record.getDataTime().compareTo(
                                    definedRequest.getBaseTime()) != 0) {
                                continue;
                            }
                            definedRequest.addBaseParam(record);
                            break;
                        }
                    } else {
                        DerivedParameterRequest request = new DerivedParameterRequest();
                        request.setParameterAbbreviation(requestedParam);
                        request.setMethod(method.getName());
                        request.addBaseParam(record);
                        request.setBaseTime(record.getDataTime());
                        listOfRequests.add(request);
                        SatelliteRecord derivedRecord = new SatelliteRecord(
                                record.getDataURI());
                        // Make sure to get the number of interpolation levels!
                        derivedRecord.setInterpolationLevels(record.getInterpolationLevels());
                        
                        derivedRecord.setPhysicalElement(originalQuery.get(PE)
                                .getConstraintValue());
                        derivedRecord.setMessageData(request);
                        derivedRecord.setCoverage(record.getCoverage());
                        // This should not be necessary but file based tile set
                        // expects it.
                        derivedRecord.setHdfFileId(record.getHdfFileId());
                        try {
                            derivedRecord.setDataURI(null);
                            derivedRecord.constructDataURI();
                        } catch (PluginException e) {
                            throw new VizException(e);
                        }
                        initResponses.add(derivedRecord);
                    }
                }
                requestInitialized = true;
            }
            for (int i = 0; i < initResponses.size(); i++) {
                Object satObject = initResponses.get(i);
                SatelliteRecord record = (SatelliteRecord) satObject;
                DerivedParameterRequest currentRequest = (DerivedParameterRequest) record
                        .getMessageData();
                if (currentRequest.getBaseParams().size() != method.getFields()
                        .size()) {
                    if (initResponses.remove(satObject)) {
                        i--;
                    }
                }
            }
            return initResponses;
        } else {
            String scriptToExecute = ScriptCreator.createScript(property);
            return Loader
                    .loadScripts(new String[] { scriptToExecute }, timeOut);
        }

    }

    public String recordKeyGenerator(PluginDataObject pdo) {
        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException {
        if (obj.getMessageData() == null) {
            IDataRecord record = null;
            try {
                record = CubeUtil.retrieveData(obj, obj.getPluginName());
            } catch (VizException e) {
                throw new VizDataCubeException(
                        "Error retrieving satellite record.", e);
            }
            return new IDataRecord[] { record };
        }

        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException {
        if (obj.getMessageData() == null) {
            IDataRecord record = null;
            try {
                record = CubeUtil.retrieveData(obj, obj.getPluginName(), req,
                        dataset);
            } catch (VizException e) {
                throw new VizDataCubeException(
                        "Error retrieving satellite record.", e);
            }
            return new IDataRecord[] { record };
        }

        boolean interpolate = false;
        int targetWidth = 0;
        int targetHeight = 0;
        ArrayList<File> hdf5Files = new ArrayList<File>();
        ArrayList<String> datasets = new ArrayList<String>();
        ArrayList<SatelliteRecord> records = new ArrayList<SatelliteRecord>();
        ArrayList<Object> bytes = new ArrayList<Object>();
        DerivedParameterRequest derivedRequest = (DerivedParameterRequest) obj
                .getMessageData();
        // clone the request in case of multiple threads.
        derivedRequest = new DerivedParameterRequest(derivedRequest);
        for (Object param : derivedRequest.getBaseParams()) {
            records.add((SatelliteRecord) param);
            hdf5Files.add(HDF5Util.findHDF5Location((SatelliteRecord) param));
            datasets.add(getDataset(((SatelliteRecord) param).getDataURI(),
                    dataset));
        }

        int largestRecIdx = 0;
        /*
         * Need to update to take into account if more than 2 sat records are
         * used in derived parameter
         */
        GridGeometry2D geo = null;
        if (!records.get(0).getSpatialObject().getNx()
                .equals(records.get(1).getSpatialObject().getNx())
                || !records.get(0).getSpatialObject().getNy()
                        .equals(records.get(1).getSpatialObject().getNy())) {
            interpolate = true;
            geo = MapUtil.getGridGeometry(records.get(largestRecIdx)
                    .getSpatialObject());
            targetWidth = records.get(largestRecIdx).getSpatialObject().getNx();
            targetHeight = records.get(largestRecIdx).getSpatialObject()
                    .getNy();
        }

        try {
            int i = 0;
            for (File file : hdf5Files) {
                IDataStore ds = DataStoreFactory.getDataStore(file);
                IDataRecord rec = null;
                if (interpolate && i != largestRecIdx) {
                    if (obj.getRecord() == null) {
                        rec = ds.retrieve("", datasets.get(i), Request.ALL);
                        int w = (int) ((ByteDataRecord) rec).getSizes()[0];
                        int h = (int) ((ByteDataRecord) rec).getSizes()[1];
                        obj.setRecord(interpolate((ByteDataRecord) rec, w, h,
                                targetWidth, targetHeight, geo));
                    }
                    // cut out the tiles from that interpolated dataset
                    // and make an image from just that small subset
                    ByteDataRecord bdr = cutTile(
                            (ByteDataRecord) obj.getRecord(), req);
                    bytes.add(bdr);
                } else {
                    rec = ds.retrieve("", datasets.get(i), req);
                    bytes.add((ByteDataRecord) rec);
                }

                i++;
            }
        } catch (Exception e) {
            throw new VizDataCubeException("Error in Satellite Data Cube", e);
        }

        derivedRequest.setArgumentRecords(bytes.toArray(new Object[] {}));
        DerivedParameterGenerator.addTask(derivedRequest);
        List<?> finalResult;
        try {
            finalResult = derivedRequest.getQueue();
        } catch (VizException e) {
            throw new VizDataCubeException(e);
        }

        if (finalResult != null && finalResult.size() == 1) {
            return new IDataRecord[] { ((ByteDataRecord) finalResult.get(0)) };
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecords(java
     * .util.List, com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException {
        for (PluginDataObject obj : objs) {
            IDataRecord[] records = getRecord(obj, req, dataset);
            obj.setMessageData(records);
        }
    }

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "satellite" };
    }

    @Override
    public Object getInventory() {
        return null;
    }

    @Override
    public void initInventory() {
        derParLibrary = DerivedParameterGenerator.getDerParLibrary();
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        return getPoints(plugin, parameters, null, queryParams);
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        return null;
    }

    private String getDataset(String dataUri, String dataset) {
        return "/" + dataUri + "/" + dataset;
    }

    private ByteDataRecord interpolate(ByteDataRecord bdr, int width,
            int height, int targetWidth, int targetHeight, GridGeometry2D geo) {
        int[] nBits = new int[] { 8 };
        GridCoverageFactory factory = new GridCoverageFactory();
        ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
        ColorModel cm = new ComponentColorModel(cs, nBits, false, true,
                Transparency.OPAQUE, DataBuffer.TYPE_BYTE);
        DataBufferByte data = new DataBufferByte(bdr.getByteData(), height
                * width);
        SampleModel sm = cm.createCompatibleSampleModel(width, height);
        WritableRaster writeRaster = WritableRaster.createWritableRaster(sm,
                data, null);

        // Create the coverage for the image
        GridCoverage2D gc = factory
                .create("in", writeRaster, geo.getEnvelope());
        GridCoverage2D projected = MapUtil.reprojectCoverage(gc,
                geo.getCoordinateReferenceSystem(), geo,
                Interpolation.getInstance(Interpolation.INTERP_NEAREST));

        Raster currSatRaster = projected.getRenderedImage().getData();
        byte[] currSat = ((DataBufferByte) currSatRaster.getDataBuffer())
                .getData();

        long newSizes[] = { (long) targetWidth, (long) targetHeight };

        ByteDataRecord rval = new ByteDataRecord(bdr.getName(), bdr.getGroup(),
                currSat, bdr.getDimension(), newSizes);
        return rval;
    }

    private ByteDataRecord cutTile(ByteDataRecord bdr, Request req) {
        int minTileX = req.getMinIndexForSlab()[0];
        int minTileY = req.getMinIndexForSlab()[1];
        int maxTileX = req.getMaxIndexForSlab()[0];
        int maxTileY = req.getMaxIndexForSlab()[1];
        int widthOfWholeImage = (int) bdr.getSizes()[0];
        int newWidth = maxTileX - minTileX;
        int newHeight = maxTileY - minTileY;
        long newSizes[] = { (long) newWidth, (long) newHeight };

        byte bytes[] = bdr.getByteData();
        byte newBytes[] = new byte[newHeight * newWidth];

        int index = 0;
        for (int j = minTileY; j < maxTileY; j++) {
            for (int i = minTileX; i < maxTileX; i++) {
                newBytes[index++] = bytes[j * widthOfWholeImage + i];

            }
        }

        ByteDataRecord rval = new ByteDataRecord(bdr.getName(), bdr.getGroup(),
                newBytes, bdr.getDimension(), newSizes);
        return rval;
    }

    private static String getPhysicalElement(DerivParamField field) {
        return field.getParam().replace("_", " ").replace("pp", ".")
                .replace("oo", "(").replace("cc", ")").replace("hh", "-");
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        if (constraints.containsKey(DERIVED)) {

            constraints = new HashMap<String, RequestConstraint>(constraints);
            constraints.remove(DERIVED);
            RequestConstraint pe = constraints.remove(PE);
            DerivParamDesc desc = derParLibrary.get(pe.getConstraintValue());
            DerivParamMethod method = desc.getMethods().get(0);

            pe = new RequestConstraint(null, ConstraintType.IN);

            for (IDerivParamField ifield : method.getFields()) {
                DerivParamField field = (DerivParamField) ifield;
                pe.addToConstraintValueList(getPhysicalElement(field));
            }
            constraints.put(PE, pe);
        }
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        Map<TimeQueryRequest, Integer> derivedRequests = new HashMap<TimeQueryRequest, Integer>();
        List<TimeQueryRequest> baseRequests = new ArrayList<TimeQueryRequest>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            if (!request.getQueryTerms().containsKey("DERIVED")) {
                baseRequests.add(request);
            } else {
                List<TimeQueryRequest> derived = getDerivedRequests(request,
                        request.getBinOffset());
                derivedRequests.put(request, derived.size());
                baseRequests.addAll(derived);
                // TODO it might be possible to merge the derived requests that
                // have dependencies that are the same as some of the non
                // derived requests to avoid hitting the db twice.
            }
        }
        TimeQueryRequestSet set = new TimeQueryRequestSet();
        set.setRequests(baseRequests.toArray(new TimeQueryRequest[0]));
        @SuppressWarnings("unchecked")
        List<List<DataTime>> baseResults = (List<List<DataTime>>) ThriftClient
                .sendRequest(set);
        List<List<DataTime>> results = new ArrayList<List<DataTime>>(
                requests.size());
        int baseIndex = 0;
        for (TimeQueryRequest request : requests) {
            if (derivedRequests.containsKey(request)) {
                int size = derivedRequests.get(request);
                List<DataTime> timeList = new ArrayList<DataTime>();
                for (int i = 0; i < size; i++) {
                    List<DataTime> derived = baseResults.get(baseIndex);
                    if (derived != null && !derived.isEmpty()
                            && timeList.size() == 0) {
                        timeList.addAll(derived);
                    } else if (derived != null && !derived.isEmpty()
                            && timeList.size() > 0) {
                        ArrayList<DataTime> newTimeList = new ArrayList<DataTime>();
                        for (DataTime result : derived) {
                            for (DataTime check : timeList) {
                                if (check.equals(result)) {
                                    newTimeList.add(result);
                                    continue;
                                }
                            }
                        }
                        timeList = newTimeList;
                    } else if ((derived == null || derived.isEmpty())
                            && timeList.size() > 0) {
                        timeList.clear();
                    }
                    baseIndex += 1;
                }
                if (request.isMaxQuery() && !timeList.isEmpty()) {
                    Collections.sort(timeList);
                    results.add(Arrays.asList(timeList.get(timeList.size() - 1)));
                } else {
                    results.add(timeList);
                }
            } else {
                results.add(baseResults.get(baseIndex));
                baseIndex += 1;
            }

        }
        return results;
    }

    private List<TimeQueryRequest> getDerivedRequests(TimeQueryRequest request,
            BinOffset binOffset) {
        List<TimeQueryRequest> result = new ArrayList<TimeQueryRequest>(2);
        Map<String, RequestConstraint> queryParams = new HashMap<String, RequestConstraint>(
                request.getQueryTerms());
        String plugin = queryParams.get("pluginName").getConstraintValue();
        String param = queryParams.get(PE).getConstraintValue();
        if (plugin.matches("satellite") && derParLibrary.containsKey(param)) {
            DerivParamDesc derParDesc = derParLibrary.get(param);
            DerivParamMethod method = derParDesc.getMethods().get(0);
            // We have a derived parameter for the requested grid
            for (IDerivParamField ifield : method.getFields()) {
                DerivParamField field = (DerivParamField) ifield;
                TimeQueryRequest newRequest = new TimeQueryRequest();
                newRequest.setBinOffset(binOffset);
                newRequest.setMaxQuery(false);
                newRequest.setQueryTerms(modifyQuery(queryParams, field));
                newRequest.setPluginName("satellite");
                result.add(newRequest);
            }
        }
        return result;
    }

    private Map<String, RequestConstraint> modifyQuery(
            Map<String, RequestConstraint> originalQuery, DerivParamField field) {
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>(
                originalQuery);
        String paramAbb = getPhysicalElement(field);
        query.put(PE, new RequestConstraint(paramAbb,
                RequestConstraint.ConstraintType.EQUALS));
        query.remove(DERIVED);

        return query;
    }
}
