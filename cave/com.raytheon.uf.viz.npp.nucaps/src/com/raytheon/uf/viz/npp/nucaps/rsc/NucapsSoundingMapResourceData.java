package com.raytheon.uf.viz.npp.nucaps.rsc;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.npp.sounding.rsc.NPPSoundingMapResourceData;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * Resource data for availability map of nucaps soundings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Engineer   Description
 * ------------ ------- ---------- --------------------------
 * Dec 16, 2015 18191   pwang      Initial creation. Color code dots base on QC value
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NucapsSoundingMapResourceData extends NPPSoundingMapResourceData {
    private static String PARAM_DATAURI = "dataURI";

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.rsc.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        NucapsSoundingMapResource resource = new NucapsSoundingMapResource(this,
                loadProperties);
        if (objects instanceof PluginDataObject[]) {
            resource.addRecords((PluginDataObject[]) objects);
        }
        return resource;
    }

    /**
     * Get nucaps data
     * @param Collection<DataTime> loadSet
     * 
     */
    @Override
    protected PluginDataObject[] requestPluginDataObjects(
            Collection<DataTime> loadSet) throws VizException {
        List<DataTime> timesToLoad = new ArrayList<DataTime>(loadSet);
        Collections.sort(timesToLoad);
        DataTime first = timesToLoad.get(0);
        DataTime last = timesToLoad.get(timesToLoad.size() - 1);
        Map<String, RequestConstraint> requestMap = new HashMap<String, RequestConstraint>(
                getMetadataMap());
        requestMap.put(
                "dataTime.refTime",
                new RequestConstraint(TimeUtil.formatToSqlTimestamp(first
                        .getValidPeriod().getStart()), TimeUtil
                        .formatToSqlTimestamp(last.getValidPeriod().getEnd())));

        PluginDataObject[] pdos;
        NPPSoundingRecord nsr;
        try {
            
            TimeRange tr = first.getValidPeriod();
            String[] params = {PARAM_DATAURI, NucapsRecord.PDV_QUALITY_FLAG};
            PointDataContainer pdc = PointDataRequest
                    .requestPointDataAllLevels(tr,
                        NucapsRecord.PLUGIN_NAME, params, null,
                            requestMap);
            int size = pdc.getCurrentSz();
            pdos = new PluginDataObject[size];
            for (int i = 0; i < size; ++i) {
                nsr = (NPPSoundingRecord)DataURIUtil.createPluginDataObject(pdc.readRandom(i).getString(PARAM_DATAURI));
                nsr.setPointDataView(pdc.readRandom(i));
                pdos[i] = nsr;
            }
            
        } catch (Exception e) {
            throw new VizException(e);
        }
        List<PluginDataObject> finalList = new ArrayList<PluginDataObject>(
                pdos != null ? pdos.length : 0);

        if (pdos != null) {
            for (Object obj : pdos) {
                PluginDataObject pdo = (PluginDataObject) obj;
                for (DataTime dt : loadSet) {
                    if (withinRange(dt.getValidPeriod(), pdo.getDataTime())) {
                        finalList.add(pdo);
                        break;
                    }
                }
            }
        }
        Collections.sort(finalList, layerComparator);
        return finalList.toArray(new PluginDataObject[finalList.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        NucapsSoundingMapResourceData other = (NucapsSoundingMapResourceData) obj;
        if (getNsharpResourceData() == null) {
            if (other.getNsharpResourceData() != null)
                return false;
        } else if (!getNsharpResourceData().equals(other.getNsharpResourceData()))
            return false;
        if (getResourceName() == null) {
            if (other.getResourceName() != null)
                return false;
        } else if (!getResourceName().equals(other.getResourceName()))
            return false;
        return true;
    }
}
