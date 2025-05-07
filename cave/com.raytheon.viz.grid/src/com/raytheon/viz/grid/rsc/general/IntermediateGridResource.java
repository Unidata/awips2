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
package com.raytheon.viz.grid.rsc.general;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.collections.CollectionUtils;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * A grid resource that caches the most recently viewed data when updates come
 * in, if the ResourceData has keepDataWhileRetrievingNext enabled. For example,
 * an update arrives for a normal grid resource, and the resource removes the
 * image of that data and then creates a new renderable image to display the
 * updated data. During the time between the removal of the previous image and
 * the creation of the new renderables, the map will be blank, or put another
 * way, have no grid data image. However, when using derived parameters, that
 * time between removal and creation of the new renderable image can be larger
 * due to the time necessary to both retrieve the raw data and perform the
 * calculations on it. This IntermediateGridResource keeps the previously
 * displayed data and image until the retrieve and calculate steps are done, and
 * then displays the new data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 02, 2021  8651     njensen   Initial creation
 * Sep 22, 2021  8651     njensen   Handle when resource time is null
 * Dec 06, 2021  8341     randerso  Added DataTime parameter to
 *                                  createRenderable()*
 * Dec 20, 2023  2036519  mapeters  Remove previousDataMap entries once new
 *                                  data is available, prevent deadlock in
 *                                  dataUpdateArrived()
 * Jan 09, 2023  2036695  mapeters  Fix auto-update for some non-spatial
 *                                  resources
 * Jul 15, 2024  2037624  mapeters  Override getPluginDataObjects so previousPdoMap
 *                                  is checked for all PDO getter methods
 *
 * </pre>
 *
 * @author njensen
 */
public class IntermediateGridResource extends GridResource<GridResourceData> {

    protected final Map<DataTime, List<PluginDataObject>> previousPdoMap = new ConcurrentHashMap<>();

    protected final Map<DataTime, List<GeneralGridData>> previousDataMap = new ConcurrentHashMap<>();

    /*
     * We synchronize on access to previousRenderableMap to make sure we don't
     * accidentally miss one when looping over the values
     */
    protected final Map<DataTime, Collection<IRenderable>> previousRenderableMap = new ConcurrentHashMap<>();

    public IntermediateGridResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        if (resourceData.isKeepDataWhileRetrievingUpdate()) {
            resourceData.addChangeListener((type, object) -> {
                if (type == ChangeType.DATA_UPDATE
                        && object instanceof DataTime) {
                    DataTime dt = (DataTime) object;
                    dataUpdateArrived(dt);
                }
            });
            registerListener((IRefreshListener) () -> {
                /*
                 * Pull any completed data requests out of the requestRunner and
                 * put them into dataMap, which also removes any corresponding
                 * entry from previousDataMap. This prevents different versions
                 * of the same data from being kept around in different places
                 * and using up unnecessary memory.
                 */
                for (DataTime dt : getDataTimes()) {
                    requestData(dt, true);
                }
            });
        }
    }

    /**
     * Convenience method to check for null and dispose all the renderables in
     * the collection.
     *
     * @param renderables
     */
    protected void disposeRenderables(Collection<IRenderable> renderables) {
        if (renderables != null) {
            for (IRenderable r : renderables) {
                disposeRenderable(r);
            }
            renderables.clear();
        }
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        previousPdoMap.clear();
        previousDataMap.clear();
        synchronized (previousRenderableMap) {
            for (Collection<IRenderable> renderables : previousRenderableMap
                    .values()) {
                disposeRenderables(renderables);
            }
            previousRenderableMap.clear();
        }
    }

    /**
     * Defers to the super method getOrCreateRenderables, however, if that
     * returns no renderables then it checks the "previous" maps to see if we
     * have data from the previous calculation that can be displayed. This makes
     * it so the display does not go blank after a notification arrives while it
     * is doing the retrieve and any derived calculations, and instead shows the
     * previous data for this time.
     */
    @Override
    protected Collection<IRenderable> getOrCreateRenderables(
            IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        Collection<IRenderable> renderables = super.getOrCreateRenderables(
                target, paintProps);

        DataTime time = paintProps.getDataTime();
        if (time == null) {
            time = getTimeForResource();
        }
        if (time == null) {
            return Collections.emptyList();
        }

        if (this.resourceData.isKeepDataWhileRetrievingUpdate()) {
            // Data was not found for the current time, check the previous data
            synchronized (previousRenderableMap) {
                if (getPaintStatus() == PaintStatus.INCOMPLETE
                        && renderables.isEmpty()) {

                    renderables = previousRenderableMap.get(time);
                    if (renderables == null) {
                        List<GeneralGridData> dataList = previousDataMap
                                .get(time);
                        if (dataList == null) {
                            return Collections.emptyList();
                        }
                        renderables = new ArrayList<>(dataList.size());
                        for (GeneralGridData data : dataList) {
                            IRenderable renderable = createRenderable(target,
                                    data, time);
                            if (renderable != null) {
                                renderables.add(renderable);
                            }
                        }
                        previousRenderableMap.put(time, renderables);
                    }
                } else {
                    // free up memory if they exist
                    disposeRenderables(previousRenderableMap.remove(time));
                }
            }

        }

        return renderables;
    }

    /**
     * This is called from notifications sent for example by the time matcher
     * when a new frame is added (and the earliest frame is removed). It should
     * NOT be called when a data update arrives.
     */
    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        previousPdoMap.remove(dataTime);
        previousDataMap.remove(dataTime);
        synchronized (previousRenderableMap) {
            disposeRenderables(previousRenderableMap.remove(dataTime));
        }
    }

    /*
     * This is overridden to keep the legend working correctly when there is no
     * data for this time in the pdoMap but there is data in the previousPdoMap.
     */
    @Override
    public List<PluginDataObject> getPluginDataObjects(DataTime time) {
        List<PluginDataObject> pdos = super.getPluginDataObjects(time);
        if (CollectionUtils.isEmpty(pdos) && time != null
                && resourceData.isKeepDataWhileRetrievingUpdate()) {
            pdos = previousPdoMap.get(time);
            if (pdos != null) {
                pdos = new ArrayList<>(pdos);
            }
        }
        return pdos;
    }

    /**
     * Simulates the super class AbstractGridResource's remove by removing the
     * time from the super class data maps and times list, which in turn enables
     * requesting data for that particular time (instead of finding it in the
     * maps/cache). But then keeps the data from the maps around in the
     * "previous" maps so that it can be displayed while the latest retrieval
     * and calculation is ongoing. AutoUpdater schedules time matching after
     * passing updates into here, which triggers a new retrieval/calculation for
     * the time.
     *
     * This should never be called if the
     * resourceData.keepDataWhileRetrievingNext is false.
     *
     * @param dataTime
     */
    protected void dataUpdateArrived(DataTime dataTime) {
        List<PluginDataObject> pdos = pdoMap.remove(dataTime);
        List<GeneralGridData> data = dataMap.remove(dataTime);

        if (pdos != null) {
            previousPdoMap.put(dataTime, pdos);
        }
        if (data != null) {
            previousDataMap.put(dataTime, data);
        }

        List<IRenderable> renderableList;
        synchronized (renderableMap) {
            renderableList = renderableMap.remove(dataTime);
        }

        synchronized (previousRenderableMap) {
            if (renderableList != null) {
                Collection<IRenderable> previousList = previousRenderableMap
                        .put(dataTime, renderableList);
                disposeRenderables(previousList);
            }
        }

        /*
         * Call super's remove to remove the time from other places such as the
         * dataTimes list, but to leave it in the "previous" maps
         */
        super.remove(dataTime);
        issueRefresh();
    }

    /**
     * Override to ensure the previousRenderableMap's renderables are also
     * reprojected.
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        synchronized (previousRenderableMap) {
            Iterator<Collection<IRenderable>> iter = previousRenderableMap
                    .values().iterator();
            while (iter.hasNext()) {
                Collection<IRenderable> renderableList = iter.next();
                boolean remove = false;
                for (IRenderable renderable : renderableList) {
                    if (!projectRenderable(renderable)) {
                        remove = true;
                        break;
                    }
                }
                /*
                 * If any one renderable fails to reproject then dispose them
                 * all, so that the whole frame gets regenerated.
                 */
                if (remove) {
                    disposeRenderables(renderableList);
                    iter.remove();
                }
            }
        }
    }

    /**
     * Overridden to use the previous data for this time if there is no current
     * data.
     */
    @Override
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        InterrogateMap result = super.interrogate(coordinate, time, keys);
        if (result != null) {
            return result;
        }
        if (time == null) {
            return null;
        }
        List<GeneralGridData> dataList = previousDataMap.get(time);
        if (dataList == null) {
            return null;
        }

        for (GeneralGridData data : dataList) {
            result = interrogate(coordinate, data, keys);
            if (result != null) {
                return result;
            }
        }

        return new InterrogateMap();
    }

    @Override
    protected void cacheData(DataTime time, List<GeneralGridData> data) {
        // Prevent previousDataMap from using up memory with stale data
        this.previousDataMap.remove(time);
        super.cacheData(time, data);
    }
}
