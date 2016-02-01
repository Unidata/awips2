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
package com.raytheon.uf.viz.d2d.core.time;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicReference;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTimeComparator;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.core.D2DLoadProperties;

/**
 * Performs D2D-style time matching
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Feb 10, 2009  1959     chammack   Initial creation
 * Jul 03, 2013  2159     bsteffen   Synchronize TimeCache access.
 * Aug 09, 2013  16448    dfriedman  Validate time match basis in
 *                                   redoTimeMatching
 * May 05, 2014  17201    dfriedman  Make same-radar time matching work more
 *                                   like A1.
 * May 05, 2014  3265     bsteffen   Better handling of resources returning null
 *                                   dataTimes.
 * May 13, 2015  4461     bsteffen   Move the logic to change frames into the
 *                                   FrameCoordinator.
 * Jul 14, 2015  13900    dfriedman  Validate descriptor of time match basis
 *                                   before time matching it.
 * Jul 30, 2015  17761    dfriedman  Allow resources to return data times based
 *                                   on base frame times.
 * Sep 10, 2015  4856     njensen    Removed unnecessary code
 * Dec 03, 2015  5147     bsteffen   Make timeMatchBasis atomic.
 * 
 * </pre>
 * 
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
public class D2DTimeMatcher extends AbstractTimeMatcher {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DTimeMatcher.class);

    /**
     * Always synchronize on an instance of TimeCache before using it to avoid
     * getting mixed state from modification on other threads.
     */
    private static class TimeCache {
        /**
         * The last set of times that the resource with these properties was
         * matched against. As long as we are matching against these same times
         * then lastFrameTimes is valid. This will be null for the
         * TimeMatchBasis.
         */
        private DataTime[] lastBaseTimes;

        /** The result of the last time matching. */
        private DataTime[] lastFrameTimes;

        /** The number of frames time matched against */
        private int lastFrameCount;

        public DataTime[] getLastBaseTimes() {
            return lastBaseTimes;
        }

        public DataTime[] getLastFrameTimes() {
            return lastFrameTimes;
        }

        public int getLastFrameCount() {
            return lastFrameCount;
        }

        public void setTimes(DataTime[] baseTimes, DataTime[] frameTimes) {
            setTimes(baseTimes, frameTimes, -1);
        }

        public void setTimes(DataTime[] baseTimes, DataTime[] frameTimes,
                int frameCount) {
            this.lastBaseTimes = baseTimes;
            this.lastFrameTimes = frameTimes;
            this.lastFrameCount = frameCount;
        }

    }

    /**
     * The resource that determines the frame times to be displayed, i.e. other
     * resources on the same descriptor will time match against the
     * timeMatchBasis. This is an atomic reference to allow safe concurrent
     * modification. It is never safe to assume that 2 calls to get will return
     * the same value so complex operations(such as redoTimeMatching) will need
     * to get a reference to the basis once at the beginning of the operation
     * and use that reference throughout the operation.
     */
    protected final AtomicReference<AbstractVizResource<?, ?>> timeMatchBasisRef = new AtomicReference<>();

    private final IDisposeListener timeMatchBasisDisposeListener = new IDisposeListener() {

        @Override
        public void disposed(AbstractVizResource<?, ?> resource) {
            timeMatchBasisRef.compareAndSet(resource, null);
        }

    };

    /** A clock time limit for loading data (optional) */
    @XmlAttribute
    protected Date clockFilter;

    /** A forecast time filter (optional) */
    @XmlAttribute
    protected long forecastFilter;

    /** A delta time filter (optional) */
    @XmlAttribute
    protected long deltaFilter;

    /** user time options interaction is selected. */
    private boolean isTimeOptionsSelected;

    /** The load mode */
    @XmlAttribute
    protected LoadMode loadMode = (LoadMode) VizGlobalsManager
            .getCurrentInstance().getPropery(VizConstants.LOADMODE_ID);

    private AbstractTimeMatchingConfigurationFactory configFactory;

    private final Map<AbstractVizResource<?, ?>, TimeCache> timeCacheMap = new IdentityHashMap<AbstractVizResource<?, ?>, D2DTimeMatcher.TimeCache>();

    public D2DTimeMatcher() {
        super();
        try {
            configFactory = AbstractTimeMatchingConfigurationFactory
                    .constructConfigurationFactory();
        } catch (VizException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error Initializing Time Matcher", e);
        }
    }

    @Override
    public void redoTimeMatching(AbstractVizResource<?, ?> resource) {
        TimeCache cache = null;
        synchronized (timeCacheMap) {
            cache = timeCacheMap.get(resource);
        }
        if (cache != null) {
            synchronized (cache) {
                cache.setTimes(null, null);
            }
        }
    }

    /**
     * Checks if a resource is contained in the {@link IResourceGroup}
     * recursively checking for {@link IResourceGroup}s in the group's list
     * 
     */
    private boolean contained(IResourceGroup group,
            AbstractVizResource<?, ?> resource) {
        ResourceList list = group.getResourceList();
        if (list.containsRsc(resource)) {
            return true;
        } else {
            for (ResourcePair rp : list) {
                if (rp.getResourceData() instanceof IResourceGroup) {
                    if (contained((IResourceGroup) rp.getResourceData(),
                            resource)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    @Override
    public void redoTimeMatching(IDescriptor descriptor) {
        synchronized (this) {
            /* Find the times for the time match basis. */
            Pair<AbstractVizResource<?, ?>, DataTime[]> basisInfo = findBasisTimes(descriptor);
            if (basisInfo == null) {
                descriptor.setFramesInfo(new FramesInfo(null, -1));
                return;
            }
            AbstractVizResource<?, ?> timeMatchBasis = basisInfo.getLeft();
            DataTime[] timeSteps = basisInfo.getRight();

            Map<AbstractVizResource<?, ?>, DataTime[]> resourceTimeMap = new HashMap<AbstractVizResource<?, ?>, DataTime[]>();
            resourceTimeMap.put(timeMatchBasis, timeSteps);

            /* Find the times for the time match basis. */
            Iterator<ResourcePair> pairIterator = descriptor.getResourceList()
                    .listIterator();
            while (pairIterator.hasNext()) {
                AbstractVizResource<?, ?> rsc = pairIterator.next()
                        .getResource();
                recursiveOverlay(descriptor, timeMatchBasis, new FramesInfo(
                        timeSteps, -1, resourceTimeMap), rsc, resourceTimeMap);
            }

            if (descriptor.getTimeMatcher() != this) {
                /*
                 * The descriptor has switched to a new time matcher so the
                 * results of this operation are no longer relevant and are
                 * discarded. This can happen when the display is cleared.
                 */
                return;
            }

            /* Update the descriptor to the new times. */
            if ((timeMatchBasis.getDescriptor() != null)
                    && (timeMatchBasis.getDescriptor() != descriptor)) {
                int idx = timeMatchBasis.getDescriptor().getFramesInfo()
                        .getFrameIndex();
                if ((idx >= 0) && (idx < timeSteps.length)) {
                    descriptor.setFramesInfo(new FramesInfo(timeSteps, idx,
                            resourceTimeMap));
                } else {
                    descriptor.setFramesInfo(new FramesInfo(timeSteps,
                            resourceTimeMap));
                }
            } else {
                descriptor.setFramesInfo(new FramesInfo(timeSteps,
                        resourceTimeMap));
            }

            /* Add Remove data for all the resources. */
            for (Entry<AbstractVizResource<?, ?>, DataTime[]> entry : resourceTimeMap
                    .entrySet()) {
                if (entry.getKey().getDescriptor() == descriptor) {
                    timeMatchUpdate(entry.getKey(), entry.getValue());
                }
            }
        }
    }

    /**
     * Recursively determine times for all resource, or if it is an
     * IResourceGroup then for all resources in it.
     * 
     * @param descriptor
     *            the descriptor that is being updated
     * @param timeMatchBasis
     *            the current timeMatchBasis, used to ensure that it is not time
     *            matched again, even if it is located in a IResourceGroup
     * @param framesInfo
     *            the matched times for the rsc passed in will be added to the
     *            map of this FramesInfo
     * @param rsc
     *            the resource being updated.
     * @param frameTimesSoure
     *            map of all previously time matched resources that may be used
     *            to determine the frame times
     */
    private void recursiveOverlay(IDescriptor descriptor,
            AbstractVizResource<?, ?> timeMatchBasis, FramesInfo framesInfo,
            AbstractVizResource<?, ?> rsc,
            Map<AbstractVizResource<?, ?>, DataTime[]> frameTimesSoure) {
        if (rsc == null) {
            return;
        }
        if (rsc instanceof IResourceGroup) {
            Map<AbstractVizResource<?, ?>, DataTime[]> completed = new HashMap<AbstractVizResource<?, ?>, DataTime[]>(
                    frameTimesSoure);
            for (ResourcePair rp : ((IResourceGroup) rsc).getResourceList()) {
                AbstractVizResource<?, ?> rsc1 = rp.getResource();
                recursiveOverlay(descriptor, timeMatchBasis, framesInfo, rsc1,
                        completed);
            }
        }

        if (rsc != timeMatchBasis) {
            TimeMatchingConfiguration config = getConfiguration(rsc
                    .getLoadProperties());
            TimeCache timeCache = getTimeCache(rsc);
            synchronized (timeCache) {
                DataTime[] timeSteps = getFrameTimes(descriptor,
                        timeMatchBasis, framesInfo, frameTimesSoure);
                if (Arrays.equals(timeSteps, timeCache.getLastBaseTimes())) {
                    framesInfo.getTimeMap().put(rsc,
                            timeCache.getLastFrameTimes());
                } else {
                    config = config.clone();
                    if ((config.getDataTimes() == null)
                            || (config.getDataTimes().length < 1)) {
                        config.setDataTimes(getLatestTimes(rsc, timeSteps));
                    }
                    populateConfiguration(config);
                    TimeMatcher tm = new TimeMatcher();
                    if (rsc instanceof ID2DTimeMatchingExtension) {
                        ((ID2DTimeMatchingExtension) rsc).modifyTimeMatching(
                                this, rsc, tm);
                    }
                    DataTime[] overlayDates = tm.makeOverlayList(
                            config.getDataTimes(), config.getClock(),
                            timeSteps, config.getLoadMode(),
                            config.getForecast(), config.getDelta(),
                            config.getTolerance());
                    timeCache.setTimes(timeSteps, overlayDates);
                    framesInfo.getTimeMap().put(rsc, overlayDates);
                }
            }
        }
    }

    /**
     * Determine the times to match this resource against, for single panel this
     * is the timeMatchBasis times, for four panel it is a bit more complex.
     */
    private DataTime[] getFrameTimes(IDescriptor descriptor,
            AbstractVizResource<?, ?> timeMatchBasis, FramesInfo frameInfo,
            Map<AbstractVizResource<?, ?>, DataTime[]> frameTimesSource) {
        DataTime[] descTimes = frameInfo.getFrameTimes();
        if ((timeMatchBasis != null)
                && (timeMatchBasis.getDescriptor() == descriptor)) {
            return descTimes;
        }

        /*
         * If the time match basis is not in this descriptor we should instead
         * fall back on the first resource in this descriptor which has times
         * for this frame. This concept was adopted from Frame::dataTime() in
         * the A1 source.
         */
        DataTime[] times = new DataTime[frameInfo.getFrameCount()];

        for (ResourcePair rp : descriptor.getResourceList()) {
            DataTime[] rscTimes = frameTimesSource.get(rp.getResource());
            if ((rscTimes == null) || (rscTimes.length != times.length)) {
                if (rp.getResource() instanceof IResourceGroup) {
                    /* Descend into resource groups. */
                    for (ResourcePair rp1 : ((IResourceGroup) rp.getResource())
                            .getResourceList()) {
                        rscTimes = frameTimesSource.get(rp1.getResource());
                        if ((rscTimes != null)
                                && (rscTimes.length == times.length)) {
                            for (int i = 0; i < times.length; i++) {
                                if ((times[i] == null) && (rscTimes[i] != null)) {
                                    times[i] = rscTimes[i];
                                }
                            }
                        }
                    }
                }
                continue;
            }
            for (int i = 0; i < times.length; i++) {
                if ((times[i] == null) && (rscTimes[i] != null)) {
                    times[i] = rscTimes[i];
                }
            }
        }
        for (int i = 0; i < times.length; i++) {
            if ((times[i] == null) && (descTimes[i] != null)) {
                times[i] = descTimes[i];
            }
        }
        return times;
    }

    /**
     * Attempts to find the frame times for a time match basis, if time match
     * basis is not defined attempts to find a new time match basis. Returns
     * null if no Time Match basis can be found. This method will update the
     * {@link #timeMatchBasisRef} if needed.
     */
    private Pair<AbstractVizResource<?, ?>, DataTime[]> findBasisTimes(
            IDescriptor descriptor) {
        int numberOfFrames = descriptor.getNumberOfFrames();
        AbstractVizResource<?, ?> timeMatchBasis = timeMatchBasisRef.get();
        if (timeMatchBasis == null) {
            Pair<AbstractVizResource<?, ?>, DataTime[]> pair = findNewBasis(
                    descriptor.getResourceList(), numberOfFrames);
            if (pair != null) {
                AbstractVizResource<?, ?> rsc = pair.getLeft();
                configureBasis(rsc);
                if (!timeMatchBasisRef.compareAndSet(null, rsc)) {
                    unconfigureBasis(rsc);
                }
            }
            return pair;
        }

        IDescriptor tmDescriptor = timeMatchBasis.getDescriptor();
        if ((tmDescriptor == descriptor)
                && !validateTimeMatchBasis(descriptor, timeMatchBasis)) {
            changeTimeMatchBasis(null);
            return findBasisTimes(descriptor);
        }
        if (tmDescriptor != null) {
            if (tmDescriptor != descriptor
                    && tmDescriptor.getTimeMatcher() == this
                    && hasContainer(descriptor)) {
                if (validateDescriptor(tmDescriptor)) {
                    redoTimeMatching(tmDescriptor);
                } else {
                    changeTimeMatchBasis(null);
                    return findBasisTimes(descriptor);
                }
            } else if (contained(tmDescriptor, timeMatchBasis) == false) {
                /* Checks to ensure the timeMatchBasis is not "orphaned" */
                timeMatchBasisRef.compareAndSet(timeMatchBasis, null);
                return findBasisTimes(descriptor);
            }
        }
        TimeCache timeCache = getTimeCache(timeMatchBasis);
        DataTime[] times = null;
        synchronized (timeCache) {
            times = timeCache.getLastFrameTimes();
            if ((times == null) || (timeCache.getLastBaseTimes() != null)
                    || (timeCache.getLastFrameCount() != numberOfFrames)) {
                times = makeEmptyLoadList(numberOfFrames, timeMatchBasis);
                timeCache.setTimes(null, times, numberOfFrames);
            }
        }
        if (times != null) {
            return new ImmutablePair<AbstractVizResource<?, ?>, DataTime[]>(
                    timeMatchBasis, times);
        } else {
            /*
             * This might fail if another thread has assigned a basis while this
             * method was looking for one. In this case the current operation
             * will proceed with the wrong basis. Proceed anyway since the basis
             * that was chosen would have been the correct basis when this
             * method began. Whatever operation set the basis should have
             * initiated redoTimeMatching which will run after this and assign
             * times with the new basis.
             */
            timeMatchBasisRef.compareAndSet(timeMatchBasis, null);
            return findBasisTimes(descriptor);
        }
    }

    /**
     * Recursive operation to search for a resource that is able to be the time
     * match basis. Returns the resource and the matched times for that
     * resource. If no resources is able to be the timeMatchBasis then null is
     * returned. This method does not actually change {@link #timeMatchBasisRef}
     * .
     */
    public Pair<AbstractVizResource<?, ?>, DataTime[]> findNewBasis(
            ResourceList resourceList, int numberOfFrames) {
        Iterator<ResourcePair> pairIterator = resourceList.iterator();
        while (pairIterator.hasNext()) {
            ResourcePair pair = pairIterator.next();
            AbstractVizResource<?, ?> rsc = pair.getResource();
            if (rsc == null) {
                continue;
            }
            if (pair.getProperties().isMapLayer()
                    || pair.getProperties().isSystemResource()) {
                continue;
            }

            if (rsc.getResourceData() instanceof IResourceGroup) {
                IResourceGroup group = (IResourceGroup) rsc.getResourceData();
                Pair<AbstractVizResource<?, ?>, DataTime[]> resultPair = findNewBasis(
                        group.getResourceList(), numberOfFrames);

                if (resultPair != null) {
                    return resultPair;
                }
            } else {
                DataTime[] times = makeEmptyLoadList(numberOfFrames, rsc);
                if (times != null) {
                    TimeCache cache = getTimeCache(rsc);
                    synchronized (cache) {
                        cache.setTimes(null, times, numberOfFrames);
                    }
                    return new ImmutablePair<AbstractVizResource<?, ?>, DataTime[]>(
                            rsc, times);
                }
            }
        }
        return null;
    }

    /**
     * Make an empty load list for a resource. Only a time match basis should
     * use this method for getting matched times.
     */
    private DataTime[] makeEmptyLoadList(int numberOfFrames,
            AbstractVizResource<?, ?> rsc) {
        TimeMatchingConfiguration config = getConfiguration(
                rsc.getLoadProperties()).clone();
        if ((config.getDataTimes() == null)
                || (config.getDataTimes().length < 1)) {
            config.setDataTimes(getLatestTimes(rsc, null));
            if ((config.getDataTimes() == null)
                    || (config.getDataTimes().length < 1)) {
                return null;
            }
        }
        populateConfiguration(config);
        DataTime[] timeSteps = TimeMatcher.makeEmptyLoadList(
                config.getDataTimes(), config.getClock(), numberOfFrames,
                config.getLoadMode(), config.getForecast(), config.getDelta());
        if ((timeSteps == null) || (timeSteps.length == 0)) {
            return null;
        }
        return timeSteps;
    }

    /**
     * Remove unused times from a resource and add in any new times.
     */
    private void timeMatchUpdate(AbstractVizResource<?, ?> rsc,
            DataTime[] timeSteps) {

        if (rsc.getResourceData() instanceof IResourceGroup) {
            for (ResourcePair tmp : ((IResourceGroup) rsc.getResourceData())
                    .getResourceList()) {
                AbstractVizResource<?, ?> tmpr = tmp.getResource();
                if (tmpr != null) {
                    timeMatchUpdate(tmpr, null);
                }
            }
        } else {
            pruneUnusedData(rsc, timeSteps);
            updateResourceWithLatest(timeSteps, rsc);
        }
    }

    /**
     * Find the Time Matching Configuration for this resource. If one can't be
     * found, return an empty configuration.
     */
    private TimeMatchingConfiguration getConfiguration(LoadProperties properties) {
        if (properties == null) {
            return new TimeMatchingConfiguration();
        }
        PerspectiveSpecificLoadProperties perspProps = properties
                .getPerspectiveProperty();
        D2DLoadProperties d2dProps = null;
        if (perspProps instanceof D2DLoadProperties) {
            d2dProps = (D2DLoadProperties) perspProps;
            if (d2dProps.getTimeConfig() != null) {
                return d2dProps.getTimeConfig();
            }
        }
        return new TimeMatchingConfiguration();
    }

    /**
     * Thread safe access to {@link #timeCacheMap}. If the map has no entry for
     * this resource then a new {@link TimeCache} is created, added to the map,
     * and returned.
     */
    private TimeCache getTimeCache(AbstractVizResource<?, ?> resource) {
        synchronized (timeCacheMap) {
            TimeCache cache = timeCacheMap.get(resource);
            if (cache == null) {
                cache = new TimeCache();
                timeCacheMap.put(resource, cache);
            }
            return cache;
        }
    }

    /**
     * Populates all unset fields of the configuration with defaults except
     * dataTimes, dataTimes should be set before calling this function.
     */
    private void populateConfiguration(TimeMatchingConfiguration config) {
        Arrays.sort(config.getDataTimes());
        if (config.getClock() == null) {
            if (SimulatedTime.getSystemTime().isRealTime()
                    && (config.getDataTimes() != null)
                    && (config.getDataTimes().length != 0)) {
                config.setClock(config.getDataTimes()[config.getDataTimes().length - 1]
                        .getValidTime().getTime());
            } else {
                config.setClock(SimulatedTime.getSystemTime().getTime());
            }
        }
        if (config.getLoadMode() == null) {
            config.setLoadMode(loadMode);
        }
        if (config.getForecast() == null) {
            config.setForecast(getForecastFilter());
        }
        if (config.getDelta() == null) {
            config.setDelta(getDeltaFilter());
        }
        if (config.getTolerance() == null) {
            config.setTolerance(TimeMatcher.DEFAULT_TOLERANCE_FACTOR);
        }
    }

    /**
     * Retrieves the latest times from a resource
     * 
     * If the resource data is an {@link AbstractRequestableResourceData} then
     * this will query for the available times.
     * 
     */
    protected DataTime[] getLatestTimes(AbstractVizResource<?, ?> rsc,
            DataTime[] timeSteps) {
        DataTime[] availableTimes = null;

        /*
         * If resource is handling requests itself, just return the datatimes
         * listed in the resource
         */
        AbstractResourceData resourceData = rsc.getResourceData();
        if (resourceData instanceof AbstractRequestableResourceData) {
            AbstractRequestableResourceData req = (AbstractRequestableResourceData) resourceData;
            if (req.isRequeryNecessaryOnTimeMatch()
                    || (rsc.getDataTimes() == null)
                    || (rsc.getDataTimes().length == 0)) {
                try {
                    availableTimes = req.getAvailableTimes();
                } catch (VizException e) {
                    statusHandler
                            .error("Unable to query times for "
                                    + rsc.getSafeName(), e);
                }
            }
        }

        if (availableTimes == null) {
            availableTimes = rsc.getMatchedDataTimes(timeSteps);
            Arrays.sort(availableTimes);
        }

        return availableTimes;
    }

    /**
     * Prunes data that is no longer used by calling
     * {@link AbstractVizResource#remove(DataTime)}
     * 
     * @param rsc
     *            the resource to prune
     * @param times
     *            the times that are still used and therefore should NOT be
     *            pruned
     */
    protected void pruneUnusedData(
            AbstractVizResource<? extends AbstractResourceData, ? extends IDescriptor> rsc,
            DataTime[] times) {
        if (times == null) {
            return;
        }
        DataTime[] rscTimes = rsc.getDataTimes();
        AbstractResourceData resourceData = rsc.getResourceData();

        for (DataTime dt : rscTimes) {
            if (dt == null) {
                continue;
            }
            boolean found = false;
            for (DataTime t2 : times) {
                if (dt.equals(t2)) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                if (resourceData != null) {
                    /*
                     * If resource has resource data, let the resource data
                     * handle the removal
                     */
                    resourceData
                            .fireChangeListeners(ChangeType.DATA_REMOVE, dt);
                } else {
                    // otherwise just remove it
                    rsc.remove(dt);
                }
            }
        }
    }

    /**
     * Takes a list of data times that are available for a resource and requests
     * the products that are missing
     * 
     * @param dataTimes
     *            available dataTimes
     * @param resource
     *            the resource to update
     */
    private void updateResourceWithLatest(DataTime[] dataTimes,
            AbstractVizResource<?, ?> resource) {
        Validate.notNull(resource, "Resource must not be null");
        if (resource.getResourceData() == null) {
            return;
        }

        if (resource.getResourceData() instanceof AbstractRequestableResourceData) {
            AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) resource
                    .getResourceData();

            DataTime[] dt = resource.getDataTimes();
            /*
             * Passing in the comparator allows it to handle null times. Ideally
             * there should be no null times but if there is this is not the
             * place to break things.
             */
            Arrays.sort(dt, new DataTimeComparator());
            try {
                PluginDataObject[] pdo = arrd.getLatestPluginDataObjects(
                        dataTimes, dt);
                if (pdo.length > 0) {
                    resource.getResourceData().update(pdo);
                }
            } catch (VizException e) {
                statusHandler.error(
                        "Unable to update data for " + resource.getSafeName(),
                        e);
            }
        }

    }

    @Override
    public void handleRemove(AbstractVizResource<?, ?> resource,
            IDescriptor descriptor) {
        timeMatchBasisRef.compareAndSet(resource, null);
        synchronized (timeCacheMap) {
            timeCacheMap.remove(resource);
        }
    }

    public LoadMode getLoadMode() {
        return loadMode;
    }

    public void setLoadMode(LoadMode loadMode) {
        this.loadMode = loadMode;
    }

    /**
     * Attempt to match the provided availableTimes against the current state of
     * the time matcher. This can be used while loading a resource to determine
     * if any matching times are possible and for preloading any data for those
     * times.
     * 
     * This method is also responsible for using the
     * AbstractTimeMatchingConfigurationFactory for this matcher to configure
     * the time match settings. The final times the resource displays may change
     * if other resources are being loaded concurrently. When a resource is done
     * loading {@link #redoTimeMatching(IDescriptor)} must be used to properly
     * add the resource to the {@link FramesInfo} of the {@link IDescriptor}.
     */
    @Override
    public DataTime[] initialLoad(LoadProperties loadProps,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        DataTime[] dataTimesToLoad = null;
        TimeMatchingConfiguration config = null;
        AbstractVizResource<?, ?> timeMatchBasis = timeMatchBasisRef.get();
        if (timeMatchBasis == null) {
            config = configFactory.getConfiguration(loadProps, this,
                    availableTimes, descriptor);
            if ((config == null) || config.isCancel()) {
                return dataTimesToLoad;
            }
            config = config.clone();
            if ((config.getDataTimes() == null)
                    || (config.getDataTimes().length < 1)) {
                config.setDataTimes(availableTimes);
            }
            populateConfiguration(config);
            dataTimesToLoad = TimeMatcher.makeEmptyLoadList(
                    config.getDataTimes(), config.getClock(),
                    descriptor.getNumberOfFrames(), config.getLoadMode(),
                    config.getForecast(), config.getDelta());
        } else {
            config = configFactory.getOverlayConfiguration(loadProps, this,
                    availableTimes, descriptor);
            if ((config == null) || config.isCancel()) {
                return dataTimesToLoad;
            }
            config = config.clone();
            if ((config.getDataTimes() == null)
                    || (config.getDataTimes().length < 1)) {
                config.setDataTimes(availableTimes);
            }
            populateConfiguration(config);
            DataTime[] existingDataTimes = getFrameTimes(descriptor,
                    timeMatchBasis, descriptor.getFramesInfo(), descriptor
                            .getFramesInfo().getTimeMap());

            TimeMatcher tm = new TimeMatcher();
            dataTimesToLoad = tm.makeOverlayList(config.getDataTimes(),
                    config.getClock(), existingDataTimes, config.getLoadMode(),
                    config.getForecast(), config.getDelta(),
                    config.getTolerance());

            if ((timeMatchBasis.getDescriptor() != null)
                    && (timeMatchBasis.getDescriptor() != descriptor)) {
                /* Still use my times, but the index from the time match basis */
                FramesInfo myFi = descriptor.getFramesInfo();
                FramesInfo tmFi = timeMatchBasis.getDescriptor()
                        .getFramesInfo();
                descriptor.setFramesInfo(new FramesInfo(myFi.getFrameTimes(),
                        tmFi.getFrameIndex()));
            }
        }

        return dataTimesToLoad;
    }

    /**
     * Changes the time match basis for the time matcher to be the specified
     * resource
     */
    public void changeTimeMatchBasis(AbstractVizResource<?, ?> resource) {
        if (resource != null) {
            configureBasis(resource);
        }
        AbstractVizResource<?, ?> prev = timeMatchBasisRef.getAndSet(resource);
        if (prev != null) {
            unconfigureBasis(prev);
        }
    }

    /**
     * Used to setup the {@link TimeMatchingConfiguration} and
     * {@link #timeMatchBasisDisposeListener} for a new timeMatchBasis. This
     * should be used before modifying the {@link TimeMatchBasisRef}.
     */
    private void configureBasis(AbstractVizResource<?, ?> newBasis) {
        TimeMatchingConfiguration config = getConfiguration(newBasis
                .getLoadProperties());
        config.setTimeMatchBasis(true);
        TimeCache timeCache = getTimeCache(newBasis);
        synchronized (timeCache) {
            timeCache.setTimes(null, null);
        }
        newBasis.registerListener(timeMatchBasisDisposeListener);
    }

    /**
     * Used to revert the {@link TimeMatchingConfiguration} and
     * {@link #timeMatchBasisDisposeListener} when a timeMatchBasis is removed.
     * This should be used after modifying the {@link TimeMatchBasisRef}.
     */
    private void unconfigureBasis(AbstractVizResource<?, ?> oldBasis) {
        TimeMatchingConfiguration config = getConfiguration(oldBasis
                .getLoadProperties());
        config.setTimeMatchBasis(false);
        TimeCache timeCache = getTimeCache(oldBasis);
        synchronized (timeCache) {
            timeCache.setTimes(null, null);
        }
        oldBasis.unregisterListener(timeMatchBasisDisposeListener);
    }

    public AbstractVizResource<?, ?> getTimeMatchBasis() {
        return timeMatchBasisRef.get();
    }

    public boolean hasTimeMatchBasis() {
        return (timeMatchBasisRef.get() != null);
    }

    public Date getClockFilter() {
        return clockFilter;
    }

    public long getForecastFilter() {
        return forecastFilter;
    }

    public long getDeltaFilter() {
        return deltaFilter;
    }

    public boolean isTimeOptionsSelected() {
        return isTimeOptionsSelected;
    }

    public void setTimeOptionsSelected(boolean isTimeOptionsSelected) {
        this.isTimeOptionsSelected = isTimeOptionsSelected;
    }

    public AbstractTimeMatchingConfigurationFactory getTimeMatchingConfigurationFactory() {
        return configFactory;
    }

    @Override
    public List<AbstractRenderableDisplay> getDisplayLoadOrder(
            List<AbstractRenderableDisplay> displays) {
        /* if any of the displays have a set time match basis then load it first */
        AbstractRenderableDisplay basisDisplay = null;
        for (AbstractRenderableDisplay display : displays) {
            if (getBasisResourcePair(display.getDescriptor().getResourceList()) != null) {
                basisDisplay = display;
                break;
            }
        }
        if (basisDisplay != null) {
            List<AbstractRenderableDisplay> results = new ArrayList<AbstractRenderableDisplay>(
                    displays);
            results.remove(basisDisplay);
            results.add(0, basisDisplay);
            return results;
        }
        return super.getDisplayLoadOrder(displays);
    }

    @Override
    public List<ResourcePair> getResourceLoadOrder(List<ResourcePair> resources) {
        /*
         * if any of the resources are set as the time match basis then load it
         * first
         */
        ResourcePair basisPair = getBasisResourcePair(resources);
        if (basisPair != null) {
            List<ResourcePair> results = new ArrayList<ResourcePair>(resources);
            results.remove(basisPair);
            results.add(0, basisPair);
            return results;
        }
        return super.getResourceLoadOrder(resources);
    }

    private ResourcePair getBasisResourcePair(List<ResourcePair> resources) {
        for (ResourcePair pair : resources) {
            if (getConfiguration(pair.getLoadProperties()).isTimeMatchBasis()) {
                return pair;
            }
            if (pair.getResourceData() instanceof IResourceGroup) {
                ResourcePair testPair = getBasisResourcePair(((IResourceGroup) pair
                        .getResourceData()).getResourceList());
                if (testPair != null) {
                    return pair;
                }
            }
        }
        return null;
    }

    @Override
    public void copyFrom(AbstractTimeMatcher timeMatcher) {
        if (timeMatcher instanceof D2DTimeMatcher) {
            D2DTimeMatcher d2d = (D2DTimeMatcher) timeMatcher;
            if (timeMatchBasisRef.get() == null) {
                this.clockFilter = d2d.clockFilter;
                this.forecastFilter = d2d.forecastFilter;
                this.deltaFilter = d2d.deltaFilter;
                this.loadMode = d2d.loadMode;
            }
        }
        resetMultiload();
    }

    @Override
    public void resetMultiload() {
        configFactory.resetMultiload();
    }

    private static boolean validateTimeMatchBasis(IDescriptor descriptor,
            AbstractVizResource<?, ?> timeMatchBasis) {
        /*
         * If a resource is shared by multiple panels (this can be the case with
         * tools, at least), then it is necessary to search all of them as
         * resource.descriptor() may not contain resource. TODO: Don't allow
         * this condition to occur?
         */
        IRenderableDisplay display = descriptor.getRenderableDisplay();
        IDisplayPaneContainer container = display != null ? display
                .getContainer() : null;
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                IRenderableDisplay paneDisplay = pane.getRenderableDisplay();
                IDescriptor paneDescriptor = paneDisplay != null ? paneDisplay
                        .getDescriptor() : null;
                if ((paneDescriptor != null)
                        && validateTimeMatchBasis(
                                paneDescriptor.getResourceList(),
                                timeMatchBasis)) {
                    return true;
                }
            }
        } else {
            return validateTimeMatchBasis(descriptor.getResourceList(),
                    timeMatchBasis);
        }
        return false;
    }

    private static boolean validateTimeMatchBasis(ResourceList list,
            AbstractVizResource<?, ?> timeMatchBasis) {
        for (ResourcePair rp : list) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc == timeMatchBasis) {
                return true;
            } else if (rp.getProperties().isMapLayer()
                    || rp.getProperties().isSystemResource()) {
                continue;
            } else if ((rsc != null)
                    && (rsc.getResourceData() instanceof IResourceGroup)) {
                IResourceGroup group = (IResourceGroup) rsc.getResourceData();
                if (validateTimeMatchBasis(group.getResourceList(),
                        timeMatchBasis)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean hasContainer(IDescriptor descriptor) {
        IRenderableDisplay display = descriptor.getRenderableDisplay();
        return display.getContainer() != null;
    }

    private static boolean validateDescriptor(IDescriptor descriptor) {
        IRenderableDisplay display = descriptor.getRenderableDisplay();
        IDisplayPaneContainer container = display != null ? display
                .getContainer() : null;
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                IRenderableDisplay paneDisplay = pane.getRenderableDisplay();
                IDescriptor paneDescriptor = paneDisplay != null ? paneDisplay
                        .getDescriptor() : null;
                if (paneDescriptor == descriptor) {
                    return true;
                }
            }
        }
        return false;
    }

}
