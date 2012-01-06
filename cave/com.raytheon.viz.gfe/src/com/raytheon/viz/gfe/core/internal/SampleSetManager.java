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
package com.raytheon.viz.gfe.core.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.core.msgs.ISampleSetChangedListener;
import com.raytheon.viz.gfe.edittool.GridID;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A manager class for SampleSet, which contain SampleData
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 14, 2008	879			rbell	    Initial creation
 * 11Jun2008    #1193       ebabin      Updates for toggling lat/lon for sample set.
 * Apr 9, 2009  1288        rjpeter     Added ISampleSetChangedListener handling.
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

public class SampleSetManager implements ISampleSetManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SampleSetManager.class);

    private static final String SAMPLE_SETS_DIR = FileUtil.join("gfe",
            "sampleSets");

    private static final String MARKER_NAME = "ISC_Marker_Set";

    private SampleId loadedSet;

    private Set<SampleId> inventory;

    private ArrayList<Coordinate> locations;

    private Map<String, List<Coordinate>> markerLocations;

    private final DataManager dataManager;

    private final Set<ISampleSetChangedListener> sampleSetChangedListeners;

    private boolean showLatLon;

    /**
     * Default constructor.
     * 
     * Gets the initial sample inventory.
     */
    public SampleSetManager(DataManager dataManager) {
        this.dataManager = dataManager;
        this.loadedSet = new SampleId();

        this.inventory = new HashSet<SampleId>();

        this.locations = new ArrayList<Coordinate>();

        this.markerLocations = new HashMap<String, List<Coordinate>>();

        this.sampleSetChangedListeners = new HashSet<ISampleSetChangedListener>();

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationFile[] files = pm.listStaticFiles(SAMPLE_SETS_DIR,
                new String[] { ".xml" }, true, true);

        for (LocalizationFile file : files) {
            String fn = LocalizationUtil.extractName(file.getName()).replace(
                    ".xml", "");
            this.inventory.add(new SampleId(fn));
        }

        // load default sample points
        String[] sampleSets = Activator.getDefault().getPreferenceStore()
                .getStringArray("DefaultSamples");
        if (sampleSets != null) {
            for (String id : sampleSets) {
                SampleId sid = new SampleId(id);
                if (this.inventory.contains(sid)) {
                    try {
                        loadSampleSet(sid, SampleSetLoadMode.ADD);
                    } catch (GFEException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);

                    }
                }
            }
        }

        // get initial marker points
        try {
            getMarkerPoints();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get initial sampleset marker points", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#sampleSetLocations(java.lang
     * .String)
     */
    public List<Coordinate> sampleSetLocations(final String setName)
            throws GFEException {
        // verify set in inventory
        boolean found = false;
        for (SampleId thisId : this.inventory) {
            if (setName.equals(thisId.getName())) {
                found = true;
                break;
            }
        }

        if (!found) {
            throw new GFEException(
                    "Attempt to get locations for unknown sample set ["
                            + setName + "]");
        }

        String fileName = FileUtil.join(SAMPLE_SETS_DIR, setName + ".xml");

        LocalizationFile file = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(fileName);

        File f = null;
        try {
            f = file.getFile(true);
        } catch (LocalizationException e) {
            if (f == null) {
                throw new GFEException(
                        "An error occurred retrieving SampleSet: " + fileName,
                        e);
            }
        }

        Object obj = null;
        try {
            obj = SerializationUtil.jaxbUnmarshalFromXmlFile(f
                    .getAbsolutePath());
        } catch (Exception e) {
            throw new GFEException("Unable to load sampledata: " + f, e);
        }
        if ((obj == null) || !(obj instanceof SampleData)) {
            throw new GFEException("Unable to load sampledata: " + f
                    + " unmarshalled as " + obj);
        }

        SampleData sampleData = (SampleData) obj;

        return sampleData.getPoints();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#loadSampleSet(com.raytheon
     * .edex.plugin.gfe.sample.SampleId,
     * com.raytheon.viz.gfe.core.internal.SampleSetManager.SampleSetLoadMode)
     */
    public void loadSampleSet(final SampleId sampleId,
            SampleSetLoadMode loadMode) throws GFEException {
        File f = PathManagerFactory.getPathManager().getStaticFile(
                FileUtil.join(SAMPLE_SETS_DIR, sampleId.getName() + ".xml"));

        Object obj = null;
        try {
            obj = SerializationUtil.jaxbUnmarshalFromXmlFile(f.getPath());
        } catch (Exception e) {
            throw new GFEException("Unable to load sampledata: " + f);
        }
        if ((obj == null) || !(obj instanceof SampleData)) {
            throw new GFEException("Unable to load sampledata: " + f
                    + " unmarshalled as " + obj);
        }

        SampleData sampleData = (SampleData) obj;

        // set the loadedSet flag appropriately
        if ((loadMode == SampleSetLoadMode.REPLACE)
                || ((loadMode == SampleSetLoadMode.ADD) && (this.locations
                        .size() == 0))) {
            this.loadedSet = sampleData.getSampleId();
        } else {
            this.loadedSet = new SampleId();
        }

        // merge in the sample set based on the current set and the load mode
        mergeSamples(sampleData.getPoints(), loadMode);

        fireSampleSetChangedListeners();

        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#clearSamples()
     */
    public void clearSamples() {
        mergeSamples(new ArrayList<Coordinate>(), SampleSetLoadMode.REPLACE);
        this.loadedSet = new SampleId(); // no loaded set
        fireSampleSetChangedListeners();
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISampleSetManager#addAnchoredSample(com.
     * vividsolutions.jts.geom.Coordinate)
     */
    public void addAnchoredSample(final Coordinate sampleLocation) {
        mergeSamples(Arrays.asList(sampleLocation), SampleSetLoadMode.ADD);
        this.loadedSet = new SampleId(); // no longer a loaded set
        fireSampleSetChangedListeners();
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#removeAnchoredSample(com.
     * vividsolutions.jts.geom.Coordinate)
     */
    public void removeAnchoredSample(final Coordinate sampleLocation) {
        removeAnchoredSample(sampleLocation,
                ISampleSetManager.DEFAULT_THRESHOLD);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#removeAnchoredSample(com.
     * vividsolutions.jts.geom.Coordinate, float)
     */
    public void removeAnchoredSample(final Coordinate sampleLocation,
            float threshold) {
        mergeSamples(Arrays.asList(sampleLocation), SampleSetLoadMode.REMOVE,
                threshold);
        this.loadedSet = new SampleId(); // no longer a loaded set
        fireSampleSetChangedListeners();
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.ISampleSetManager#addAnchoredMarker(com.
     * vividsolutions.jts.geom.Coordinate)
     */
    public void addAnchoredMarker(final Coordinate location, final GridID gid) {
        String set = activeMarkerSet(gid);
        List<Coordinate> locations = markerLocations.get(set);
        if (locations == null) {
            locations = new ArrayList<Coordinate>();
        }
        locations.add(location);
        markerLocations.put(set, locations);
        saveSampleSet(locations, new SampleId(set));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#removeAnchoredMarker(com.
     * vividsolutions.jts.geom.Coordinate)
     */
    public void removeAnchoredMarker(final Coordinate location, final GridID gid) {
        removeAnchoredMarker(location, gid, ISampleSetManager.DEFAULT_THRESHOLD);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#removeAnchoredMarker(com.
     * vividsolutions.jts.geom.Coordinate, float)
     */
    public void removeAnchoredMarker(final Coordinate location,
            final GridID gid, float threshold) {
        boolean found = false;
        String set = activeMarkerSet(gid);
        GeodeticCalculator gdc = new GeodeticCalculator();
        gdc.setStartingGeographicPoint(location.x, location.y);
        ArrayList<Coordinate> toRemove = new ArrayList<Coordinate>();
        for (Coordinate markerLoc : this.markerLocations.get(set)) {
            gdc.setDestinationGeographicPoint(markerLoc.x, markerLoc.y);
            float d = (float) gdc.getOrthodromicDistance();
            if (d <= threshold) {
                toRemove.add(markerLoc);
                found = true;
                break;
            }
        }

        if (found) {
            this.markerLocations.get(set).removeAll(toRemove);
            saveSampleSet(this.markerLocations.get(set), new SampleId(set));
        }
        fireSampleSetChangedListeners();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#anchoredMarkerAtLocation(
     * com.vividsolutions.jts.geom.Coordinate, float)
     */
    public boolean anchoredMarkerAtLocation(final Coordinate location,
            final GridID gid, float threshold) {
        String set = activeMarkerSet(gid);
        if (this.markerLocations.containsKey(set)) {
            GeodeticCalculator gdc = new GeodeticCalculator();
            gdc.setStartingGeographicPoint(location.x, location.y);

            for (Coordinate markerLoc : this.markerLocations.get(set)) {
                gdc.setDestinationGeographicPoint(markerLoc.x, markerLoc.y);
                float d = (float) gdc.getOrthodromicDistance();
                if (d <= threshold) {
                    // fireSampleSetChangedListeners();
                    return true;
                }
            }
            // fireSampleSetChangedListeners();
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#saveSampleSet(com.vividsolutions
     * .jts.geom.Coordinate[], com.raytheon.edex.plugin.gfe.sample.SampleId)
     */
    public boolean saveSampleSet(final List<Coordinate> sampleLocations,
            final SampleId sampleId) {

        SampleData sd = new SampleData(sampleId, sampleLocations);

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);

        LocalizationFile file = pm.getLocalizationFile(lc,
                FileUtil.join(SAMPLE_SETS_DIR, sampleId.getName() + ".xml"));

        try {
            SerializationUtil
                    .jaxbMarshalToXmlFile(sd, file.getFile().getPath());
            file.save();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving to localization server", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error serializing to file",
                    e);
        }

        this.inventory.add(sampleId);

        // // send a notification to interested users
        // networkNotification(notification.inventory(),
        // notification.additions(),
        // notification.deletions(), notification.changes());

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#saveActiveSampleSet(com.raytheon
     * .edex.plugin.gfe.sample.SampleId)
     */
    public boolean saveActiveSampleSet(final SampleId sampleId) {
        this.loadedSet = sampleId;
        return saveSampleSet(this.locations, sampleId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#deleteSampleSet(com.raytheon
     * .edex.plugin.gfe.sample.SampleId)
     */
    public boolean deleteSampleSet(final SampleId sampleId) {
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
                        FileUtil.join(SAMPLE_SETS_DIR, sampleId.getName()
                                + ".xml"));

        LocalizationContext context = file.getContext();
        if (context.getLocalizationLevel() != LocalizationLevel.USER) {
            statusHandler.handle(Priority.PROBLEM, "Unable to delete "
                    + sampleId.getName()
                    + ", because it is not a sampleset owned by you.");
            return false;
        }

        try {
            file.delete();
            this.inventory.remove(sampleId);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting from localization server", e);
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISampleSetManager#networkNotification(com.raytheon
     * .edex.plugin.gfe.sample.SampleId[],
     * com.raytheon.edex.plugin.gfe.sample.SampleId[],
     * com.raytheon.edex.plugin.gfe.sample.SampleId[],
     * com.raytheon.edex.plugin.gfe.sample.SampleId[])
     */
    public void networkNotification(final SampleId[] anInventory,
            final SampleId[] additions, final SampleId[] deletions,
            final SampleId[] changes) throws VizException {

        // logDebug << "NetworkNotification newInv=" << anInventory
        // << " add=" << additions << " del=" << deletions << " chg="
        // << changes << std::endl;
        // logDebug << "OldInventory=" << anInventory << std::endl;
        // logDebug << "Active loaded set=" << _loadedSet << std::endl;

        // store the new inventory
        this.inventory = new HashSet<SampleId>(Arrays.asList(anInventory));

        // loaded sample set changed?, check by name field for a match
        for (SampleId changesId : changes) {
            if (changesId.getName().equals(this.loadedSet.getName())) {
                // logDebug << "LoadedSampleSet changed " << _loadedSet <<
                // std::endl;
                loadSampleSet(changesId, SampleSetLoadMode.REPLACE);
            }
            if (changesId.getName().equals(SampleSetManager.MARKER_NAME)) {
                // logDebug << "MarkerSet changed " << changesId << std::endl;
                getMarkerPoints();
            }
        }

        // loaded sample set deleted?
        for (SampleId deletionsId : deletions) {
            if (deletionsId.getName().equals(this.loadedSet.getName())) {
                // logDebug << "LoadedSampleSet deleted " << _loadedSet
                // << ' ' << deletionsId << std::endl;
                this.loadedSet = new SampleId();
            }
            if (deletionsId.getName().equals(SampleSetManager.MARKER_NAME)) {
                // logDebug << "MarkerSet deleted " << deletionsId << std::endl;
                getMarkerPoints();
            }
        }

        // loaded sample set "added", may simply be a rename
        for (SampleId additionsId : additions) {
            if (additionsId.getName().equals(this.loadedSet.getName())) {
                // logDebug << "LoadedSampleSet added " << _loadedSet
                // << " " << additionsId << std::endl;
                loadSampleSet(additionsId, SampleSetLoadMode.REPLACE);
            }
            if (additionsId.getName().equals(SampleSetManager.MARKER_NAME)) {
                // logDebug << "MarkerSet added " << additionsId << std::endl;
                getMarkerPoints();
            }
        }

        // inventory changed?
        if ((additions.length > 0) || (deletions.length > 0)
                || (changes.length > 0)) {
        }
        // logDebug << "Active set is now: " << _loadedSet << std::endl;
        fireSampleSetChangedListeners();
    }

    /**
     * mergeSamples(...) with default threshold
     * 
     * @param mergeSet
     * @param mergeMode
     */
    private void mergeSamples(final List<Coordinate> mergeSet,
            SampleSetLoadMode mergeMode) {
        mergeSamples(mergeSet, mergeMode, ISampleSetManager.DEFAULT_THRESHOLD);
    }

    /**
     * The mergeSet is merged with _locations. The merge mode can be ADD,
     * REMOVE, or REPLACE. The threshold is specified for the REMOVE mode to
     * perform a search.
     * 
     * First the _locations list is copied to a temporary variable. If the merge
     * mode is replace, then _locations is set to mergeSet, deletions is set to
     * the temporary variable, and additions is set to mergeSet. If the merge
     * mode is add, then we set additions to mergeSet, deletions to length 0,
     * and append mergeSet to _locations. If the merge mode is remove, we find
     * all entries within the threshold and remove them from _locations and add
     * it to deletions. Calls sendSampleSetChangedNotification(_locations,
     * deletions, additions).
     * 
     * Threshold is ignored except for REMOVE mode.
     * 
     * @param mergeSet
     * @param mergeMode
     * @param threshold
     */
    private void mergeSamples(final List<Coordinate> mergeSet,
            SampleSetLoadMode mergeMode, float threshold) {

        switch (mergeMode) {
        case ADD:
            this.locations.addAll(mergeSet);
            break;
        case REMOVE:
            GeodeticCalculator gdc = new GeodeticCalculator();
            for (Coordinate mergeCoord : mergeSet) {
                gdc.setStartingGeographicPoint(mergeCoord.x, mergeCoord.y);
                Iterator<Coordinate> locationIterator = this.locations
                        .iterator();
                while (locationIterator.hasNext()) {
                    Coordinate locationsCoord = locationIterator.next();
                    gdc.setDestinationGeographicPoint(locationsCoord.x,
                            locationsCoord.y);
                    float d = (float) gdc.getOrthodromicDistance();
                    if (d <= threshold) {
                        locationIterator.remove();
                    }
                }
            }
            break;
        case REPLACE:
            this.locations = new ArrayList<Coordinate>(mergeSet);
            break;
        }
    }

    /**
     * Keeps the _markerLocations up-to-date.
     * 
     * Gets the set of points, sends out notification of changes.
     */
    private void getMarkerPoints() throws VizException {
        // ensure it is in the inventory
        this.markerLocations.clear();

        for (Iterator<SampleId> it = this.inventory.iterator(); it.hasNext();) {
            SampleId id = it.next();
            if (id.getName().startsWith(MARKER_NAME)) {
                markerLocations.put(id.getName(),
                        this.sampleSetLocations(id.getName()));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getLoadedSet()
     */
    public SampleId getLoadedSet() {
        return this.loadedSet;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getInventory()
     */
    public SampleId[] getInventory() {
        return this.inventory.toArray(new SampleId[this.inventory.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getInventoryAsList()
     */
    public ArrayList<SampleId> getInventoryAsList() {
        ArrayList<SampleId> ids = new ArrayList<SampleId>();
        for (SampleId id : this.inventory) {
            ids.add(id);
        }
        return ids;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getInventoryAsStrings()
     */
    public String[] getInventoryAsStrings() {
        String[] retVal = new String[this.inventory.size()];
        int i = 0;
        for (SampleId id : this.inventory) {
            retVal[i] = id.getName();
            i++;
        }

        return retVal;
    }

    public String activeMarkerSet(GridID gid) {
        // get office type from GridID
        String ot = gid.getParm().getOfficeType();
        if ((ot != null) && (ot.length() > 0)) {
            return "ISC_Marker_Set_" + ot;
        } else {
            return "";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getLocations()
     */
    public List<Coordinate> getLocations() {
        return new ArrayList<Coordinate>(this.locations);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#getMarkerLocations()
     */
    public Map<String, List<Coordinate>> getMarkerLocations() {
        return this.markerLocations;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#isShowLatLon()
     */
    public boolean isShowLatLon() {
        return showLatLon;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ISampleSetManager#setShowLatLon(boolean)
     */
    public void setShowLatLon(boolean showLatLon) {
        this.showLatLon = showLatLon;
        fireSampleSetChangedListeners();

    }

    public void loadSampleSet(final SampleId sampleId, String loadMode)
            throws GFEException {
        loadSampleSet(sampleId, SampleSetLoadMode.valueOf(loadMode));
    }

    @Override
    public void addSampleSetChangedListener(ISampleSetChangedListener listener) {
        sampleSetChangedListeners.add(listener);
    }

    @Override
    public void removeSampleSetChangedListener(
            ISampleSetChangedListener listener) {
        sampleSetChangedListeners.remove(listener);
    }

    /**
     * Fires the sample set changed listeners.
     */
    private void fireSampleSetChangedListeners() {
        for (ISampleSetChangedListener listener : sampleSetChangedListeners) {
            listener.sampleSetChanged(this);
        }
    }
}
