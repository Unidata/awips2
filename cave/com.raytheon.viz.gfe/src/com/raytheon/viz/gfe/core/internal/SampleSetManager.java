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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.CopyOnWriteArraySet;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.gfe.ifpclient.IFPClient;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.core.msgs.ISampleSetChangedListener;
import com.raytheon.viz.gfe.edittool.GridID;
import org.locationtech.jts.geom.Coordinate;

/**
 * A manager class for SampleSet, which contain SampleData
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 14, 2008  879      rbell     Initial creation
 * Jun 11, 2008  1193     ebabin    Updates for toggling lat/lon for sample set.
 * Apr 09, 2009  1288     rjpeter   Added ISampleSetChangedListener handling.
 * Aug 06, 2013  1561     njensen   Use pm.listFiles() instead of
 *                                  pm.listStaticFiles()
 * Sep 30, 2013  2361     njensen   Use JAXBManager for XML
 * Sep 08, 2104  3592     randerso  Changed to use new pm listStaticFiles().
 *                                  Reworked inventory to use a map to better
 *                                  handle files at multiple localization levels
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Nov 19, 2015  5129     dgilling  Support new IFPClient.
 * Feb 10, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Jun 14, 2017  6297     bsteffen  Make listeners thread safe.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author rbell
 */
public class SampleSetManager
        implements ISampleSetManager, ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SampleSetManager.class);

    public static final String SAMPLE_SETS_DIR = "gfe" + IPathManager.SEPARATOR
            + "sampleSets";

    private static final String MARKER_NAME = "ISC_Marker_Set";

    private final IFPClient ifpClient;

    private SampleId loadedSet;

    private SortedMap<String, SampleId> inventory;

    private ArrayList<Coordinate> locations;

    private Map<String, List<Coordinate>> markerLocations;

    private final Set<ISampleSetChangedListener> sampleSetChangedListeners;

    private boolean showLatLon;

    private IPathManager pathManager;

    private LocalizationFile sampleSetDir;

    /**
     * Default constructor.
     *
     * Gets the initial sample inventory.
     * 
     * @param ifpClient
     */
    public SampleSetManager(IFPClient ifpClient) {
        this.loadedSet = new SampleId();

        this.locations = new ArrayList<>();

        this.markerLocations = new HashMap<>();

        this.sampleSetChangedListeners = new CopyOnWriteArraySet<>();

        this.ifpClient = ifpClient;

        this.inventory = new TreeMap<>();
        ServerResponse<List<SampleId>> sr = this.ifpClient.getSampleInventory();
        if (sr.isOkay()) {
            for (SampleId id : sr.getPayload()) {
                this.inventory.put(id.getName(), id);
            }
        } else {
            statusHandler.error(String.format(
                    "Unable to get sample inventory from IFPServer: %s",
                    sr.message()));
        }

        this.pathManager = PathManagerFactory.getPathManager();
        this.sampleSetDir = this.pathManager.getLocalizationFile(
                this.pathManager.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE),
                SAMPLE_SETS_DIR);
        this.sampleSetDir.addFileUpdatedObserver(this);

        // load default sample points
        String[] sampleSets = GFEPreference.getStringArray("DefaultSamples");
        if (sampleSets != null) {
            for (String name : sampleSets) {
                SampleId sid = inventory.get(name);
                if (sid != null) {
                    loadSampleSet(sid, SampleSetLoadMode.ADD);
                }
            }
        }

        getMarkerPoints();
    }

    @Override
    public void dispose() {
        this.sampleSetDir.removeFileUpdatedObserver(this);
    }

    @Override
    public List<Coordinate> sampleSetLocations(final String setName) {
        // verify set in inventory
        SampleId sampleId = this.inventory.get(setName);

        if (sampleId == null) {
            statusHandler
                    .error("Attempt to get locations for unknown sample set ["
                            + setName + "]");
            return Collections.emptyList();
        }

        String fileName = FileUtil.join(SAMPLE_SETS_DIR,
                sampleId.getName() + ".xml");

        ILocalizationFile lf = pathManager.getLocalizationFile(
                pathManager.getContext(LocalizationType.COMMON_STATIC,
                        sampleId.getAccess()),
                fileName);

        List<Coordinate> points = Collections.emptyList();
        try (InputStream inStream = lf.openInputStream()) {
            SampleData sampleData = SampleData.getJAXBManager()
                    .unmarshalFromInputStream(inStream);
            points = sampleData.getPoints();
        } catch (IOException | LocalizationException
                | SerializationException e) {
            statusHandler.error("Unable to load sampledata: " + lf, e);
        }

        return points;
    }

    @Override
    public void loadSampleSet(final SampleId sampleId,
            SampleSetLoadMode loadMode) {
        ServerResponse<SampleData> sr = ifpClient.getSampleData(sampleId);
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Failure to get sample data from from IFPServer for [%s]: %s",
                    sampleId.getName(), sr.message()));
            return;
        }
        SampleData sampleData = sr.getPayload();

        // set the loadedSet flag appropriately
        if ((loadMode == SampleSetLoadMode.REPLACE)
                || ((loadMode == SampleSetLoadMode.ADD)
                        && (this.locations.isEmpty()))) {
            this.loadedSet = sampleData.getSampleId();
        } else {
            this.loadedSet = new SampleId();
        }

        // merge in the sample set based on the current set and the load mode
        mergeSamples(sampleData.getPoints(), loadMode);

        fireSampleSetChangedListeners();

        return;
    }

    @Override
    public void clearSamples() {
        mergeSamples(new ArrayList<Coordinate>(), SampleSetLoadMode.REPLACE);
        // no loaded set
        this.loadedSet = new SampleId();
        fireSampleSetChangedListeners();
        return;
    }

    @Override
    public void addAnchoredSample(final Coordinate sampleLocation) {
        mergeSamples(Arrays.asList(sampleLocation), SampleSetLoadMode.ADD);
        // no longer a loaded set
        this.loadedSet = new SampleId();
        fireSampleSetChangedListeners();
        return;
    }

    @Override
    public void removeAnchoredSample(final Coordinate sampleLocation) {
        removeAnchoredSample(sampleLocation,
                ISampleSetManager.DEFAULT_THRESHOLD);
    }

    @Override
    public void removeAnchoredSample(final Coordinate sampleLocation,
            float threshold) {
        mergeSamples(Arrays.asList(sampleLocation), SampleSetLoadMode.REMOVE,
                threshold);
        // no longer a loaded set
        this.loadedSet = new SampleId();
        fireSampleSetChangedListeners();
        return;
    }

    @Override
    public void addAnchoredMarker(final Coordinate location, final GridID gid) {
        String set = activeMarkerSet(gid);
        List<Coordinate> locations = markerLocations.get(set);
        if (locations == null) {
            locations = new ArrayList<>();
        }
        locations.add(location);
        markerLocations.put(set, locations);
        saveSampleSet(locations, new SampleId(set));
    }

    @Override
    public void removeAnchoredMarker(final Coordinate location,
            final GridID gid) {
        removeAnchoredMarker(location, gid,
                ISampleSetManager.DEFAULT_THRESHOLD);
    }

    @Override
    public void removeAnchoredMarker(final Coordinate location,
            final GridID gid, float threshold) {
        boolean found = false;
        String set = activeMarkerSet(gid);
        GeodeticCalculator gdc = new GeodeticCalculator();
        gdc.setStartingGeographicPoint(location.x, location.y);
        List<Coordinate> toRemove = new ArrayList<>();
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

    @Override
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

    @Override
    public boolean saveSampleSet(final List<Coordinate> sampleLocations,
            final SampleId sampleId) {
        SampleData sd = new SampleData(sampleId, sampleLocations);

        ServerResponse<?> sr = ifpClient.saveSampleData(Arrays.asList(sd));
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Failure to save sample data to IFPServer for [%s]: %s",
                    sampleId.getName(), sr.message()));
        }

        return true;
    }

    @Override
    public boolean saveActiveSampleSet(final SampleId sampleId) {
        this.loadedSet = sampleId;
        return saveSampleSet(this.locations, sampleId);
    }

    @Override
    public boolean deleteSampleSet(final SampleId sampleId) {
        ServerResponse<?> sr = ifpClient
                .deleteSampleData(Arrays.asList(sampleId));
        if (!sr.isOkay()) {
            statusHandler.error(
                    String.format("Failure to delete sample set [%s]: %s",
                            sampleId.getName(), sr.message()));
        } else if (sampleId.getName().equals(loadedSet.getName())) {
            loadedSet = new SampleId();
        }

        return true;
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {

        String name = LocalizationUtil.extractName(message.getFileName())
                .replace(".xml", "");
        SampleId id = new SampleId(name, false,
                message.getContext().getLocalizationLevel());

        switch (message.getChangeType()) {
        case ADDED:
        case UPDATED:
            SampleId existing = this.inventory.get(id.getName());
            if ((existing == null)
                    || (existing.getAccess().compareTo(id.getAccess()) <= 0)) {
                this.inventory.put(id.getName(), id);

                // loaded sample set "added", may simply be a rename
                if (id.getName().equals(this.loadedSet.getName())) {
                    loadSampleSet(id, SampleSetLoadMode.REPLACE);
                }
                if (id.getName().equals(SampleSetManager.MARKER_NAME)) {
                    getMarkerPoints();
                }
            }
            break;

        case DELETED:
            LocalizationFile[] files = pathManager.listStaticFiles(
                    LocalizationType.COMMON_STATIC, message.getFileName(),
                    new String[] { ".xml" }, false, true);

            if (files.length == 0) {
                this.inventory.remove(id.getName());
            } else {
                this.inventory.put(id.getName(), new SampleId(id.getName(),
                        false, files[0].getContext().getLocalizationLevel()));
            }

            if (id.getName().equals(this.loadedSet.getName())) {
                this.loadedSet = new SampleId();
            }

            if (id.getName().equals(SampleSetManager.MARKER_NAME)) {
                getMarkerPoints();
            }
            break;

        default:
            statusHandler.error("Unexpected FileChangeType received: "
                    + message.getChangeType().name());
            break;
        }

        fireSampleSetChangedListeners();
    }

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
            this.locations = new ArrayList<>(mergeSet);
            break;
        }
    }

    /**
     * Keeps the _markerLocations up-to-date.
     *
     * Gets the set of points, sends out notification of changes.
     */
    private void getMarkerPoints() {
        // ensure it is in the inventory
        this.markerLocations.clear();

        for (String name : this.inventory.keySet()) {
            if (name.startsWith(MARKER_NAME)) {
                markerLocations.put(name, this.sampleSetLocations(name));
            }
        }
    }

    @Override
    public SampleId getLoadedSet() {
        return this.loadedSet;
    }

    @Override
    public SampleId[] getInventory() {
        return this.inventory.values()
                .toArray(new SampleId[this.inventory.size()]);
    }

    @Override
    public String[] getInventoryAsStrings() {
        String[] retVal = new String[this.inventory.size()];
        int i = 0;
        for (String name : this.inventory.keySet()) {
            retVal[i] = name;
            i += 1;
        }

        return retVal;
    }

    @Override
    public String activeMarkerSet(GridID gid) {
        // get office type from GridID
        String ot = gid.getParm().getOfficeType();
        if ((ot != null) && (ot.length() > 0)) {
            return "ISC_Marker_Set_" + ot;
        } else {
            return "";
        }
    }

    @Override
    public List<Coordinate> getLocations() {
        return new ArrayList<>(this.locations);
    }

    @Override
    public Map<String, List<Coordinate>> getMarkerLocations() {
        return this.markerLocations;
    }

    @Override
    public boolean isShowLatLon() {
        return showLatLon;
    }

    @Override
    public void setShowLatLon(boolean showLatLon) {
        this.showLatLon = showLatLon;
        fireSampleSetChangedListeners();

    }

    /**
     * Load sample set (for Python use)
     *
     * @param sampleId
     *            id of sample set to load
     * @param loadMode
     *            load mode
     * @throws GFEException
     */
    public void loadSampleSet(final SampleId sampleId, String loadMode)
            throws GFEException {
        loadSampleSet(sampleId, SampleSetLoadMode.valueOf(loadMode));
    }

    @Override
    public void addSampleSetChangedListener(
            ISampleSetChangedListener listener) {
        sampleSetChangedListeners.add(listener);
    }

    @Override
    public void removeSampleSetChangedListener(
            ISampleSetChangedListener listener) {
        sampleSetChangedListeners.remove(listener);
    }

    private void fireSampleSetChangedListeners() {
        for (ISampleSetChangedListener listener : sampleSetChangedListeners) {
            listener.sampleSetChanged(this);
        }
    }
}
