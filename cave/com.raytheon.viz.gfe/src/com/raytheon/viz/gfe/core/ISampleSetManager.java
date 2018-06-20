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

package com.raytheon.viz.gfe.core;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.viz.gfe.core.msgs.ISampleSetChangedListener;
import com.raytheon.viz.gfe.edittool.GridID;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A manager class for SampleSet, which contain SampleData
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 30, 2008             chammack    Initial creation
 * Apr 09, 2009  1288       rjpeter     Added add/remove method for sample set listener
 * Sep 09, 2014  3592       randerso    Added dispose method, 
 *                                      removed getInventoryAsList and networkNotification,
 *                                      improved JavaDoc
 * Aug 13, 2015  4749       njensen     Extends DisposableManager                                     
 *                                      
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface ISampleSetManager extends DisposableManager {
    /**
     * Sample Set Load Mode
     */
    public static enum SampleSetLoadMode {
        /**
         * Add sample set to existing sample points
         */
        ADD,

        /**
         * Remove sample set from existing sample points
         */
        REMOVE,

        /**
         * Replase existing sample points with sample set
         */
        REPLACE
    }

    public static final float DEFAULT_THRESHOLD = 0.0f;

    /**
     * Returns the set of sample points for the named sample set. If the sample
     * set is not known, an empty list will be returned.
     * 
     * @param setName
     * @return the locations
     */
    public List<Coordinate> sampleSetLocations(final String setName);

    /**
     * Loads the named sample set and mixes it with the active sample set in a
     * method defined by loadMode. ADD combines the two sets, REMOVE removes the
     * new samples from the existing set, and REPLACE removes all old samples
     * before adding the new samples.
     * 
     * Gets the network pointer using net() and calls the MRSDbClient's
     * getMRSData(sampleSetName, tempData). The ADD, REMOVE, or REPLACE
     * operation is performed by the mergeSamples(tempData, loadMode).
     * 
     * @param sampleId
     * @param loadMode
     */
    public void loadSampleSet(final SampleId sampleId,
            SampleSetLoadMode loadMode);

    /**
     * Clears all anchored samples.
     * 
     * Calls mergeSamples() with an empty list of coordinates and the mergeMode
     * REPLACE.
     */
    public void clearSamples();

    /**
     * Adds an anchored sample at the sample location.
     * 
     * Calls mergeSamples(sampleLocation, ADD).
     * 
     * @param sampleLocation
     */
    public void addAnchoredSample(final Coordinate sampleLocation);

    /**
     * removeAnchoredSample(...) with default threshold of 0.0
     * 
     * @param sampleLocation
     */
    public void removeAnchoredSample(final Coordinate sampleLocation);

    /**
     * Removes an anchored sample at the sample location if threshold is zero.
     * If threshold is non-zero, then removes the anchored sample closest to the
     * sampleLocation within the threshold.
     * 
     * Calls mergeSamples().
     * 
     * @param sampleLocation
     * @param threshold
     */
    public void removeAnchoredSample(final Coordinate sampleLocation,
            float threshold);

    /**
     * Adds an anchored maker at the location.
     * 
     * Appends the new marker location, the save will result in a network
     * notification, which will then update the marker points, which in turn
     * will send out a MarkerSet changed notification.
     * 
     * @param location
     */
    public void addAnchoredMarker(final Coordinate location, final GridID gid);

    /**
     * removeAnchoredMarker(...) with default threshold
     * 
     * @param location
     */
    public void removeAnchoredMarker(final Coordinate location, final GridID gid);

    /**
     * Removes an anchored marker at the location if threshold is zero. If
     * threshold is non-zero, then removes the anchored marker closest to the
     * location within the threshold.
     * 
     * Deletes an entry, calls saveSampleSet() to save it to the server, which
     * results in a network notification, which updates the marker points, which
     * sends out a marker Set changed notification.
     * 
     * @param location
     * @param threshold
     */
    public void removeAnchoredMarker(final Coordinate location,
            final GridID gid, float threshold);

    /**
     * Boolean function which returns true if there is an anchored marker at the
     * given location.
     * 
     * @param location
     * @param threshold
     * @return true if anchored marker at location
     */
    public boolean anchoredMarkerAtLocation(final Coordinate location,
            final GridID gid, float threshold);

    /**
     * Saves the given sample data under the given name. Returns true if
     * successful.
     * 
     * @param sampleLocations
     * @param sampleId
     * @return true if successful
     */
    public boolean saveSampleSet(final List<Coordinate> sampleLocations,
            final SampleId sampleId);

    /**
     * Saves the active sample set under the given name. Returns true if
     * successful.
     * 
     * Gets the network pointer from net() and calls the MRSDbClient's
     * saveMRSData() function. The format of _locations is the same as the
     * database server expects. Then calls networkNotification().
     * 
     * @param sampleId
     * @return true if successful
     */
    public boolean saveActiveSampleSet(final SampleId sampleId);

    /**
     * Deletes the named sample set from the database. Returns true if
     * successful.
     * 
     * Gets the network pointer using net() and issues the removeMRSData()
     * command. Returns the status from the network. Then calls
     * networkNotification().
     * 
     * @param sampleId
     * @return true if successful
     */
    public boolean deleteSampleSet(final SampleId sampleId);

    /**
     * @return the loadedSet
     */
    public SampleId getLoadedSet();

    /**
     * @return the inventory
     */
    public SampleId[] getInventory();

    /**
     * @return the inventory
     */
    public String[] getInventoryAsStrings();

    /**
     * @return the locations
     */
    public List<Coordinate> getLocations();

    /**
     * @return the markerLocations
     */
    public Map<String, List<Coordinate>> getMarkerLocations();

    /**
     * Is lat/lon display enabled?
     * 
     * @return true if lat/lon display enabled
     */
    public boolean isShowLatLon();

    /**
     * Set whether lat lon display is enabled
     * 
     * @param showLatLon
     */
    public void setShowLatLon(boolean showLatLon);

    /**
     * Register a sample set changed listener with the sample set manager.
     * 
     * Notifies the recipient of when the sample set changes
     * 
     * @param listener
     *            the sample set changed listener
     */
    public void addSampleSetChangedListener(ISampleSetChangedListener listener);

    /**
     * Unregister a sample set changed listener from the sample set manager
     * 
     * @param listener
     *            the sample set changed listener
     */
    public void removeSampleSetChangedListener(
            ISampleSetChangedListener listener);

    /**
     * Gets the active marker set
     * 
     * @param gid
     *            The grid id
     * @return The active marker set
     */
    public String activeMarkerSet(GridID gid);
}