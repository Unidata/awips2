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
package com.raytheon.uf.edex.plugin.acarssounding.tools;

import static com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingConstants.EMPTY_SOUNDING;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingConstants;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingLayer;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.tools.Airport;
import com.raytheon.uf.common.dataplugin.acarssounding.tools.AirportsBean;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2009            jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 24, 2014 DR15038    M.Porricelli In createSounding, use 
 *                                      difference between flight
 *                                      level and airport elevation
 *                                      to determine which airport
 *                                      to use
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SoundingBuilder {

    private final Log logger = LogFactory.getLog(getClass());

    File dataDir = null;

    private AirportsBean airports = null;

    public SoundingBuilder(AirportsBean airports) {
        this.airports = airports;

        logger.info("SoundingBuilder complete");

        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

        dataDir = pathMgr.getFile(ctx, ACARSSoundingTools.BASE_PATH
                + ACARSSoundingTools.DATA_PATH);

    }

    /**
     * 
     * 
     * @param data
     *            Data containing the time/uri and acarsRecords to be processed.
     * @param sdngs
     *            List of soundings that could possibly include the data to be
     *            processed.
     * @return Zero or more generated sounding based on the data
     */
    public ACARSSoundingRecord[] createSoundings(IntermediateData data,
            List<ACARSSoundingRecord> sndgs) {
        List<ACARSSoundingRecord> soundings = new ArrayList<ACARSSoundingRecord>();

        if (data != null) {

            updateSoundings(data, sndgs);

            List<ACARSRecord> obs = data.getRecordList();
            if ((obs != null)
                    && (obs.size() >= ACARSSoundingTools.MIN_OBS_FOR_SOUNDING)) {

                if (hasFlightPhase(obs)) {
                    obs = fixupObservations(obs);
                } else {
                    // do something else for data that has not reported flight
                    // phase. For now return an empty sounding.
                    return EMPTY_SOUNDING;
                }

                // At this point the flight data is in correct time/height
                // order.

                // Determine if we need to break the observations into separate
                // sublists.
                List<List<ACARSRecord>> breaks = findBreaks(obs);
                if (logger.isDebugEnabled()) {
                    logger.debug("Total of " + breaks.size()
                            + " possible soundings after break");
                }
                for (List<ACARSRecord> tList : breaks) {
                    if (tList != null) {
                        ACARSSoundingTools.accept(tList);
                        if ((tList == null) || (tList.size() == 0)) {
                            logger.info("Rejected potential sounding for "
                                    + data.getTailNumber());
                        } else {
                            ACARSSoundingRecord sounding = createSounding(data,
                                    tList);
                            if (sounding != null) {
                                soundings.add(sounding);
                            }
                        }
                    }
                }
                data.reconcile(logger);
            }
        }
        return ((soundings.size() > 0) ? soundings
                .toArray(new ACARSSoundingRecord[soundings.size()])
                : EMPTY_SOUNDING);
    }

    /**
     * 
     * 
     * 
     * @param obsData
     * @return A not null list of 1 or more lists of observations.
     */
    private List<List<ACARSRecord>> findBreaks(List<ACARSRecord> obsData) {
        List<List<ACARSRecord>> breaks = new ArrayList<List<ACARSRecord>>();

        List<ACARSRecord> tList = new ArrayList<ACARSRecord>();
        breaks.add(tList);
        ACARSRecord lastRec = null;
        for (ACARSRecord r : obsData) {

            // we need to do some checks to see if we need to
            // create a new sublist.

            if (lastRec != null) {
                long dT = r.getTimeObs().getTimeInMillis()
                        - lastRec.getTimeObs().getTimeInMillis();
                if (dT >= ACARSSoundingTools.BREAK_TIME_DIFF) {
                    tList = new ArrayList<ACARSRecord>();
                    breaks.add(tList);
                } else {
                    int dA = r.getFlightLevel() - lastRec.getFlightLevel();
                    if (dA < 0) {
                        dA *= -1;
                    }
                    if (dA >= ACARSSoundingTools.MIN_DELTA_ALT) {
                        tList = new ArrayList<ACARSRecord>();
                        breaks.add(tList);
                    } else {
                        // other rules
                    }
                }
            }
            tList.add(r);
            lastRec = r;
        }

        return breaks;
    }

    /**
     * 
     * @param data
     * @param obsData
     * @return
     */
    private ACARSSoundingRecord createSounding(IntermediateData data,
            List<ACARSRecord> obsData) {
        ACARSSoundingRecord sounding = null;
        if (obsData != null) {
            // examine the lower MAX_FIRST meters of the data to determine which
            // airport to use.
            Airport airport = null;
            Double dist = Double.MAX_VALUE;
            ACARSRecord recObs = null;
            for (ACARSRecord r : obsData) {
                Airport a = airports
                        .nearest(r, ACARSSoundingTools.MAX_DISTANCE);
                if (a != null
                        && (r.getFlightLevel() - a.getElevation()) < ACARSSoundingTools.MAX_FIRST) {
                    if ((a.getDistance() < dist)) {
                        // Keep track of the obs that contributes the distance.
                        recObs = r;
                        dist = a.getDistance();
                        airport = a;
                    }
                }
            }
 // If we have an airport we'll use that to create the sounding
            if (airport != null) {
                sounding = new ACARSSoundingRecord();
                Calendar soundingTime = recObs.getTimeObs();
                sounding.setTimeObs(soundingTime);
                sounding.setDataTime(new DataTime(soundingTime));

                SurfaceObsLocation loc = new SurfaceObsLocation(airport.getId());
                loc.assignLocation(airport.getLatitude(),
                        airport.getLongitude());
                loc.setElevation(airport.getElevation().intValue());
                sounding.setLocation(loc);
                sounding.setTailNumber(data.getTailNumber());

                try {
                    sounding.constructDataURI();
                    // we have a sounding, so add the layer data.
                    for (ACARSRecord r : obsData) {
                        r.setUsedInSounding(true);
                        sounding.addLevel(ACARSSoundingTools.createLayer(r));
                    }
                    determineFlightPhase(sounding);
                } catch (Exception e) {
                    logger.error("Unable to construct datauri", e);
                    sounding = null;
                }
            }
        }

        return sounding;
    }

    /**
     * 
     * @param data
     * @param sndgs
     */
    private void updateSoundings(IntermediateData data,
            List<ACARSSoundingRecord> sndgs) {

        // Check if we have any soundings to check against. If none we will
        // just return.
        if ((sndgs != null) && (sndgs.size() > 0)) {
            for (ACARSSoundingRecord rec : sndgs) {

                List<ACARSRecord> obs = data.getRecordList();
                if (obs != null) {
                    List<ACARSSoundingLayer> layers = getLayers(rec.getLevels());

                    // Now we have a time ordered list layers in the current
                    // sounding,
                    // so now check each obs record against the layers data
                    for (int cRec = 0; cRec < obs.size();) {
                        ACARSRecord r = obs.get(cRec);

                        boolean getNext = true;
                        boolean incNext = true;

                        ACARSSoundingLayer lastLayer = null;
                        for (ACARSSoundingLayer layer : layers) {
                            // If we find an obs with the same time as one in
                            // the sounding, get rid of it.
                            if (layer.getTimeObs().equals(r.getTimeObs())) {
                                if (logger.isDebugEnabled()) {
                                    logger.debug("Deleting duplicate layer"
                                            + r.getDataURI());
                                }
                                obs.remove(cRec);
                                incNext = false;
                            } else {
                                if (lastLayer != null) {
                                    // Is the obs time between the current
                                    // layers? If so we want to find out if
                                    // it needs to be included.
                                    if (ACARSSoundingTools.checkBetween(
                                            lastLayer, r, layer)) {
                                        logger.info("Found candidate layer"
                                                + r.getDataURI());
                                        obs.remove(cRec);
                                        incNext = false;
                                    } else {
                                        incNext = true;
                                    }
                                }
                            }
                        }
                        // Force a loop exit.
                        if (!getNext) {
                            break;
                        }
                        cRec += (incNext) ? 1 : 0;
                    }
                }
            }
        }
    }

    /**
     * Returns a sorted not-null list of layers that are contained in a set of
     * layers.
     * 
     * @param data
     * @return
     */
    private static List<ACARSSoundingLayer> getLayers(
            Set<ACARSSoundingLayer> data) {
        List<ACARSSoundingLayer> layers = new ArrayList<ACARSSoundingLayer>();

        if ((data != null) && (data.size() > 0)) {
            layers.addAll(data);
            Collections.sort(layers, ACARSSoundingTools.ACARS_LAYER_COMPARATOR);
        }
        return layers;
    }

    /**
     * Determine whether the specified sounding is ascending or descending.
     * 
     * @param sounding
     * @return
     */
    private void determineFlightPhase(ACARSSoundingRecord sounding) {

        Set<ACARSSoundingLayer> layers = sounding.getLevels();

        ACARSSoundingLayer loLayer = null;
        ACARSSoundingLayer hiLayer = null;
        // Find the highest and lowest layers in the sounding.
        for (ACARSSoundingLayer layer : layers) {
            if (hiLayer == null) {
                hiLayer = layer;
            } else if (layer.getFlightLevel() > hiLayer.getFlightLevel()) {
                hiLayer = layer;
            }
            if (loLayer == null) {
                loLayer = layer;
            } else if (layer.getFlightLevel() < loLayer.getFlightLevel()) {
                loLayer = layer;
            }
        }
        // The assumption here is that the lowest layer is also the
        // closest to the airport so that if the lowest is also the oldest
        // then the flight will be ascending.
        if (loLayer.getTimeObs().getTimeInMillis() < hiLayer.getTimeObs()
                .getTimeInMillis()) {
            sounding.setPhase(ACARSSoundingConstants.ASCENDING_PHASE);
        } else {
            sounding.setPhase(ACARSSoundingConstants.DESCENDING_PHASE);
        }
    }

    /**
     * Correct for out of order observations. This can occur when data is
     * received without seconds information. Several observations may be
     * received within the same minute and be out of order
     * 
     * @param obsData
     * @return
     */
    private List<ACARSRecord> fixupObservations(List<ACARSRecord> obsData) {
        List<ACARSRecord> retData = null;
        // Ensure the data is sorted by time.
        Collections.sort(obsData, ACARSSoundingTools.ACARS_TIME_COMPARATOR);

        List<List<ACARSRecord>> times = new ArrayList<List<ACARSRecord>>();
        List<ACARSRecord> currTime = new ArrayList<ACARSRecord>();
        times.add(currTime);
        ACARSRecord lastRec = obsData.get(0);
        if (lastRec != null) {
            currTime.add(lastRec);
            for (int i = 1; i < obsData.size(); i++) {
                ACARSRecord r = obsData.get(i);
                if (r != null) {
                    // Do the observations have the same time?
                    if (ACARSSoundingTools.ACARS_TIME_COMPARATOR.compare(
                            lastRec, r) != 0) {
                        // Different times, so create a new list
                        currTime = new ArrayList<ACARSRecord>();
                        times.add(currTime);
                        // and add the obs to it instead.
                        lastRec = r;
                    }
                    currTime.add(r);
                }
            } // for
              // Now we have a list of lists
            for (List<ACARSRecord> tList : times) {
                // Only a single item, skip it.
                if (tList.size() > 1) {
                    int descendCount = 0;
                    int ascendCount = 0;
                    for (ACARSRecord r : tList) {
                        switch (r.getFlightPhase()) {
                        case ACARSSoundingTools.ASC_FLGT: {
                            ascendCount++;
                            break;
                        }
                        case ACARSSoundingTools.DES_FLGT: {
                            descendCount++;
                            break;
                        }
                        } // switch
                    }
                    if (descendCount > ascendCount) {
                        Collections.sort(tList,
                                ACARSSoundingTools.ACARS_DES_COMPARATOR);
                    } else if (ascendCount > descendCount) {
                        Collections.sort(tList,
                                ACARSSoundingTools.ACARS_ASC_COMPARATOR);
                    }
                }
                assignTimes(tList);
            } // for
        }

        retData = new ArrayList<ACARSRecord>();
        for (List<ACARSRecord> tList : times) {
            removeDuplicateFlightLevels(tList);
            for (ACARSRecord r : tList) {
                if (r != null) {
                    retData.add(r);
                }
            }
        } // for
        return retData;
    }

    /**
     * Given a list of ACARS records with the same observation distribute the
     * observation time seconds by dividing 60 seconds by the number of
     * observations.
     * 
     * @param data
     */
    private void assignTimes(List<ACARSRecord> data) {
        if ((data != null) && (data.size() > 1)) {
            int n = data.size();
            int dt = 60 / n;
            for (int i = 0; i < data.size(); i++) {
                ACARSRecord r = data.get(i);
                if (logger.isDebugEnabled()) {
                    logger.debug("Correcting time for " + r.getTailNumber());
                }
                Calendar c = r.getTimeObs();
                c.set(Calendar.SECOND, (dt * i));
                r.setDataTime(new DataTime(TimeTools.copy(c)));
                try {
                    r.setDataURI(null);
                    r.constructDataURI();
                } catch (Exception e) {
                    logger.error("Unable to construct datauri in assignTimes",
                            e);
                }
            }
        }
    }

    /**
     * 
     * @param data
     */
    private void removeDuplicateFlightLevels(List<ACARSRecord> data) {
        if ((data != null) && (data.size() > 1)) {
            ACARSRecord base = data.get(0);
            for (int i = 1; i < data.size(); i++) {
                ACARSRecord curr = data.get(i);
                if (base.getFlightLevel().equals(curr.getFlightLevel())) {
                    data.set(i, null);
                } else {
                    base = curr;
                }

            }
        }
    }

    /**
     * 
     * @param data
     * @return
     */
    private boolean hasFlightPhase(List<ACARSRecord> data) {
        boolean retValue = true;
        for (ACARSRecord r : data) {
            if (r.getFlightPhase() == null) {
                retValue = false;
                break;
            }
        }
        return retValue;
    }
}
