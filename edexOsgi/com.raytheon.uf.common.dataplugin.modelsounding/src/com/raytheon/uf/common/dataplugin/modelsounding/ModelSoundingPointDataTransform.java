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
package com.raytheon.uf.common.dataplugin.modelsounding;

import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.ALL_DATA;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.DATAURI;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.ELEVATION;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.FORECAST_HOUR;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LATITUDE;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LONGITUDE;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.LVL_PARAMETERS;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.NUM_LEVELS;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.REF_TIME;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.STATION_ID;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.STATION_NUMBER;
import static com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters.WMO_HEADER;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataServerRequest;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * A class for converting point data into sounding sites.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 06, 2011           bsteffen    Initial creation
 * Dec 02, 2013  2537     bsteffen    Move to common
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ModelSoundingPointDataTransform {

    /**
     * Use all point data parameters to build sounding sites for all sites which
     * match the query defined by fields, values, and operands.
     * 
     */
    public static List<SoundingSite> getSoundingSites(
            Map<String, RequestConstraint> constraints) throws Exception {
        return getSoundingSites(constraints, ALL_DATA);
    }

    /**
     * Use the specified point data parameters to build sounding sites for all
     * sites which match the query defined by fields, values, and operands.
     * 
     */
    public static List<SoundingSite> getSoundingSites(
            Map<String, RequestConstraint> constraints, List<String> parameters)
            throws Exception {
        Set<String> parametersSet = new HashSet<String>(parameters);
        if (!parametersSet.contains(NUM_LEVELS)) {
            /*
             * if you have any level based parameters you must include num
             * levels
             */
            for (String lvlParam : LVL_PARAMETERS) {
                if (parametersSet.contains(lvlParam)) {
                    parametersSet.add(NUM_LEVELS);
                    break;
                }
            }
        }
        /* copy to avoid modification */
        Map<String, RequestConstraint> fullConstraints = new HashMap<String, RequestConstraint>(
                constraints);
        fullConstraints.put(PluginDataObject.PLUGIN_NAME_ID,
                new RequestConstraint(SoundingSite.PLUGIN_ID));
        fullConstraints.put(PointDataServerRequest.REQUEST_MODE_KEY,
                new RequestConstraint(PointDataServerRequest.REQUEST_MODE_2D));
        fullConstraints.put(PointDataServerRequest.REQUEST_PARAMETERS_KEY,
                new RequestConstraint(parametersSet));
        PointDataServerRequest pdr = new PointDataServerRequest();
        pdr.setRcMap(fullConstraints);
        PointDataContainer pdc = (PointDataContainer) RequestRouter.route(pdr);
        if (pdc == null) {
            return Collections.emptyList();
        }
        return getSoundingSites(pdc);
    }

    /**
     * Build sounding sites from the data in the container.
     * 
     * @param pdc
     * @return
     */
    public static List<SoundingSite> getSoundingSites(PointDataContainer pdc) {
        List<SoundingSite> sites = new ArrayList<SoundingSite>(
                pdc.getCurrentSz());
        for (int i = 0; i < pdc.getCurrentSz(); i++) {
            sites.add(getSoundingSite(pdc.readRandom(i)));
        }
        return sites;
    }

    /**
     * Build a single sounding site from the data in the view.
     * 
     * @param pdv
     * @return
     */
    public static SoundingSite getSoundingSite(PointDataView pdv) {
        // All the code from here on is boilerplate code for determining what
        // parameters are in the view and setting the appropriate field in the
        // Sounding Site.
        Set<String> parameters = pdv.getContainer().getParameters();
        SoundingSite site = null;
        if (parameters.contains(DATAURI)) {
            // Parsing from the dataURI gets several fields for us.
            site = new SoundingSite(pdv.getString(DATAURI));
        } else {
            site = new SoundingSite();
            site.setLocation(new SurfaceObsLocation());
            // All of these things would have been in dataURI
            if (parameters.contains(LATITUDE)) {
                site.getLocation().setLatitude(
                        pdv.getNumber(LATITUDE).doubleValue());
            }
            if (parameters.contains(LONGITUDE)) {
                site.getLocation().setLongitude(
                        pdv.getNumber(LONGITUDE).doubleValue());
            }
            if (parameters.contains(STATION_ID)) {
                site.getLocation().setStationId(pdv.getString(STATION_ID));
            }
            if (parameters.contains(FORECAST_HOUR)) {
                if (parameters.contains(REF_TIME)) {
                    Date refTime = new Date(
                            pdv.getNumber(REF_TIME).longValue() * 1000);
                    int fcstTime = pdv.getNumber(FORECAST_HOUR).intValue() * 3600;
                    site.setDataTime(new DataTime(refTime, fcstTime));
                }
            } else if (parameters.contains(REF_TIME)) {
                // This might not be the best idea most people will want
                // forecast time also
                site.setDataTime(new DataTime(new Date(pdv.getNumber(REF_TIME)
                        .longValue() * 1000)));
            }
        }
        site.setPointDataView(pdv);

        if (parameters.contains(ELEVATION)) {
            site.getLocation()
                    .setElevation(pdv.getNumber(ELEVATION).intValue());
        }
        // Record parameters
        if (parameters.contains(WMO_HEADER)) {
            site.setWmoHeader(pdv.getString(WMO_HEADER));
        }
        if (parameters.contains(STATION_NUMBER)) {
            site.setSiteId(String.format("%06d", pdv.getNumber(STATION_NUMBER)
                    .intValue()));
        }

        return site;
    }

}
