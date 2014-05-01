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
package com.raytheon.edex.plugin.ldadhydro.dao;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ldadhydro.HydroLdadRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * 
 * Transform LDAD HYDRO records into Point Data Model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LdadhydroPointDataTransform {

    private Log logger = LogFactory.getLog(getClass());

    private LdadHydroDao dao;

    private PointDataDescription description;

    public LdadhydroPointDataTransform() throws JAXBException, PluginException {
        this.dao = new LdadHydroDao("ldadhydro");
        this.description = dao.getPointDataDescription(null);
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {

        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof HydroLdadRecord))
                    continue;

                File f = this.dao.getFullFilePath(p);

                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                HydroLdadRecord mesor = (HydroLdadRecord) p;
                PointDataView pdv = buildView(pdc, mesor);
                mesor.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            HydroLdadRecord record) {
        PointDataView pdv = container.append();
        if (record.getObservationTime() != null) {
            pdv.setLong("timeObs", record.getObservationTime()
                    .getTimeInMillis());
        }
        if (record.getLocation() != null) {
            pdv.setFloat("latitude", (float) record.getLatitude());
            pdv.setFloat("longitude", (float) record.getLongitude());
            pdv.setFloat("elevation", record.getElevation());
        }
        if (record.getReportTime() != null) {
            pdv.setFloat("reportTime", record.getReportTime().floatValue());
        }
        if (record.getReceivedTime() != null) {
            pdv.setFloat("receivedTime", record.getReceivedTime().floatValue());
        }
        if (record.getStationId() != null) {
            pdv.setString("stationId", record.getStationId());
        }
        pdv.setLong("numericWMOid", record.getNumericWMOid());
        if (record.getBelowSurface() != null) {
            pdv.setFloat("belowSurfac", record.getBelowSurface());
        }
        if (record.getRiverStage() != null) {
            pdv.setFloat("riverStage", record.getRiverStage());
        }
        if (record.getPoolElevation() != null) {
            pdv.setFloat("poolElevation", record.getPoolElevation());
        }
        if (record.getTailwaterStage() != null) {
            pdv.setFloat("tailwaterStage", record.getTailwaterStage());
        }
        if (record.getRiverVelocity() != null) {
            pdv.setFloat("riverVelocity", record.getRiverVelocity());
        }
        if (record.getRiverInflow() != null) {
            pdv.setFloat("riverInflow", record.getRiverInflow());
        }
        if (record.getRiverFlow() != null) {
            pdv.setFloat("riverFlow", record.getRiverFlow());
        }
        if (record.getComputedOutflow() != null) {
            pdv.setFloat("computedOutflow", record.getComputedOutflow());
        }
        if (record.getWaterTemperature() != null) {
            pdv.setFloat("waterTemperature", record.getWaterTemperature());
        }
        if (record.getVoltageBattery() != null) {
            pdv.setFloat("voltageBattery", record.getVoltageBattery());
        }
        if (record.getWaterConductance() != null) {
            pdv.setFloat("waterConductance", record.getWaterConductance());
        }
        if (record.getWaterOxygen() != null) {
            pdv.setFloat("waterOxygen", record.getWaterConductance());
        }
        if (record.getWaterPH() != null) {
            pdv.setFloat("waterPH", record.getWaterPH());
        }
        if (record.getRelHumidity() != null) {
            pdv.setFloat("relHumidity", record.getRelHumidity());
        }
        if (record.getRiverReportChangeTime() != null) {
            pdv.setFloat("riverReportChangeTime", record
                    .getRiverReportChangeTime().floatValue());
        }
        if (record.getTemperature() != null) {
            pdv.setFloat("temperature", record.getTemperature());
        }
        if (record.getDewpoint() != null) {
            pdv.setFloat("dewpoint", record.getDewpoint());
        }
        if (record.getWindDir() != null) {
            pdv.setFloat("windDir", record.getWindDir());
        }
        if (record.getWindSpeed() != null) {
            pdv.setFloat("windSpeed", record.getWindSpeed());
        }
        if (record.getWindSpeedPeak() != null) {
            pdv.setFloat("windSpeedPeak", record.getWindSpeedPeak());
        }
        if (record.getWindGust() != null) {
            pdv.setFloat("windGust", record.getWindGust().floatValue());
        }
        if (record.getPrecipAccum() != null) {
            pdv.setFloat("precipAccum", record.getPrecipAccum());
        }
        if (record.getPrecip5min() != null) {
            pdv.setFloat("precip5min", record.getPrecip5min());
        }
        if (record.getPrecip1hr() != null) {
            pdv.setFloat("precip1hr", record.getPrecip1hr());
        }
        if (record.getPrecip3hr() != null) {
            pdv.setFloat("precip3hr", record.getPrecip3hr());
        }
        if (record.getPrecip6hr() != null) {
            pdv.setFloat("precip6hr", record.getPrecip6hr());
        }
        if (record.getPrecip12hr() != null) {
            pdv.setFloat("precip12hr", record.getPrecip12hr());
        }
        if (record.getPrecip18hr() != null) {
            pdv.setFloat("precip18hr", record.getPrecip18hr());
        }
        if (record.getPrecip24hr() != null) {
            pdv.setFloat("precip24hr", record.getPrecip24hr());
        }
        return pdv;
    }
}
