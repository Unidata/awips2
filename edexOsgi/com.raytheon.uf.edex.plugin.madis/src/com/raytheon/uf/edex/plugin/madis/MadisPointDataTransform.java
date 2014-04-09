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
package com.raytheon.uf.edex.plugin.madis;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord.QCD;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * Provides a transform from madisRecords to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 Mar 2013  1746       dhladky      Created 
 * 10 Jun 2013  1763       dhladky      Updates for speed.
 * 08 Jul 2013  2171       dhladky     Removed dataURI
 * 21 Mar 2014  2939       dhladky     Fixed mismatches in HDF5, DB records.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisPointDataTransform {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisPointDataTransform.class);

    public static final String DATASET = "dataset";

    public static final String COMMA = ",";

    public static final String IN = "in";

    public static final String DEWPOINT = "dewpoint";

    public static final String DEWPOINT_QCD = "dewpoint_qcd";

    public static final String DEWPOINT_QCA = "dewpoint_qca";

    public static final String DEWPOINT_QCR = "dewpoint_qcr";

    public static final String RH = "rh";

    public static final String RH_QCD = "rh_qcd";

    public static final String RH_QCA = "rh_qca";

    public static final String RH_QCR = "rh_qcr";

    public static final String ALTIMETER = "altimeter";

    public static final String ALTIMETER_QCD = "altimeter_qcd";

    public static final String ALTIMETER_QCA = "altimeter_qca";

    public static final String ALTIMETER_QCR = "altimeter_qcr";

    public static final String TEMPERATURE = "temperature";

    public static final String TEMPERATURE_QCD = "temperature_qcd";

    public static final String TEMPERATURE_QCA = "temperature_qca";

    public static final String TEMPERATURE_QCR = "temperature_qcr";

    public static final String WINDDIRECTION = "windDirection";

    public static final String WINDDIRECTION_QCD = "windDirection_qcd";

    public static final String WINDDIRECTION_QCA = "windDirection_qca";

    public static final String WINDDIRECTION_QCR = "windDirection_qcr";

    public static final String PRECIPRATE = "precipRate";

    public static final String PRECIPRATE_QCD = "precipRate_qcd";

    public static final String PRECIPRATE_QCA = "precipRate_qca";

    public static final String PRECIPRATE_QCR = "precipRate_qcr";

    public static final String WINDSPEED = "windSpeed";

    public static final String WINDSPEED_QCD = "windSpeed_qcd";

    public static final String WINDSPEED_QCA = "windSpeed_qca";

    public static final String WINDSPEED_QCR = "windSpeed_qcr";

    public static final String WINDGUST = "windGust";

    public static final String WINDGUST_QCD = "windGust_qcd";

    public static final String WINDGUST_QCA = "windGust_qca";

    public static final String WINDGUST_QCR = "windGust_qcr";

    public static final String PRECIPITALWATER = "precipitalWater";

    public static final String PRECIPITALWATER_QCD = "precipitalWater_qcd";

    public static final String PRECIPITALWATER_QCA = "precipitalWater_qca";

    public static final String PRECIPITALWATER_QCR = "precipitalWater_qcr";

    public static final String PRESSURE = "pressure";

    public static final String PRESSURE_QCD = "pressure_qcd";

    public static final String PRESSURE_QCA = "pressure_qca";

    public static final String PRESSURE_QCR = "pressure_qcr";

    // -------------- location info ------------------//
    public static final String STATION_NAME = "stationName";

    public static final String STATION_ID = "stationId";

    public static final String LONGITUDE = "longitude";

    public static final String LATITUDE = "latitude";

    public static final String ELEVATION = "elevation";

    // -----------These for actual record ------------//

    public static final String PROVIDER = "provider";

    public static final String SUB_PROVIDER = "sub_provider";

    public static final String RESTRICTION = "restriction";

    public static final String TIME_OBS = "timeObs";
        
    public static final String ID = "id";
    
    public static final String EQUAL = "=";

    public static final String[] ALL_PARAMS = { DATASET, DEWPOINT,
            DEWPOINT_QCD, DEWPOINT_QCA, DEWPOINT_QCR, RH, RH_QCD, RH_QCA,
            RH_QCR, ALTIMETER, ALTIMETER_QCD, ALTIMETER_QCA, ALTIMETER_QCR,
            TEMPERATURE, TEMPERATURE_QCD, TEMPERATURE_QCA, TEMPERATURE_QCR,
            WINDDIRECTION, WINDDIRECTION_QCD, WINDDIRECTION_QCA,
            WINDDIRECTION_QCR, PRECIPRATE, PRECIPRATE_QCD, PRECIPRATE_QCA,
            PRECIPRATE_QCR, WINDSPEED, WINDSPEED_QCD, WINDSPEED_QCA,
            WINDSPEED_QCR, WINDGUST, WINDGUST_QCD, WINDGUST_QCA, WINDGUST_QCR,
            PRECIPITALWATER, PRECIPITALWATER_QCD, PRECIPITALWATER_QCA,
            PRECIPITALWATER_QCR, PRESSURE, PRESSURE_QCD, PRESSURE_QCA,
            PRESSURE_QCR };

    public static final String ALL_PARAMS_LIST;

    static {
        ALL_PARAMS_LIST = StringUtil.join(ALL_PARAMS, ',');
    }

    private MadisDao dao;

    private PointDataDescription description;

    public MadisPointDataTransform() throws PluginException {

        this.dao = new MadisDao(MadisRecord.PLUGIN_NAME);
        this.description = dao.getPointDataDescription(null);
    }

    /**
     * Takes in PDO's puts out populated PointData
     * 
     * @param pdo
     * @return
     */
    public PluginDataObject[] toPointData(PluginDataObject[] pdos) {

        if (pdos != null && pdos.length > 0) {

            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
            long time = System.currentTimeMillis();

            for (PluginDataObject p : pdos) {
                if (!(p instanceof MadisRecord)) {
                    statusHandler.handle(Priority.WARN,
                            "Wrong PDO for container");
                    continue;
                }
                try {
                    File f = this.dao.getFullFilePath(p);
                    PointDataContainer pdc = pointMap.get(f);
                    if (pdc == null) {
                        pdc = PointDataContainer.build(this.description);
                        pointMap.put(f, pdc);
                    }

                    MadisRecord mr = (MadisRecord) p;
                    PointDataView pdv = buildView(pdc, mr);
                    mr.setPointDataView(pdv);
                } catch (Exception e) {
                    statusHandler.handle(Priority.INFO,
                            "Can't create pointData container", e);
                }
            }

            long time2 = System.currentTimeMillis();
            statusHandler.handle(Priority.INFO, "MADIS PointData create time: "
                    + (time2 - time) + " ms");
        }

        return pdos;
    }

    /**
     * Builds the PointDataView
     * 
     * @param container
     * @param record
     * @return
     */
    private PointDataView buildView(PointDataContainer container,
            MadisRecord record) {
        PointDataView pdv = container.append();

        pdv.setLong(TIME_OBS, record.getDataTime().getRefTime().getTime());
        pdv.setString(PROVIDER, record.getProvider());
        pdv.setString(SUB_PROVIDER, record.getSubProvider());
        pdv.setInt(RESTRICTION, record.getRestriction());
        pdv.setInt(DATASET, record.getDataset());
        // dewpoint
        pdv.setFloat(DEWPOINT, record.getDewpoint());
        pdv.setString(DEWPOINT_QCD, record.getDewpoint_qcd().toString());
        pdv.setInt(DEWPOINT_QCA, record.getDewpoint_qca());
        pdv.setInt(DEWPOINT_QCR, record.getDewpoint_qcr());
        // relative humidity
        pdv.setFloat(RH, record.getRh());
        pdv.setString(RH_QCD, record.getRh_qcd().toString());
        pdv.setInt(RH_QCA, record.getRh_qca());
        pdv.setInt(RH_QCR, record.getRh_qcr());
        // altimeter setting
        pdv.setFloat(ALTIMETER, record.getAltimeter());
        pdv.setString(ALTIMETER_QCD, record.getAltimeter_qcd().toString());
        pdv.setInt(ALTIMETER_QCA, record.getAltimeter_qca());
        pdv.setInt(ALTIMETER_QCR, record.getAltimeter_qcr());
        // temperature
        pdv.setFloat(TEMPERATURE, record.getTemperature());
        pdv.setString(TEMPERATURE_QCD, record.getTemperature_qcd().toString());
        pdv.setInt(TEMPERATURE_QCA, record.getTemperature_qca());
        pdv.setInt(TEMPERATURE_QCR, record.getTemperature_qcr());
        // dew point depression
        pdv.setFloat(WINDDIRECTION, record.getWindDirection());
        pdv.setString(WINDDIRECTION_QCD, record.getWindDirection_qcd()
                .toString());
        pdv.setInt(WINDDIRECTION_QCA, record.getWindDirection_qca());
        pdv.setInt(WINDDIRECTION_QCR, record.getWindDirection_qcr());
        // precip rate
        pdv.setFloat(PRECIPRATE, record.getPrecipRate());
        pdv.setString(PRECIPRATE_QCD, record.getPrecipRate_qcd().toString());
        pdv.setInt(PRECIPRATE_QCA, record.getPrecipRate_qca());
        pdv.setInt(PRECIPRATE_QCR, record.getPrecipRate_qcr());
        // WINDSPEED (Wind?)
        pdv.setFloat(WINDSPEED, record.getWindSpeed());
        pdv.setString(WINDSPEED_QCD, record.getWindSpeed_qcd().toString());
        pdv.setInt(WINDSPEED_QCA, record.getWindSpeed_qca());
        pdv.setInt(WINDSPEED_QCR, record.getWindSpeed_qcr());
        // Wind Gust
        pdv.setFloat(WINDGUST, record.getWindGust());
        pdv.setString(WINDGUST_QCD, record.getWindGust_qcd().toString());
        pdv.setInt(WINDGUST_QCA, record.getWindGust_qca());
        pdv.setInt(WINDGUST_QCR, record.getWindGust_qcr());
        // Precipital Water
        pdv.setFloat(PRECIPITALWATER, record.getPrecipitalWater());
        pdv.setString(PRECIPITALWATER_QCD, record.getPrecipitalWater_qcd()
                .toString());
        pdv.setInt(PRECIPITALWATER_QCA, record.getPrecipitalWater_qca());
        pdv.setInt(PRECIPITALWATER_QCR, record.getPrecipitalWater_qcr());
        // Pressure
        pdv.setFloat(PRESSURE, record.getPressure());
        pdv.setString(PRESSURE_QCD, record.getPressure_qcd().toString());
        pdv.setInt(PRESSURE_QCA, record.getPressure_qca());
        pdv.setInt(PRESSURE_QCR, record.getPressure_qcr());
    
        return pdv;
    }

    /**
     * Creates a MadisRecord from a PointDataContainer
     * 
     * @param pdc
     * @return
     */
    public static MadisRecord toMadisRecord(MadisRecord mr, PointDataView pdv) {

        mr.setDataset(pdv.getInt(DATASET));
        // dewpoint
        mr.setDewpoint(pdv.getFloat(DEWPOINT));
        mr.setDewpoint_qcd(QCD.fromString(pdv.getString(DEWPOINT_QCD)));
        mr.setDewpoint_qca(pdv.getInt(DEWPOINT_QCA));
        mr.setDewpoint_qcr(pdv.getInt(DEWPOINT_QCR));
        // relative humidty
        mr.setRh(pdv.getFloat(RH));
        mr.setRh_qcd(QCD.fromString(pdv.getString(RH_QCD)));
        mr.setRh_qca(pdv.getInt(RH_QCA));
        mr.setRh_qcr(pdv.getInt(RH_QCR));
        // altimeter setting
        mr.setAltimeter(pdv.getFloat(ALTIMETER));
        mr.setAltimeter_qcd(QCD.fromString(pdv.getString(ALTIMETER_QCD)));
        mr.setAltimeter_qca(pdv.getInt(ALTIMETER_QCA));
        mr.setAltimeter_qcr(pdv.getInt(ALTIMETER_QCR));
        // temperature
        mr.setTemperature(pdv.getFloat(TEMPERATURE));
        mr.setTemperature_qcd(QCD.fromString(pdv.getString(TEMPERATURE_QCD)));
        mr.setTemperature_qca(pdv.getInt(TEMPERATURE_QCA));
        mr.setTemperature_qcr(pdv.getInt(TEMPERATURE_QCR));
        // wind direction
        mr.setWindDirection(pdv.getNumber(WINDDIRECTION).intValue());
        mr.setWindDirection_qcd(QCD.fromString(pdv.getString(WINDDIRECTION_QCD)));
        mr.setWindDirection_qca(pdv.getInt(WINDDIRECTION_QCA));
        mr.setWindDirection_qcr(pdv.getInt(WINDDIRECTION_QCR));
        // precip rate
        mr.setPrecipRate(pdv.getFloat(PRECIPRATE));
        mr.setPrecipRate_qcd(QCD.fromString(pdv.getString(PRECIPRATE_QCD)));
        mr.setPrecipRate_qca(pdv.getInt(PRECIPRATE_QCA));
        mr.setPrecipRate_qcr(pdv.getInt(PRECIPRATE_QCR));
        // WINDSPEED
        mr.setWindSpeed(pdv.getFloat(WINDSPEED));
        mr.setWindSpeed_qcd(QCD.fromString(pdv.getString(WINDSPEED_QCD)));
        mr.setWindSpeed_qca(pdv.getInt(WINDSPEED_QCA));
        mr.setWindSpeed_qcr(pdv.getInt(WINDSPEED_QCR));
        // Wind Gust
        mr.setWindGust(pdv.getFloat(WINDGUST));
        mr.setWindGust_qcd(QCD.fromString(pdv.getString(WINDGUST_QCD)));
        mr.setWindGust_qca(pdv.getInt(WINDGUST_QCA));
        mr.setWindGust_qcr(pdv.getInt(WINDGUST_QCR));
        // Precipital Water
        mr.setPrecipitalWater(pdv.getFloat(PRECIPITALWATER));
        mr.setPrecipitalWater_qcd(QCD.fromString(pdv
                .getString(PRECIPITALWATER_QCD)));
        mr.setPrecipitalWater_qca(pdv.getInt(PRECIPITALWATER_QCA));
        mr.setPrecipitalWater_qcr(pdv.getInt(PRECIPITALWATER_QCR));
        // Pressure
        mr.setPressure(pdv.getFloat(PRESSURE));
        mr.setPressure_qcd(QCD.fromString(pdv.getString(PRESSURE_QCD)));
        mr.setPressure_qca(pdv.getInt(PRESSURE_QCA));
        mr.setPressure_qcr(pdv.getInt(PRESSURE_QCR));

        return mr;
    }

    /**
     * Gets Madis fields out of PointData.
     * 
     * @param madisRecord
     * @return populated Madis record
     * @throws PluginException
     */
    public static MadisRecord populatePointDataFields(MadisRecord record) {

        PointDataQuery request = null;
        PointDataContainer result = null;

        try {
            request = new PointDataQuery(MadisRecord.PLUGIN_NAME);
            request.requestAllLevels();
            request.addParameter(ID, String.valueOf(record.getId()), EQUAL);
            request.setParameters(ALL_PARAMS_LIST);
            result = request.execute();
            
            if (result != null) {

                result.setCurrentSz(result.getAllocatedSz());
                PointDataView pdv = result.readRandom(0);
                record = toMadisRecord(record, pdv);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return record;
    }

    /**
     * Gets Madis fields out of PointData.
     * 
     * @param madisRecord
     *            []
     * @return populated Madis record Array
     * @throws PluginException
     */

    public static PluginDataObject[] populatePointDataFields(
            PluginDataObject[] records) {

        PointDataQuery request = null;
        PointDataContainer result = null;
        StringBuilder ids = new StringBuilder();

        for (int i = 0; i < records.length; i++) {
            ids.append(records[i].getId());
            if (i < records.length - 1) {
                ids.append(COMMA);
            }
        }

        try {

            request = new PointDataQuery(MadisRecord.PLUGIN_NAME);
            request.requestAllLevels();
            request.addParameter(ID, ids.toString(), IN);
            request.setParameters(ALL_PARAMS_LIST);
            result = request.execute();
            
            // correlate up the PointDataViews with the correct records. 
            HashMap<Integer, PointDataView> pdvs = new HashMap<Integer, PointDataView>(records.length);
            for (int i = 0; i < records.length; i++) {
                PointDataView pdv = result.readRandom(i);
                // correlate ID from record with ID from PDV
                int id = pdv.getInt(ID);
                pdvs.put(id, pdv);
            }

            for (int i = 0; i < records.length; i++) {
                int id = records[i].getId();
                records[i] = toMadisRecord((MadisRecord)records[i], pdvs.get(id));
            }

            if (result != null) {
                statusHandler.info("Results: " + result.getCurrentSz()
                        + " point data views.");
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to populate point data records!", e);
        }

        return records;
    }

}
