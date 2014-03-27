package com.raytheon.uf.edex.plugin.madis.ogc;

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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.plugin.madis.MadisPointDataTransform;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * Madis Feature Factory Impl
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   1746       dhladky      Initial creation
 * jan 22, 2014 2713       dhladky     Calendar conversion.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisFeatureFactory implements FeatureFactory {
  
    private static final String LOCATION_KEY = "location";
   
    private static final String MADIS_NS = "http://madis.edex.uf.raytheon.com";

    private static SimpleFeatureType _madisType;

    private static String name = "madis";
    
    public MadisFeatureFactory() {
        super();
    }
       
    /**
     * Converts MadisRecords to SimpleFeatures
     * 
     * @param pdos
     * @return
     */
    @Override
    public List<SimpleFeature> convert(PluginDataObject[] pdos) {
        if (CollectionUtil.isNullOrEmpty(pdos)) {
            return new ArrayList<SimpleFeature>(0);
        }
        ArrayList<SimpleFeature> rval = new ArrayList<SimpleFeature>(
                pdos.length);
        for (PluginDataObject pdo : pdos) {
            if (pdo instanceof MadisRecord) {
                rval.add(getAsFeature((MadisRecord) pdo));
            }
        }
        return rval;
    }
    
    /**
     * @param pdo
     * @return
     */
    public static SimpleFeature getAsFeature(MadisRecord record) {
        
        SimpleFeatureBuilder builder = new SimpleFeatureBuilder(
                getFeatureType());
        builder.set(LOCATION_KEY, record.getLocation().getLocation());
        builder.set(MadisPointDataTransform.STATION_ID, record.getLocation().getStationId());
        builder.set(MadisPointDataTransform.TIME_OBS, record.getTimeObs());
        builder.set(MadisPointDataTransform.PROVIDER, record.getProvider());
        builder.set(MadisPointDataTransform.SUB_PROVIDER, record.getSubProvider());
        builder.set(MadisPointDataTransform.DATASET, record.getDataset());
        builder.set(MadisPointDataTransform.RESTRICTION, record.getRestriction());
        builder.set(MadisPointDataTransform.DEWPOINT, record.getDewpoint());
        builder.set(MadisPointDataTransform.DEWPOINT_QCD, record.getDewpoint_qcd().name());
        builder.set(MadisPointDataTransform.DEWPOINT_QCA, record.getDewpoint_qca());
        builder.set(MadisPointDataTransform.DEWPOINT_QCR, record.getDewpoint_qcr());
        builder.set(MadisPointDataTransform.RH, record.getRh());
        builder.set(MadisPointDataTransform.RH_QCD, record.getRh_qcd().name());
        builder.set(MadisPointDataTransform.RH_QCA, record.getRh_qca());
        builder.set(MadisPointDataTransform.RH_QCR, record.getRh_qcr());
        builder.set(MadisPointDataTransform.ALTIMETER, record.getAltimeter());
        builder.set(MadisPointDataTransform.ALTIMETER_QCD, record.getAltimeter_qcd().name());
        builder.set(MadisPointDataTransform.ALTIMETER_QCA, record.getAltimeter_qca());
        builder.set(MadisPointDataTransform.ALTIMETER_QCR, record.getAltimeter_qcr());
        builder.set(MadisPointDataTransform.TEMPERATURE, record.getTemperature());
        builder.set(MadisPointDataTransform.TEMPERATURE_QCD, record.getTemperature_qcd().name());
        builder.set(MadisPointDataTransform.TEMPERATURE_QCA, record.getTemperature_qca());
        builder.set(MadisPointDataTransform.TEMPERATURE_QCR, record.getTemperature_qcr());
        builder.set(MadisPointDataTransform.WINDDIRECTION, record.getWindDirection());
        builder.set(MadisPointDataTransform.WINDDIRECTION_QCD, record.getWindDirection_qcd().name());
        builder.set(MadisPointDataTransform.WINDDIRECTION_QCA, record.getWindDirection_qca());
        builder.set(MadisPointDataTransform.WINDDIRECTION_QCR, record.getWindDirection_qcr());
        builder.set(MadisPointDataTransform.PRECIPRATE, record.getPrecipRate());
        builder.set(MadisPointDataTransform.PRECIPRATE_QCD, record.getPrecipRate_qcd().name());
        builder.set(MadisPointDataTransform.PRECIPRATE_QCA, record.getPrecipRate_qca());
        builder.set(MadisPointDataTransform.PRECIPRATE_QCR, record.getPrecipRate_qcr());
        builder.set(MadisPointDataTransform.WINDSPEED, record.getWindSpeed());
        builder.set(MadisPointDataTransform.WINDSPEED_QCD, record.getWindSpeed_qcd().name());
        builder.set(MadisPointDataTransform.WINDSPEED_QCA, record.getWindSpeed_qca());
        builder.set(MadisPointDataTransform.WINDSPEED_QCR, record.getWindSpeed_qcr());
        builder.set(MadisPointDataTransform.WINDGUST, record.getWindGust());
        builder.set(MadisPointDataTransform.WINDGUST_QCD, record.getWindGust_qcd().name());
        builder.set(MadisPointDataTransform.WINDGUST_QCA, record.getWindGust_qca());
        builder.set(MadisPointDataTransform.WINDGUST_QCR, record.getWindGust_qcr());
        builder.set(MadisPointDataTransform.PRECIPITALWATER, record.getPrecipitalWater());
        builder.set(MadisPointDataTransform.PRECIPITALWATER_QCD, record.getPrecipitalWater_qcd().name());
        builder.set(MadisPointDataTransform.PRECIPITALWATER_QCA, record.getPrecipitalWater_qca());
        builder.set(MadisPointDataTransform.PRECIPITALWATER_QCR, record.getPrecipitalWater_qcr());
        builder.set(MadisPointDataTransform.PRESSURE, record.getPressure());
        builder.set(MadisPointDataTransform.PRESSURE_QCD, record.getPressure_qcd().name());
        builder.set(MadisPointDataTransform.PRESSURE_QCA, record.getPressure_qca());
        builder.set(MadisPointDataTransform.PRESSURE_QCR, record.getPressure_qcr());

        return builder.buildFeature(record.getDataURI());
    }
    
    public static SimpleFeatureType getFeatureType() {
        if (_madisType == null) {
            SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
            builder.setCRS(MapUtil.LATLON_PROJECTION);
            builder.setName(name);
            builder.setNamespaceURI(MADIS_NS);
            builder.setDefaultGeometry(LOCATION_KEY);
            builder.add(LOCATION_KEY, Point.class);
            builder.add(MadisPointDataTransform.STATION_ID, String.class);
            builder.add(MadisPointDataTransform.TIME_OBS, Calendar.class);
            builder.add(MadisPointDataTransform.PROVIDER, String.class);
            builder.add(MadisPointDataTransform.SUB_PROVIDER, String.class);
            builder.add(MadisPointDataTransform.DATASET, Integer.class);
            builder.add(MadisPointDataTransform.RESTRICTION, Integer.class);
            builder.add(MadisPointDataTransform.DEWPOINT, Float.class);
            builder.add(MadisPointDataTransform.DEWPOINT_QCD, String.class);
            builder.add(MadisPointDataTransform.DEWPOINT_QCA, Integer.class);
            builder.add(MadisPointDataTransform.DEWPOINT_QCR, Integer.class);
            builder.add(MadisPointDataTransform.RH, Float.class);
            builder.add(MadisPointDataTransform.RH_QCD, String.class);
            builder.add(MadisPointDataTransform.RH_QCA, Integer.class);
            builder.add(MadisPointDataTransform.RH_QCR, Integer.class);
            builder.add(MadisPointDataTransform.ALTIMETER, Float.class);
            builder.add(MadisPointDataTransform.ALTIMETER_QCD, String.class);
            builder.add(MadisPointDataTransform.ALTIMETER_QCA, Integer.class);
            builder.add(MadisPointDataTransform.ALTIMETER_QCR, Integer.class);
            builder.add(MadisPointDataTransform.TEMPERATURE, Float.class);
            builder.add(MadisPointDataTransform.TEMPERATURE_QCD, String.class);
            builder.add(MadisPointDataTransform.TEMPERATURE_QCA, Integer.class);
            builder.add(MadisPointDataTransform.TEMPERATURE_QCR, Integer.class);
            builder.add(MadisPointDataTransform.WINDDIRECTION, Integer.class);
            builder.add(MadisPointDataTransform.WINDDIRECTION_QCD, String.class);
            builder.add(MadisPointDataTransform.WINDDIRECTION_QCA, Integer.class);
            builder.add(MadisPointDataTransform.WINDDIRECTION_QCR, Integer.class);
            builder.add(MadisPointDataTransform.PRECIPRATE, Float.class);
            builder.add(MadisPointDataTransform.PRECIPRATE_QCD, String.class);
            builder.add(MadisPointDataTransform.PRECIPRATE_QCA, Integer.class);
            builder.add(MadisPointDataTransform.PRECIPRATE_QCR, Integer.class);
            builder.add(MadisPointDataTransform.WINDSPEED, Float.class);
            builder.add(MadisPointDataTransform.WINDSPEED_QCD, String.class);
            builder.add(MadisPointDataTransform.WINDSPEED_QCA, Integer.class);
            builder.add(MadisPointDataTransform.WINDSPEED_QCR, Integer.class);
            builder.add(MadisPointDataTransform.WINDGUST, Float.class);
            builder.add(MadisPointDataTransform.WINDGUST_QCD, String.class);
            builder.add(MadisPointDataTransform.WINDGUST_QCA, Integer.class);
            builder.add(MadisPointDataTransform.WINDGUST_QCR, Integer.class);
            builder.add(MadisPointDataTransform.PRECIPITALWATER, Float.class);
            builder.add(MadisPointDataTransform.PRECIPITALWATER_QCD, String.class);
            builder.add(MadisPointDataTransform.PRECIPITALWATER_QCA, Integer.class);
            builder.add(MadisPointDataTransform.PRECIPITALWATER_QCR, Integer.class);
            builder.add(MadisPointDataTransform.PRESSURE, Float.class);
            builder.add(MadisPointDataTransform.PRESSURE_QCD, String.class);
            builder.add(MadisPointDataTransform.PRESSURE_QCA, Integer.class);
            builder.add(MadisPointDataTransform.PRESSURE_QCR, Integer.class);
           
            _madisType = builder.buildFeatureType();
        }
        return _madisType;
    }
     

    public String getName() {
        return name;
    }

}
