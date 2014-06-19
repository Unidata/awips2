/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.wxxm;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.measure.Measure;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Quantity;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_2_1.AbstractGeometryType;
import net.opengis.gml.v_3_2_1.AbstractTimeObjectType;
import net.opengis.gml.v_3_2_1.CodeWithAuthorityType;
import net.opengis.gml.v_3_2_1.DirectPositionType;
import net.opengis.gml.v_3_2_1.FeaturePropertyType;
import net.opengis.gml.v_3_2_1.LocationPropertyType;
import net.opengis.gml.v_3_2_1.PointType;
import net.opengis.gml.v_3_2_1.TimeInstantPropertyType;
import net.opengis.gml.v_3_2_1.TimeInstantType;
import net.opengis.gml.v_3_2_1.TimePeriodType;
import net.opengis.gml.v_3_2_1.TimePositionType;
import net.opengis.om.v_1_0_0_gml32.ProcessPropertyType;
import net.opengis.swe.v_1_0_1_gml32.PhenomenonPropertyType;
import net.opengis.swe.v_1_0_1_gml32.TimeObjectPropertyType;

import com.eurocontrol.avwx.v_1_1_1.AirspaceType;
import com.eurocontrol.avwx.v_1_1_1.ObjectFactory;
import com.eurocontrol.wx.v_1_1_1.AirTemperatureType;
import com.eurocontrol.wx.v_1_1_1.ForecastType;
import com.eurocontrol.wx.v_1_1_1.ForecastType.ValidTime;
import com.eurocontrol.wx.v_1_1_1.ObservationType;
import com.eurocontrol.wx.v_1_1_1.PressureType;
import com.eurocontrol.wx.v_1_1_1.UomAngleType;
import com.eurocontrol.wx.v_1_1_1.UomDistanceType;
import com.eurocontrol.wx.v_1_1_1.UomPressureType;
import com.eurocontrol.wx.v_1_1_1.UomSpeedType;
import com.eurocontrol.wx.v_1_1_1.UomTemperatureType;
import com.eurocontrol.wx.v_1_1_1.VerticalDistanceType;
import com.eurocontrol.wx.v_1_1_1.WeatherIntensityType;
import com.eurocontrol.wx.v_1_1_1.WindDirectionType;
import com.eurocontrol.wx.v_1_1_1.WindSpeedType;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.gml3_2_1.GeometryConverter;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.AbstractWfsSource;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Abstract base for translation between data records and WXXM GML JAXB objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2012            bclement     Initial creation
 * Jun 18, 2014 2061       bsteffen    Replace Amount with Measure
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public abstract class AbstractWxxm32Translator<T extends PluginDataObject>
        implements IPdoWxxmTranslator {

    protected final Class<T> pdoClass;

    public static final String version = "3.2.1";

    protected static final ObjectFactory avFactory = new ObjectFactory();

    protected static final com.eurocontrol.wx.v_1_1_1.ObjectFactory wxFactory = new com.eurocontrol.wx.v_1_1_1.ObjectFactory();

    protected static final GeometryConverter geoConverter = new GeometryConverter();

    protected static final net.opengis.gml.v_3_2_1.ObjectFactory gmlFactory = new net.opengis.gml.v_3_2_1.ObjectFactory();

    protected static final net.opengis.om.v_1_0_0_gml32.ObjectFactory omFactory = new net.opengis.om.v_1_0_0_gml32.ObjectFactory();

    protected static final net.opengis.swe.v_1_0_1_gml32.ObjectFactory sweFactory = new net.opengis.swe.v_1_0_1_gml32.ObjectFactory();

	// taken from UCAR example
    public static final String AIRCRAFT_URN = "urn:icao:Aircraft:type";

	// FIXME guess based on aircraft URN
	public static final String AIRFRAME_URN = "urn:icao:Airframe:type";

    // From ucar.edu WXCM primer
    public static final String FDC_AIRCRAFT_REPORT = "urn:fdc:icao:procedure:AircraftReport";

    // From ucar.edu WXCM primer
    public static final String ICAO_CODE_PREFIX = "urn:icao:code:weatherStation";

    // FIXME
    public static final String FLIGHT_URN = "";

    public static final double DEFAULT_NULL_VALUE = -9999;

    protected JAXBElement<?> sample;

    protected static final String GEOM_KEY = "location";

    protected static final String TIME_KEY = "time";

    protected static final String TEMP_KEY = "temperature";

    protected static final String WND_DIR_KEY = "windspeed";

    protected static final String WND_SPD_KEY = "winddir";

    protected static final String PRESS_KEY = "pressure";

    protected static final String TURB_KEY = "turbulence";

    protected static final String ICE_KEY = "icing";

    protected static final String DPT_KEY = "dewpoint";

    protected static final String TAIL_KEY = "tailnumber";

    protected static final String STATION_KEY = "stationid";

    protected static final String ALTITUDE_KEY = "altitude";

    protected static final String FLIGHT_KEY = "flightnumber";

    protected static final String DEFAULT_WXXM_TIME_FIELD = "airspaceWxObservation.observation.samplingTime";

    protected static final String DEFAULT_PDO_TIME_FIELD = "dataTime.refTime";

    protected static final Map<String, WeatherIntensityType> intensityMap;

    static {
        Map<String, WeatherIntensityType> map = new HashMap<String, WeatherIntensityType>();
        map.put("", WeatherIntensityType.NONE);
        map.put("NEG", WeatherIntensityType.NONE);
        map.put("SMOOTHLGT", WeatherIntensityType.LIGHT);
        map.put("TRACE", WeatherIntensityType.LIGHT);
        map.put("TRACELGT", WeatherIntensityType.LIGHT);
        map.put("LGT", WeatherIntensityType.LIGHT);
        map.put("LGTMOD", WeatherIntensityType.LIGHT_MODERATE);
        map.put("MOD", WeatherIntensityType.MODERATE);
        map.put("MODSEV", WeatherIntensityType.MODERATE_SEVERE);
        map.put("SEV", WeatherIntensityType.SEVERE);
        map.put("EXTRM", WeatherIntensityType.SEVERE);
        intensityMap = Collections.unmodifiableMap(map);
    }

    protected static final String AIRSPACE_OBSERVED_PROPERTY = "http://www.eurocontrol.int/ont/avwx/1.1/wx.owl#AirspaceWx";

    protected static final String AERODROME_OBSERVED_PROPERTY = "http://www.eurocontrol.int/ont/avwx/1.1/wx.owl#AerodromeWx";

    protected static final String AVIATION_FORECAST_PROCEDURE = "urn:fdc:faa.gov:AviationForecast";

    protected static final String WEATHER_OFFICE_URN = "urn:gov:noaa:nws:weatherOffice";

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractWxxm32Translator.class);

    /**
     * 
     */
    public AbstractWxxm32Translator(Class<T> pdoClass, JAXBElement<?> sample) {
        this.pdoClass = pdoClass;
        this.sample = sample;
    }

    protected static Map<String, String> loadFieldMap(String path) {
        Map<String, String> map = new HashMap<String, String>();

        // Always have the default time mapped
        map.put(DEFAULT_WXXM_TIME_FIELD, DEFAULT_PDO_TIME_FIELD);
        InputStream in = null;
        try {
            // read the rest from the file path param
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext edexStaticBase = pathMgr.getContext(
                    LocalizationContext.LocalizationType.EDEX_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);

            File fieldMapFile = pathMgr.getFile(edexStaticBase, path);

            Properties fieldprops = new Properties();
            in = new FileInputStream(fieldMapFile);
            fieldprops.load(in);

            for (String xpath : fieldprops.stringPropertyNames()) {
                map.put(xpath, fieldprops.getProperty(xpath));
            }
        } catch (Exception e) {
            statusHandler.error("Problem initializing field map", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                }
            }
        }

        return map;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.PdoGmlTranslator#translate(com.raytheon.
     * uf.common.dataplugin.PluginDataObject[])
     */
    @SuppressWarnings("unchecked")
    @Override
    public ArrayList<JAXBElement<?>> translate(PluginDataObject[] pdos)
            throws Exception {
        if (pdos == null) {
            return new ArrayList<JAXBElement<?>>(0);
        }
        ArrayList<JAXBElement<?>> rval = new ArrayList<JAXBElement<?>>(
                pdos.length);
        for (PluginDataObject pdo : pdos) {
            if (pdoClass.isAssignableFrom(pdo.getClass())) {
                rval.add(translate((T) pdo));
            }
        }
        return rval;
    }

    /**
     * Hook method for subclasses to do bulk preprocessing
     * 
     * @param pdos
     * @return
     */
    protected PluginDataObject[] preprocess(PluginDataObject[] pdos) {
        return pdos;
    }

    /**
     * Translate pdo into GML 3.2.1 WXXM Object
     * 
     * @param pdo
     * @return
     * @throws Exception
     */
    public abstract JAXBElement<?> translate(T pdo) throws Exception;

    protected double getNullValue() {
        return DEFAULT_NULL_VALUE;
    }

    /**
     * Create a new code from string and code space
     * 
     * @param str
     * @param codeSpace
     * @return
     */
    protected CodeWithAuthorityType asCode(String str, String codeSpace) {
        CodeWithAuthorityType rval = new CodeWithAuthorityType();
        rval.setValue(str);
        rval.setCodeSpace(codeSpace);
        return rval;
    }

    /**
     * Converts to Pascals if needed
     * 
     * @param amount
     * @return null if amount is null
     */
    protected PressureType getPressure(Measure<?, Pressure> amount) {
        if (amount == null) {
            return null;
        }
        double paValue = amount.doubleValue(SI.PASCAL);
        PressureType rval = new PressureType();
        rval.setValue(paValue);
        rval.setUom(UomPressureType.PA);
        return rval;
    }

    /**
     * @param value
     * @param uom
     * @return null if value is null
     */
    protected <Q extends Quantity> Measure<Double, Q> createAmount(
            Double value, Unit<Q> uom) {
        if (value == null) {
            return null;
        }
        return Measure.valueOf(value, uom);
    }

    /**
     * @param value
     * @param uom
     * @return null if value is null
     */
    protected <Q extends Quantity> Measure<Double, Q> createAmount(
            double value, Unit<Q> uom, double missingVal) {
        if (value == missingVal) {
            return null;
        }
        return Measure.valueOf(value, uom);
    }

    /**
     * @param value
     * @param uom
     * @return null if value is null
     */
    protected <Q extends Quantity> Measure<Integer, Q> createAmount(
            Integer value, Unit<Q> uom) {
        if (value == null) {
            return null;
        }
        return Measure.valueOf(value, uom);
    }

    /**
     * Assumes Celsius units
     * 
     * @param value
     * @return null if value equal to {@link #NULL_VALUE}
     */
    protected AirTemperatureType getAirTemp(Double value) {
        return getAirTemp(value, UomTemperatureType.C);
    }

    /**
     * @param value
     * @param uom
     * @return null if value equal to {@link #NULL_VALUE}
     */
    protected AirTemperatureType getAirTemp(Double value, UomTemperatureType uom) {
        if (value == null || value == getNullValue()) {
            return null;
        }
        AirTemperatureType rval = new AirTemperatureType();
        rval.setUom(UomTemperatureType.C);
        rval.setValue(value);
        return rval;
    }

    /**
     * Converts amount to Celcius if needed
     * 
     * @param amount
     * @return null if amount is null
     */
    protected AirTemperatureType getAirTemp(Measure<?, Temperature> amount) {
        if (amount == null) {
            return null;
        }
        double celValue = amount.doubleValue(SI.CELSIUS);
        return getAirTemp(celValue, UomTemperatureType.C);
    }

    /**
     * Converts amount to Knots if needed
     * 
     * @param amount
     * @return null if amount is null
     */
    protected WindSpeedType getWindSpeed(Measure<?, Velocity> amount) {
        if (amount == null) {
            return null;
        }
        double knotVal = amount.doubleValue(NonSI.KNOT);
        WindSpeedType rval = new WindSpeedType();
        rval.setValue(knotVal);
        rval.setUom(UomSpeedType.KT);
        return rval;
    }

    /**
     * Converts to degrees if needed
     * 
     * @param amount
     * @return null if amount is null
     */
    protected WindDirectionType getWindDir(Measure<?, Angle> amount) {
        if (amount == null) {
            return null;
        }
        double degValue = amount.doubleValue(NonSI.DEGREE_ANGLE);
        WindDirectionType rval = new WindDirectionType();
        rval.setValue(degValue);
        rval.setUom(UomAngleType.DEG);
        return rval;
    }

    /**
     * Wrap reference time in time object property type
     * 
     * @param pdo
     * @return
     */
    protected TimeObjectPropertyType getRefTime(PluginDataObject pdo) {
        DataTime time = pdo.getDataTime();
        Date refTime = time.getRefTime();
        TimeObjectPropertyType rval = new TimeObjectPropertyType();
        TimeInstantType inst = getInstant(refTime);
        JAXBElement<AbstractTimeObjectType> timeObj = gmlFactory
                .createAbstractTimeObject(inst);
        rval.setAbstractTimeObject(timeObj);
        return rval;
    }

    /**
     * Create instant from date
     * 
     * @param d
     * @return
     */
    protected TimeInstantType getInstant(Date d) {
        TimeInstantType inst = new TimeInstantType();
        TimePositionType pos = new TimePositionType();
        String formatted = LayerTransformer.format(d);
        pos.setValue(Arrays.asList(formatted));
        inst.setTimePosition(pos);
        return inst;
    }

    /**
     * Create instant from date
     * 
     * @param d
     * @return
     */
    protected TimeInstantPropertyType getInstantProp(Date d) {
        TimeInstantPropertyType rval = new TimeInstantPropertyType();
        rval.setTimeInstant(getInstant(d));
        return rval;
    }

    /**
     * Create time position from date
     * 
     * @param d
     * @return
     */
    protected TimePositionType getTimePosition(Date d) {
        TimePositionType rval = new TimePositionType();
        rval.setValue(Arrays.asList(LayerTransformer.format(d)));
        return rval;
    }

    /**
     * Sets valid and sampling times for forecast
     * 
     * @param target
     * @param pdo
     */
    protected void setTimes(ForecastType target, Date start, Date end) {
        TimePeriodType time = getTimePeriod(start, end);
        ValidTime vtime = new ValidTime();
        JAXBElement<AbstractTimeObjectType> timeElem = gmlFactory
                .createAbstractTimeObject(time);
        vtime.setAbstractTimeObject(timeElem);
        target.setValidTime(vtime);
        TimeObjectPropertyType prop = new TimeObjectPropertyType();
        prop.setAbstractTimeObject(timeElem);
        target.setSamplingTime(prop);
    }

    /**
     * Create time period from valid period
     * 
     * @param time
     * @return
     */
    protected TimePeriodType getTimePeriod(DataTime time) {
        TimeRange range = time.getValidPeriod();
        return getTimePeriod(range.getStart(), range.getEnd());
    }

    /**
     * Create time period from valid period
     * 
     * @param time
     * @return
     */
    protected TimePeriodType getTimePeriod(Date start, Date end) {
        TimePeriodType rval = new TimePeriodType();
        TimePositionType startpos = getTimePosition(start);
        TimePositionType endpos = getTimePosition(end);
        rval.setBeginPosition(startpos);
        rval.setEndPosition(endpos);
        return rval;
    }

    /**
     * Create aviation forecast procedure
     * 
     * @return
     */
    protected ProcessPropertyType getForecastProcedure() {
        ProcessPropertyType rval = new ProcessPropertyType();
        rval.setHref(AVIATION_FORECAST_PROCEDURE);
        return rval;
    }

    /**
     * Get flight level as vertical distance
     * 
     * @param value
     * @return null if value is null
     */
    protected VerticalDistanceType getFlightLevel(Integer value) {
        if (value == null) {
            return null;
        }
        VerticalDistanceType rval = new VerticalDistanceType();
        rval.setValue(value);
        rval.setUom(UomDistanceType.FL);
        return rval;
    }

    /**
     * Converts to feet if needed
     * 
     * @param amount
     * @return null if amount is null
     */
    protected VerticalDistanceType getVertDist(Measure<?, Length> amount) {
        if (amount == null) {
            return null;
        }
        double ftValue = amount.doubleValue(NonSI.FOOT);
        VerticalDistanceType rval = new VerticalDistanceType();
        rval.setValue(ftValue);
        rval.setUom(UomDistanceType.FT);
        return rval;
    }

    protected JAXBElement<LocationPropertyType> asLocation(Geometry geom) {
        LocationPropertyType lpt = new LocationPropertyType();
        AbstractGeometryType geomType = geoConverter.convert(geom);
        JAXBElement<AbstractGeometryType> gElem = gmlFactory
                .createAbstractGeometry(geomType);
        lpt.setAbstractGeometry(gElem);
        return gmlFactory.createLocation(lpt);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.PdoGmlTranslator#getVersion()
     */
    @Override
    public String getVersion() {
        return version;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.wxxm.PdoWxxmTranslator#getFeatureType()
     */
    @Override
    public WfsFeatureType getFeatureType() {
        QualifiedName name = new QualifiedName(sample.getName());
        String crs = AbstractWfsSource.defaultCRS;
        OgcGeoBoundingBox bbox = AbstractWfsSource.fullBbox;
        return new WfsFeatureType(name, name.getName(), crs, bbox);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.wxxm.PdoWxxmTranslator#getRecordClass()
     */
    @Override
    public Class<? extends PluginDataObject> getRecordClass() {
        return pdoClass;
    }

    /**
     * Create a ProcessPropertyType referencing an external document/type
     * 
     * @param urn
     *            the process urn to reference
     * @return
     */
    public ProcessPropertyType createProcedure(String href) {
        ProcessPropertyType process = omFactory.createProcessPropertyType();
        process.setHref(href);
        return process;
    }

    /**
     * Create a PhenomenonPropertyType referencing an external document/type
     * 
     * @param href
     * @return
     */
    public PhenomenonPropertyType createObservedProperty(String href) {
        PhenomenonPropertyType property = sweFactory
                .createPhenomenonPropertyType();
        property.setHref(href);
        return property;
    }

    /**
     * @param geom
     * @param base
     * @param top
     * @return
     */
    protected FeaturePropertyType createFeatureOfInterest(
            JAXBElement<AbstractGeometryType> geom, Measure<?, Length> base,
            Measure<?, Length> top) {
        FeaturePropertyType rval = new FeaturePropertyType();
        AirspaceType airspaceType = avFactory.createAirspaceType();
        if (base != null) {
            airspaceType.setBase(getVertDist(base));
        }
        if (top != null) {
            airspaceType.setTop(getVertDist(top));
        }
        LocationPropertyType locationPropertyType = gmlFactory
                .createLocationPropertyType();
        locationPropertyType.setAbstractGeometry(geom);
        JAXBElement<LocationPropertyType> location = gmlFactory
                .createLocation(locationPropertyType);
        airspaceType.setLocation(location);
        JAXBElement<AirspaceType> airspace = avFactory
                .createAirspace(airspaceType);
        rval.setAbstractFeature(airspace);
        return rval;
    }

    /**
     * @param aircraftObsLocation
     * @return
     */
    protected FeaturePropertyType createFeatureOfInterest(
            AircraftObsLocation aircraftLocation) {
        return createFeatureOfInterest(aircraftLocation.getLatitude(),
                aircraftLocation.getLongitude(),
                aircraftLocation.getFlightLevel());
    }

    /**
     * @param aircraftObsLocation
     * @return
     */
    protected FeaturePropertyType createFeatureOfInterest(
            SurfaceObsLocation surfaceLocation) {
        return createFeatureOfInterest(surfaceLocation.getLatitude(),
                surfaceLocation.getLongitude(), surfaceLocation.getElevation());
    }

    /**
     * @param aircraftObsLocation
     * @return
     */
    protected FeaturePropertyType createFeatureOfInterest(double latitude,
            double longitude, Integer altitudeMSL) {
        FeaturePropertyType feature = gmlFactory.createFeaturePropertyType();

        // create gml location
        DirectPositionType directPositionType = gmlFactory
                .createDirectPositionType();

        // determine number of dimensions
        int dims = altitudeMSL == null ? 2 : 3;

        // TODO - Bad hard-coded srs, can we get this from the data itself?
        if (dims == 2) {
            directPositionType.setAxisLabels(Arrays.asList("Latitude",
                    "Longitude"));
            directPositionType.setSrsName("urn:ogc:def:crs:EPSG::4326");
            directPositionType.setSrsDimension(new BigInteger("2"));
            directPositionType.setValue(Arrays.asList(new Double(latitude),
                    new Double(longitude)));
        } else {
            directPositionType.setAxisLabels(Arrays.asList("Latitude",
                    "Longitude", "Altitude"));
            directPositionType
                    .setSrsName("urn:ogc:def:crs:EPSG::4326_plus_z_in_m_AMSL");
            directPositionType.setSrsDimension(new BigInteger("3"));

            // TODO - convert from meters MSL to meters above ellipsoid
            directPositionType.setValue(Arrays.asList(new Double(latitude),
                    new Double(longitude), new Double(altitudeMSL)));
        }

        PointType pointType = gmlFactory.createPointType();
        pointType.setPos(directPositionType);
        pointType.setSrsName(directPositionType.getSrsName());
        pointType.setSrsDimension(directPositionType.getSrsDimension());
        pointType.setAxisLabels(directPositionType.getAxisLabels());
        JAXBElement<PointType> point = gmlFactory.createPoint(pointType);

        LocationPropertyType locationPropertyType = gmlFactory
                .createLocationPropertyType();
        locationPropertyType.setAbstractGeometry(point);
        JAXBElement<LocationPropertyType> location = gmlFactory
                .createLocation(locationPropertyType);

        AirspaceType airspaceType = avFactory.createAirspaceType();
        airspaceType.setLocation(location);
        JAXBElement<AirspaceType> airspace = avFactory
                .createAirspace(airspaceType);

        feature.setAbstractFeature(airspace);
        return feature;
    }

    /**
     * Add procedure, observed property, and feature of interest to observation
     * 
     * @param obs
     * @param location
     */
    protected void addAircraftObsInformation(ObservationType obs,
            AircraftObsLocation location) {
        // Add procedure, this is currently the same in AIREP, PIREP and ACARS
        String procedure = FDC_AIRCRAFT_REPORT;
        obs.setProcedure(createProcedure(procedure));
        // Add observed property
        // This is potentially from the ontology, like
        // http://www.eurocontrol.int/ont/avwx/1.1/wx.owl#AirspaceWx
        obs.setObservedProperty(createObservedProperty(AIRSPACE_OBSERVED_PROPERTY));
        // add feature of interest, for pirep we are using aircraft location
        obs.setFeatureOfInterest(createFeatureOfInterest(location));
    }

}
