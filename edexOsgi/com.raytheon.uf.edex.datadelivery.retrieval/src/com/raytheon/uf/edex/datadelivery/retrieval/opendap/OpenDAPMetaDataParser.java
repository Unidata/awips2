package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.datadelivery.retrieval.util.LookupManager;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterConfig;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterLookup;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.Link;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.MetaDataParser;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDAPMetaDataExtracter.DAP_TYPE;
import com.vividsolutions.jts.geom.Coordinate;

import dods.dap.AttributeTable;
import dods.dap.DAS;

/**
 * Parse OpenDAP MetaData. This class should remain package-private, all access
 * should be limited through the {@link OpenDapServiceFactory}.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218     dhladky      Initial creation
 * Jul 23, 2012            djohnson     Use collection name for dataset name.
 * Aug 10, 2012 1022       djohnson     {@link DataSetMetaData} requires provider name.
 * Aug 15, 2012 0743       djohnson     Set the date on {@link DataSetMetaData}.
 * Aug 22, 2012 0743       djohnson     Store data type as an enum.
 * Aug 31, 2012 1125       djohnson     Rename getCollectionAndCycle() to getDataSetNameAndCycle().
 * Sep 24, 2012 1209       djohnson     If no parseable cycle found, set NO_CYCLE.
 * Oct 20, 2012 1163       dhladky      Updated lookup table generation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 12, 2012 supplement dhladky      Restored operation of ensembles.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 08, 2013            dhladky      Performance enhancements, specific model fixes.
 * Jan 18, 2013 1513       dhladky      Level look up improvements.
 * Jan 24, 2013 1527       dhladky      Changed 0DEG to FRZ
 * Sept 25, 2013 1797      dhladky      separated time from gridded time
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * Dec 18, 2013 2636       mpduff       Calculate a data availability delay for the dataset.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

class OpenDAPMetaDataParser extends MetaDataParser {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDAPMetaDataParser.class);

    OpenDAPMetaDataParser() {
        serviceConfig = HarvesterServiceManager.getInstance().getServiceConfig(
                ServiceType.OPENDAP);
    }

    /**
     * Gets the levels for types we are aware of. {@link DataLevelType} Right
     * now it can only recognize MB (pressure levels) for DAP types. It also
     * rudimentarily recognizes Heights Above Sea Level, SEAB. dz = number of
     * levels levMin = minimum level value levMax = maximum level value
     * 
     * @param type
     * @param collectionName
     * @param gdsmd
     * @param dz
     * @param levMin
     * @param levMax
     * @return
     */
    private Levels getLevels(DataLevelType type, String collectionName,
            GriddedDataSetMetaData gdsmd, Double dz, Float levMin, Float levMax) {

        Levels levels = new Levels();

        final LevelType levelType = type.getType();
        try {

            levels.setName(levelType.toString());
            levels.setLevelType(type.getId());

            if (!LookupManager.getInstance().levelLookupExists(collectionName)) {
                // create new default lookups
                if (levelType.equals(LevelType.MB)
                        || levelType.equals(LevelType.SEAB)) {

                    List<Double> levelList = OpenDAPParseUtility.getInstance()
                            .parseLevels(gdsmd.getUrl(),
                                    serviceConfig.getConstantValue("LEV"));
                    LookupManager.getInstance().modifyLevelLookups(
                            collectionName, dz, levMin, levMax, levelList);
                }
            }

            if (levelType.equals(LevelType.MB)) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);

            } else if (levelType.equals(LevelType.SEAB)) {
                List<Double> levelList = LookupManager.getInstance()
                        .getLevels(collectionName).getLevelXml();
                levels.setLevel(levelList);
            } else {
                // default added when only one
                levels.addLevel(Double.NaN);
            }

            levels.setDz(dz);

            // TODO: Add more level type, if and when they exist
            if (!gdsmd.getLevelTypes().containsKey(type)) {
                gdsmd.addLevelType(type, levels);
            }

        } catch (Exception e) {
            logParsingException(levelType.toString(), "Level info",
                    collectionName, gdsmd.getUrl());
        }

        return levels;
    }

    /**
     * Process parameters against lookups and DAP constants
     * 
     * @param das
     * @param dataSet
     * @param gdsmd
     * @param link
     * @param collection
     * @param dataDateFormat
     * @return
     */
    private Map<String, Parameter> getParameters(DAS das,
            GriddedDataSet dataSet, GriddedDataSetMetaData gdsmd, Link link,
            Collection collection, String dataDateFormat) {

        final String collectionName = dataSet.getCollectionName();
        final String url = gdsmd.getUrl();
        double dz = 0.00;
        float levMin = 0.0f;
        float levMax = 0.0f;
        boolean hasLevels = false;
        final Coordinate upperLeft = new Coordinate();
        final Coordinate lowerRight = new Coordinate();
        final GriddedCoverage griddedCoverage = new GriddedCoverage();
        dataSet.setCoverage(griddedCoverage);

        final GridCoverage gridCoverage = collection.getProjection()
                .getGridCoverage();
        // TODO: haven't figure out how to tell the difference on these from
        // the provider metadata yet
        gridCoverage.setSpacingUnit(serviceConfig.getConstantValue("DEGREE"));
        gridCoverage.setFirstGridPointCorner(Corner.LowerLeft);
        ParameterLookup plx = LookupManager.getInstance().getParameters(
                collectionName);
        // keep track of any new params you run across
        ArrayList<Parameter> newParams = new ArrayList<Parameter>();

        Map<String, Parameter> parameters = new HashMap<String, Parameter>();
        final String timecon = serviceConfig.getConstantValue("TIME");
        final String size = serviceConfig.getConstantValue("SIZE");
        final String minimum = serviceConfig.getConstantValue("MINIMUM");
        final String maximum = serviceConfig.getConstantValue("MAXIMUM");
        final String time_step = serviceConfig.getConstantValue("TIME_STEP");
        final String lat = serviceConfig.getConstantValue("LAT");
        final String lon = serviceConfig.getConstantValue("LON");
        final String resolution = serviceConfig.getConstantValue("RESOLUTION");
        final String lev = serviceConfig.getConstantValue("LEV");
        final String nc_global = serviceConfig.getConstantValue("NC_GLOBAL");
        final String data_type = serviceConfig.getConstantValue("DATA_TYPE");
        final String title = serviceConfig.getConstantValue("TITLE");
        final String ens = serviceConfig.getConstantValue("ENS");
        final String long_name = serviceConfig.getConstantValue("LONG_NAME");
        final String missing_value = serviceConfig
                .getConstantValue("MISSING_VALUE");
        final String fill_value = serviceConfig.getConstantValue("FILL_VALUE");
        final String fill = new Float(GridUtil.GRID_FILL_VALUE).toString();

        // process globals first
        // process time
        if (das.getAttributeTable(timecon) != null) {
            try {
                AttributeTable at = das.getAttributeTable(timecon);
                GriddedTime time = new GriddedTime();
                // format of time
                time.setFormat(dataDateFormat);
                // number of times
                time.setNumTimes(new Integer(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(size).getValueAt(0))).intValue());
                // minimum time val
                time.setStartDate(OpenDAPParseUtility.getInstance().parseDate(
                        at.getAttribute(minimum).getValueAt(0)));
                // maximum time val
                time.setEndDate(OpenDAPParseUtility.getInstance().parseDate(
                        at.getAttribute(maximum).getValueAt(0)));
                // step
                List<String> step = OpenDAPParseUtility
                        .getInstance()
                        .parseTimeStep(at.getAttribute(time_step).getValueAt(0));
                time.setStep(new Double(step.get(0)).doubleValue());
                time.setStepUnit(Time.findStepUnit(step.get(1))
                        .getDurationUnit());
                gdsmd.setTime(time);

                // Calculate dataset availability delay
                long startMillis = time.getStart().getTime();
                long now = TimeUtil.newGmtCalendar().getTimeInMillis();
                long offset = (now - startMillis) / TimeUtil.MILLIS_PER_MINUTE;
                dataSet.setAvailabilityOffset((int) offset);

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.debug("Dataset Name: "
                            + dataSet.getDataSetName());
                    statusHandler.debug("StartTime:    " + time.getStart());
                    statusHandler.debug("Offset:       "
                            + dataSet.getAvailabilityOffset());
                }
            } catch (Exception le) {
                logParsingException(timecon, "Time", collectionName, url);
            }
        }
        // process latitude
        if (das.getAttributeTable(lat) != null) {
            try {
                AttributeTable at = das.getAttributeTable(lat);
                // ny
                gridCoverage.setNy(new Integer(OpenDAPParseUtility
                        .getInstance()
                        .trim(at.getAttribute(size).getValueAt(0))).intValue());
                // dy
                gridCoverage.setDy(new Float(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(resolution).getValueAt(0)))
                        .floatValue());
                // first latitude point
                gridCoverage.setLa1(new Double(OpenDAPParseUtility
                        .getInstance().trim(
                                at.getAttribute(minimum).getValueAt(0)))
                        .doubleValue());

                upperLeft.y = new Double(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(maximum).getValueAt(0)))
                        .doubleValue();

                lowerRight.y = new Double(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(minimum).getValueAt(0)))
                        .doubleValue();

            } catch (Exception le) {
                logParsingException(lat, "Latitude", collectionName, url);
            }
        }
        // process longitude
        if (das.getAttributeTable(lon) != null) {
            try {
                AttributeTable at = das.getAttributeTable(lon);
                // nx
                gridCoverage.setNx(new Integer(OpenDAPParseUtility
                        .getInstance()
                        .trim(at.getAttribute(size).getValueAt(0))).intValue());
                // dx
                gridCoverage.setDx(new Float(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(resolution).getValueAt(0)))
                        .floatValue());
                // min Lon
                double minLon = new Double(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(minimum).getValueAt(0)))
                        .doubleValue();
                // max Lon
                double maxLon = new Double(OpenDAPParseUtility.getInstance()
                        .trim(at.getAttribute(maximum).getValueAt(0)))
                        .doubleValue();

                gridCoverage.setLo1(minLon);
                upperLeft.x = minLon;
                lowerRight.x = maxLon;

            } catch (Exception le) {
                logParsingException(lon, "Longitude", collectionName, url);
            }
        }
        // process level settings
        if (das.getAttributeTable(lev) != null) {
            try {
                AttributeTable at = das.getAttributeTable(lev);
                dz = new Double(OpenDAPParseUtility.getInstance().trim(
                        at.getAttribute(resolution).getValueAt(0)))
                        .doubleValue();
                levMin = new Float(OpenDAPParseUtility.getInstance().trim(
                        at.getAttribute(minimum).getValueAt(0))).floatValue();
                levMax = new Float(OpenDAPParseUtility.getInstance().trim(
                        at.getAttribute(maximum).getValueAt(0))).floatValue();
                hasLevels = true;

            } catch (Exception le) {
                logParsingException(lev, "Levels", collectionName, url);
            }
        }
        // process any other globals
        if (das.getAttributeTable(nc_global) != null) {
            try {
                AttributeTable at = das.getAttributeTable(nc_global);
                dataSet.setDataSetType(DataType
                        .valueOfIgnoreCase(OpenDAPParseUtility.getInstance()
                                .trim(at.getAttribute(data_type).getValueAt(0))));
                String description = at.getAttribute(title).getValueAt(0);
                gdsmd.setDataSetDescription(OpenDAPParseUtility.getInstance()
                        .trim(description));
            } catch (Exception ne) {
                logParsingException(nc_global, "Global Dataset Info",
                        collectionName, url);
            }
        }
        // process ensembles
        if (das.getAttributeTable(ens) != null) {
            try {
                AttributeTable at = das.getAttributeTable(ens);
                dataSet.setEnsemble(OpenDAPParseUtility.getInstance()
                        .parseEnsemble(at));
            } catch (Exception en) {
                logParsingException(ens, "Ensemble", collectionName, url);
            }
        }

        // process the parameters
        for (Enumeration<?> e = das.getNames(); e.hasMoreElements();) {

            String name = (String) e.nextElement();
            // filter out globals
            if (!name.equals(ens) && !name.equals(nc_global)
                    && !name.equals(lev) && !name.equals(lon)
                    && !name.equals(lat) && !name.equals(timecon)) {

                // regular parameter parsing
                try {

                    AttributeTable at = das.getAttributeTable(name);
                    Parameter parm = new Parameter();
                    parm.setDataType(dataSet.getDataSetType());

                    if (plx != null) {
                        ParameterConfig paramLookUp = plx
                                .getParameterByProviderName(name);
                        if (paramLookUp != null) {
                            parm.setName(paramLookUp.getAwips());
                            parm.setProviderName(paramLookUp.getGrads());
                        }
                    }

                    // UNKNOWN PARAMETER!!!!!!
                    // Add it to the list used for lookup
                    if (parm.getName() == null
                            || parm.getProviderName() == null) {
                        parm.setName(name);
                        parm.setProviderName(name);
                        newParams.add(parm);
                    }

                    // TODO: Figure out why some NCDC datasets have no param
                    // descriptions, default fill, or missing vals.
                    String description = "unknown description";
                    try {
                        description = OpenDAPParseUtility.getInstance().trim(
                                at.getAttribute(long_name).getValueAt(0));

                    } catch (Exception iae) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Invalid DAP description block! " + name);
                    }

                    parm.setDefinition(description);
                    parm.setUnits(OpenDAPParseUtility.getInstance().parseUnits(
                            description));

                    try {
                        parm.setMissingValue(OpenDAPParseUtility.getInstance()
                                .trim(at.getAttribute(missing_value)
                                        .getValueAt(0)));
                    } catch (Exception iae) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Invalid DAP missing value block! " + name);
                        parm.setMissingValue(fill);
                    }

                    try {
                        parm.setFillValue(OpenDAPParseUtility
                                .getInstance()
                                .trim(at.getAttribute(fill_value).getValueAt(0)));
                    } catch (Exception iae) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Invalid DAP fill value block! " + name);
                        parm.setMissingValue(fill);
                    }

                    DataLevelType type = parseLevelType(parm, hasLevels);
                    parm.setLevels(getLevels(type, collectionName, gdsmd, dz,
                            levMin, levMax));
                    parm.addLevelType(type);
                    parameters.put(name, parm);

                } catch (Exception le) {
                    logParsingException(name, "Parameter", collectionName, url);
                }
            }
        }

        // If necessary, update the parameter lookups
        if (!newParams.isEmpty()) {
            LookupManager.getInstance().modifyParamLookups(collectionName,
                    newParams);
        }

        String nameAndDescription = link.getName() + "_Coverage_"
                + gridCoverage.getNx() + "_X_" + gridCoverage.getNy() + "_Y_"
                + gridCoverage.getProjectionType();
        gridCoverage.setName(nameAndDescription);

        try {
            gridCoverage.initialize();
        } catch (GridCoverageException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        griddedCoverage.setGridCoverage(gridCoverage);

        return parameters;
    }

    /**
     * Logs a parsing exception.
     * 
     * @param name
     *            the OpenDAP field
     * @param object
     *            the name of the type of object being parsed
     */
    private void logParsingException(String name, String object,
            String collectionName, String url) {
        statusHandler.handle(Priority.PROBLEM, " Couldn't parse " + object
                + ": " + name + " dataset: " + collectionName + " url: " + url);
    }

    /**
     * Get the correct level type
     * 
     * @param param
     * @return
     */
    private DataLevelType parseLevelType(Parameter param, boolean hasLevels) {

        // find first word
        DataLevelType type = null;
        String[] s1 = param.getDefinition().substring(3).split(" ");

        // SEA ICE special case
        if (param.getDefinition().contains(LevelType.SEAB.getLevelType())) {
            type = new DataLevelType(LevelType.SEAB);
        }
        // fixed height, only one layer 2m AGL
        else if (param.getProviderName().endsWith(
                serviceConfig.getConstantValue("TWOM"))) {
            type = new DataLevelType(LevelType.FHAG);
            type.addLayer(new Double(2).doubleValue());
            type.setUnit(serviceConfig.getConstantValue("METER"));
        }
        // fixed height, only one layer 10m AGL
        else if (param.getProviderName().endsWith(
                serviceConfig.getConstantValue("TENM"))) {
            type = new DataLevelType(LevelType.FHAG);
            type.addLayer(new Double(10).doubleValue());
            type.setUnit(serviceConfig.getConstantValue("METER"));
        }
        // FRZ freezing level, catches one's with on the end of the param name
        // hgt0c etc
        else if (param.getProviderName().endsWith(LevelType.FRZ.getLevelType())) {
            type = new DataLevelType(LevelType.FRZ);
        }

        // Really special cases presented by NOMADS data sets
        if (type == null) {

            String w1 = s1[0];
            String w2 = s1[1];
            if (w1.contains("=")) {
                String[] s2 = w1.split("=");
                if (s2[0].startsWith(LevelType.PVL.getLevelType())) {
                    type = new DataLevelType(LevelType.PVL);
                    type.addLayer(new Double(s2[1]).doubleValue());
                    type.setUnit(s1[1]);
                }
            } else {
                String[] s2 = w1.split("-");
                // layer case
                if (s2.length == 2) {

                    // fixed height above ground level
                    try {
                        type = new DataLevelType(LevelType.FHAG);
                        type.addLayer(new Double(s2[0]).doubleValue());
                        type.addLayer(new Double(s2[1]).doubleValue());
                        type.setUnit(s1[1]);
                    } catch (Exception se) {
                        // not numbers
                        if (w1.equals(LevelType.U.getLevelType())) {
                            type = new DataLevelType(LevelType.U);
                        } else if (w1.equals(LevelType.V.getLevelType())) {
                            type = new DataLevelType(LevelType.V);
                        }
                    }

                } else {
                    Double wVal = null;

                    try {
                        wVal = new Double(w1).doubleValue();

                        // sigma level
                        if (LevelType.SIGL.getLevelType().equals(w2)) {
                            type = new DataLevelType(LevelType.SIGL);
                            type.addLayer(wVal);
                            type.setUnit(serviceConfig
                                    .getConstantValue("KELVIN"));
                        } else {
                            // fixed height, only one layer
                            type = new DataLevelType(LevelType.FHAG);
                            type.addLayer(wVal);
                            type.setUnit(w2);
                        }

                    } catch (Exception e) {
                        // first not an Integer, other layers

                        if (w1.startsWith("(")) {
                            type = new DataLevelType(LevelType.MB);
                        } else if (w1.equals(LevelType.SFC.getLevelType())) {
                            type = new DataLevelType(LevelType.SFC);
                        } else if (w1.equals(LevelType.TROP.getLevelType())) {
                            type = new DataLevelType(LevelType.TROP);
                        } else if (w1.equals(LevelType.MAXW.getLevelType())) {
                            type = new DataLevelType(LevelType.MAXW);
                        } else if (w1.equals(LevelType.HSCLW.getLevelType())) {
                            type = new DataLevelType(LevelType.HSCLW);
                        } else if (w1.equals(LevelType.LSCLW.getLevelType())) {
                            type = new DataLevelType(LevelType.LSCLW);
                        } else if (w1.equals(LevelType.EL.getLevelType())) {
                            type = new DataLevelType(LevelType.EL);
                        } else if (w1.equals(LevelType.CCL.getLevelType())) {
                            type = new DataLevelType(LevelType.CCL);
                        } else if (w1.equals(LevelType.CBL.getLevelType())) {
                            type = new DataLevelType(LevelType.CBL);
                        } else if (w1.equals(LevelType.CTL.getLevelType())) {
                            type = new DataLevelType(LevelType.CTL);
                        } else if (w1.equals(LevelType.MSL.getLevelType())) {
                            type = new DataLevelType(LevelType.MSL);
                        } else if (w1.equals(LevelType.EA.getLevelType())) {
                            type = new DataLevelType(LevelType.EA);
                        } else if (w1.equals(LevelType.FRZ.getLevelType())) {
                            type = new DataLevelType(LevelType.FRZ);
                        } else if (w1.equals(LevelType.LCY.getLevelType())) {
                            type = new DataLevelType(LevelType.LCY);
                        } else if (w1.equals(LevelType.MCY.getLevelType())) {
                            type = new DataLevelType(LevelType.MCY);
                        } else if (w1.equals(LevelType.HCY.getLevelType())) {
                            type = new DataLevelType(LevelType.HCY);
                        } else if (w1.equals(LevelType.PBL.getLevelType())) {
                            type = new DataLevelType(LevelType.PBL);
                        } else if (w1.equals(LevelType.MWSL.getLevelType())) {
                            type = new DataLevelType(LevelType.MWSL);
                        }
                    }
                }
            }
        }

        if (type == null) {

            // If description contains surface, make the assumption it is
            // surface
            if (param.getDefinition().contains(LevelType.SFC.getLevelType())) {
                type = new DataLevelType(LevelType.SFC);
            }

            // We'll make the assumption it is a MB based level for a last gasp
            if (hasLevels && type == null) {
                type = new DataLevelType(LevelType.MB);
            }

            if (type == null) {
                type = new DataLevelType(LevelType.UNKNOWN);
            }
        }

        return type;
    }

    @Override
    public List<DataSetMetaData<?>> parseMetaData(Provider provider,
            LinkStore store, Collection collection, String dataDateFormat) {

        final Map<OpenDapGriddedDataSet, List<DataSetMetaData<?>>> metaDatas = new HashMap<OpenDapGriddedDataSet, List<DataSetMetaData<?>>>();

        Set<String> linkKeys = new TreeSet<String>(store.getLinkKeys());

        if (CollectionUtil.isNullOrEmpty(linkKeys)) {
            return null;
        }

        for (String linkKey : linkKeys) {
            OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();
            dataSet.setCollectionName(collection.getName());
            dataSet.setProviderName(provider.getName());

            final GriddedDataSetMetaData gdsmd = new OpenDapGriddedDataSetMetaData();

            Link link = store.getLink(linkKey);

            List<String> vals = null;
            try {
                vals = OpenDAPParseUtility.getInstance()
                        .getDataSetNameAndCycle(linkKey, collection.getName());
            } catch (Exception e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to get cycle and dataset name set...", e1);
            }
            dataSet.setDataSetName(vals.get(0));
            gdsmd.setDataSetName(dataSet.getDataSetName());
            gdsmd.setProviderName(dataSet.getProviderName());

            // Reuse existing dataset if we've already parsed it
            Set<OpenDapGriddedDataSet> keySet = metaDatas.keySet();
            if (keySet.contains(dataSet)) {
                for (OpenDapGriddedDataSet existingDataSet : keySet) {
                    if (existingDataSet.equals(dataSet)) {
                        dataSet = existingDataSet;
                        break;
                    }
                }
            }

            DAS das = (DAS) link.getLinks().get(DAP_TYPE.DAS.getDapType());
            // set url first, used for level lookups
            gdsmd.setUrl(link.getUrl().replace(
                    serviceConfig.getConstantValue("META_DATA_SUFFIX"),
                    serviceConfig.getConstantValue("BLANK")));
            dataSet.setParameters(getParameters(das, dataSet, gdsmd, link,
                    collection, dataDateFormat));
            GriddedTime dataSetTime = gdsmd.getTime();
            if (dataSetTime == null) {
                throw new IllegalStateException(
                        "The time cannot be null for a DataSetMetaData object!");
            }
            dataSet.setTime(dataSetTime);
            gdsmd.setDate(new ImmutableDate(dataSetTime.getStart()));

            List<Integer> forecastHoursAsInteger = new ArrayList<Integer>();
            for (String forecastHour : gdsmd.getTime().getFcstHours()) {
                try {
                    forecastHoursAsInteger.add(Integer.parseInt(forecastHour));
                } catch (NumberFormatException nfe) {
                    statusHandler.warn("Unable to parse [" + forecastHour
                            + "] as an integer!");
                }
            }
            dataSet.getForecastHours().addAll(forecastHoursAsInteger);

            // The opendap specific info
            Map<String, String> cyclesToUrls = new HashMap<String, String>();
            String cycle = vals.get(1);
            cyclesToUrls.put(cycle, gdsmd.getUrl());

            String cycleAsParseableNum = vals.get(2);

            int cycleAsNum = GriddedDataSetMetaData.NO_CYCLE;
            if (cycleAsParseableNum != null) {
                try {
                    cycleAsNum = Integer.parseInt(cycleAsParseableNum);

                    // Only add the cycle to the dataset if it is NOT the
                    // NO_CYCLE, since that is the object the GUI uses
                    dataSet.getCycles().add(cycleAsNum);
                } catch (NumberFormatException nfe) {
                    // This should never happen since we check for it in the
                    // OpenDAPConstants class, but that is why we freak out if
                    // it DOES happen
                    throw new IllegalArgumentException(nfe.getMessage(), nfe);
                }
            }

            // The NO_CYCLE constant will always update the single url for daily
            // release models, otherwise the parsed cycle is used
            gdsmd.setCycle(cycleAsNum);
            dataSet.cycleUpdated(cycleAsNum);
            dataSet.getCyclesToUrls().put(cycleAsNum, gdsmd.getUrl());

            if (dataSet.getTime() == null) {
                throw new IllegalStateException(
                        "The time cannot be null for a DataSet object!");
            }

            List<DataSetMetaData<?>> toStore = metaDatas.get(dataSet);
            if (toStore == null) {
                toStore = new ArrayList<DataSetMetaData<?>>();
                metaDatas.put(dataSet, toStore);
            }
            toStore.add(gdsmd);
        }

        List<DataSetMetaData<?>> parsedMetadatas = new ArrayList<DataSetMetaData<?>>();
        for (DataSet<GriddedTime, GriddedCoverage> dataSet : metaDatas.keySet()) {
            List<DataSetMetaData<?>> dataSetMetaDatas = metaDatas.get(dataSet);

            storeDataSet(dataSet);
            storeMetaData(dataSetMetaDatas, dataSet);

            parsedMetadatas.addAll(dataSetMetaDatas);
        }

        return parsedMetadatas;
    }

}
