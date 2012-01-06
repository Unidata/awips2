package gov.noaa.nws.ncep.viz.cloudHeight;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.GempakConstants;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.MissingValueTester;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PCLibrary;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PCLibrary.LevelType;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PSLibrary;
import gov.noaa.nws.ncep.viz.cloudHeight.CloudHeightResource.StationData;
import gov.noaa.nws.ncep.viz.cloudHeight.soundings.SoundingLevels.LevelValues;
import gov.noaa.nws.ncep.viz.cloudHeight.soundings.SoundingModel;
import gov.noaa.nws.ncep.viz.cloudHeight.soundings.SoundingModelReader;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.ComputationalMethod;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.PixelValueMethod;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.SoundingDataSourceType;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.ICloudHeightCapable;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.McidasFileBasedTileSet;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.McidasSatResource;
import gov.noaa.nws.ncep.viz.rsc.satellite.units.NcIRPixelToTempConverter;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;

import java.awt.Rectangle;
import java.io.File;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.xml.bind.JAXBException;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.BufferUtil;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.rsc.hdf5.FileBasedTileSet;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Cloud Height Processor
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/19/09		 #106		Greg Hull		Created
 * 07/22/09					M. Li		TO10 -> TO11
 * 09/27/09      #169       Greg Hull   NCMapEditor
 * 03/04/2009				M. Gao		Using localization extension to replace NmapCommon class
 * 05/23/2010               G. Hull     Use ICloudHeightCapable. Use UnitConverter (don't assume Celsius)
 * 11/18/2010	 327        M. Li	    add isAlreadyOpen
 * 01/05/2011   393     Archana    Added logic to compute the cloud-height using station data
 * 02/28/2011   393    Archana     Added logic to compute the cloud height
 *                                                   using the moist adiabatic method
 * 03/09/2011   393    Archana     Added logic to implement pixel selection from a pixel
 *                                                   area around the user clicked point.
 * </pre>
 * 
 * @version 1
 */
public class CloudHeightProcesser {

    private CloudHeightDialog cldHghtDlg = null;

    private CloudHeightResource cldHghtRsc = null; // should the dlg or a mngr
                                                   // class have the rsc?

    private ICloudHeightCapable satRsc = null;

    private UnitConverter tempUnitsConverter = null;

    private UnitConverter celsiusToKelvinConverter = null;

    private NcIRPixelToTempConverter pixelToTemperatureConverter = null;

    // private NCMapEditor mapEditor;
    private NCDisplayPane seldPane;

    private int maxIntervalInHoursForValidStationData;

    private ArrayList<SoundingModel> sndingModels = null;

    private String sndingSrcStr = null;

    private String sndingSrcTimeStr = null;

    private double sndingSrcDist;

    private String pixValStr = "N/A"; // the 'raw' data

    private DataTime satelliteImageTime = null;

    // options (real defaults not set here)
    private SoundingDataSourceType sndingDataSrc = SoundingDataSourceType.STANDARD_ATM;

    private Double maxSndingDist = null;

    private ComputationalMethod compMthd = ComputationalMethod.STANDARD;

    private boolean useSnglPix = false;

    private int pixAreaRad = 10;

    private PixelValueMethod pixValMthd = PixelValueMethod.MAX_VALUE;

    private String currSndingDataSource = null;

    private List<LevelValues> soundingData = null;

    private List<NcSoundingLayer> aListOfNcSoundingLayers = null;

    StationData stnData = null;

    public static class CloudHeightData {
        protected double cloudHght; // in meters

        protected double cloudPres;

        CloudHeightData(double hght, double prs) {
            cloudHght = hght;
            cloudPres = prs;
        }
    }

    public CloudHeightProcesser(NCDisplayPane p, CloudHeightDialog dlg) {
        seldPane = p;
        cldHghtDlg = dlg;
        aListOfNcSoundingLayers = new ArrayList<NcSoundingLayer>(0);
        // tell the dialog what units the data values will be in.
        //
        cldHghtDlg.setWorkingUnits(SI.METER, SI.CELSIUS, SI.METER);
        celsiusToKelvinConverter = SI.CELSIUS.getConverterTo(SI.KELVIN);
        pixelToTemperatureConverter = new NcIRPixelToTempConverter();
        // set cldHghtRsc and satRsc
        getResources();

        SoundingModelReader sndingMdlRdr = new SoundingModelReader(
                ((LocalizationManager) LocalizationManager.getInstance())
                        .getFilename("soundingModelsTable"));

        try {
            sndingModels = (ArrayList<SoundingModel>) sndingMdlRdr
                    .getSoundingModels();
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    public void setPane(NCDisplayPane newPane) {
        if (seldPane == newPane) {
            return;
        }

        removeCloudHeightResource();

        seldPane = newPane;

        getResources();
    }

    public void processCloudHeight(Coordinate latlon, boolean mouseDown) {
        List<CloudHeightData> cloudHeights = new ArrayList<CloudHeightProcesser.CloudHeightData>(
                0);
        if (cldHghtDlg != null && cldHghtDlg.isOpen() && latlon != null) {

            cldHghtDlg.displayStatusMsg("");

            if (satRsc == null) {
                getResources();

                if (satRsc == null) {
                    cldHghtDlg.clearFields();
                    cldHghtDlg
                            .displayStatusMsg("Satellite IR Image is not loaded.");
                    return;
                }
            }

            if (cldHghtRsc == null) { // a stupid user could have unloaded the
                                      // cloud height resource
                getResources();
            }

            String pixValStr = "N/A"; // the 'raw' data
            Double tempC = new Double(0.0);
            Double pixVal = Double.NaN;

            // get the options
            if (mouseDown) {
                sndingDataSrc = cldHghtDlg.getSoundingDataSourceType();
                maxSndingDist = cldHghtDlg.getMaxSoundingDist(); // in working
                                                                 // units of
                                                                 // meters

                compMthd = cldHghtDlg.getComputationalMethod();
                pixValMthd = cldHghtDlg.getPixelValueMethod();
                pixAreaRad = cldHghtDlg.getPixelAreaDimension();
                useSnglPix = cldHghtDlg.isPixelValueFromSinglePixel();
            }

            pixVal = getPixelValueFromTheUserClickedCoordinate(latlon, satRsc,
                    useSnglPix, pixAreaRad, pixValMthd);

            if (pixVal != null) {
                pixValStr = pixVal.toString();
            }

            Double tempK = null;
            if (pixelToTemperatureConverter != null && pixVal != null)
                tempK = pixelToTemperatureConverter.convert(pixVal
                        .doubleValue());

            if (tempK == null) {
                cldHghtDlg
                        .displayStatusMsg("Error: Unable to compute the brightness temperature.");
                return;
            } else
                tempC = SI.KELVIN.getConverterTo(SI.CELSIUS).convert(
                        tempK.doubleValue());

            // get the sounding data
            if (sndingDataSrc == SoundingDataSourceType.STANDARD_ATM) {

                sndingSrcStr = new String("Standard Atm");
                sndingSrcTimeStr = new String("N/A");
                sndingSrcDist = Double.NaN; // flag to display empty

                if (currSndingDataSource != "Standard") {
                    currSndingDataSource = "Standard";

                    for (SoundingModel sndMod : sndingModels) {
                        // TODO : Add checks for Summer/Winter and valid region
                        //
                        if (sndMod.getName().equalsIgnoreCase("Standard")) {
                            // if( sndMod.getNeLat() > latlon.y ) ...
                            soundingData = (ArrayList<LevelValues>) sndMod
                                    .getSoundingLevels().getLevelValues();
                            break;
                        }
                    }
                }
            } else {
                soundingData = getSoundingDataFromStationData(latlon);
            }

            if (compMthd.compareTo(ComputationalMethod.STANDARD) == 0) {
                cloudHeights = computeCloudHeights(soundingData, tempC);
            }

            if (((cloudHeights != null) && cloudHeights.isEmpty())
                    || (compMthd.compareTo(ComputationalMethod.MOIST_ADIABATIC) == 0)) {
                if (aListOfNcSoundingLayers != null) {
                    float tempInKelvin = (float) celsiusToKelvinConverter
                            .convert(tempC);
                    if (!(aListOfNcSoundingLayers.isEmpty()))
                        cloudHeights = computeCloudHeightByMoistAdiabaticMethod(
                                aListOfNcSoundingLayers, tempInKelvin);

                    /*
                     * If the moist adiabatic method is unable to return a valid
                     * cloud height or if no station data is returned for the
                     * current frame time ( this happens, because even if there
                     * is UAIR data in the database, if the 'nil' field in the
                     * uair table is set to TRUE, the UAIR data is considered to
                     * be invalid and therefore not retrieved by NcSoundingQuery
                     * ), so we go back by
                     * 'maxIntervalInHoursForValidStationData' hours to compute
                     * a fresh set of the sounding data
                     */
                    if (cloudHeights.isEmpty()
                            || (aListOfNcSoundingLayers.isEmpty()))
                        cloudHeights = computeCloudHeightFromPreviousValidSatationDataForClosestStation(
                                stnData, maxIntervalInHoursForValidStationData,
                                satelliteImageTime, tempInKelvin);

                }

            }
            if (tempUnitsConverter != null && tempC != null)
                // Convert the temperature into the units selected by the user
                tempC = tempUnitsConverter.convert(tempC);

            // Update the GUI.
            cldHghtDlg.setLatLon(latlon.y, latlon.x);

            cldHghtDlg.setSoundingDataSource(sndingSrcStr);

            cldHghtDlg.setSoundingDataTime(sndingSrcTimeStr);

            cldHghtDlg.setSoundingDataDistance(sndingSrcDist);

            cldHghtDlg.setPixelValue(pixValStr);

            cldHghtDlg.setTemperature(tempC);

            cldHghtDlg.clearAltCloudHeights();

            if (cloudHeights.size() == 0) {
                cldHghtDlg.setPrimaryCloudHeight(Double.NaN, Double.NaN);
                cldHghtDlg.displayStatusMsg("Unable to compute Cloud Height");
            } else {
                cldHghtDlg.setPrimaryCloudHeight(cloudHeights.get(0).cloudHght,
                        cloudHeights.get(0).cloudPres);
                if (cloudHeights.size() > 1) {
                    cldHghtDlg.displayStatusMsg("Multiple Cloud Levels Found.");

                    for (int ch = cloudHeights.size() - 1; ch > 0; ch--) {
                        cldHghtDlg.addAltCloudHeight(
                                cloudHeights.get(ch).cloudHght,
                                cloudHeights.get(ch).cloudPres);
                    }
                }
            }

            // draw the marker
            cldHghtRsc.setSelectedLoc(latlon);
        }

        seldPane.refresh();
    }

    /**
     * Finds the sounding data for the station closest to the point clicked by
     * the user
     * 
     * @param latlon
     *            - the coordinates of the point clicked by the user
     * @return A list of LevelValues for all the soundings from this station
     *         that are time-matched to the satellite image time
     */
    private List<LevelValues> getSoundingDataFromStationData(Coordinate latlon) {

        if (sndingDataSrc == SoundingDataSourceType.STATION_DATA) {
            sndingSrcTimeStr = "N/A";
            sndingSrcStr = "Station Data";
            sndingSrcDist = cldHghtDlg.getMaxSoundingDist();
            maxIntervalInHoursForValidStationData = cldHghtDlg
                    .getMaxValidIntervalInHoursForStationData();
            stnData = cldHghtRsc.getStationData(latlon, maxSndingDist,
                    satelliteImageTime, maxIntervalInHoursForValidStationData);
            if (stnData != null
                    && cldHghtRsc.minimumDistance != cldHghtRsc.INVALID_DISTANCE) {
                if (stnData.stationId != null && !stnData.stationId.isEmpty()) {
                    sndingSrcStr = new String(stnData.stationId);
                }
                sndingSrcTimeStr = new String(stnData.stationRefTime.toString());
                sndingSrcDist = cldHghtRsc.minimumDistance;

                /*
                 * Store the list of NcSoundingLayers in a member variable since
                 * it might be needed for the moist-adiabatic method of
                 * cloud-height computation
                 */
                aListOfNcSoundingLayers = getListOfNcSoundingLayerForThisStation(stnData);
                if (compMthd.compareTo(ComputationalMethod.STANDARD) == 0) {
                    soundingData = getListOfLevelValuesFromListOfNcSoundingLayer(aListOfNcSoundingLayers);
                }
            }
            // TODO could check that the station is actually different than the
            // previous station.

        }
        return this.soundingData;
    }

    /***
     * Determines the cloud height using the moist adiabatic method
     * 
     * @param listOfNcSoundingLayer
     *            - a list of sounding data from the station closest to the
     *            user-clicked point on the IR image.
     * @param tmpk
     *            - the cloud temperature in Kelvin
     * @return a list of <code>CloudHeightData<code> containing the primary  
     * height and pressure for the cloud, if the computations succeed. Otherwise, it returns 
     * an empty list of  <code>CloudHeightData<code>.
     */

    private List<CloudHeightData> computeCloudHeightByMoistAdiabaticMethod(
            List<NcSoundingLayer> listOfNcSoundingLayer, float tmpk) {
        List<CloudHeightData> cldHgtDataList = new ArrayList<CloudHeightProcesser.CloudHeightData>(
                0);
        NcSoundingLayer unstableSoundingAt600mb = PSLibrary.psUstb(
                listOfNcSoundingLayer, 600);

        if (unstableSoundingAt600mb != null) {
            float pressure = unstableSoundingAt600mb.getPressure();
            float tmpc = unstableSoundingAt600mb.getTemperature();
            float dwpc = unstableSoundingAt600mb.getDewpoint();
            if (pressure != GempakConstants.RMISSD
                    && tmpc != GempakConstants.RMISSD
                    && dwpc != GempakConstants.RMISSD) {

                float te = PRLibrary.prThte(pressure, tmpc, dwpc);
                float tmst = PRLibrary.prTmst(te, 500, 0);
                float t500 = GempakConstants.RMISSD;

                for (NcSoundingLayer thisNcSounding : listOfNcSoundingLayer) {
                    if (thisNcSounding.getPressure() == 500) {
                        t500 = thisNcSounding.getTemperature();
                        break;
                    }
                }

                float bli = ((MissingValueTester.isDataValueMissing(t500) || MissingValueTester
                        .isDataValueMissing(tmst)) ? GempakConstants.RMISSD
                        : (t500 - PRLibrary.prTmkc(tmst)));
                float pmst = PRLibrary.prPmst(te, tmpk);
                // System.out.println ( "pmst = " + pmst );
                List<NcSoundingLayer> nearestSoundingLevels = PCLibrary.pcFndl(
                        listOfNcSoundingLayer, pmst,
                        PCLibrary.VerticalCoordinate.PRESSURE,
                        PCLibrary.SearchOrder.BOTTOM_UP);
                CloudHeightData newCldHgtData = new CloudHeightData(Float.NaN,
                        Float.NaN);
                if (nearestSoundingLevels != null
                        && !nearestSoundingLevels.isEmpty() && bli >= 0) {
                    // if bli >= 0 and a sounding is obtained- means its a
                    // stable sounding
                    if (PCLibrary.getLocationOfLevel() == LevelType.BETWEEN_LEVELS) {
                        NcSoundingLayer interpolatedSounding = PCLibrary
                                .interpolateBetweenTwoSoundingLayers(
                                        nearestSoundingLevels, pmst,
                                        PCLibrary.VerticalCoordinate.PRESSURE);
                        newCldHgtData = new CloudHeightData(
                                interpolatedSounding.getGeoHeight(),
                                interpolatedSounding.getPressure());
                    } else if (PCLibrary.getLocationOfLevel() == LevelType.EXACT_MATCH) {
                        NcSoundingLayer tempSounding = nearestSoundingLevels
                                .get(0);
                        newCldHgtData = new CloudHeightData(
                                tempSounding.getGeoHeight(),
                                tempSounding.getPressure());
                    }
                }
                cldHgtDataList.add(newCldHgtData);
            }

        }

        return cldHgtDataList;
    }

    /**
     * Queries the database to get sounding data for the input station
     * 
     * @param stationData
     *            - the station for which sounding data needs to be retrieved
     * @return a list of sounding layers if the database query succeeds or an
     *         empty list otherwise
     */
    private List<NcSoundingLayer> getListOfNcSoundingLayerForThisStation(
            StationData stationData) {
        List<NcSoundingLayer> ncSoundingLayerList = new ArrayList<NcSoundingLayer>(
                0);
        System.out.println("Time stamp of station: "
                + stationData.stationRefTime.toString());
        NcSoundingCube thisSoundingCube = NcSoundingQuery
                .soundingQueryByStnNum(stationData.stationRefTime
                        .getRefTimeAsCalendar().getTimeInMillis(),
                        new String[] { new String(stationData.stationNumber) }, // stationData.stationNumber
                        "UAIR", NcSoundingLayer.DataType.ALLDATA, true, "-1");
        if (thisSoundingCube != null) {
            List<NcSoundingProfile> listOfSoundingProfiles = thisSoundingCube
                    .getSoundingProfileList();
            if (listOfSoundingProfiles != null
                    && !listOfSoundingProfiles.isEmpty()) {
                for (NcSoundingProfile eachSoundingprofile : listOfSoundingProfiles) {
                    ncSoundingLayerList.addAll(eachSoundingprofile
                            .getSoundingLyLst());
                }
            }
        }
        return ncSoundingLayerList;
    }

    /***
     * Creates a list of <code>LevelValues</code> using the height and pressure
     * of each <code>NcSoundingLayer</code> in the input list of
     * <code>NcSoundingLayer</code>
     * 
     * @param listOfNcSoundingLayer
     * @return a list of {@code}LevelValues
     */
    private List<LevelValues> getListOfLevelValuesFromListOfNcSoundingLayer(
            List<NcSoundingLayer> listOfNcSoundingLayer) {
        List<LevelValues> aListOfLevelValues = new ArrayList<LevelValues>(0);
        if (listOfNcSoundingLayer != null && !listOfNcSoundingLayer.isEmpty()) {
            for (NcSoundingLayer eachNcSoundingLayer : listOfNcSoundingLayer) {

                LevelValues newLvl = new LevelValues();
                newLvl.setHeight(eachNcSoundingLayer.getGeoHeight());
                newLvl.setPressure(eachNcSoundingLayer.getPressure());
                // temperature in the sounding layer is already stored in
                // celsius
                newLvl.setTemperature(eachNcSoundingLayer.getTemperature());
                aListOfLevelValues.add(newLvl);
            }
        }
        return aListOfLevelValues;
    }

    // TODO if the temp is lower than the min Temp in the sounding then should
    // we return posInf and disp ">max"?
    /**
     * Computes the cloud height using the Standard method
     * 
     * @param soundingLevels
     *            - list of <code>LevelValues</code>
     * @param tempC
     *            - the cloud temperatue in Celsius
     * @return a list of <code>CloudHeightData</code> containing (one or more)
     *         computed cloud height and pressure information
     */
    private List<CloudHeightData> computeCloudHeights(
            List<LevelValues> soundingLevels, Double tempC) {
        List<CloudHeightData> cldHghts = new ArrayList<CloudHeightData>(0);

        double tempMax = -9999.0;
        int lindx = 0;
        double tempCDoubleVal = tempC.doubleValue();
        if (soundingLevels != null && soundingLevels.size() > 0) {
            int soundindLevelListSize = soundingLevels.size();
            for (lindx = soundindLevelListSize - 1; lindx > 0; lindx--) {
                LevelValues topLvl = soundingLevels.get(lindx);
                LevelValues btmLvl = soundingLevels.get(lindx - 1);
                double topTemp = topLvl.getTemperature();
                double btmTemp = btmLvl.getTemperature();
                if (tempCDoubleVal == topTemp) {
                    // System.out.println("tempCDoubleVal equals topTemp and is: "+
                    // tempCDoubleVal);
                }

                if (tempCDoubleVal == btmTemp) {
                    // System.out.println("tempCDoubleVal equals btmTemp and is: "+
                    // tempCDoubleVal);
                }
                // logic from NMAP source snghgt.f
                tempMax = (topTemp > tempMax ? topTemp : tempMax); // find the
                                                                   // maximum
                                                                   // temperature
                double sign = (tempCDoubleVal - topTemp)
                        * (tempCDoubleVal - btmTemp);
                if (sign <= 0.0) {
                    if (sign == 0.0) {
                        if (tempCDoubleVal == topLvl.getTemperature()) {
                            cldHghts.add(new CloudHeightData(
                                    topLvl.getHeight(), topLvl.getPressure()));
                        }
                    } else {
                        /*
                         * TODO: The logic below is partly implemented from the
                         * legacy file pcintt.f Once the entire PC-library is
                         * re-implemented in Java, this may be replaced with a
                         * call to the Java method that contains the logic in
                         * pcintt.f
                         */
                        // interpolate the temp linearly and the pressure
                        // logarithmically.
                        boolean istempCBetweenBtmTempAndTopTemp = false;
                        if (((topTemp < btmTemp) && (topTemp < tempCDoubleVal) && (tempCDoubleVal < btmTemp))
                                || ((btmTemp < topTemp)
                                        && (btmTemp < tempCDoubleVal) && (tempCDoubleVal < topTemp))) {
                            istempCBetweenBtmTempAndTopTemp = true;
                        }
                        if (istempCBetweenBtmTempAndTopTemp) {
                            if (btmLvl.getPressure() != GempakConstants.RMISSD
                                    && (topLvl.getPressure() != GempakConstants.RMISSD)) {
                                double rmult = (tempCDoubleVal - topTemp)
                                        / (btmTemp - topTemp);
                                double plog = Math.log(btmLvl.getPressure())
                                        - Math.log(topLvl.getPressure());
                                cldHghts.add(new CloudHeightData((topLvl
                                        .getHeight() + rmult
                                        * (btmLvl.getHeight() - topLvl
                                                .getHeight())),
                                        (topLvl.getPressure() * Math.exp(rmult
                                                * plog))));
                            }
                        }
                    }
                }
            }

            // if( tempCDoubleVal > tempMax){
            // System.out.println("The cloud temperature is warmer than the entire sounding data");
            // }
            // System.out.println("Maximum temperature is: "+tempMax);
        }
        return cldHghts;
    }

    // check for a satellite IR image resource and for
    // an existing cloud height resource or create a new one
    private void getResources() {
        if (cldHghtRsc != null && satRsc != null) {
            return;
        }
        ResourceList rscs = seldPane.getDescriptor().getResourceList();

        for (ResourcePair r : rscs) {
            if (r.getResource() instanceof CloudHeightResource) {
                cldHghtRsc = (CloudHeightResource) r.getResource();
                break;
            } else if (r.getResource() instanceof ICloudHeightCapable) {
                if (((ICloudHeightCapable) r.getResource())
                        .isCloudHeightCompatible()) {
                    satRsc = (ICloudHeightCapable) r.getResource();
                    satelliteImageTime = seldPane.getDescriptor()
                            .getTimeForResource(r.getResource());
                    // create a converter so we will always get the temp in
                    // celsius
                    if (satRsc.getTemperatureUnits() != SI.CELSIUS
                            || (tempUnitsConverter == null)) {
                        tempUnitsConverter = satRsc.getTemperatureUnits()
                                .getConverterTo(SI.CELSIUS);
                    }

                }
                // else {
                // cldHghtDlg.displayStatusMsg(
                // "The Satellite Image must be an IR Image");
                // }
            }
        }

        if (satRsc == null) {
            cldHghtDlg.appendStatusMsg("No Satellite IR image is loaded.");
        }

        if (cldHghtRsc == null) {
            try {
                CloudHeightResourceData srd = new CloudHeightResourceData();
                cldHghtRsc = srd.construct(new LoadProperties(),
                        seldPane.getDescriptor());
                seldPane.getDescriptor().getResourceList().add(cldHghtRsc);
                cldHghtRsc.init(seldPane.getTarget());
            } catch (VizException e) {
                e.printStackTrace();
            }
            seldPane.refresh();
        }
        // if satRsc is not set then we will check for it later and print msgs
        // til
        // it is loaded.
    }

    public void close() {
        removeCloudHeightResource();
    }

    private void removeCloudHeightResource() {
        satRsc = null;

        if (cldHghtRsc != null) {
            seldPane.getDescriptor().getResourceList().removeRsc(cldHghtRsc);
            cldHghtRsc = null;
            seldPane.refresh();
        }
    }

    /**
     * Gets an array of pixels around the user clicked point
     * 
     * @param startingPoint
     *            - the user clicked point
     * @param addToX
     *            - decides whether to add the incremental difference to the
     *            x-coordinate
     * @param size
     *            - number of points in the array
     * @return the array of pixels surrounding the user clicked point.
     */
    private PixelLocation[] generateNeighboringPixelLocationsFromASinglePixelLocation(
            PixelLocation startingPoint, boolean addToX, int size) {
        PixelLocation[] arrayOfPixelLocations = new PixelLocation[size];
        int counter = -(int) size / 2;
        if (addToX) {
            for (int i = 0; i < size; i++) {
                arrayOfPixelLocations[i] = new PixelLocation(
                        startingPoint.xCoord + counter, startingPoint.yCoord);
                counter++;
            }
        } else {
            for (int i = 0; i < size; i++) {
                arrayOfPixelLocations[i] = new PixelLocation(
                        startingPoint.xCoord, startingPoint.yCoord + counter);
                counter++;
            }
        }
        return arrayOfPixelLocations;
    }

    /**
     * Gets the previous valid sounding data for the input station and uses the
     * moist-adiabatic method to compute the cloud height.
     * 
     * @param nearestStationData
     *            - the station closest to the user clicked point on the screen.
     * @param maxInterval
     *            - the maximum time in the past, (in hours) within which the
     *            station data is searched
     * @param imageTime
     *            - satellite image time
     * @param tempInKelvin
     *            - the cloud temperature ( in Kelvin ) at the pixel point
     *            clicked by the user
     * @return the list of <code>CloudHeightData </code>
     */
    private List<CloudHeightData> computeCloudHeightFromPreviousValidSatationDataForClosestStation(
            StationData nearestStationData, int maxInterval,
            DataTime imageTime, float tempInKelvin) {
        List<CloudHeightData> cloudHeightData = new ArrayList<CloudHeightProcesser.CloudHeightData>(
                0);
        if (stnData != null) {
            Calendar limitingCalendar = imageTime.getRefTimeAsCalendar();
            limitingCalendar.add(Calendar.HOUR_OF_DAY, -maxInterval);
            DataTime satelliteImageTimeLimit = new DataTime(limitingCalendar);
            for (int hourToDeduct = 1; hourToDeduct <= maxInterval; hourToDeduct++) {
                Calendar tempCalendar = nearestStationData.stationRefTime
                        .getRefTimeAsCalendar();
                tempCalendar.add(Calendar.HOUR_OF_DAY, -hourToDeduct);
                nearestStationData.stationRefTime = new DataTime(tempCalendar);
                if (satelliteImageTimeLimit
                        .greaterThan(nearestStationData.stationRefTime))
                    break;
                this.aListOfNcSoundingLayers = getListOfNcSoundingLayerForThisStation(nearestStationData);
                if (!this.aListOfNcSoundingLayers.isEmpty())
                    cloudHeightData = computeCloudHeightByMoistAdiabaticMethod(
                            this.aListOfNcSoundingLayers, tempInKelvin);
                if (!cloudHeightData.isEmpty())
                    break;
            }
        }
        return cloudHeightData;
    }

    /**
     * 
     * @param coord
     * @param satRsc
     * @param isSinglePixelNeeded
     * @param pixelAreaDimension
     * @param pixelSelectionMethod
     * @return
     */
    private Double getPixelValueFromTheUserClickedCoordinate(Coordinate coord,
            ICloudHeightCapable satRsc, boolean isSinglePixelNeeded,
            int pixelAreaDimension,
            CloudHeightDialog.PixelValueMethod pixelSelectionMethod) {
        Double pixVal = Double.NaN;
        double[] in = new double[2];
        double[] out = new double[2];
        double[] outCoord = new double[2];
        int newArrDimensions = (pixelAreaDimension * 2) + 1;
        PixelLocation[][] squarePixelArea = new PixelLocation[newArrDimensions][newArrDimensions];
        in[0] = MapUtil.correctLon(coord.x);
        in[1] = MapUtil.correctLat(coord.y);
        if (satRsc != null && satRsc instanceof McidasSatResource) {
            int maxX = 0;
            int maxY = 0;
            int minX = 0;
            int minY = 0;
            FileBasedTileSet tileSet = ((McidasSatResource) satRsc)
                    .getTileSet();
            if (tileSet instanceof McidasFileBasedTileSet) {
                PluginDataObject pdo = ((McidasFileBasedTileSet) tileSet)
                        .getPdo();
                if (pdo != null && pdo instanceof McidasRecord) {

                    /* Get the geometry associated with the Mcidas file */
                    GridGeometry2D gridGeom = getGridGeometryForMcidasData(pdo,
                            satRsc);
                    try {
                        MathTransform localProjToLatLon = CRS
                                .findMathTransform(
                                        gridGeom.getCoordinateReferenceSystem(),
                                        DefaultGeographicCRS.WGS84);
                        MathTransform latLonToLocalProj = localProjToLatLon
                                .inverse();
                        MathTransform mtGridToCRS = gridGeom
                                .getGridToCRS(PixelInCell.CELL_CORNER);
                        MathTransform invmtCRSToGrid = mtGridToCRS.inverse();
                        latLonToLocalProj.transform(in, 0, out, 0, 1);
                        invmtCRSToGrid.transform(out, 0, outCoord, 0, 1);
                        /* Get the raw data from the HDF5 file */
                        File file = HDF5Util.findHDF5Location(pdo);
                        if (file.exists()) {
                            IDataRecord idr = null;

                            idr = CubeUtil.retrieveData(pdo, "mcidas");
                            AbstractStorageRecord asr = (AbstractStorageRecord) idr;
                            long[] sizes = asr.getSizes();
                            maxX = (int) sizes[0];
                            maxY = (int) sizes[1];

                            if (((ByteDataRecord) idr).validateDataSet()) {
                                byte[] arrayOfBytes = ((ByteDataRecord) idr)
                                        .getByteData();
                                ByteBuffer byteBuffer = BufferUtil.wrapDirect(
                                        arrayOfBytes, new Rectangle(0, 0, maxX,
                                                maxY));
//                                ByteDataPreparer bdp = new ByteDataPreparer();

                                /*
                                 * Wrap the raw data of bytes into a
                                 * ByteDataPreparer object
                                 */
//                                bdp = (ByteDataPreparer) bdp
//                                        .newInstance(byteBuffer, new Rectangle(
//                                                0, 0, maxX, maxY), new int[] {
//                                                maxX, maxY });

                                /*
                                 * Get the actual brightness-temperature ( pixel
                                 * value ) information contained from the byte
                                 * array
                                 */
//                                double tempDbl = bdp.getValue(byteBuffer,
//                                        (int) (outCoord[0]),
//                                        (int) (outCoord[1]), -128.0f, 127.0f);
                                double tempDbl = 0;

                                if (isSinglePixelNeeded) {
                                    pixVal = new Double((tempDbl));
                                } else {
                                    int[] pixCoord = new int[2];
                                    pixCoord[0] = (int) outCoord[0];
                                    pixCoord[1] = (int) outCoord[1];

                                    if (pixCoord != null
                                            && pixCoord.length == 2) {
                                        PixelLocation userClickedPoint = new PixelLocation(
                                                pixCoord[0], pixCoord[1]);
                                        /*
                                         * In the pixel area generate the middle
                                         * column of pixel locations
                                         */

                                        PixelLocation[] tempPixelArea = this
                                                .generateNeighboringPixelLocationsFromASinglePixelLocation(
                                                        userClickedPoint,
                                                        false, newArrDimensions);
                                        int midpoint = ((int) (pixelAreaDimension));

                                        /*
                                         * For each PixelLocaton in the column,
                                         * generate the entire row to complete
                                         * the matrix
                                         */
                                        for (int i = 0; i < newArrDimensions; i++) {
                                            squarePixelArea[i][midpoint] = tempPixelArea[i];
                                            squarePixelArea[i] = this
                                                    .generateNeighboringPixelLocationsFromASinglePixelLocation(
                                                            squarePixelArea[i][midpoint],
                                                            true,
                                                            newArrDimensions);
                                        }

                                        double[][] arrayOfPixVal = new double[newArrDimensions][newArrDimensions];

                                        /*
                                         * Generate the NxN array of pixel
                                         * values
                                         */
                                        for (int i = 0; i < newArrDimensions; i++) {
                                            for (int j = 0; j < newArrDimensions; j++) {
//                                                arrayOfPixVal[i][j] = bdp
//                                                        .getValue(
//                                                                byteBuffer,
//                                                                (int) (squarePixelArea[i][j].xCoord),
//                                                                (int) (squarePixelArea[i][j].yCoord),
//                                                                -128.0f, 127.0f);
                                            }
                                        }

                                        /*
                                         * Get either the maximum or the most
                                         * frequently used pixel value
                                         */
                                        if (pixelSelectionMethod == PixelValueMethod.MAX_VALUE) {
                                            pixVal = new Double(
                                                    getMaxPixValFromPixelArray(arrayOfPixVal));
                                        } else {
                                            pixVal = new Double(
                                                    getMostFrequentPixelValueFromPixelArray(arrayOfPixVal));
                                        }

                                    }

                                }
                            }
                        }
                    } catch (VizException e) {
                        e.printStackTrace();
                    } catch (FactoryException e) {
                        e.printStackTrace();
                    } catch (NoninvertibleTransformException e) {
                        e.printStackTrace();
                    } catch (TransformException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        return pixVal;
    }

    /**
     * Retrieves the grid geometry for the mcidas data
     * 
     * @param pdo
     *            - The Mcidas Record
     * @param satRsc
     *            - The Mcidas satellite resource
     * @return the grid geometry depending upon the type of projection described
     *         in the Mcidas data
     */
    private GridGeometry2D getGridGeometryForMcidasData(PluginDataObject pdo,
            ICloudHeightCapable satRsc) {
        GridGeometry2D mcidasGeom = null;
        if (pdo instanceof McidasRecord && satRsc instanceof McidasSatResource) {
            String projection = ((McidasRecord) pdo).getProjection();
            if (projection.equalsIgnoreCase("STR")
                    || projection.equalsIgnoreCase("MER")
                    || projection.equalsIgnoreCase("LCC")) {
                mcidasGeom = MapUtil.getGridGeometry(((ISpatialEnabled) pdo)
                        .getSpatialObject());
            } else {
                mcidasGeom = ((McidasSatResource) satRsc)
                        .createNativeGeometry(pdo);
            }
        }
        return mcidasGeom;
    }

    /**
     * Finds the maximum pixel value in a 2D array of pixel values
     * 
     * @param arrayOfPixVal
     *            - the 2D array to search
     * @return the maximum pixel value in the array
     */
    private double getMaxPixValFromPixelArray(double[][] arrayOfPixVal) {
        double maxPixVal = 0.0f;
        if (arrayOfPixVal != null && arrayOfPixVal.length > 0) {
            int arraySize = arrayOfPixVal.length;
            for (int i = 0; i < arraySize; i++) {
                for (int j = 0; j < arraySize; j++) {
                    if (arrayOfPixVal[i][j] > maxPixVal)
                        maxPixVal = arrayOfPixVal[i][j];
                }
            }
        }
        return maxPixVal;
    }

    /**
     * Finds the most frequently occurring pixel value in the input 2D array of
     * pixel values.
     * 
     * @param arrayOfPixVal
     *            - the input 2D array to search
     * @return the most frequently occurring pixel value
     */
    private double getMostFrequentPixelValueFromPixelArray(
            double[][] arrayOfPixVal) {
        double modePixVal = 0.0f;
        Map<Double, Integer> frequencyMap = new HashMap<Double, Integer>(0);
        int counter = 0;
        if (arrayOfPixVal != null && arrayOfPixVal.length > 0) {
            int arraySize = arrayOfPixVal.length;
            for (int i = 0; i < arraySize; i++) {
                for (int j = 0; j < arraySize; j++) {
                    Double currentPixVal = new Double(arrayOfPixVal[i][j]);
                    /*
                     * If the hash-map contains the pixel value, get its current
                     * counter
                     */
                    if (frequencyMap.containsKey(currentPixVal))
                        counter = frequencyMap.get(currentPixVal);

                    /* update the counter */
                    counter++;

                    /*
                     * put the pixel value in the hash-map, along with its
                     * corresponding counter value
                     */
                    frequencyMap.put(currentPixVal, new Integer(counter));

                    /* reset the counter value */
                    counter = 0;
                }
            }

            /*
             * Loop through the pixel values in the map to get the most frequent
             * pixel value
             */
            int mostFrequentPixVal = 0;
            Set<Double> keySet = frequencyMap.keySet();
            for (Double eachDbl : keySet) {
                if (frequencyMap.get(eachDbl).intValue() > mostFrequentPixVal) {
                    mostFrequentPixVal = frequencyMap.get(eachDbl).intValue();
                    modePixVal = eachDbl.doubleValue();
                }
            }

        }
        return modePixVal;
    }

    /**
     * Stores the x-y grid location of Mcidas Data as a single class object
     * 
     * @author archana
     */
    protected class PixelLocation {
        protected int xCoord = 0;

        protected int yCoord = 0;

        public PixelLocation(int x, int y) {
            xCoord = x;
            yCoord = y;
        }

    }

}
