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
package com.raytheon.uf.viz.preciprate;

import java.awt.Rectangle;
import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Map;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.xml.bind.JAXB;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.preciprate.PrecipRateRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.viz.preciprate.xml.PrecipRateXML;
import com.raytheon.uf.viz.preciprate.xml.SCANConfigPrecipRateXML;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.units.PiecewisePixel;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarTextResource.IRadarTextGeneratingResource;
import com.raytheon.viz.radar.rsc.image.RadarRadialResource;
import com.raytheon.viz.radar.util.DataUtilities;

public class PrecipRateResource extends RadarRadialResource implements
        IRadarTextGeneratingResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipRateResource.class);

    /* formatter */
    private DecimalFormat df = new DecimalFormat();

    private SCANConfigPrecipRateXML cfgMXL = null;

    private PrecipRateRecord precipRecord;
    
    private PrecipRateResourceData data = null;

    public PrecipRateResource(PrecipRateResourceData data,
            LoadProperties props, IRadarInterrogator interrogator)
            throws VizException {

        super(data, props, interrogator);
        this.data = data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        if (precipRecord == null) {
            return "No Data Available";
        }

        StringBuilder prefix = new StringBuilder();
        prefix.append(precipRecord.getIcao());
        prefix.append(" ");
        prefix.append("Precipitation Rate (" + precipRecord.getParameterUnit()
                + ")");

        return prefix.toString();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        precipRecord = null;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.df = new DecimalFormat();
        df.setMinimumIntegerDigits(1);
        df.setMaximumFractionDigits(2);
        df.setMinimumFractionDigits(1);
        df.setDecimalSeparatorAlwaysShown(true);
        super.initInternal(target);
    }

    /**
     * Get and load the style rule
     * 
     * @return
     */
    public ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add("preciprate");
        match.setParameterName(paramList);
        return match;
    }

    /**
     * Read in the precip rate xml.
     */
    public void readPrecipRateConfig() {
        try {
            if (cfgMXL != null) {
                return;
            }

            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(getFullDefaultConfigName());

            cfgMXL = JAXB.unmarshal(path, SCANConfigPrecipRateXML.class);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Get the full path and file name of the precip rate xml.
     * 
     * @return The full path and file name of the precip rate xml.
     */
    public String getFullDefaultConfigName() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("scan").append(fs);
        sb.append("config").append(fs);
        sb.append("preciprate").append(fs);
        sb.append("SCANconfig_precipRate.xml");

        return sb.toString();
    }

    /**
     * @param record
     */
    @Override
    public void addRecord(PluginDataObject record) {
        if (!(record instanceof PrecipRateRecord)) {
            statusHandler.handle(Priority.PROBLEM, this.getClass().getName()
                    + " expected : " + PrecipRateRecord.class.getName()
                    + " Got: " + record);
            return;
        }

        PrecipRateRecord pRecord = (PrecipRateRecord) record;
        if (precipRecord == null) {
            precipRecord = pRecord;
        }

        VizPrecipRateRadarRecord radarRecord = new VizPrecipRateRadarRecord(
                pRecord);

        super.addRecord(radarRecord);
    }

    @Override
    protected VizRadarRecord createVizRadarRecord(RadarRecord radarRecord) {
        return (VizPrecipRateRadarRecord) radarRecord;
    }

    @Override
    public VizPrecipRateRadarRecord getRadarRecord(DataTime time) {
        return (VizPrecipRateRadarRecord) super.getRadarRecord(time);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.image.RadarRadialResource#toImageData(com.
     * raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     * com.raytheon.uf.common.dataplugin.radar.RadarRecord, java.awt.Rectangle)
    
    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, final RadarRecord record,
            final Rectangle rect) throws VizException {
        return target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarImageDataRetrievalAdapter(record, null, rect) {
                        }, params);
    }
     */

    @Override
    protected ColorMapParameters getColorMapParameters(IGraphicsTarget target,
            RadarRecord record) throws VizException {

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        colorMapParameters = new ColorMapParameters();
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, getMatchCriteria());
        String colormapfile = ((ImagePreferences) sr.getPreferences())
                .getDefaultColormap();

        IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
        ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
        colorMapParameters.setColorMap(colorMap);

        colorMapParameters.setDisplayUnit(record.getUnitObject());
        colorMapParameters.setColorMapMax(255);
        colorMapParameters.setColorMapMin(0);
        colorMapParameters.setDataMax(255);
        colorMapParameters.setDataMin(0);
        colorMapParameters.setDataMapping(((ImagePreferences) sr
                .getPreferences()).getDataMapping());
        double[] d1 = {1,255};
        double[] d2 = {0,25.4};
        @SuppressWarnings({ "rawtypes", "unchecked" })
		PiecewisePixel pw = new PiecewisePixel(NonSI.INCH.divide(NonSI.HOUR), d1, d2);
        colorMapParameters.setDataUnit(pw);
        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);
        return colorMapParameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.AbstractRadarResource#getUpperText(com.raytheon
     * .uf.common.time.DataTime)
     */
    @Override
    public String[] getUpperText(DataTime time) {
        VizPrecipRateRadarRecord precipRecord = getRadarRecord(time);
        if (precipRecord == null) {
            return null;
        }
        ArrayList<String> array = new ArrayList<String>();
        StringBuilder sb;
        Double value = null;

        if (cfgMXL == null) {
            readPrecipRateConfig();
        }

        array.add("INSTANTANEOUS POLAR PRECIPITATION RATE "
                + precipRecord.getUnit());
        array.add("");

        Map<DHRValues, Double> dhrMap = precipRecord.getDhrMap();

        /*
         * If the DHR map is null then return the text that is already in the
         * array.
         */
        if (dhrMap == null) {
            return array.toArray(new String[0]);
        }

        ArrayList<PrecipRateXML> prArray = cfgMXL.getPrecipRates();

        // Loop over the precip rates from the xml.
        for (PrecipRateXML precipRate : prArray) {
            // If the DHR line is not suppose to be displayed the then continue.
            if (precipRate.isDisplayed() == false) {
                continue;
            }

            sb = new StringBuilder();
            sb.append(precipRate.getDisplayString()).append(" :   ");

            if (precipRate.getEnumID().toUpperCase()
                    .equals(DHRValues.BIASAPPLIEDFLAG.name())) {
                if (dhrMap.get(DHRValues.valueOf(precipRate.getEnumID()
                        .toUpperCase())) == 0) {
                    sb.append("   F");
                } else {
                    sb.append("   T");
                }
            } else {
                value = dhrMap.get(DHRValues.valueOf(precipRate.getEnumID()
                        .toUpperCase()));

                if (value == null) {
                    sb.append("    NO DATA");
                } else {
                    sb.append(String.format("%1.2f", value));
                }
            }

            if (precipRate.getEnumID().toUpperCase()
                    .equals(DHRValues.MAXPRECIPRATEALLOW.name())) {
                sb.append(" (");
                sb.append(df.format(ScanUtils.MM_TO_INCH
                        * precipRecord.getHailcap()));
                sb.append(" ").append(precipRecord.getUnit()).append(")");
            }

            array.add(sb.toString());
        }

        return array.toArray(new String[array.size()]);
    }

    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        if (latLon == null) {
            return "NO DATA";
        }
        FramesInfo currInfo = descriptor.getFramesInfo();
        DataTime time = currInfo.getTimeForResource(this);
        RadarRecord record = getRadarRecord(time);
        if (record == null) {
            return "NO DATA";
        }

        Integer dataVal = null;
        try {
            dataVal = new RadarDataInterrogator(record).getDataValue(latLon
                    .asLatLon());
            if (dataVal == 0) {
            	return "NO DATA";
            }
            
        } catch (Exception e) {
            UFStatus.getHandler().handle(
                    Priority.PROBLEM,
                    "Error inspecting precip rate data: "
                            + e.getLocalizedMessage(), e);
        }

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        double val = params.getDataToDisplayConverter().convert(dataVal);
                       
        if (val >= ScanUtils.MM_TO_INCH * precipRecord.getHailcap()
                || Double.isNaN(val)) {
            return String.format(
                    "%s %s %s",
                    new DecimalFormat("#.##").format(ScanUtils.MM_TO_INCH
                            * precipRecord.getHailcap()), record.getUnit(),
                    "(Hail Cap)");
        }
        return String.format("%s %s", new DecimalFormat("#.##").format(val),
                record.getUnit());
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        return null;
    }
    
    protected byte[] createConversionTable(ColorMapParameters params,
            RadarRecord record) {
    	
        UnitConverter dataToImage = params.getDataToImageConverter();;
        Unit<?> dataUnit = params.getDataUnit();
        // precompute the converted value for every possible value in the
        // record.
        byte[] table = new byte[record.getNumLevels()];
        for (int i = 0; i < record.getNumLevels(); i++) {
            double image = dataToImage.convert(i);
            if (Double.isNaN(image)) {
                double d = dataUnit.getConverterTo(params.getDisplayUnit())
                        .convert(i);
                if (Double.isNaN(d)) {
                    // This means that the data is a non-numeric value, most
                    // likely a flag of some sort
                    if (record.getNumLevels() <= 16) {
                        // For 3 and 4 bit products products try to match the
                        // flag value to a string value in the params
                        String value = record.getDecodedThreshold(i).toString();
                        for (DataMappingEntry entry : params.getDataMapping()
                                .getEntries()) {
                            if (value.equals(entry.getLabel())
                                    || value.equals(entry.getSample())) {
                                table[i] = entry.getPixelValue().byteValue();
                                break;
                            }
                        }
                    } else {
                        // For 8 bit products the flag value should be
                        // specifically handled in the style rules. For example
                        // if 1 is a flag for RF than pixel value 1 in the style
                        // rules will need to be RF. This is not
                        // a graceful seperation of data and representation but
                        // it works
                        table[i] = (byte) i;
                    }
                } else {
                    // the data value is outside the range of the colormap
                    UnitConverter image2disp = params
                            .getImageToDisplayConverter();
                    if (image2disp == null) {
                        continue;
                    }
                    for (int j = 0; j < 256; j++) {
                        double disp = image2disp.convert(j);
                        if (Double.isNaN(disp)) {
                            continue;
                        }
                        if (d < disp) {
                            // Map data values smaller than the colormap min to
                            // 0, which should be no data.
                            // table[i] = (byte) 0;
                            // If we want small values to appear as the lowest
                            // data value than do this next line instead
                            // This was changed for the DUA product so
                            // differences less than -5 get mapped to a data
                            // value.
                            table[i] = (byte) j;
                            break;
                        }
                        if (d > disp) {
                            // map data values larger than the colormap max to
                            // the highest value
                            table[i] = (byte) j;
                        }

                    }
                }
            } else {
                table[i] = (byte) Math.round(image);
            }
        }
        return table;
    }

}
