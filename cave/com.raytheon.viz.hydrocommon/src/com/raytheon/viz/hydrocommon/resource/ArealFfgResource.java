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
package com.raytheon.viz.hydrocommon.resource;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.ArealData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 22, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ArealFfgResource extends
        AbstractVizResource<ArealFfgResourceData, MapDescriptor> {

    /**
     * The Unit Converter
     */
    private UnitConverter cvt;

    /** The ColorMapParameters */
    private ColorMapParameters parameters;

    /**
     * List of Colorvalue objects
     */
    private List<Colorvalue> colorSet;

    private IFont font = null;

    private int lastFontSize = -999;

    private int fontSize;

    private List<DrawableString> stringList = new ArrayList<DrawableString>();

    private boolean fontChanged = false;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected ArealFfgResource(ArealFfgResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        String userId = System.getProperty("user.name");
        colorSet = HydroDisplayManager.getInstance().getFFGColorMap(userId,
                "FFG", resourceData.getDuration() * 60 * 1000);
        initColorMapParams();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        HydroDisplayManager manager = HydroDisplayManager.getInstance();

        // Check the font size
        fontSize = manager.getFontSize();
        if ((font == null) || (lastFontSize != fontSize)) {
            if (font != null) {
                font.dispose();
            }

            font = target.initializeFont("Dialog", fontSize, null);
            font.setSmoothing(false);
            lastFontSize = fontSize;
            fontChanged = true;
        }

        getData();

        for (DrawableString ds : this.stringList) {
            if (fontChanged) {
                ds.font = font;
            }
            target.drawStrings(ds);
        }
        fontChanged = false;
    }

    private void getData() {
        if (this.resourceData.isDataChanged()) {
            this.resourceData.setDataChanged(false);
            List<ArealData> arealDataList = this.resourceData
                    .getArealDataList();
            stringList.clear();
            for (ArealData ad : arealDataList) {
                double lat = ad.getLat();
                double lon = ad.getLon() * -1; // Need to reverse the sign on
                                               // Longitude
                double value = ad.getValue();
                String lid = ad.getLid();
                int index = (int) Math.floor(cvt.convert(value));

                Colorvalue cv = colorSet.get(index);
                RGB rgb = RGBColors.getRGBColor(cv.getColorname()
                        .getColorName());

                StringBuilder sb = new StringBuilder();
                if (this.resourceData.isDisplayIds()) {
                    sb.append(lid);
                    if (this.resourceData.isDisplayValues()) {
                        sb.append("\n" + value);
                    }
                } else {
                    if (this.resourceData.isDisplayValues()) {
                        sb.append(value);
                    }
                }

                DrawableString ds = new DrawableString(sb.toString(), rgb);

                Coordinate c = new Coordinate(lon, lat);
                double[] centerpixels = descriptor.worldToPixel(new double[] {
                        c.x, c.y });

                Coordinate valueCoor = new Coordinate(centerpixels[0],
                        centerpixels[1]);

                ds.setCoordinates(valueCoor.x, valueCoor.y);
                ds.font = font;
                ds.horizontalAlignment = HorizontalAlignment.CENTER;
                ds.verticallAlignment = VerticalAlignment.MIDDLE;

                stringList.add(ds);
            }
        }
    }

    private void initColorMapParams() {
        ColorMap colorMap = new ColorMap(colorSet.size());
        DataMappingPreferences dmPref = new DataMappingPreferences();
        int i = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(i, new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f));

            DataMappingEntry entry = new DataMappingEntry();
            entry.setPixelValue((double) i);
            entry.setDisplayValue(cv.getId().getThresholdValue());
            dmPref.addEntry(entry);

            i++;
        }
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (i - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);

        dmPref.getEntries().get(0).setLabel("");
        dmPref.getEntries().get(1).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);

        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = NonSI.INCH;

        Unit<?> dataUnit = SI.MILLIMETER.divide(100);

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(dataUnit);
        parameters.setImageUnit(dmPref.getImageUnit(dataUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub

    }

    @Override
    public String getName() {
        String noData = "";
        if ((resourceData.getArealDataList() == null)
                || (resourceData.getArealDataList().size() == 0)) {
            noData = "No Data Available";
        }

        String name = null;
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int hours = this.resourceData.getDuration()
                / HydroConstants.MILLIS_PER_HOUR;
        String hourStr = "hour";
        if (hours != 1) {
            hourStr = hourStr.concat("s");
        }

        if (this.resourceData == null) {
            name = "FFG No Data Available";
        } else {
            name = "FFG Areal " + resourceData.getResolution() + " " + hours
                    + " " + hourStr + " "
                    + sdf.format(this.resourceData.getDataDate()) + " "
                    + noData;
        }

        return name;
    }
}
