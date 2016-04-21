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
package com.raytheon.uf.viz.bufrsigwx.rsc;

import java.util.Comparator;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.bufrsigwx.TropHeightData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.bufrsigwx.common.SigWxCommon;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

/**
 * Provides a resource that will display troppopause height/locations data for a
 * given reference time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/25/2009             jsanchez    Initial creation.
 * Sep 28, 2009 3099       bsteffen    Updated to conform with common SigWxResource
 * Jul 29, 2014 #3465      mapeters    Updated deprecated drawStrings() calls.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class SigWxTropHeightResource extends SigWxResource {

    private static UnitConverter meterToHft = SI.METER.getConverterTo(SI
            .HECTO(NonSI.FOOT));

    private static final String format = "%3.0f";

    private static final String LAT_STR = "latitude";

    private static final String LON_STR = "longitude";

    private static final String HGT_STR = "height";

    private static final String TYPE_STR = "tropType";

    private static final String NUM_OF_POINTS_STR = "numOfPoints";

    private static final int MAX = 2;

    private static final int MIN = 3;

    protected SigWxTropHeightResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public String getName() {
        String layerString = resourceData.getMetadataMap().get("wxLayer")
                .getConstraintValue();
        SigWxLayer layer = SigWxLayer.valueOf(layerString);
        String level = "";

        if (layer == SigWxLayer.SWH) {
            level = "High";
        } else if (layer == SigWxLayer.SWM) {
            level = "Medium";
        }
        return level + " Level SIGWX Trop Heights";
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        Number[] hdf5Lats = pdv.getNumberAllLevels(LAT_STR);
        Number[] hdf5Lons = pdv.getNumberAllLevels(LON_STR);
        Number[] hdf5Hgts = pdv.getNumberAllLevels(HGT_STR);
        int numOfPoints = pdv.getInt(NUM_OF_POINTS_STR);
        int tropType = pdv.getInt(TYPE_STR);
        TropHeightData data;
        for (int i = 0; i < numOfPoints; i++) {
            if (hdf5Hgts[i].floatValue() != SigWxCommon.MISSING) {
                data = new TropHeightData(hdf5Lons[i].floatValue(), hdf5Lats[i]
                        .floatValue(), hdf5Hgts[i].floatValue(), tropType);
                paintTropHeights(target, color, data);

            }
        }
    }

    public class TropHeightComparator implements Comparator<TropHeightData> {

        public int compare(TropHeightData o1, TropHeightData o2) {
            if (o1 == null && o2 == null) {
                return 0;
            } else if (o2 == null) {
                return 1;
            } else if (o1 == null) {
                return -1;
            }
            return o1.getHeight().compareTo(o2.getHeight());

        }
    }

    /**
     * Draws teh trop heights on CAVE
     * 
     * @param target
     * @param color
     * @param kind
     * @param data
     * @throws VizException
     */
    private void paintTropHeights(IGraphicsTarget target, RGB color,
            TropHeightData data) throws VizException {
        double lat = data.getLatitude();
        double lon = data.getLongitude();
        if (lat == SigWxCommon.MISSING || lon == SigWxCommon.MISSING) {
            return;
        }
        double[] locationPixel = SigWxCommon.lonLatToWorldPixel(descriptor,
                lon, lat);
        double height = data.getHeight();

        height = meterToHft.convert(height);
        String heightStr = SigWxCommon.format(height, format);
        String[] lines = { heightStr };
        if (data.getTropType() == MAX) {
            lines = new String[] { "H", heightStr };

        } else if (data.getTropType() == MIN) {
            lines = new String[] { heightStr, "L" };

        }
        DrawableString string = new DrawableString(lines, color);
        string.font = font;
        string.setCoordinates(locationPixel[0], locationPixel[1]);
        string.horizontalAlignment = HorizontalAlignment.CENTER;
        string.verticallAlignment = VerticalAlignment.MIDDLE;
        target.drawStrings(string);
    }

    protected String[] getParameters() {
        return new String[] { LAT_STR, LON_STR, NUM_OF_POINTS_STR, HGT_STR,
                TYPE_STR };
    }
}
