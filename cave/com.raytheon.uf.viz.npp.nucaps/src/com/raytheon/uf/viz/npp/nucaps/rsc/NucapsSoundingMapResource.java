package com.raytheon.uf.viz.npp.nucaps.rsc;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXB;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.npp.nucaps.xml.NucapsConfig;
import com.raytheon.uf.viz.npp.sounding.rsc.NPPSoundingMapResource;
import com.raytheon.uf.viz.npp.sounding.rsc.NPPSoundingMapResourceData;

/**
 * Sounding available resource. Draws points on map where data is available
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2015      18191 pwang       Initial version.
 * Feb 03, 2016      18588 wkwock      Fix update nucaps data issue.
 * Apr 14, 2016      18588 wkwock      Improve the performance.
 * OCT 23, 2017      19924 wkwock      Make color configurable.
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */

public class NucapsSoundingMapResource extends NPPSoundingMapResource {
    private RGB passedColor;

    private RGB partialPassedColor;

    private RGB failedColor;

    private RGB otherColor;

    private static final String DEFAULT_CONFIG_XML = "nucaps"
            + IPathManager.SEPARATOR + "nucapsConfig.xml";

    protected NucapsSoundingMapResource(NPPSoundingMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    protected NucapsSoundingMapResource(
            NucapsSoundingMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        readNucapsConfig();
    }

    @Override
    public synchronized void addRecords(PluginDataObject... records) {
        try {
            PluginDataObject[] allRecords = ((NucapsSoundingMapResourceData) this.resourceData)
                    .updatePluginDataObjects(records);
            super.addRecords(allRecords);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error adding record from update: "
                            + e.getLocalizedMessage(),
                    e);
        }
    }

    public synchronized void addRecordsNoUpdate(PluginDataObject... records) {
        super.addRecords(records);
    }

    /**
     * Color code dots base on QC value
     * 
     * @param IGraphicsTarget
     *            target, PaintProperties paintProps
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        Collection<NPPSoundingRecord> records = getCurrentRecords();
        if (records == null) {
            return;
        }
        List<DrawableCircle> circles = new ArrayList<DrawableCircle>(
                records.size());
        for (NPPSoundingRecord record : records) {
            double lat = record.getLatitude();
            double lon = record.getLongitude();
            double[] pixel = descriptor.worldToPixel(new double[] { lon, lat });
            DrawableCircle circle = new DrawableCircle();
            circle.setCoordinates(pixel[0], pixel[1]);
            circle.screenRadius = getRadius();
            circle.numberOfPoints = (int) (circle.screenRadius * 4);
            circle.basics.color = getQCColor(record.getPointDataView()
                    .getInt(NucapsRecord.PDV_QUALITY_FLAG));
            circle.filled = true;
            circles.add(circle);
        }
        target.drawCircle(circles.toArray(new DrawableCircle[0]));

    }

    /**
     * get QC color
     * 
     * @param qcFlag
     * @return RGB color
     */
    private RGB getQCColor(int qcFlag) {
        switch (qcFlag) {
        case 0:
            return passedColor;
        case 1:
        case 17:
            return partialPassedColor;
        case 9:
        case 25:
            return failedColor;
        default:
            return otherColor;
        }
    }

    /**
     * read NUCAPS configuration
     */
    private void readNucapsConfig() {
        IPathManager pm = PathManagerFactory.getPathManager();

        File file = pm.getStaticFile(DEFAULT_CONFIG_XML);
        try {
            NucapsConfig nucapsConfig = JAXB.unmarshal(file, NucapsConfig.class);

            if (nucapsConfig.getPassedColorName() != null) {
                passedColor = RGBColors
                        .getRGBColor(nucapsConfig.getPassedColorName());
            } else {
                statusHandler.warn(
                        "Color for passed QC is not defined. Defaulted to green.");
            }

            if (nucapsConfig.getPartiallyPassedColorName() != null) {
                partialPassedColor = RGBColors.getRGBColor(
                        nucapsConfig.getPartiallyPassedColorName());
            } else {
                statusHandler.warn(
                        "Color for partially passed QC is not defined. Defaulted to yellow.");
            }

            if (nucapsConfig.getTotallyFailedColorName() != null) {
                failedColor = RGBColors
                        .getRGBColor(nucapsConfig.getTotallyFailedColorName());
            } else {
                statusHandler.warn(
                        "Color for failed QC is not defined. Defaulted to red.");
            }

            if (nucapsConfig.getDefaultColorName() != null) {
                otherColor = RGBColors
                        .getRGBColor(nucapsConfig.getDefaultColorName());
            } else {
                statusHandler.warn(
                        "Color for default QC is not defined. Defaulted to gray.");
            }
        } catch (Exception e) {
            statusHandler.error("Failed to parse nucapsConfig.xml", e);
        }

        if (passedColor == null) {
            passedColor = new RGB(0, 255, 0);
        }
        if (partialPassedColor == null) {
            partialPassedColor = new RGB(255, 255, 0);
        }
        if (failedColor == null) {
            failedColor = new RGB(255, 0, 0);
        }
        if (otherColor == null) {
            otherColor = new RGB(190, 190, 190);
        }
    }
}
