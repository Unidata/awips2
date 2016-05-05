package com.raytheon.uf.viz.npp.nucaps.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
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
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */

public class NucapsSoundingMapResource extends NPPSoundingMapResource {
    private RGB green = new RGB(0, 255, 0);

    private RGB yellow = new RGB(255, 255, 0);

    private RGB red = new RGB(255, 0, 0);

    private RGB gray = new RGB(190, 190, 190);

    protected NucapsSoundingMapResource(
            NPPSoundingMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    protected NucapsSoundingMapResource(
            NucapsSoundingMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }
    
    @Override
    public synchronized void addRecords (PluginDataObject... records){
        try {
           PluginDataObject[] allRecords=((NucapsSoundingMapResourceData)this.resourceData).updatePluginDataObjects(records);
           super.addRecords(allRecords);
        } catch (VizException e) {
            statusHandler.handle(
                Priority.PROBLEM,
                "Error adding record from update: "
                    + e.getLocalizedMessage(), e);
       }
    }

    public synchronized void addRecordsNoUpdate (PluginDataObject... records){
       super.addRecords(records);
    }

    /**
     * Color code dots base on QC value
     * 
     * @param IGraphicsTarget target,
     *        PaintProperties paintProps
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
            circle.basics.color = getQCColor(record.getPointDataView().getInt(
                    NucapsRecord.PDV_QUALITY_FLAG));
            circle.filled = true;
            circles.add(circle);
        }
        target.drawCircle(circles.toArray(new DrawableCircle[0]));

    }

    /**
     * QC color model: 0: Green, 1/17: Yellow, 9/25: Red, others: Gray
     * 
     * @param qcFlag
     * @return RGB color
     */
    private RGB getQCColor(int qcFlag) {
        switch (qcFlag) {
        case 0:
            // pass
            return green;
        case 1:
        case 17:
            // Partially failed
            return yellow;
        case 9:
        case 25:
            // Totally failed
            return red;
        default:
            return gray;
        }
    }
}
