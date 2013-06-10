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
package com.raytheon.uf.viz.npp.sounding.rsc;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDisplay;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Sounding available resource. Draws points on map where data is available
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NPPSoundingMapResource extends
        AbstractVizResource<NPPSoundingMapResourceData, IMapDescriptor> {

    private NPPSoundingMapInputHandler inputManager;

    private Collection<NPPSoundingRecord> allRecords = new ArrayList<NPPSoundingRecord>();

    private Map<DataTime, Collection<NPPSoundingRecord>> groupedRecords = new HashMap<DataTime, Collection<NPPSoundingRecord>>();

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected NPPSoundingMapResource(NPPSoundingMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.inputManager = new NPPSoundingMapInputHandler(this);
        this.dataTimes = new ArrayList<DataTime>();
        getCapability(EditableCapability.class).setEditable(true);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    if (object instanceof PluginDataObject[]) {
                        addRecords((PluginDataObject[]) object);
                    }
                }
            }
        });
    }

    Collection<NPPSoundingRecord> getCurrentRecords() {
        return groupedRecords.get(descriptor.getTimeForResource(this));
    }

    public synchronized void addRecords(PluginDataObject... records) {
        for (PluginDataObject record : records) {
            if (record instanceof NPPSoundingRecord) {
                if (allRecords.contains(record) == false) {
                    allRecords.add((NPPSoundingRecord) record);
                }
            }
        }
        Map<DataTime, Collection<NPPSoundingRecord>> groupedRecords = resourceData
                .groupRecordTimes(allRecords);
        List<DataTime> dataTimes = new ArrayList<DataTime>(
                groupedRecords.keySet());
        Collections.sort(dataTimes);
        this.dataTimes = dataTimes;
        this.groupedRecords = groupedRecords;
    }

    @Override
    public synchronized void remove(DataTime dataTime) {
        Collection<NPPSoundingRecord> records = groupedRecords.remove(dataTime);
        if (records != null) {
            allRecords.removeAll(records);
        }
        super.remove(dataTime);
    }

    @Override
    protected void disposeInternal() {
        inputManager.dispose();
        getResourceContainer().unregisterMouseHandler(inputManager);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        Collection<NPPSoundingRecord> records = this.groupedRecords.get(time);
        if (records == null) {
            return;
        }
        RGB color = getCapability(ColorableCapability.class).getColor();
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
            circle.basics.color = color;
            circle.filled = true;
            circles.add(circle);
        }
        target.drawCircle(circles.toArray(new DrawableCircle[0]));

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
        getResourceContainer().registerMouseHandler(inputManager);
    }

    /**
     * Check if the resource is editable
     * 
     */
    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    double getRadius() {
        return 5 * getCapability(MagnificationCapability.class)
                .getMagnification();
    }

    @Override
    public String getName() {
        return resourceData.getResourceName() + " Availability";
    }

    /**
     * @param closestRecord
     */
    void loadSoundingResource(NPPSoundingRecord record) {
        // Build metadata map for sounding resource
        HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        RequestConstraint rc = new RequestConstraint(null,
                ConstraintType.BETWEEN);
        rc.setBetweenValueList(new String[] {
                String.valueOf(record.getLongitude() - 0.01),
                String.valueOf(record.getLongitude() + 0.01) });
        metadataMap.put(NPPSoundingRecord.LONGITUDE, rc);
        rc = new RequestConstraint(null, ConstraintType.BETWEEN);
        rc.setBetweenValueList(new String[] {
                String.valueOf(record.getLatitude() - 0.01),
                String.valueOf(record.getLatitude() + 0.01) });
        metadataMap.put(NPPSoundingRecord.LATITUDE, rc);

        // Construct nsharp resource data for loading
        AbstractNPPNSharpResourceData nsResourceData = resourceData
                .newNsharpResourceData();
        nsResourceData.setCoordinate(new Coordinate(record.getLongitude(),
                record.getLatitude()));
        nsResourceData.setMetadataMap(metadataMap);
        ResourcePair pair = new ResourcePair();
        pair.setResourceData(nsResourceData);
        pair.setLoadProperties(new LoadProperties());
        NsharpSkewTPaneDisplay display = new NsharpSkewTPaneDisplay();
        display.setDescriptor(new NsharpSkewTPaneDescriptor());
        display.getDescriptor().getResourceList().add(pair);
        String editorId = DescriptorMap.getEditorId(display.getDescriptor()
                .getClass().getName());
        AbstractEditor editor = UiUtil.createOrOpenEditor(editorId,
                display.cloneDisplay());
        Bundle b = new Bundle();
        b.setDisplays(new AbstractRenderableDisplay[] { display });
        Job j = new BundleProductLoader(editor, b);
        j.schedule();
    }
}
