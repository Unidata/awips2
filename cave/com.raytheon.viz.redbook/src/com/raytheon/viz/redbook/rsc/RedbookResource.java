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
package com.raytheon.viz.redbook.rsc;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.Validate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.dataplugin.redbook.RedbookWMOMap;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.extratext.ExtraTextResourceData;
import com.raytheon.uf.viz.core.rsc.extratext.IExtraTextGeneratingResource;
import com.raytheon.viz.redbook.rsc.RedbookFrame.RedbookStatus;

/**
 * Redbook display resource
 * 
 * The redbook display resource is implemented as a series of redbook "frames"
 * (one per redbook file). The redbook frame class reads the redbook byte blob,
 * and draws the file to the screen when the paint method is invoked.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 28, 2008  1162     chammack  Initial creation
 * May 21, 2013  2001     njensen   Fixed display of messages
 * Mar 13, 2014  2907     njensen   split edex.redbook plugin into common and
 *                                  edex redbook plugins
 * Jun 26, 2015  4512     mapeters  Updated for RedbookWMOMap API changes
 * Oct 27, 2015  4798     bsteffen  Throw VizException for missing svg.
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * May 19, 2016  3253     bsteffen  Allow extra legend to be rendered as extra
 *                                  text.
 * 
 * </pre>
 * 
 * @author chammack
 */
public class RedbookResource extends
        AbstractVizResource<RedbookResourceData, MapDescriptor> implements
        IResourceDataChanged, IExtraTextGeneratingResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookResource.class);

    private final Map<DataTime, RedbookFrame> redbookFrames;

    private DataTime displayedDataTime;

    private IFont font;

    private WxSymbols wxSymbols;

    private String humanReadableName;

    private double magnification = 1.0;

    private boolean magnificationChanged = false;

    protected RedbookResource(RedbookResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        this.dataTimes = new ArrayList<>();
        resourceData.addChangeListener(this);
        this.redbookFrames = new HashMap<>();
        wxSymbols = new WxSymbols();
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }
        for (RedbookFrame frame : this.redbookFrames.values()) {
            frame.dispose();
        }
    }

    @Override
    public String getName() {
        if (this.getResourceData().getCustomLegend() != null) {
            return this.getResourceData().getCustomLegend();
        } else {
            if (this.humanReadableName == null) {
                buildHumanReadableName();
            }

            return this.humanReadableName;
        }
    }

    private void buildHumanReadableName() {
        this.humanReadableName = "Redbook Resource";

        RequestConstraint wmo = this.resourceData.getMetadataMap().get(
                "wmoTTAAii");
        if (wmo != null) {
            RedbookWMOMap map;
            try {
                map = RedbookWMOMap.load();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading redbook mapping: "
                                + "Unable to load redbook mapping file", e);
                return;
            }
            for (String wmoStr : wmo.getConstraintValue().split(",")) {
                wmoStr = wmoStr.trim();
                if (wmoStr.isEmpty()) {
                    continue;
                }
                RedbookWMOMap.Info info = map.getValue(wmoStr);
                if (info != null && info.name != null) {
                    this.humanReadableName = info.name;
                    break;
                }
            }
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        for (RedbookFrame frame : this.redbookFrames.values()) {
            frame.setDescriptor(descriptor);
        }
    }

    @Override
    public DataTime[] getDataTimes() {
        Set<DataTime> dataTimeSet = this.redbookFrames.keySet();
        DataTime[] dataTimes = dataTimeSet.toArray(new DataTime[dataTimeSet
                .size()]);
        return dataTimes;
    }

    @Override
    public void remove(DataTime dataTime) {
        RedbookFrame frame = this.redbookFrames.remove(dataTime);
        if (frame != null) {
            frame.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        ExtraTextResourceData.addExtraTextResource(descriptor);
        for (RedbookFrame frame : this.redbookFrames.values()) {
            if (!frame.hasInited()) {
                frame.init(target);
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.displayedDataTime = paintProps.getDataTime();

        RedbookStatus status = null;
        RedbookFrame frame = this.redbookFrames.get(this.displayedDataTime);
        if (frame != null) {
            magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();

            if (magnificationChanged || font == null) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
                font = target.initializeFont(target.getDefaultFont()
                        .getFontName(), (float) (8 * magnification), null);
                magnificationChanged = false;
            }

            if (!frame.hasInited()) {

                status = frame.init(target);
            }

            frame.paint(target, paintProps);
        }

        if (status != null) {
            if (status.unhandledPackets) {
                statusHandler
                        .debug("Warning: Unrecognized redbook packets found. Rendering may not be complete.");
            } else if (status.vectorRenderingWarning) {
                statusHandler
                        .debug("Warning: Some redbook vectors could not be rendered. Rendering may not be complete.");
            }
        }
    }

    public IFont getRenderingFont() {
        return this.font;
    }

    public WxSymbols getWxSymbols() {
        return this.wxSymbols;
    }

    public double getMagnification() {
        return this.magnification;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (RedbookFrame frame : this.redbookFrames.values()) {
            frame.deInit();
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            Validate.isTrue(object instanceof PluginDataObject[], "Expected a "
                    + PluginDataObject[].class + ", Got: " + object.getClass());

            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject record : pdos) {
                Validate.notNull(record, "PluginDataObject was null");
                Validate.isTrue(record instanceof RedbookRecord,
                        "RedbookResource expects RedbookRecords, got " + record);

                addRecord((RedbookRecord) record);
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                magnificationChanged = true;
            }
        }
        issueRefresh();
    }

    public void addRecord(RedbookRecord record) {

        RedbookRecord redbookRecord = record;
        DataTime dataTime = redbookRecord.getDataTime();
        if (resourceData.getBinOffset() != null) {
            dataTime = resourceData.getBinOffset().getNormalizedTime(dataTime);
        }
        File hdf5Loc = HDF5Util.findHDF5Location(redbookRecord);

        IDataStore ds = DataStoreFactory.getDataStore(hdf5Loc);
        IDataRecord dr;
        try {
            dr = ds.retrieve(redbookRecord.getDataURI(), "redbookData",
                    Request.ALL);
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to retrieve redbook data from repository", e);
            return;
        }

        if (!(dr instanceof ByteDataRecord)) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Expected to find byteData in repository, found: " + dr);
            return;
        }

        ByteDataRecord bdr = (ByteDataRecord) dr;

        RedbookFrame redbookFrame = new RedbookFrame(this, this.descriptor, bdr);

        this.redbookFrames.put(dataTime, redbookFrame);
    }

    @Override
    public String[] getExtraText(DataTime time) {
        RedbookFrame frame = this.redbookFrames.get(time);
        if (frame != null) {
            return frame.getExtraLegend();
        }
        return new String[0];
    }

}
