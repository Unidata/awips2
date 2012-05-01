/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic.RadarMLResource
 * 
 * 12-16-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarTileSet;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * This class is based on Raytheon's code
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/16/2011   #541       S. Gurung   Initial creation
 * 03/30/2012   #651       S. Gurung   Fixed NullPointerException bug in resourceChanged
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarMLResource extends RadarGraphicsResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMLResource.class);

    private Map<DataTime, Map<Integer, IWireframeShape>> shapes;

    private boolean refresh = false;

    private Map<Integer, LineStyle> style;

    private static final String USING_BEAM_EDGE = "Using Beam Edge";

    private static final String USING_BEAM_CENTER = "Using Beam Center";

    private static final int X_OFFSET = 300;

    private static final int Y_OFFSET = 50;

    private IFont textFont;
   
    /**
     * @param rrd
     * @param loadProps
     * @param interrogator
     * @throws VizException
     */
    public RadarMLResource(RadarResourceData rrd, LoadProperties loadProps,
            IRadarInterrogator interrogator) throws VizException {
        super(rrd, loadProps, interrogator);
        shapes = new HashMap<DataTime, Map<Integer, IWireframeShape>>();

        style = new HashMap<Integer, LineStyle>();
        style.put(1, LineStyle.DASHED);
        style.put(2, LineStyle.SOLID);
        style.put(3, LineStyle.SOLID);
        style.put(4, LineStyle.DASHED);
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.ncradar.rsc.AbstractRadarResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
	public void initResource(IGraphicsTarget target) throws VizException {
        super.queryRecords();
        // initialize the font
        textFont = target.initializeFont(java.awt.Font.DIALOG, 11.0f,
        	 new IFont.Style[] {});
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.ncradar.rsc.graphic.RadarGraphicsResource#paintInternal
     * (com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
	protected void paintFrame(AbstractFrameData frameData,
			   IGraphicsTarget target, PaintProperties paintProps ) throws VizException{
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        Map<Integer, IWireframeShape> shapeMap = shapes.get(paintProps
                .getDataTime());
        if (shapeMap == null || refresh == true) {
            if (shapeMap != null) {
                for (IWireframeShape w : shapeMap.values()) {
                    if (w != null) {
                        w.dispose();
                    }
                }
            }
            shapeMap = new HashMap<Integer, IWireframeShape>();
            shapes.put(paintProps.getDataTime(), shapeMap);
            displayedDate = null;

            IWireframeShape ws = null;

            if ((paintProps == null) || (paintProps.getDataTime() == null)) {
                return;
            }

            displayedDate = paintProps.getDataTime();

            displayedLevel = displayedDate.getLevelValue().floatValue();

            // retrieve record if not yet populated
            RadarRecord radarRecord = getRadarRecord(displayedDate);
            if (radarRecord == null) {
                return;
            }

            File loc = HDF5Util.findHDF5Location(radarRecord);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radarRecord);
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            ProjectedCRS crs = radarRecord.getCRS();
            // Per section 3.3.3
            generalEnvelope.setCoordinateReferenceSystem(crs);
            generalEnvelope.setRange(0, -256000 * 2, 256000 * 2);
            generalEnvelope.setRange(1, -256000 * 2, 256000 * 2);

            // [-2048, 2048] == range of 4095 (inclusive 0), plus 1 because
            // GGR is exclusive (?)
            GeneralGridGeometry gg = new GeneralGridGeometry(
                    new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                            4096, 4096 }, false), generalEnvelope);
            SymbologyBlock block = radarRecord.getSymbologyBlock();
            ReferencedCoordinate coordinate1;
            ReferencedCoordinate coordinate2;
            Map<Integer, Coordinate[]> coordinates = new HashMap<Integer, Coordinate[]>();
            if (block != null) {
                for (int i = 0; i < block.getNumLayers(); i++) {
                    for (int j = 1; j < block.getNumPackets(i); j++) {
                        if (block.getPacket(i, j) instanceof LinkedContourVectorPacket) {
                            List<LinkedVector> vector = ((LinkedContourVectorPacket) block
                                    .getPacket(i, j)).getVectors();
                            Coordinate[] coords = new Coordinate[vector.size() + 1];
                            for (int l = 0; l < coords.length - 1; l++) {
                                if (!coordinates.containsKey(vector.get(l)
                                        .getTheColor())) {
                                    coordinates.put(Integer.valueOf(vector.get(
                                            l).getTheColor()), coords);
                                }

                                // transform the coordinates to the correct
                                // locations
                                coordinate1 = new ReferencedCoordinate(
                                        rectifyCoordinate(new Coordinate(vector
                                                .get(l).getI1(), vector.get(l)
                                                .getJ1())), gg,
                                        Type.GRID_CENTER);
                                coordinate2 = new ReferencedCoordinate(
                                        rectifyCoordinate(new Coordinate(vector
                                                .get(l).getI2(), vector.get(l)
                                                .getJ2())), gg,
                                        Type.GRID_CENTER);
                                try {
                                    // coords[l] = coordinate1.asLatLon();
                                    coords[l] = coordinate2.asLatLon();
                                } catch (TransformException e1) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e1.getLocalizedMessage(), e1);

                                } catch (FactoryException e1) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e1.getLocalizedMessage(), e1);

                                }
                            }
                            coords[coords.length - 1] = coords[0];
                        }
                    }
                }
                refresh = false;
            }

            // looping through the coordinates in order to create a wireframe
            // shape
            for (Integer num : coordinates.keySet()) {
                if (shapeMap.get(num) == null) {
                    ws = target.createWireframeShape(true, this.descriptor);
                } else {
                    ws = shapeMap.get(num);
                }
                ws.addLineSegment(coordinates.get(num));
                shapeMap.put(num, ws);
            }
        }

        if (shapeMap != null) {
            for (Integer num : shapeMap.keySet()) {
                LineStyle lineStyle = style.get(num);
                if (getCapability(OutlineCapability.class).getLineStyle() != LineStyle.DEFAULT) {
                    lineStyle = getCapability(OutlineCapability.class)
                            .getLineStyle();
                }
                target.drawWireframeShape(shapeMap.get(num),
                        getCapability(ColorableCapability.class).getColor(),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(), lineStyle);
            }
        }

        // paint the legend for melting layer
        IExtent extent = paintProps.getView().getExtent();
        double ratio = extent.getWidth() / paintProps.getCanvasBounds().width;
        textFont.setMagnification((float) (getCapability(
                MagnificationCapability.class).getMagnification().floatValue() * 1.5));
        LineStyle dashedLine = LineStyle.DASHED;
        LineStyle solidLine = LineStyle.SOLID;
        if (getCapability(OutlineCapability.class).getLineStyle() != LineStyle.DEFAULT) {
            dashedLine = getCapability(OutlineCapability.class).getLineStyle();
            solidLine = getCapability(OutlineCapability.class).getLineStyle();
        }

        String[] text = new String[2];
        text[0] = USING_BEAM_EDGE;
        text[1] = USING_BEAM_CENTER;
        target.clearClippingPlane();

        target.drawLine(extent.getMinX() + (X_OFFSET - X_OFFSET / 2.7) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize())
                        * ratio, 0, extent.getMinX() + (X_OFFSET - 10) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize())
                        * ratio, 0, getCapability(ColorableCapability.class)
                        .getColor(), getCapability(OutlineCapability.class)
                        .getOutlineWidth(), dashedLine);

        target.drawLine(extent.getMinX() + (X_OFFSET - X_OFFSET / 2.7) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize() * 2)
                        * ratio, 0, extent.getMinX() + (X_OFFSET - 10) * ratio,
                extent.getMinY() + (Y_OFFSET - 5 + textFont.getFontSize() * 2)
                        * ratio, 0, getCapability(ColorableCapability.class)
                        .getColor(), getCapability(OutlineCapability.class)
                        .getOutlineWidth(), solidLine);
        RGB[] rgbs = new RGB[text.length];
        for (int i = 0; i < text.length; i++) {
            rgbs[i] = getCapability(ColorableCapability.class).getColor();
        }
        DrawableString info = new DrawableString(text, rgbs);
        info.font = textFont;
        info.horizontalAlignment = HorizontalAlignment.LEFT;
        info.verticallAlignment = VerticalAlignment.TOP;
        info.setCoordinates(extent.getMinX() + X_OFFSET * ratio,
                extent.getMinY() + Y_OFFSET * ratio);
        info.textStyle = TextStyle.NORMAL;
        target.drawStrings(info);
        target.setupClippingPlane(extent);
    }
    
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        for (Map<Integer, IWireframeShape> innerMap : shapes.values()) {
            if (innerMap != null) {
                for (IWireframeShape ws : innerMap.values()) {
                    if (ws != null) {
                        ws.dispose();
                    }
                }
            }
        }

        if (textFont != null) {
            textFont.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.ncradar.rsc.graphic.RadarGraphicsResource#resourceChanged
     * (com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            refresh = true;
        }
        else if ( type == ChangeType.CAPABILITY ){
        	RadarResourceData radarRscData = ( RadarResourceData ) this.resourceData;
        	if (object instanceof ImagingCapability ){
        		ImagingCapability imgCap = getCapability(ImagingCapability.class);
           		ImagingCapability newImgCap = ( ImagingCapability ) object;
        		imgCap.setBrightness(newImgCap.getBrightness(), false);
        		imgCap.setContrast(newImgCap.getContrast(), false);
        		imgCap.setAlpha(newImgCap.getAlpha(), false);
                radarRscData.setAlpha(  imgCap.getAlpha()  );
                radarRscData.setBrightness(  imgCap.getBrightness() );
                radarRscData.setContrast(  imgCap.getContrast() );
	
        	}
        	else if (object instanceof ColorMapCapability ){
        		
        		ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
        		ColorMapCapability newColorMapCap = (ColorMapCapability) object;
        		if (newColorMapCap.getColorMapParameters() != null) {
	        		colorMapCap.setColorMapParameters(newColorMapCap.getColorMapParameters(), false);
	        		ColorMap theColorMap = ( ColorMap ) colorMapCap.getColorMapParameters().getColorMap();
	        		String colorMapName = colorMapCap.getColorMapParameters().getColorMapName();
	        		radarRscData.setColorMapName( colorMapName );
	        	    radarRscData.getRscAttrSet().setAttrValue( "colorMapName", colorMapName );
	        	    ColorBarFromColormap cBar = radarRscData.getColorBar();
	        	    cBar.setColorMap( theColorMap );
	        	    radarRscData.getRscAttrSet().setAttrValue( "colorBar", cBar );
	        	    radarRscData.setIsEdited( true );
        		}

        	}
        	
        	issueRefresh();
        	
        }
        super.resourceChanged(type, object);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.ncradar.rsc.graphic.RadarGraphicsResource#project(org.
     * opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        refresh = true;
        issueRefresh();
    }

    public Coordinate rectifyCoordinate(Coordinate c) {
        c.x += 2048;
        c.y += 2048;

        c.y = 4096 - c.y;
        return c;
    }
}
