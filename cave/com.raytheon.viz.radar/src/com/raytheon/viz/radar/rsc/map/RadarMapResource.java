package com.raytheon.viz.radar.rsc.map;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

public class RadarMapResource extends
		AbstractVizResource<RadarMapResourceData, MapDescriptor> implements
		RemoveListener {
	private static RadarMapResource mapRsc = null;
			
	private static RadarMapResourceData mapRscData = null;
	
	private static AbstractEditor mapEditor = null;
	
	private static RadarMapMouseHandler mouseHandler;

	private static Cursor waitCursor = null;

    private static Control cursorControl;

    private static boolean mouseHandlerRegistered = false;
    
    public static void bringMapEditorToTop() {
    	// get current map editor, if non existent create one.
        try {
            if (mapEditor != null
                    && PlatformUI.getWorkbench() != null
                    && PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null
                    && PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage() != null) {
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().bringToTop(mapEditor);
                mapEditor.refresh();
            }
        } catch (Exception e) {
        }
    }
    
    public static AbstractEditor getMapEditor() {
        return mapEditor;
    }

    public static RadarMapResource getMapRsc() {
        return mapRsc;
    }
    
    private RadarMapResourceData radarMapResourceData;
    
    /** The set of symbols with similar attributes across many locations */
    private SymbolLocationSet symbolSet = null;

    private SymbolLocationSet symbolToMark = null;
        
    private static List<RadarStation> points = new ArrayList<RadarStation>();
    
    private RadarStation pickedPoint = new RadarStation();
    
    public void setPickedPoint(RadarStation point) {
    	this.pickedPoint = null;
        this.pickedPoint = point;
    }

    public List<RadarStation> getPoints() {
        return points;
    }
    
    public void setPoints(List<RadarStation> points) {
        if (points == null) {
            this.pickedPoint = null;
            symbolToMark = null;
            symbolSet = null;
            this.points.clear();
        } else {
            this.points = points;
        }
    }

    public void addPoint(RadarStation point) {
        points.add(point);
    }

    protected RadarMapResource(RadarMapResourceData radarMapResourceData,
    		LoadProperties loadProperties) {
    	super(radarMapResourceData, loadProperties);
    	
        getCapability(EditableCapability.class).setEditable(true);

        this.radarMapResourceData = radarMapResourceData;
    }

    public static void startWaitCursor() {
        waitCursor = new Cursor(Display.getCurrent(), SWT.CURSOR_WAIT);
        cursorControl = Display.getCurrent().getCursorControl();
        if (cursorControl != null && waitCursor != null)
            cursorControl.setCursor(waitCursor);
    }

    public static void stopWaitCursor() {
        if (cursorControl != null && waitCursor != null) {
            cursorControl.setCursor(null);
        }
        if (waitCursor != null) {
            waitCursor.dispose();
            waitCursor = null;
        }
    }
    
    private static void createMapEditor() {
    	deleteRadarMapResource();
        try {
        	mapEditor = (AbstractEditor) EditorUtil.getActiveEditor();
        } catch (Exception ve) {
            System.out
                    .println("RadarMapResource Could not load initial editor: "
                            + ve.getMessage());
            ve.printStackTrace();
        }
    }
    
    public static void registerMouseHandler() {
        if (mouseHandlerRegistered)
            return;

        mouseHandler = getMouseHandler();
        if (mapEditor != null && mouseHandler != null) {
            mapEditor.registerMouseHandler((IInputHandler) mouseHandler);
            mouseHandlerRegistered = true;
        }
    }

    public static void unregisterMouseHandler() {
        if (!mouseHandlerRegistered)
            return;
        mouseHandler = getMouseHandler();
        if (mapEditor != null && mouseHandler != null) {
            mapEditor.unregisterMouseHandler((IInputHandler) mouseHandler);
            mouseHandlerRegistered = false;
        }
    }
    
    /**
    * Create a new MapResource and add it to the current editor.
    * 
    * @return the MapResource
    */
   public static RadarMapResource getOrCreateRadarMapResource() {
	   
       if (mapRsc == null) {
           if (mapEditor == null) {
        	   createMapEditor();
           }
           if (mapEditor != null) {
               IMapDescriptor desc = (IMapDescriptor) mapEditor
                       .getActiveDisplayPane().getRenderableDisplay()
                       .getDescriptor();
               try {
                   if (mapRscData == null)
                       mapRscData = new RadarMapResourceData();
                   mapRsc = mapRscData.construct(new LoadProperties(), desc);
                   
                   createRadarMapMarkers();
                   
                   desc.getResourceList().add(mapRsc);
                   mapRsc.init(mapEditor.getActiveDisplayPane().getTarget());
                   mouseHandler = getMouseHandler();
                   mapEditor
                           .registerMouseHandler((IInputHandler) mouseHandler);

               } catch (Exception e) {
                   e.printStackTrace();
               }
           }
       }
       return mapRsc;
   }
   
   private static void createRadarMapMarkers() {
       String query = "SELECT lat, lon, rda_id FROM radar_spatial where rda_id like 'K%' ;";
       List<Object[]> rows = null;
       try {
    	   rows = DirectDbQuery.executeQuery(query, "metadata",
                   QueryLanguage.SQL);
       } catch (VizException e) {
           e.printStackTrace();
           rows = new ArrayList<Object[]>();
       }
      
       for (int i = 0; i < rows.size(); i++) {
    	   RadarStation pnt = new RadarStation();
    	   Object[] pntObject = rows.get(i);
    	   pnt.setLat((Float) pntObject[0]);
    	   pnt.setLon((Float) pntObject[1]);
    	   pnt.setName((String) pntObject[2]);
    	   pnt.setRdaId((String) pntObject[2]);
    	   mapRsc.addPoint(pnt);
       }
   }
   
   public static void deleteRadarMapResource() {
	   System.out.println("RadarMapResource:deleteRadarMapResource ");
       if (mapRsc != null) {
           mapRsc.dispose();
           mapRsc = null;
       }
   }
   
   /**
    * Called when resource is disposed
    * 
    * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
    */
   @Override
   public void disposeInternal() {
       if (mapEditor != null) {
           mapEditor.unregisterMouseHandler(mouseHandler);
           mouseHandler = null;
           mapEditor = null;
       }
       pickedPoint = null;
       //points = null;
       symbolSet = null;
       symbolToMark = null;
       mapRsc = null;
       mapRscData = null;
       if (waitCursor != null)
           waitCursor.dispose();
       waitCursor = null;
       mouseHandlerRegistered = false;
   }

   public CoordinateReferenceSystem getCoordinateReferenceSystem() {
       if (descriptor == null)
           return null;
       return descriptor.getCRS();
   }
   
   @Override
   public String getName() {
       return "NEXRAD Stations";
   }

   @Override
   public void initInternal(IGraphicsTarget target) throws VizException {
       // make the map resource editable
       EditableManager.makeEditable(this,
               getCapability(EditableCapability.class).isEditable());
   }
   
   public boolean isApplicable(PixelExtent extent) {
       return true;
   }
   
   private void generateSymbolForDrawing() {
       String type;
       float lineWidth = radarMapResourceData.getMarkerWidth();
       Boolean clear = false;

       String category = new String("Marker");
       double sizeScale = radarMapResourceData.getMarkerSize();

       if (points.isEmpty() == true) {
           symbolSet = null;
       } else {
           Coordinate[] locations = new Coordinate[points.size()];
           Color[] colors = new Color[] { new Color(255,255,255) };
           int i = 0;
           for (RadarStation p : points) {
               double lon, lat;
               lon = p.getLon();
               lat = p.getLat();
               locations[i++] = new Coordinate(lon, lat);
           }
           type = radarMapResourceData.getMarkerType().toString();
           symbolSet = new SymbolLocationSet(null, colors, lineWidth,
                   sizeScale, clear, locations, category, type);

       }

       // generate symbol for picked stn to mark X       
       if (this.pickedPoint.getLon() != null) {
           Coordinate[] locations = new Coordinate[0];
           double lon, lat;
           lon = this.pickedPoint.getLon();
           lat = this.pickedPoint.getLat();
           locations[0] = new Coordinate(lon, lat);
           
           type = radarMapResourceData.getStnMarkerType().toString();
           Color[] colors = new Color[] { new Color(0,255,0) };
           symbolToMark = new SymbolLocationSet(null, colors, lineWidth,
                   sizeScale * 2, clear, locations, category, type);
       } else
           symbolToMark = null;
   }
   
   @Override
   public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
           throws VizException {
	   
	   getOrCreateRadarMapResource();
       generateSymbolForDrawing();
       
       DisplayElementFactory df = new DisplayElementFactory(target,
               this.descriptor);
       if (symbolSet != null) {
           ArrayList<IDisplayable> elements = df.createDisplayElements(
                   symbolSet, paintProps);
           for (IDisplayable each : elements) {
               try {
                   each.draw(target, paintProps);
                   each.dispose();
               } catch (Exception e) {
                   e.printStackTrace();
               }
           }
       }
       if (symbolToMark != null) {
           ArrayList<IDisplayable> elements = df.createDisplayElements(
                   symbolToMark, paintProps);
           for (IDisplayable each : elements) {
               try {
                   each.draw(target, paintProps);
                   each.dispose();
               } catch (Exception e) {
                   e.printStackTrace();
               }
           }
       }
   }
   
   public boolean isProjectable(CoordinateReferenceSystem mapData) {
       return true;
   }
   
   @Override
   public void project(CoordinateReferenceSystem mapData) throws VizException {
       // System.out.println("RadarMapResource: project ");
   }
   
   private static RadarMapMouseHandler getMouseHandler() {
       if (mouseHandler == null) {
           mouseHandler = new RadarMapMouseHandler();
       }
       return mouseHandler;
   }
   
   @Override
   public void notifyRemove(ResourcePair rp) throws VizException {
       // TODO Auto-generated method stub
   }
   
   public boolean isEditable() {
       return getCapability(EditableCapability.class).isEditable();
   }

   public void setEditable(boolean enable) {
       getCapability(EditableCapability.class).setEditable(enable);
       EditableManager.makeEditable(this,
               getCapability(EditableCapability.class).isEditable());
   }

}
