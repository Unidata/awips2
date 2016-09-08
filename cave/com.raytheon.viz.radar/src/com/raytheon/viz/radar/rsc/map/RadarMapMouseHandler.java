package com.raytheon.viz.radar.rsc.map;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.BundleUtil;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.uf.viz.core.procedures.BundleUtil.BundleDataItem;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;
import com.raytheon.viz.ui.actions.LoadBundleHandler;
import com.raytheon.viz.ui.actions.LoadPerspectiveHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;

public class RadarMapMouseHandler extends InputHandlerDefaultImpl {

	private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMapMouseHandler.class);
	
	public RadarMapMouseHandler() {
		instance = this;
	}

	private static final double RadarMinDistance = 45000;
	
	private static RadarMapMouseHandler instance;
	
	private double lat, lon;

    public double getLat() {
        return lat;
    }

    public double getLon() {
        return lon;
    }
    
    public static RadarMapMouseHandler getAccess() {
        return instance;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int) handle left button, so user be able to shift map while it is
     * down
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int button) {
        return false;

    }

    @Override
    public boolean handleMouseMove(Event e) {
    	int x = e.x;
    	int y = e.y;
    	AbstractEditor mapEditor = RadarMapResource.getMapEditor();
        
    	if (mapEditor != null) {
            
        	Coordinate loc = mapEditor.translateClick(x, y);
            if (loc == null)
                return false;
            
            List<RadarStation> pts = RadarMapResource
        			.getOrCreateRadarMapResource().getPoints();
            
            RadarStation pt = getPtWithinMinDist(pts, loc);
            
            Shell shell = ((Control) e.widget).getShell();
            Cursor cursor = null;
            
            if (pt != null) {
            	cursor = new Cursor(Display.getCurrent(), SWT.CURSOR_HAND);
            } else {
            	cursor = new Cursor(Display.getCurrent(), SWT.CURSOR_ARROW);
            }
            
        	shell.setCursor(cursor);
        	
        }
    	
        return false;
    }
    
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     * handle right button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
    	
    	boolean returnStatus = false;
    	
        if (!RadarMapResource.getMapRsc().isEditable())
            return false;
        
        
        
        // left mouse button
        if (button == 1) {
            AbstractEditor mapEditor = RadarMapResource.getMapEditor();
            if (mapEditor != null) {
            	
                Coordinate loc = mapEditor.translateClick(x, y);
                if (loc == null)
                    return false;
                
                List<RadarStation> pts = RadarMapResource
            			.getOrCreateRadarMapResource().getPoints();
                RadarStation pt = getPtWithinMinDist(pts, loc);
                
                if (pt != null) {
	                try {
	                	
	                	IWorkbenchWindow window = VizWorkbenchManager.getInstance()
	                            .getCurrentWindow();
	                	AbstractVizPerspectiveManager mgr = VizPerspectiveListener.getInstance(
	                            window).getActivePerspectiveManager();
	
	                    if (mgr != null) {
	                        //mgr.openNewEditor();
	                        
	                        Map<String, String> variableSubstitutions = new HashMap<>();
	                        // TODO: dynamically select this from some control
	                        variableSubstitutions.put("product1", "94");
	                        variableSubstitutions.put("product2", "99");
	                        
	                        new LoadBundleHandler("bundles/site/Radar_" + pt.getName().toLowerCase() + ".xml", 
	                        		variableSubstitutions, null, true).execute(null);
	                        returnStatus = true;
	                    }
						
					} catch (ExecutionException e) {
						e.printStackTrace();
						return false;
					}
                }
            }
        }
     
        return returnStatus;
    }
    
    
    /**
     * Gets the nearest point of an selected element to the input point
     * 
     * @param el
     *            element
     * @param pt
     *            input point
     * @return 
     */
    private RadarStation getPtWithinMinDist(
    		List<RadarStation> points, Coordinate pt) {
    	
        RadarStation thePoint = null;
        double minDistance = RadarMinDistance;
        
        GeodeticCalculator gc;
        // can't assume this is a map Editor/MapDescriptor
        AbstractEditor mapEditor = RadarMapResource.getMapEditor();
        if (mapEditor != null) {
            IMapDescriptor desc = (IMapDescriptor) mapEditor
                    .getActiveDisplayPane().getRenderableDisplay()
                    .getDescriptor();
            gc = new GeodeticCalculator(desc.getCRS());
            gc.setStartingGeographicPoint(pt.x, pt.y);
            for (RadarStation selectPoint : points) {

                gc.setDestinationGeographicPoint(selectPoint.getLon(),
                		selectPoint.getLat());
                double dist;
                try {
                    dist = gc.getOrthodromicDistance();
                    if (dist < minDistance) {
                        minDistance = dist;
                        thePoint = selectPoint;
                    }
                } catch (Exception e) {
                    statusHandler.handle(
                    		Priority.ERROR,"getOrthodromicDistance exception.",e);
                }
            }
           
            RadarMapResource.getOrCreateRadarMapResource()
            .setPickedPoint(thePoint);
        }
        return thePoint;

    }
    
    
}
