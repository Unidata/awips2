package com.raytheon.viz.radar.rsc.map;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;

public class RadarMapMouseHandler extends InputHandlerDefaultImpl {

	private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMapMouseHandler.class);
	
	public RadarMapMouseHandler() {
		instance = this;
	}

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
    public boolean handleMouseMove(int x, int y) {
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
        if (!RadarMapResource.getMapRsc().isEditable())
            return false;
        // button 1 is left mouse button
        if (button == 1) {
            AbstractEditor mapEditor = RadarMapResource.getMapEditor();
            if (mapEditor != null) {
                // Check if mouse is in geographic extent
                Coordinate loc = mapEditor.translateClick(x, y);
                if (loc == null)
                    return false;
               
                List<RadarStation> points = RadarMapResource
                        .getOrCreateRadarMapResource().getPoints();
                if (points.isEmpty() == false) {

                    // get the stn close to loc "enough" and retrieve report for it
                    List<RadarStation> stnPtDataLineLst = getPtWithinMinDist(
                            points, loc);
                    if (stnPtDataLineLst != null) {                            
                        /*
                         * do something here
                         * 
                        obsQry.getObservedSndData(stnPtDataLineLst,
                                loadDia.getObsDialog().isRawData(),
                                soundingLysLstMap);
                        */
                    }
                }
                
                
            }
        }

        return false;
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
    private List<RadarStation> getPtWithinMinDist(
            List<RadarStation> points, Coordinate pt) {

        RadarStation thePoint = null;
        
        double minDistance = 45000;
        GeodeticCalculator gc;
        List<RadarStation> thePoints = new ArrayList<RadarStation>();
        // can't assume this is a map Editor/MapDescriptor
        AbstractEditor mapEditor = RadarMapResource.getMapEditor();
        if (mapEditor != null) {
            IMapDescriptor desc = (IMapDescriptor) mapEditor
                    .getActiveDisplayPane().getRenderableDisplay()
                    .getDescriptor();
            gc = new GeodeticCalculator(desc.getCRS());
            gc.setStartingGeographicPoint(pt.x, pt.y);
            for (Object point : points) {

                gc.setDestinationGeographicPoint(((RadarStation) point).getLon(),
                        ((RadarStation) point).getLat());
                double dist;
                try {
                    dist = gc.getOrthodromicDistance();
                    if (dist < minDistance) {

                        minDistance = dist;
                        thePoint = (RadarStation) point;
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                                    "RadarMapMouseHandler: getOrthodromicDistance exception.",
                                    e);
                }
            }
            // Chin, there may be more than one point for a selected stn. As
            // user may selected more than one data time,
            // For same stn, each data time will have one point to represent it.
            // So, a stn may have more than one points
            if (thePoint != null) {
                for (Object point : points) {
                    if ((thePoint.getLat() == ((RadarStation) point).getLat())
                            && (thePoint.getLon() == ((RadarStation) point).getLon())) {
                        thePoints.add((RadarStation) point);
                    }
                }

                // marked X on selected point
                RadarMapResource.getOrCreateRadarMapResource()
                        .setPickedPoint(thePoint);

            }

        }
        return thePoints;

    }
    
    
}
