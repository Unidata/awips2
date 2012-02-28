/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.status.StatusConstants;
//import com.raytheon.viz.core.contours.Activator;
//import gov.noaa.nws.ncep.viz.common.ui.NmapUiUtils;
import gov.noaa.nws.ncep.gempak.parameters.marker.MARKER;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.Activator;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourSupport.ContourGroup;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import com.raytheon.viz.pointdata.PointWindDisplay;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Display grid point values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June, 2010    164        M. Li     	Initial creation
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */

public class GridPointMarkerDisplay implements IRenderable {

    private final IMapDescriptor descriptor;

    private SymbolLocationSet gridPointMarkerSet;
    

    
	public GridPointMarkerDisplay(String markerAttr, IMapDescriptor descriptor, ISpatialObject gridLocation) {

        this.descriptor = descriptor;
        
        gridPointMarkerSet = null;
        
    	int nx = gridLocation.getNx();
    	int ny = gridLocation.getNy();
    	Coordinate[] locations = new Coordinate[nx*ny];
        String markerName = "PLUS_SIGN";
        Color markerColor = new Color(255, 255, 255);
        double markerSize = 1.0;
        float markerWidth = 1.0f;

        if (markerAttr != null) {
        	MARKER marker = new MARKER(markerAttr);
        	RGB color = marker.getMarkerColor();
        	markerColor = new Color(color.red, color.green, color.blue);
        	markerName = marker.getMarkerName();
        	markerSize = marker.getMarkerSize();
        	markerWidth = marker.getMarkerWidth();
        }
        
    	Color[] colors = new Color[] {markerColor};
    	
    	int n = 0;
    	for (int i = 0; i < nx; i++) {
    		for (int j = 0; j < ny; j++) {
    			ReferencedCoordinate c = new ReferencedCoordinate(
    					new Coordinate(i, j),
    					MapUtil.getGridGeometry(gridLocation), Type.GRID_CORNER);

    			try {
    				if (c != null) {
    					double lat = c.asLatLon().y;
    					double lon = c.asLatLon().x;
    					if (j == 0 && lat == 90) lat -= 0.2;
    					if (j == ny - 1 && lat == -90) lat += 0.2;
    					
    					locations[n++] = new Coordinate(lon, lat);
    				}	
    			 } catch (TransformException e) {
    				 e.printStackTrace();
    			 } catch (FactoryException e) {
    				 e.printStackTrace();
    			 }
    		}
    	}
    	
    	gridPointMarkerSet = new SymbolLocationSet (
				null,
				colors,
				markerWidth,
				markerSize,
				false,
				locations,
				"Markers",
				markerName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        
    	if (paintProps.isZooming()) {
    		return;
    	}
    	
    	if (gridPointMarkerSet != null) {
			DisplayElementFactory df = new DisplayElementFactory (target, this.descriptor);
			ArrayList<IDisplayable> elements = df.createDisplayElements(gridPointMarkerSet, paintProps);
			for (IDisplayable each : elements)
			{
				if (each == null) continue;

				each.draw(target);
				each.dispose();
			}
		}
    	
    }
    
   
}
