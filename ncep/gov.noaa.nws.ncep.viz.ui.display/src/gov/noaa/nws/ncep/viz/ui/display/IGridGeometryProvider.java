package gov.noaa.nws.ncep.viz.ui.display;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;


public interface IGridGeometryProvider {

	public abstract GeneralGridGeometry getGridGeometry();
	
	public abstract String getProviderName();
	
	public abstract double[] getMapCenter();
	
	// either a double from 0 to 1.0 or FitToScreen or SizeOfImage
	//	
	public enum ZoomLevelStrings { 
		FitToScreen, SizeOfImage
	}
	
	public abstract String getZoomLevel();
	
	public abstract void setZoomLevel( String zl );
	
}
