package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;

import java.util.List;

import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IExecutableExtensionFactory;

import com.raytheon.uf.viz.core.exception.VizException;

public interface INcAreaProviderFactory /*extends IExecutableExtension*/ {

	// datalocation, configData (from ext point) may be ignored.
	abstract void initialize( String sourceName, 
						      String dataLoc, String configData ) throws VizException;
	
	abstract AreaSource getAreaSource();
	
	abstract List<AreaName> getAvailableAreaNames();
	
	abstract IGridGeometryProvider createGeomProvider( String areaName ) throws VizException;
	
	abstract List<VizException> getInitializationExceptions();
	
//	abstract String getSourceDataLocation();
}
