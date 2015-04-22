package gov.noaa.nws.ncep.viz.common.display;

import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/19/09      #972      ghull        Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public interface INatlCntrsRenderableDisplay extends IRenderableDisplay, IPaneLayoutable {

    public abstract double getZoomLevel();

    public abstract double[] getMapCenter();
    
//    public abstract void setPaneName( String p );    
//    public abstract String getPaneName();

//   interface for PredefinedArea's to support NonMaps and others? What would it be for CrossSections..??? 
//    or can we limit it to Rbd Loadable displays?    	
    
    public abstract PredefinedArea getInitialArea();
    public void setInitialArea( PredefinedArea ia );
//    public abstract IGridGeometryProvider getCurrentArea();    
}
