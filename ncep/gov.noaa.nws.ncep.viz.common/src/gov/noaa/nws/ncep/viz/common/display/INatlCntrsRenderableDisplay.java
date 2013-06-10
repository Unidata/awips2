package gov.noaa.nws.ncep.viz.common.display;

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

    //??? Don't think we need thsi for NonMaps
//    public IGridGeometryProvider findAreaProviderResource( String areaProviderName );    
//    public void setPredefinedArea( PredefinedArea pArea ) throws VizException {
    // create a PredefinedArea using the current gridGeometry/center/zoom.
    // 
//    public PredefinedArea getCurrentArea();
    public abstract double getZoomLevel();

    public abstract double[] getMapCenter();
    
    public abstract void setPaneName( String p );
    
    public abstract String getPaneName();

// moved to IPaneLayoutable 
//    public abstract INcPaneID getPaneId();
//    
//    public abstract void setPaneId( INcPaneID pid);
//    
    // TODO? if null then set to the descriptors gridGeom??
//   interface for PredefinedArea's to support NonMaps and others? What would it be for CrossSections..??? 
//    or can we limit it to Rbd Loadable displays?    	
    
    public abstract IGridGeometryProvider getInitialArea();
    public void setInitialArea( IGridGeometryProvider ia );
//    public void setInitialArea( PredefinedArea ia );
    public abstract IGridGeometryProvider getCurrentArea();
    
//    public abstract void setInitialArea(IGridGeometryProvider area);
}
