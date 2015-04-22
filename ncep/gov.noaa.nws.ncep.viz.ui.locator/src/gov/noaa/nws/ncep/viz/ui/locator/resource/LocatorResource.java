package gov.noaa.nws.ncep.viz.ui.locator.resource;

import gov.noaa.nws.ncep.viz.common.CoorBean;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

import org.eclipse.swt.graphics.RGB;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class displays the Locator .
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *
 * 12/10/2011   #561        Greg Hull   make this a real nc resource
 * 12/14/1212   #903        Greg Hull   fontSize attribute, and color
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 * 
 */
public class LocatorResource extends AbstractVizResource<LocatorResourceData, IMapDescriptor> 
				implements INatlCntrsResource, PropertyChangeListener  {

	private LocatorResourceData locRscData;
	
	protected Coordinate currCoor = null;
	
	private IFont font = null;
	
	private LocatorDataSource locDataSources[] = 
		 		new LocatorDataSource[LocatorResourceData.MAX_NUM_SOURCES];

	private LocatorDisplayAttributes dispAttrs[] =
		        new LocatorDisplayAttributes[LocatorResourceData.MAX_NUM_SOURCES];

	private IGraphicsTarget currTarget = null;
	private PaintProperties currPaintProps = null; 
	
	public LocatorResource(LocatorResourceData resourceData, LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		locRscData = resourceData;
	}

	
	@Override
	protected void initInternal( IGraphicsTarget target ) throws VizException {
		CoorBean.getInstance().addPropertyChangeListener( this );

		LocatorDataSourceMngr dataSourceMngr = LocatorDataSourceMngr.getInstance();
		
		if( dataSourceMngr == null ) {
			throw new VizException("Error creating Locator Data Sources");
		}

		getDisplayAttributes();
		
		// we are in the init thread but we could always spawn a new thread for 
		// each 
		for( int p= 0 ; p<LocatorResourceData.MAX_NUM_SOURCES ; p++ ) {
			if( dispAttrs[p] != null ) {
				LocatorDataSource locDataSrc = 
					LocatorDataSourceMngr.getInstance().getLocatorDataSource( 
							dispAttrs[p].getLocatorSource() );
				if( locDataSrc != null ) {
					locDataSrc.loadSourceData();
				}

				locDataSources[p] = locDataSrc;
			}
		}		
	}
	

	@Override
	protected void paintInternal(IGraphicsTarget target, PaintProperties paintProps) 
																	throws VizException {
		currTarget = target;
		currPaintProps = paintProps;

		if( font == null ) {
			font = target.initializeFont( locRscData.getFontName(), 
					1.0f*locRscData.getFontSize(), null );//IFont.Style.BOLD );
			if( font == null ) {
				font = target.initializeFont( target.getDefaultFont().getFontName(), 
						1.0f*locRscData.getFontSize(), null );
			}
		}
		drawLocatorInfo();
	}
	
	// 
	private void  drawLocatorInfo( ) throws VizException {
		
		if( currCoor == null ) {
			return;
		}
		currTarget.clearClippingPlane();

		double r = currPaintProps.getView().getExtent().getWidth() / currPaintProps.getCanvasBounds().width;       
		double y0 = currPaintProps.getView().getExtent().getMaxY() - (7 * r);
		double x0 = currPaintProps.getView().getExtent().getMinX() + (18 * r);
		
		for( int pos=0; pos<LocatorResourceData.MAX_NUM_SOURCES ; pos++ ) {	
			
			double maxHeight = 0.0; 

			String locLabelStr = "";
			
			if( locDataSources[pos] == null ) {
				continue;
			}
			
			locLabelStr = locDataSources[pos].getLocatorString( 
								currCoor, dispAttrs[pos] );				
//			System.out.println("label is : "+locLabelStr );
			
			Rectangle2D textBounds = currTarget.getStringBounds( font, locLabelStr );
			
			if (textBounds.getHeight() > maxHeight) {
				if( locLabelStr !=null && ( !locLabelStr.isEmpty()) )	
					maxHeight = currTarget.getStringBounds(font,"J_/").getHeight();//textBounds.getHeight();
		    }
			
			currTarget.drawString( font, locLabelStr, x0, y0, 0, IGraphicsTarget.TextStyle.BLANKED, 
					locRscData.getColor(), HorizontalAlignment.LEFT, 0.0 );
			y0 -= (maxHeight * r);			
		}

//		target.drawStrings(font, getLocatorLabels(coor), x0,y0,0.0, IGraphicsTarget.TextStyle.BLANKED, new RGB[]{color,new RGB(255,0,0),new RGB(0,255,0)}, HorizontalAlignment.LEFT, VerticalAlignment.BOTTOM);
		currTarget.setupClippingPlane( currPaintProps.getClippingPane() );		
	}

	// TODO : we have to re-draw the label on every mouse move and to do this
	// we issue a refresh to trigger the paint. But this causes every resource in the
	// display to repaint which is overkill. If we can figure out how to only 'refresh' this
	// one resource we can remove the refresh and just call drawLocatorInfo.
	//
	@Override
	public void propertyChange(PropertyChangeEvent evt) {		
		currCoor = (Coordinate)evt.getNewValue();		
		
		if( currTarget != null && currPaintProps != null ) {
			//drawLocatorInfo();			
			issueRefresh();
		}
	}
	
	// This didn't work so using the propertyChange instead.
//	@Override     
//	public String inspect(ReferencedCoordinate coord) throws VizException {
//		try {
//			currCoor = coord.asLatLon();
//		} catch (TransformException e) {
//			e.printStackTrace();
//		} catch (FactoryException e) {
//			e.printStackTrace();
//		}
//		return null;
//    }

	private void getDisplayAttributes() throws VizException {
		// create a dataSource for each enabled LocatorDataSource
		for( int p= 0 ; p<LocatorResourceData.MAX_NUM_SOURCES ; p++ ) {

			dispAttrs[p] = locRscData.getDisplayAttributesForPosition( p );
	
			if( dispAttrs[p] == null ) {
				locDataSources[p] = null;
			}
			else {
				if( dispAttrs[p].getLocatorSource().isEmpty() ) {
					locDataSources[p] = null;
				}
				else {
					locDataSources[p] =  
						LocatorDataSourceMngr.getInstance().getLocatorDataSource( 
								dispAttrs[p].getLocatorSource() );
					if( locDataSources[p] != null && 
							!locDataSources[p].isDataLoaded() ) {
						locDataSources[p].loadSourceData(); 
					}
				}
			}
		}		
	}
	
	@Override
	public void resourceAttrsModified() {
		try {
			getDisplayAttributes();

			if( font != null ) {
				font.dispose();
				font = null;
			}

		} catch (VizException e) {
			System.out.println( e.getMessage() );
		}
	}					
					
	
	@Override
	protected void disposeInternal() {
		if( font != null ) {
			font.dispose();
			font = null;
		}
		
		CoorBean.getInstance().removePropertyChangeListener( this );
	}
}
