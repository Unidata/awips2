package gov.noaa.nws.ncep.viz.resources.colorBar;

import gov.noaa.nws.ncep.viz.ui.display.IColorBar;
import gov.noaa.nws.ncep.viz.ui.display.NcSelectedPaneResource;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar.ColorBarOrientation;
//import gov.noaa.nws.ncep.viz.ui.display.ColorBar.ColorBarAnchorLocation;
//import gov.noaa.nws.ncep.viz.ui.display.ColorBar.ColorBarOrientation;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import org.eclipse.swt.graphics.Rectangle;

/**
* 
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#     Engineer    Description
* ------------ ----------  ----------- --------------------------
* 04/11/10      #259        Greg Hull    Initial Creation.
* 07/11/11                  Greg Hull    changed to a Resource and create ColorBarResourceData.
* 
* </pre>
* 
* @author ghull
* @version 1
*/
public class ColorBarResource extends AbstractVizResource<ColorBarResourceData, IDescriptor> {

	public ColorBarResource( ColorBarResourceData resourceData,
			LoadProperties loadProperties) {
		super(resourceData, loadProperties);
		cBarData = resourceData;
	}

	ColorBarResourceData cBarData;
	
	IExtent colorBarExtent=null;
	
	private IFont font=null;
	
	public void setColorBar( IColorBar cbar ) {
		cBarData.setColorBar( cbar );
	}
	
	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		//System.out.println("ColorBar initInternal");
	}

	@Override
	protected void paintInternal( IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		IColorBar colorBar = cBarData.getColorbar();
		
		if( colorBar == null ) {
			return;
		}
		
		target.clearClippingPlane();

		IExtent pixExtents = paintProps.getView().getExtent();
		Rectangle canvasBounds = paintProps.getCanvasBounds();		
//		float scaleFactor = ( colorBar.getOrientation() == ColorBarOrientation.Horizontal ?
//				              (float)canvasBounds.width/(float)colorBar.getPixelLength() : 
//				                (float)canvasBounds.height/(float)colorBar.getPixelLength() );
//		float scaleFactor = ( colorBar.getOrientation() == ColorBarOrientation.Horizontal ?
//	                         (float)pixExtents.getWidth()/(float)colorBar.getPixelLength() : 
//	                         (float)pixExtents.getHeight()/(float)colorBar.getPixelLength() );

		int numIntrvls = colorBar.getNumIntervals();
		double minX=0, maxX=0;
		double minY=0, maxY=0;
		double intrvlXsize = 0;
		double intrvlYsize = 0;
				
		double xScaleFactor= pixExtents.getWidth()/(double)canvasBounds.width;
		double yScaleFactor= pixExtents.getHeight()/(double)canvasBounds.height;

		double cbarPixLength = ( colorBar.getOrientation() == ColorBarOrientation.Horizontal ?
				 				   colorBar.getLengthAsRatio() * pixExtents.getWidth() : 
				 					 colorBar.getLengthAsRatio() * pixExtents.getHeight() );
		double cbarPixWidth = ( colorBar.getOrientation() == ColorBarOrientation.Horizontal ?
				   				   (double)colorBar.getWidthInPixels() * yScaleFactor : 
				   				      (double)colorBar.getWidthInPixels() * xScaleFactor );
		double xMargin = 2 * xScaleFactor;
		double yMargin = 2 * yScaleFactor;
		
		if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft ) {			
			minX = pixExtents.getMinX() + xMargin;
			minY = pixExtents.getMinY() + yMargin;

			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {				
				maxX = minX + cbarPixLength;		
				maxY = minY + cbarPixWidth;
			}
			else {
				maxX = minX + cbarPixWidth;		
				maxY = minY + cbarPixLength;
			}
		}
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperRight ) {
			maxX = pixExtents.getMaxX() - xMargin;
			minY = pixExtents.getMinY() + yMargin;

			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				maxY = minY + cbarPixWidth;
			}
			else {
				minX = maxX - cbarPixWidth;		
				maxY = minY + cbarPixLength;
			}
		}	
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft ) {
			minX = pixExtents.getMinX() + xMargin;
			maxY = pixExtents.getMaxY() - yMargin;

			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				maxX = minX + cbarPixLength;		
				minY = maxY - cbarPixWidth;
			}
			else {
				maxX = minX + cbarPixWidth;		
				minY = maxY - cbarPixLength;
			}
		}
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerRight ) {
			maxX = pixExtents.getMaxX() - xMargin;
			maxY = pixExtents.getMaxY() - yMargin;

			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				minY = maxY - cbarPixWidth;
			}
			else {
				minX = maxX - cbarPixWidth;		
				minY = maxY - cbarPixLength;
			}
		}
		else {
			return;
		}

		colorBarExtent = new PixelExtent( minX, maxX, minY, maxY );
		
		if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {			
			intrvlXsize = cbarPixLength/numIntrvls;
			intrvlYsize = cbarPixWidth;
		}
		else {
			intrvlYsize = cbarPixLength/numIntrvls;
			intrvlXsize = cbarPixWidth;
		}

		double intMinX=minX;
		double intMaxX=0;
		double intMinY=minY;
		double intMaxY=0;
		
		double nonInfRangeLength = cbarPixLength;		
		
		if( colorBar.getIntervalMin(0) == Float.NEGATIVE_INFINITY ) {
			nonInfRangeLength -= cbarPixLength/numIntrvls;
		}
		if( colorBar.getIntervalMax(numIntrvls-1) == Float.POSITIVE_INFINITY ) {
			nonInfRangeLength -= cbarPixLength/numIntrvls;
		}

		double pixPerUnit = nonInfRangeLength/colorBar.getDiscreteRange();

		try {
			
			for( int r=0 ; r< numIntrvls ; r++ ) {
				// compute the new maxX and Ys for the interval
				if( colorBar.getDrawToScale() ) {
					if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
						if( colorBar.getIntervalMin(r) == Float.NEGATIVE_INFINITY ||
							     colorBar.getIntervalMax(r) == Float.POSITIVE_INFINITY ) {
						    intrvlXsize = cbarPixLength/numIntrvls;
						}
						else {
							intrvlXsize = pixPerUnit *
								(double)(colorBar.getIntervalMax(r) - colorBar.getIntervalMin(r));
						}
					}
					else {
						if( colorBar.getIntervalMin(r) == Float.NEGATIVE_INFINITY ||
							     colorBar.getIntervalMax(r) == Float.POSITIVE_INFINITY ) {
						    intrvlYsize = cbarPixLength/numIntrvls;
						}
						else {
							intrvlYsize = pixPerUnit *
								(double)(colorBar.getIntervalMax(r) - colorBar.getIntervalMin(r));
						}
					}					
				}

				intMaxX = intMinX + intrvlXsize;
				intMaxY = intMinY + intrvlYsize;
				
				target.drawShadedRect( new PixelExtent( intMinX, intMaxX, intMinY, intMaxY), 
						                          colorBar.getRGB( r ), 1.0, null );

				String labelStr = colorBar.getLabelString( r );
					
				if( labelStr != null ) {
					double lblX = intMinX;
					double lblY = intMinY;
					HorizontalAlignment labelHorizAlign = getLabelHorizAlignment(
							colorBar.getOrientation(), colorBar.getAnchorLoc(), (r == 0), false );

					VerticalAlignment labelVertAlign = getLabelVertAlignment(
							colorBar.getOrientation(), colorBar.getAnchorLoc(), (r == 0), false );
					
					if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft || 
						 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperRight) &&
						   colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
						lblY += cbarPixWidth;
					}
					if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft || 
						 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft) &&
						   colorBar.getOrientation() == ColorBarOrientation.Vertical ) {
						lblX += cbarPixWidth;
					}

					target.drawString(font, labelStr, lblX, lblY, 0.0, 
							TextStyle.NORMAL, colorBar.getLabelColor(), labelHorizAlign,
							labelVertAlign, 0.0 );
				}		
			

				// increment the interval x&y
				if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
					intMinX = intMaxX;
				}
				else {
					intMinY = intMaxY;
				}
			}
			
			// draw the last label
			String labelStr = colorBar.getLabelString( numIntrvls );
			
			if( labelStr != null ) {
				double lblX = intMinX;
				double lblY = intMinY;
				HorizontalAlignment labelHorizAlign = getLabelHorizAlignment(
						colorBar.getOrientation(), colorBar.getAnchorLoc(), 
						false, true );

				VerticalAlignment labelVertAlign = getLabelVertAlignment(
						colorBar.getOrientation(), colorBar.getAnchorLoc(), 
						false, true );
				
				if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft || 
					 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperRight) &&
					   colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
					lblY += cbarPixWidth;
				}
				if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft || 
					 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft) &&
					   colorBar.getOrientation() == ColorBarOrientation.Vertical ) {
					lblX += cbarPixWidth;
				}

				target.drawString(font, labelStr, lblX, lblY, 0.0, 
						TextStyle.NORMAL, colorBar.getLabelColor(), labelHorizAlign,
						labelVertAlign, 0.0 );
			}		

			// draw the border around the colorbar
			if (numIntrvls > 0) //check gets rid of the ghost outline if the colormap was cleared
			  target.drawRect(colorBarExtent, colorBar.getLabelColor(), 1.0f, 1.0d);

		} catch (VizException e) {
		}
		
	}

	private HorizontalAlignment getLabelHorizAlignment( ColorBarOrientation orient, ColorBarAnchorLocation anchor,
			                                    boolean isFirst, boolean isLast ) {	
		if( cBarData.getColorbar().getOrientation() == ColorBarOrientation.Vertical ) {
			if( cBarData.getColorbar().getAnchorLoc() == ColorBarAnchorLocation.UpperLeft ||
				cBarData.getColorbar().getAnchorLoc() == ColorBarAnchorLocation.LowerLeft ) {	
				return HorizontalAlignment.LEFT;
			}
			else {// on the right
				return HorizontalAlignment.RIGHT;
			}
		}
		else {  // Horizontal
			if( isFirst ) {
				return HorizontalAlignment.LEFT;
			}
			else if( isLast ) {
				return HorizontalAlignment.RIGHT;
			}
			else {
				return HorizontalAlignment.CENTER;
			}
		}
	}
	
	private VerticalAlignment getLabelVertAlignment( ColorBarOrientation orient, ColorBarAnchorLocation anchor,
            							boolean isFirst, boolean isLast ) {	
		if( cBarData.getColorbar().getOrientation() == ColorBarOrientation.Horizontal ) {
			if( cBarData.getColorbar().getAnchorLoc() == ColorBarAnchorLocation.UpperLeft ||
				cBarData.getColorbar().getAnchorLoc() == ColorBarAnchorLocation.UpperRight ) {	
				return VerticalAlignment.TOP;
			}
			else {// on the right
				return VerticalAlignment.BOTTOM;
			}
		}
		else {  // Horizontal
			if( isFirst ) {
				return VerticalAlignment.TOP;
			}
			else if( isLast ) {
				return VerticalAlignment.BOTTOM;
			}
			else {
				return VerticalAlignment.MIDDLE;
			}
		}
	}

	@Override
	protected void disposeInternal() {
		// TODO Auto-generated method stub
		
	}

}
