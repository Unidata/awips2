package gov.noaa.nws.ncep.viz.resources.colorBar;

import gov.noaa.nws.ncep.viz.ui.display.IColorBar;

import gov.noaa.nws.ncep.viz.ui.display.IColorBar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar.ColorBarOrientation;
//import gov.noaa.nws.ncep.viz.ui.display.ColorBar.ColorBarAnchorLocation;
//import gov.noaa.nws.ncep.viz.ui.display.ColorBar.ColorBarOrientation;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;


import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

/**
* 
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#     Engineer    Description
* ------------ ----------  ----------- --------------------------
* 04/11/10      #259        Greg Hull    Initial Creation.
* 07/11/11                  Greg Hull    changed to a Resource and create ColorBarResourceData.
* 06/07/12      #717        Archana      Updated paintInternal() to label the colorbar and 
*                                        display its unit 
* 06/07/12      #794        Archana      Updated paintInternal() to reverse 
*                                        the color order in the color-bar.
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
		getCapabilities().addCapability(ColorMapCapability.class);
	}

	ColorBarResourceData cBarData;
	
	IExtent colorBarExtent=null;
	
	
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

       
        RGB labelColor  = colorBar.getLabelColor();
        String labelStr;
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
		double xMargin = 2 * xScaleFactor; //TODO change per orientation and alignment
		double yMargin = 2 * yScaleFactor;//TODO change per orientation and alignment
		
		double textX  = 0;
		double textY = 0;
		String unitStr = colorBar.getDisplayUnitStr();
		if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft ) {			
			minX = pixExtents.getMinX() + xMargin;
			minY = pixExtents.getMinY() + yMargin + 50;
			textX = minX + 10;
			textY = minY+yMargin - 10;
			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {				
				maxX = minX + cbarPixLength;		
				maxY = minY + cbarPixWidth;
				textY = minY;
			}
			else {
				maxX = minX + cbarPixWidth;		
				maxY = minY + cbarPixLength;
			}
		}
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperRight ) {
			maxX = pixExtents.getMaxX() - xMargin;
			minY = pixExtents.getMinY() + yMargin + 50;
            textY = minY-yMargin - 5;
			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				maxY = minY + cbarPixWidth;
				textX = minX;
			}
			else {
				minX = maxX - cbarPixWidth;		
				maxY = minY + cbarPixLength;
	            if ( unitStr != null )
	            	textX = minX - 15* unitStr.length();
			}


		}	
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft ) {
			minX = pixExtents.getMinX() + xMargin;
			maxY = pixExtents.getMaxY() - yMargin;
            textX = minX + 10;
          
			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				maxX = minX + cbarPixLength;		
				minY = maxY - cbarPixWidth;
				textX = minX;
				textY = minY - 50;
			}
			else {
				maxX = minX + cbarPixWidth;		
				minY = maxY - cbarPixLength;
				textY = minY+yMargin-8;
			}
		}
		else if( colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerRight ) {
			maxX = pixExtents.getMaxX() - xMargin;
			maxY = pixExtents.getMaxY() - yMargin;
//			textX = maxX-130;
			if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				minY = maxY - cbarPixWidth;
				textX = minX;
				textY = minY - 50;
				 
			}
			else {
				minX = maxX - cbarPixWidth;		
				minY = maxY - cbarPixLength;
				textY = minY-10;
	            if ( unitStr != null )
	            	textX = minX - 15* unitStr.length();
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

		

		if(colorBar.getShowLabels() && unitStr != null ){
			   DrawableString unitStrToDraw = new DrawableString(unitStr, labelColor);
			   unitStrToDraw.setCoordinates(textX, textY);			
			   target.drawStrings(unitStrToDraw);
		}

		double pixPerUnit = nonInfRangeLength/colorBar.getDiscreteRange();

		try {
			int startIndex = 0;
			int endIndex = numIntrvls;

			if ( colorBar != null ){
				  if (colorBar.getReverseOrder()){
			               startIndex = numIntrvls - 1;
			               endIndex   = -1;
				  }
				 
				  int r = startIndex;
			  
	              while ( startIndex != -1 && startIndex != endIndex){
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
					

                    	RGB currentColor = colorBar.getRGB( r );
                    	if ( currentColor != null ){
        					target.drawShadedRect( new PixelExtent( intMinX, intMaxX, intMinY, intMaxY), 
        							currentColor, 1.0, null );                    		
                    	}

                    String tmpLbl = colorBar.getLabelString( r );
                    labelStr  = ( tmpLbl != null ) ?  new String ( tmpLbl ) : null;
						
					if( labelStr != null ) {
						
						double lblX = intMinX ;
						
						double lblY = intMinY;

						if(colorBar.getNumPixelsToReAlignLabel() != 0 )
							lblY += colorBar.getNumPixelsToReAlignLabel();
						
						HorizontalAlignment labelHorizAlign = getLabelHorizAlignment(
								colorBar.getOrientation(), colorBar.getAnchorLoc(), (r == 0), false );

						VerticalAlignment labelVertAlign = getLabelVertAlignment(
								colorBar.getOrientation(), colorBar.getAnchorLoc(), (r == 0), false );

						
						
						if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft || 
							 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperRight) &&
							   colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
							lblY += cbarPixWidth;
							if(colorBar.getNumPixelsToReAlignLabel() != 0 ){
								lblY -= colorBar.getNumPixelsToReAlignLabel();
								lblX += colorBar.getNumPixelsToReAlignLabel();
							}

						}
						
						if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft || 
								 colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerRight) &&
								   colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {

								if(colorBar.getNumPixelsToReAlignLabel() != 0 ){
									lblY -= colorBar.getNumPixelsToReAlignLabel();
									lblX += colorBar.getNumPixelsToReAlignLabel();
								}

							}
						
						
						if( (colorBar.getAnchorLoc() == ColorBarAnchorLocation.LowerLeft || 
							 colorBar.getAnchorLoc() == ColorBarAnchorLocation.UpperLeft) &&
							   colorBar.getOrientation() == ColorBarOrientation.Vertical ) {
							lblX += cbarPixWidth ;
						}
						
						DrawableString strToDraw      = new DrawableString(labelStr,labelColor);
						strToDraw.horizontalAlignment = labelHorizAlign;
						strToDraw.verticallAlignment  = labelVertAlign;
	              	    if(colorBar.getShowLabels()){
	          		      strToDraw.setCoordinates(lblX, lblY);
	          		      target.drawStrings(strToDraw);          		  
	                	}
						
					}		
				

					// increment the interval x&y
					if( colorBar.getOrientation() == ColorBarOrientation.Horizontal ) {
						intMinX = intMaxX;
					}
					else {
						intMinY = intMaxY;
					}	            	  
	            	  
	            	 if ( startIndex > endIndex ){
	            		    r--; 
	            		    startIndex--;
	            	 }
	            	 else{
	            		    r++;
	            		    startIndex++;
	            	 }
	              }

	  			// draw the last label
	  			
	  			String tmpLbl = colorBar.getLabelString( numIntrvls );
	  			
	  			labelStr  = ( tmpLbl != null ) ?  new String ( tmpLbl ) : null;
	  			
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

	  				DrawableString strToDraw      = new DrawableString(labelStr,labelColor);
	  				strToDraw.horizontalAlignment = labelHorizAlign;
	  				strToDraw.verticallAlignment  = labelVertAlign;
	            	if(colorBar.getShowLabels()){
	      		         strToDraw.setCoordinates(lblX, lblY);
	      		         target.drawStrings(strToDraw);          		  
	            	}

	  			}		

	  			// draw the border around the colorbar
	  			if (numIntrvls > 0) //check gets rid of the ghost outline if the colormap was cleared
	  			  target.drawRect(colorBarExtent, labelColor, 1.0f, 1.0d);
			}

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
