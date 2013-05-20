package gov.noaa.nws.ncep.viz.resources.colorBar;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarOrientation;
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
* 07/10/12      #743        Archana      Updated paintInternal() to accommodate GEMPAK CLRBAR parameter
*                                        Updated paintInternal() to fix label displacement while changing the 
*                                        zoom or the underlying area.                                         
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
		
		if( colorBar == null || !colorBar.isDrawColorBar()) {
			return;
		}

       
        RGB colorOfAllLabels  = colorBar.getLabelColor();
        RGB colorOfThisLabel = colorOfAllLabels;
        String labelStr;
		target.clearClippingPlane();

		IExtent pixExtents     = paintProps.getView().getExtent();
        double pixExtentsMinX = pixExtents.getMinX();
        double pixExtentsMaxX = pixExtents.getMaxX();
        double pixExtentsMinY = pixExtents.getMinY();
        double pixExtentsMaxY = pixExtents.getMaxY();
        double pixExtentsWidth = pixExtents.getWidth();
        double pixExtentsHeight = pixExtents.getHeight();
        
		int numIntrvls         = colorBar.getNumIntervals();

		double minX=0, maxX=0;
		double minY=0, maxY=0;
		double intrvlXsize = 0;
		double intrvlYsize = 0;
				
		double xScaleFactor= pixExtentsWidth/(double)paintProps.getCanvasBounds().width;
		double yScaleFactor= pixExtentsHeight/(double)paintProps.getCanvasBounds().height;
		double cbarPixLength;
        double lengthRatio = colorBar.getLengthAsRatio();
        double xViewCoord = colorBar.getXPixelCoordFraction();
        double yViewCoord = colorBar.getYPixelCoordFraction();
        ColorBarOrientation colorBarOrientation = colorBar.getOrientation();
		ColorBarAnchorLocation anchorLocation = colorBar.getAnchorLoc();
		
		if(colorBarOrientation == null || anchorLocation == null )
			return;
		
       	if(colorBarOrientation == ColorBarOrientation.Horizontal){
        	     if(lengthRatio == 1)
        		    lengthRatio   = 0.999;
        	      cbarPixLength = lengthRatio* pixExtentsWidth*(1-xViewCoord);
        	}
        	else{
//        		if(lengthRatio == 1)
//        		   lengthRatio = 0.996;
        	      cbarPixLength = lengthRatio* pixExtentsHeight*(1-yViewCoord);
        	}
//
//		double cbarPixWidth = ( colorBarOrientation == ColorBarOrientation.Horizontal ?
//				   				   (double)colorBar.getWidthInPixels() * yScaleFactor : 
//				   				      (double)colorBar.getWidthInPixels() * xScaleFactor );

		double cbarPixWidth = ( colorBarOrientation == ColorBarOrientation.Horizontal ?
				   (double)colorBar.getWidthInPixels()/1000 * pixExtentsHeight : 
				      (double)colorBar.getWidthInPixels()/1000 * pixExtentsWidth );		
		
		double yMargin = 2 * yScaleFactor;

		double textX  = 0;
		double textY = 0;

        
		String unitStr = colorBar.getDisplayUnitStr();

		/*
		 * Calculate the location of the 4 corners of the colorbar based on its
		 * orientation and anchor location. If the current length ratio causes the color bar to
		 * be partially rendered beyond the visible screen area, move it back in. 
		 */

		if( anchorLocation == ColorBarAnchorLocation.UpperLeft ) {			
			
			minX = pixExtentsMinX + pixExtentsWidth*xViewCoord  ;
			minY = pixExtentsMinY + pixExtentsHeight*yViewCoord ;

			if( colorBarOrientation == ColorBarOrientation.Horizontal ) {				
				maxX = minX + cbarPixLength ;		
				maxY = minY + cbarPixWidth ;
				
				if(maxX > pixExtentsMinX + pixExtentsWidth ){
                    cbarPixLength = pixExtentsMaxX - minX;
                    maxX = minX + cbarPixLength;
				} 
			}
			else {
				maxX = minX + cbarPixWidth ;		
				maxY = minY + cbarPixLength ;
				if(maxY > pixExtentsMinY + pixExtentsHeight ){
					cbarPixLength = pixExtentsMaxY - pixExtentsHeight*yViewCoord;
					maxY = minY + cbarPixLength;
				}				
			}


		}
		else if( anchorLocation == ColorBarAnchorLocation.UpperRight ) {
			maxX = pixExtentsMaxX - pixExtentsWidth*xViewCoord;
			minY = pixExtentsMinY + pixExtentsHeight*yViewCoord ;
			if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				maxY = minY + cbarPixWidth;

				if(minX < pixExtentsMinX ){
                    cbarPixLength = maxX - pixExtentsMinX;
                    minX = maxX - cbarPixLength;
				}
			}
			else {
				minX = maxX - cbarPixWidth;		
				maxY = minY + cbarPixLength;
				if(maxY > pixExtentsMinY + pixExtentsHeight ){
					cbarPixLength = pixExtentsMaxY - pixExtentsHeight*yViewCoord;
					maxY = minY + cbarPixLength;
				}				
			}
			

		}	
		else if( anchorLocation == ColorBarAnchorLocation.LowerLeft ) {

			minX = pixExtentsMinX + pixExtentsWidth*xViewCoord;
			maxY = pixExtentsMaxY - pixExtentsHeight*yViewCoord;
			
          
			if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
				maxX = minX + cbarPixLength;		
				minY = maxY - cbarPixWidth;
				
				if( maxX > pixExtentsMaxX ){
					cbarPixLength = pixExtentsMaxX - minX;
					maxX          = minX + cbarPixLength*0.999;
					
				}
			}
			else {
				
				maxX = minX + cbarPixWidth;		
				minY = maxY - cbarPixLength;
                if( (minY -yMargin*3) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = maxY - minY;
                }

			}
			
			
		}
		else if( anchorLocation == ColorBarAnchorLocation.LowerRight ) {
			
			maxX = pixExtentsMaxX - pixExtentsWidth*xViewCoord;
			maxY =  pixExtentsMaxY - pixExtentsHeight*yViewCoord;

			if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
				minX = maxX - cbarPixLength;		
				minY = maxY - cbarPixWidth;
				 if(minX < pixExtentsMinX){
					 cbarPixLength = maxX - pixExtentsMinX;
					 minX = maxX - cbarPixLength;
				 }
			}
			else {
				minX = maxX - cbarPixWidth;		
				minY = maxY - cbarPixLength;

                if( (minY -yMargin*3) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = maxY - minY;
                }
			}

		}
		else if(anchorLocation == ColorBarAnchorLocation.CenterLeft){
			minX = pixExtentsMinX + pixExtentsWidth*xViewCoord  ;
			if(colorBarOrientation == ColorBarOrientation.Vertical){
				maxX = minX+cbarPixWidth;
				minY = pixExtentsMinY + pixExtentsHeight/2 -cbarPixLength/2;
				maxY = pixExtentsMinY + pixExtentsHeight/2 +cbarPixLength/2;
				
                if( (minY - yMargin*11) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = pixExtentsMaxY - minY;
                	maxY = minY+cbarPixLength;
                }
				
			}else{
				maxX = minX + cbarPixLength;
				minY = pixExtentsMinY + pixExtentsHeight/2 - cbarPixWidth/2 ;
				maxY = minY + cbarPixWidth;
				if(maxX >= pixExtentsMaxX)
					maxX = minX + cbarPixLength*0.998;
			}
			
		}
		else if(anchorLocation == ColorBarAnchorLocation.CenterRight){

			maxX = pixExtentsMaxX - pixExtentsWidth*xViewCoord  ;

			
			if(colorBarOrientation == ColorBarOrientation.Vertical){
				minX = maxX - cbarPixWidth;
				minY = pixExtentsMinY + pixExtentsHeight/2 -cbarPixLength/2;
				maxY = pixExtentsMinY + pixExtentsHeight/2 +cbarPixLength/2;
                if( (minY -yMargin*11) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = pixExtentsMaxY - minY;
                	maxY = minY+cbarPixLength;
                }				
		
			}else{
				minX = maxX - cbarPixLength;
				minY = pixExtentsMinY + pixExtentsHeight/2 - cbarPixWidth/2 ;
				maxY = minY + cbarPixWidth;
				if(minX < pixExtentsMinX)
					minX = maxX - cbarPixLength*0.998;
			}
			

		}
		else if(anchorLocation == ColorBarAnchorLocation.CenterCenter){
			if(colorBarOrientation == ColorBarOrientation.Vertical){
                minX = pixExtentsMinX + pixExtentsWidth/2 - cbarPixWidth/2;
                maxX = minX + cbarPixWidth;
                minY  = pixExtentsMinY + pixExtentsHeight/2 -cbarPixLength/2;
                maxY = pixExtentsMinY + pixExtentsHeight/2 +cbarPixLength/2;
                
                if( (minY -yMargin*11) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = pixExtentsMaxY - minY;
                	maxY = minY+cbarPixLength;
                }                
                
			}else{
				minY  = pixExtentsMinY + pixExtentsHeight/2 -cbarPixWidth/2;
                maxY  = pixExtentsMinY + pixExtentsHeight/2 +cbarPixWidth/2;
                minX  = pixExtentsMinX + pixExtentsWidth/2 - cbarPixLength/2;
                maxX  = minX + cbarPixLength;
			}
			

		}
		else if(anchorLocation == ColorBarAnchorLocation.UpperCenter){
			if(colorBarOrientation == ColorBarOrientation.Vertical){
				minX = pixExtentsMinX + pixExtentsWidth/2 - cbarPixWidth/2;
				maxX = minX+cbarPixWidth;
				minY = pixExtentsMinY + pixExtentsHeight*yViewCoord;
				maxY = minY + cbarPixLength;
                //If the color bar overshoots the boundary, it is brought back inside. 
				if(maxY > pixExtentsMinY + pixExtentsHeight ){
					cbarPixLength = pixExtentsMaxY - pixExtentsHeight*(yViewCoord);
					maxY = minY + cbarPixLength;
				}
					
			}else{
                minX  = pixExtentsMinX + pixExtentsWidth/2 - cbarPixLength/2;
                maxX  = minX + cbarPixLength;
                minY  = pixExtentsMinY + pixExtentsHeight*yViewCoord;
                maxY  = minY + cbarPixWidth;
				if(maxX > pixExtentsMinX + pixExtentsWidth ){
					cbarPixWidth = pixExtentsMaxX - pixExtentsWidth;
					maxX = minX + cbarPixWidth;
				}                
			}
			
			
		}else if (anchorLocation == ColorBarAnchorLocation.LowerCenter){
			if(colorBarOrientation == ColorBarOrientation.Vertical){
				minX = pixExtentsMinX + pixExtentsWidth/2 - cbarPixWidth/2;
				maxX = minX+cbarPixWidth;
				maxY = pixExtentsMaxY- pixExtentsHeight*yViewCoord;
				minY = maxY - cbarPixLength;
				
                if( (minY -yMargin*11) < pixExtentsMinY ){
                	minY = pixExtentsMinY + yMargin*11;
                	cbarPixLength = pixExtentsMaxY - minY;
                	maxY = minY+cbarPixLength;
                }
				
			}else{
                minX  = pixExtentsMinX + pixExtentsWidth/2 - cbarPixLength/2;
                maxX  = minX + cbarPixLength;
                maxY  = pixExtentsMaxY- pixExtentsHeight*yViewCoord;
                minY = maxY - cbarPixWidth;
			}
			
		}	
		else {
			return;
		}
		
		if( colorBarOrientation == ColorBarOrientation.Horizontal ) {			
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

		

		if(colorBar.getShowLabels() && unitStr != null && !unitStr.isEmpty() && numIntrvls > 0){
			   DrawableString unitStrToDraw = new DrawableString(unitStr, colorOfAllLabels);
			   textX = minX;
			   textY = minY - yMargin*3;
			   unitStrToDraw.setCoordinates(textX, textY);	
			   if(colorBarOrientation == ColorBarOrientation.Vertical 
					   &&( anchorLocation == ColorBarAnchorLocation.LowerRight 
							   || anchorLocation == ColorBarAnchorLocation.CenterRight 
							   || anchorLocation == ColorBarAnchorLocation.UpperRight))
				   unitStrToDraw.horizontalAlignment = HorizontalAlignment.RIGHT;
			   
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
						if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
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
					

                    	RGB currentIntrvlColor = colorBar.getRGB( r );
                    	if(!colorBar.isDrawBoxAroundColorBar())//requirement for the GEMPAK parameter CLRBAR
                    		colorOfThisLabel = new RGB ( currentIntrvlColor.red,
                    				                     currentIntrvlColor.green,
                    				                     currentIntrvlColor.blue);
                    	
                    	PixelExtent eachShadedRectangleExtent = new PixelExtent( intMinX, intMaxX, intMinY, intMaxY);
                    	if ( currentIntrvlColor != null ){
                    		target.drawShadedRect( eachShadedRectangleExtent, currentIntrvlColor, 1.0, null );                    		
                    	}

                    String tmpLbl = colorBar.getLabelString( r );
                    labelStr  = ( tmpLbl != null ) ?  new String ( tmpLbl ) : null;
						
					if( labelStr != null && labelStr.compareTo("NaN") != 0) {
						
						double lblX = intMinX;
						
						double lblY = intMinY;
						
						HorizontalAlignment labelHorizAlign = getLabelHorizAlignment(
								colorBarOrientation, anchorLocation, (r == 0), false );

						VerticalAlignment labelVertAlign = getLabelVertAlignment(
								colorBarOrientation, anchorLocation, (r == 0), false );

						/*
						 * Fix for the labels flying off when the zoom or the underlying area is changed:
						 * Create a separate pixelextent from the coordinates of each interval in the colorbar 
						 * Get the label coordinates from this new pixelextent.(Anyhow, the label coordinates are
						 * dependant on the coordinates of the corresponding colorbar interval. )
						 */
						PixelExtent pixEx = new PixelExtent(eachShadedRectangleExtent.getMinX(), 
	                            eachShadedRectangleExtent.getMaxX(),
	                            eachShadedRectangleExtent.getMinY(),
	                            eachShadedRectangleExtent.getMaxY());
                                lblX = pixEx.getMinX();
                                if(colorBar.getClass().getSimpleName().compareTo("ColorBar") == 0 ){
                                    
                                	//lblX = intMaxX;
                                
                                    lblY = pixEx.getMaxY();
                                }
                                else{
						            lblY = pixEx.getMinY();
                                }

                               
                        if( (anchorLocation == ColorBarAnchorLocation.LowerLeft 
							 || anchorLocation == ColorBarAnchorLocation.UpperLeft
							 || anchorLocation == ColorBarAnchorLocation.CenterLeft
							 || anchorLocation == ColorBarAnchorLocation.CenterCenter
							 || anchorLocation == ColorBarAnchorLocation.LowerCenter
							 || anchorLocation == ColorBarAnchorLocation.UpperCenter
							 ) &&
							   colorBarOrientation == ColorBarOrientation.Vertical ) {
                        	   lblX += cbarPixWidth  ;
                        	
						}
						else if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
								if(colorBar.getClass().getSimpleName().compareTo("ColorBar") == 0 ){
								    lblX = pixEx.getMaxX();
									lblY+= yScaleFactor;
								}
								else
							      lblY+= cbarPixWidth;
								
							}
                      
				        if(colorBar.isAlignLabelInTheMiddleOfInterval() ){
				        	//typically for radar legends
				        	if(colorBarOrientation == ColorBarOrientation.Vertical){
				        		lblY += yScaleFactor*23;
				        	}else{
				        	    lblX+= xScaleFactor*30;	
				        	}
				        	
				        }
							

						DrawableString strToDraw      = new DrawableString(labelStr,colorOfThisLabel);
						strToDraw.horizontalAlignment = labelHorizAlign;
						strToDraw.verticallAlignment  = labelVertAlign;
	              	    if(colorBar.getShowLabels()){
	          		      strToDraw.setCoordinates(lblX, lblY );
	          		      target.drawStrings(strToDraw);          		  
	              	    }

					}		
				

					// increment the interval x&y
					if( colorBarOrientation == ColorBarOrientation.Horizontal ) {
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
	  			
	  			if( labelStr != null && labelStr.compareTo("NaN") != 0) {
	  				
	  				double lblX = intMinX;
	  				double lblY = intMinY;

	  					if(colorBar.getClass().getSimpleName().compareTo("ColorBar") == 0 ){
                    	    if(colorBarOrientation == ColorBarOrientation.Vertical ){
	  						    intMaxY = intMinY - (numIntrvls) * intrvlYsize;
                    	        lblY = intMaxY + yScaleFactor*5;
                    	        lblX = ( ( anchorLocation == ColorBarAnchorLocation.UpperRight
                    	        		 || anchorLocation == ColorBarAnchorLocation.CenterRight
                    	        		 || anchorLocation == ColorBarAnchorLocation.LowerRight)?
                    	        		intMinX:intMaxX);
                    	    }else{
                    	    	
                    	    	lblX = minX + xScaleFactor*20;
                    	    	lblY = intMaxY; 
                    	    }
	  					}
 				

  				
	  				HorizontalAlignment labelHorizAlign = getLabelHorizAlignment(
	  						colorBarOrientation, anchorLocation, 
	  						false, true );

	  				VerticalAlignment labelVertAlign = getLabelVertAlignment(
	  						colorBarOrientation, anchorLocation, 
	  						false, true );
	  				
	  				if(!colorBar.isDrawBoxAroundColorBar())
	  					colorOfThisLabel = colorBar.getRGB(numIntrvls-1);//requirement for the GEMPAK parameter CLRBAR

	  				DrawableString strToDraw      = new DrawableString(labelStr,colorOfThisLabel);
	  				strToDraw.horizontalAlignment = labelHorizAlign;
	  				strToDraw.verticallAlignment  = labelVertAlign;

	            	if(colorBar.getShowLabels()){
	      		         strToDraw.setCoordinates(lblX, lblY  );
	      		         target.drawStrings(strToDraw);
	      		     }

	  			}		

	  			// draw the border around the colorbar
	  			if (numIntrvls > 0 && colorBar.isDrawBoxAroundColorBar()) {
	  			//check for 'numIntrvls' gets rid of the ghost outline if the colormap was cleared
	  				colorBarExtent = new PixelExtent( minX, maxX, minY, maxY );
	  				target.drawRect(colorBarExtent, colorOfAllLabels, 1.0f, 1.0d);
	  			}
			}

		} catch (VizException e) {
		}
		
	}
	
	private HorizontalAlignment getLabelHorizAlignment( ColorBarOrientation orient, ColorBarAnchorLocation anchor,
			                                    boolean isFirst, boolean isLast ) {
		ColorBarAnchorLocation cBarAnchorLoc = cBarData.getColorbar().getAnchorLoc();
		if( cBarData.getColorbar().getOrientation() == ColorBarOrientation.Vertical ) {
			if( cBarAnchorLoc == ColorBarAnchorLocation.UpperLeft ||
				cBarAnchorLoc == ColorBarAnchorLocation.LowerLeft  ||
				cBarAnchorLoc == ColorBarAnchorLocation.CenterLeft ||
				cBarAnchorLoc == ColorBarAnchorLocation.CenterCenter ||
				cBarAnchorLoc == ColorBarAnchorLocation.UpperCenter ||
				cBarAnchorLoc == ColorBarAnchorLocation.LowerCenter) {	
				return HorizontalAlignment.LEFT;
			}
			else {// on the right
				return HorizontalAlignment.RIGHT;
			}
		}
		else {  // Horizontal
			if( isFirst || isLast) {
				  return HorizontalAlignment.RIGHT;
			}
//			else if( isLast ) {
//				return HorizontalAlignment.RIGHT;
//				}
			else {
				return HorizontalAlignment.CENTER;
			}
		}
	}
	
	private VerticalAlignment getLabelVertAlignment( ColorBarOrientation orient, ColorBarAnchorLocation anchor,
            							boolean isFirst, boolean isLast ) {	
		if( cBarData.getColorbar().getOrientation() == ColorBarOrientation.Horizontal ) {
			return VerticalAlignment.TOP;
		}
		else {  
			if( isFirst ) {
				return VerticalAlignment.BOTTOM;
			}
			else if( isLast ) {
				return VerticalAlignment.BOTTOM; //
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
