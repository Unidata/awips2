package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.gempak.parameters.line.LineDataStringParser;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourSupport.ContourGroup;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Generalized contour renderable
 * 
 * May be embedded in other renderable displays or form the basis of contour
 * resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 10, 2008	#1233	    chammack	Initial creation
 * Mar 01, 2010 #164		M. Li	    Applied to NC Perspective
 * Mar 08, 2011				M. Li		abstract class -> general class 
 * Apr 05, 2011				M. Li		increase threading retries from 10 to 100
 * Nov,02, 2011             X. Guo      Added HILO relative
 * Feb,15, 2012             X. Guo      Cached contour information
 * Mar,01, 2012             X. Guo      Handle five zoom levels
 * Mar,13, 2012             X. Guo      Added createContours()
 * Mar,15, 2012             X. Guo      Set synchronized block in ContoutSupport
 * Aug 19, 2013  #743       S. Gurung   Added code to display the colorbar for gridded fills (from Archana's branch) 
 * Sep 11, 2013  #1036      S. Gurung   Added TEXT attribute related code changes (for contour labels) 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ContourRenderable implements IRenderable {

    private ContourGroup[] contourGroup;

    private IMapDescriptor descriptor;

    private LineStyle lineStyle;

    private RGB color;

    private int outlineWidth;
    
    private final String uuid;

    private static final double METERS_AT_BASE_ZOOMLEVEL = 5000000.0;

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 5;
    
    private int actual_contour_level = 1	;
    
    private boolean reproj;

//    public abstract IDataRecord getData();
//
//    public abstract MathTransform getTransform();
//
//    public abstract GeneralGridGeometry getGridGeometry();
//
//    public abstract ContourAttributes getContourAttributes();
    
    protected ContourAttributes contourAttributes;
    
    protected IDataRecord data;
    
    protected GeneralGridGeometry gridGeometry;
    
    protected GridRelativeHiLoDisplay gridRelativeHiLoDisplay;

    private double lastMagnification = 1.0;

    //private IFont font;
    
    private String name;
    
    private String cint;
    
    private double defaultZoomLevel;
    
    private double zoomLevelInterval;

    private ColorBarResource cBarResource = null;

    /**
     * Constructor
     * 
     * @param styleRule
     * @param descriptor
     * @param callback
     * @param lineStyle
     */
    public ContourRenderable(IMapDescriptor descriptor) {
        this.descriptor = descriptor;
        uuid = UUID.randomUUID().toString();
    }

    public ContourRenderable(IDataRecord data, IMapDescriptor descriptor, GeneralGridGeometry gridGeometry, 
    		ContourAttributes contourAttributes, String fullName ) {
        this.descriptor = descriptor;
        uuid = UUID.randomUUID().toString();
        
        this.data = data;
        this.gridGeometry = gridGeometry;
        this.contourAttributes = contourAttributes;
        this.name = fullName;
        this.defaultZoomLevel = 0.0;
        this.zoomLevelInterval = 0.0;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint (IGraphicsTarget target, PaintProperties paintProps) throws VizException {
    	
    	initContourGroup ( paintProps );
    	
    		double density = 1.0;//paintProps.getDensity();
    		double magnification = 1.0; //paintProps.getMagnification();

    		if (density > 4.0f) {
    			density = 4.0f;
    		}

    		if (contourGroup != null) {
    			if (this.lastMagnification != magnification) { // || font == null) {
    				this.lastMagnification = magnification;
    				/*if (this.font != null) {
    					font.dispose();
    				}
    				font = target.getDefaultFont();

    				font = target.initializeFont(font.getFontName(),
    						(float) (font.getFontSize() / 1.4 * magnification),
    						null);*/
    			}

    			int i = contourGroup.length - 1;
//    			double mapWidth = ((MapDescriptor) this.descriptor)
//                	.getMapWidth();
//    			double widthInMeters = mapWidth * paintProps.getZoomLevel();
    			double rangeHi, rangeLo,zoomlvl;
    			zoomlvl = paintProps
						.getView().getExtent().getWidth()/paintProps
						.getCanvasBounds().width;
    			while (i >= 0) {
    				rangeHi = Double.MAX_VALUE;
    				rangeLo = 0.0;
    				if ( contourGroup.length > 1 ) {
    					if ( i == contourGroup.length - 1) {
    						rangeLo = zoomLevelInterval /2 + ( i-1)*zoomLevelInterval;
    					}
    					else if ( i == 0 ){
    						rangeHi = zoomLevelInterval /2;
    					}
    					else {
    						rangeHi = zoomLevelInterval /2 + i*zoomLevelInterval;
    						rangeLo = zoomLevelInterval /2 + ( i-1)*zoomLevelInterval;
    					}
//    					System.out.println ( "==actual_contour_level:"+ actual_contour_level +" zoomLevelInterval:"+zoomLevelInterval + " zoomlvl:" + zoomlvl + " rangeHi:"+ rangeHi + " rangeLo:" + rangeLo);
    				}
//    				double curlvl = (METERS_AT_BASE_ZOOMLEVEL)
//                    	* (Math.pow(ZOOM_REACTION_FACTOR, i));
    				if ( rangeHi >= zoomlvl && zoomlvl > rangeLo ) {
                
    					// Run contours if:
    					// 1. Contours was never ran before -or-
    					// 2. The area currently viewed is outside of the
    					// contoured window -or-
    					// 3. The density has changed
    					if (contourGroup[i] == null || getMapProject()
    							|| contourGroup[i].lastDensity != density) {

    						MathTransform mathTransformFromGrid = gridGeometry.getGridToCRS(PixelInCell.CELL_CORNER);

    						// If required data unavailable, quit now
    						if (mathTransformFromGrid == null
    								|| data == null
    								|| gridGeometry == null) {
    							return;
    						}

    						ContourGroup cg = null;
    						
    						float pixelDensity = (float) (paintProps
									.getCanvasBounds().width / paintProps
									.getView().getExtent().getWidth());

    						ContourSupport cntrSp = new ContourSupport(data, contourGroup.length-i-1,
    									paintProps.getView().getExtent(),
    									density, mathTransformFromGrid,
    									gridGeometry,
    									descriptor.getGridGeometry(), target,
    									descriptor, contourAttributes, name, pixelDensity,
    									contourGroup[i]);
    						cntrSp.createContours();
    						cg = cntrSp.getContours (); 

    						if (cg != null) {
    							// Dispose old wireframe shapes
    							disposeContourGroup (contourGroup[i]);
                        
    							contourGroup[i] = new ContourGroup();
    							contourGroup[i].zoomLevel = cg.zoomLevel;
    							contourGroup[i].posValueShape = cg.posValueShape;
    							contourGroup[i].negValueShape = cg.negValueShape;
    							contourGroup[i].fillShapes = cg.fillShapes;
    							contourGroup[i].parent = cg.parent;
    							contourGroup[i].lastUsedPixelExtent = cg.lastUsedPixelExtent;
    							contourGroup[i].lastDensity = cg.lastDensity;
    							contourGroup[i].cvalues = new ArrayList<Double>(cg.cvalues);
    							contourGroup[i].fvalues = new ArrayList<Double>(cg.fvalues);
    							contourGroup[i].data = new HashMap< String, Geometry>(cg.data);
    							contourGroup[i].grid = cg.grid;
    							contourGroup[i].clrbar = cg.clrbar;
    							contourGroup[i].labelParms = cg.labelParms;
    							contourGroup[i].labels= cg.labels;
    							if ( contourGroup[i].posValueShape != null )
    								contourGroup[i].posValueShape.compile();
    							if ( contourGroup[i].negValueShape != null )
    								contourGroup[i].negValueShape.compile();
    							if ( contourGroup[i].fillShapes != null )
    								contourGroup[i].fillShapes.compile();
    								
    						     contourGroup[i].colorBarForGriddedFill = cg.colorBarForGriddedFill;
                                 if ( contourGroup[i].colorBarForGriddedFill != null){
                              	    
                              	   AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
                              	  
                              	   IRenderableDisplay disp = editor.getActiveDisplayPane().getRenderableDisplay();
                              	   
                                     if (disp != null) {
                                         
   							           ResourcePair rp =  ResourcePair.constructSystemResourcePair( 
   	  							              new ColorBarResourceData( contourGroup[i].colorBarForGriddedFill ) );
   							           cBarResource = (ColorBarResource)  rp.getResourceData().construct(rp.getLoadProperties(), disp.getDescriptor());
   						               if(cBarResource != null ){
   						     	               cBarResource.setColorBar(contourGroup[i].colorBarForGriddedFill);
   						     	               cBarResource.init(target);
   						     	
   						               }  
                                     }
                                 }
                                 else {
                                	 cBarResource = null;
                                 }
    							
    						} 
    						else {
    							target.setNeedsRefresh(true);
    						}
    					}

    					LineStyle posLineStyle = null;
                        LineStyle negLineStyle = null;
                        if (this.lineStyle == null) {
                            posLineStyle = LineStyle.SOLID;
                            negLineStyle = LineStyle.DASHED_LARGE;
                        } else {
                            posLineStyle = this.lineStyle;
                            negLineStyle = this.lineStyle;
                        }

                        if (contourGroup[i] != null
                                && paintProps.getView().getExtent().intersects(
                                        contourGroup[i].lastUsedPixelExtent)) {
                        	if ( contourGroup[i].fillShapes != null )
                        		target.drawShadedShape(contourGroup[i].fillShapes , 0.5f);
                        	if ( contourGroup[i].posValueShape != null )
                        		target.drawWireframeShape(
                                    contourGroup[i].posValueShape, this.color,
                                    this.outlineWidth, posLineStyle, contourGroup[i].labelParms.font);
                        	if ( contourGroup[i].negValueShape != null )
                        		target.drawWireframeShape(
                                    contourGroup[i].negValueShape, this.color,
                                    this.outlineWidth, negLineStyle, contourGroup[i].labelParms.font);
                           
                        	
           				 if( contourGroup[i].labels != null ) {           	                	
           	    				target.drawStrings( contourGroup[i].labels );
           	    			}

                        } else {
                            // see if we can display a higher level
                            if (i > 0) {
                                int j = i - 1;
                                if (contourGroup[j] != null) {
                                    if (contourGroup[j].posValueShape != null) {
                                        if (contourGroup[j].lastUsedPixelExtent
                                                .intersects(paintProps
                                                        .getView().getExtent())) {
                                        	if ( contourGroup[j].fillShapes != null )
                                        		target.drawShadedShape( contourGroup[j].fillShapes, 0.5f);
                                        	if ( contourGroup[j].posValueShape != null )
                                        		target.drawWireframeShape(
                                                            contourGroup[j].posValueShape,
                                                            this.color,
                                                            this.outlineWidth,
                                                            posLineStyle, contourGroup[i].labelParms.font);
                                        	if ( contourGroup[j].negValueShape != null )
                                        		target.drawWireframeShape(
                                                            contourGroup[j].negValueShape,
                                                            this.color,
                                                            this.outlineWidth,
                                                            negLineStyle, contourGroup[i].labelParms.font);
                                        }
                                        
                                    	
                       				 if( contourGroup[j].labels != null ) {
                       	    				target.drawStrings( contourGroup[j].labels );
                       	    			}
                                    }
                                }
                            }
                        }

                        target.setNeedsRefresh(true);
                        break;
    				}
    				i--;
    			
    			}
    		}
    		
    		if ( cBarResource != null )
    	    	cBarResource.paint(target, paintProps);
    	
    }    
    public void paintContour(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	
        synchronized (this) {

        	LineDataStringParser lineAttr = new LineDataStringParser(contourAttributes.getLine());
        	this.color = GempakColor.convertToRGB(lineAttr.getInstanceOfLineBuilder().getLineColorsList().get(0));
        	this.lineStyle = lineAttr.getInstanceOfLineBuilder().getLineStyleList().get(0);
        	this.outlineWidth = lineAttr.getInstanceOfLineBuilder().getLineWidthList().get(0);
        	

            if (contourGroup != null) {
            	LineStyle posLineStyle = null;
                LineStyle negLineStyle = null;
                if (this.lineStyle == null) {
                    posLineStyle = LineStyle.SOLID;
                    negLineStyle = LineStyle.DASHED_LARGE;
                } else {
                    posLineStyle = this.lineStyle;
                    negLineStyle = this.lineStyle;
                }
                
                int i = contourGroup.length - 1;
                  
                while (i >= 0) {

                        if (contourGroup[i] != null
                                && paintProps.getView().getExtent().intersects(
                                        contourGroup[i].lastUsedPixelExtent)) {
                        	if ( contourGroup[i].fillShapes != null )
                        		target.drawShadedShape(contourGroup[i].fillShapes , 0.5f);
                        	if ( contourGroup[i].posValueShape != null )
                        		target.drawWireframeShape(
                                    contourGroup[i].posValueShape, this.color,
                                    this.outlineWidth, posLineStyle, contourGroup[i].labelParms.font);
                        	if ( contourGroup[i].negValueShape != null )
                        		target.drawWireframeShape(
                                    contourGroup[i].negValueShape, this.color,
                                    this.outlineWidth, negLineStyle, contourGroup[i].labelParms.font);   
                        	

                            if( contourGroup[i].labels != null ) {
                				target.drawStrings( contourGroup[i].labels );
                			}
                            
                        } 
                    i--;
                }
                
            }
        }
    }

    public void createContours (IGraphicsTarget target, PaintProperties paintProps) throws VizException {
    	
    	initContourGroup ( paintProps );

    		double density = 1.0;//paintProps.getDensity();
    		double magnification = 1.0; //paintProps.getMagnification();

    		if (density > 4.0f) {
    			density = 4.0f;
    		}

    		if (contourGroup != null) {
    			if (this.lastMagnification != magnification) { // || font == null) {
    				this.lastMagnification = magnification;
    				/*if (this.font != null) {
    					font.dispose();
    				}
    				
    				font = target.getDefaultFont();

    				font = target.initializeFont(font.getFontName(),
    						(float) (font.getFontSize() / 1.4 * magnification),
    						null);*/
    				
    			}

    			int i = contourGroup.length - 1;
//    			double mapWidth = ((MapDescriptor) this.descriptor)
//                	.getMapWidth();
//    			double widthInMeters = mapWidth * paintProps.getZoomLevel();
    			double rangeHi, rangeLo,zoomlvl;
    			zoomlvl = paintProps
						.getView().getExtent().getWidth()/paintProps
						.getCanvasBounds().width;
    			while (i >= 0) {
    				rangeHi = Double.MAX_VALUE;
    				rangeLo = 0.0;
    				if ( contourGroup.length > 1 ) {
    					if ( i == contourGroup.length - 1) {
    						rangeLo = zoomLevelInterval /2 + ( i-1)*zoomLevelInterval;
    					}
    					else if ( i == 0 ){
    						rangeHi = zoomLevelInterval /2;
    					}
    					else {
    						rangeHi = zoomLevelInterval /2 + i*zoomLevelInterval;
    						rangeLo = zoomLevelInterval /2 + ( i-1)*zoomLevelInterval;
    					}
//    					System.out.println ( "==actual_contour_level:"+ actual_contour_level +" zoomLevelInterval:"+zoomLevelInterval + " zoomlvl:" + zoomlvl + " rangeHi:"+ rangeHi + " rangeLo:" + rangeLo);
    				}
//    				double curlvl = (METERS_AT_BASE_ZOOMLEVEL)
//                    	* (Math.pow(ZOOM_REACTION_FACTOR, i));
    				if ( rangeHi >= zoomlvl && zoomlvl > rangeLo ) {
                
    					// Run contours if:
    					// 1. Contours was never ran before -or-
    					// 2. The area currently viewed is outside of the
    					// contoured window -or-
    					// 3. The density has changed
    					if (contourGroup[i] == null || getMapProject()
    							|| contourGroup[i].lastDensity != density) {

    						MathTransform mathTransformFromGrid = gridGeometry.getGridToCRS(PixelInCell.CELL_CORNER);

    						// If required data unavailable, quit now
    						if (mathTransformFromGrid == null
    								|| data == null
    								|| gridGeometry == null) {
    							return;
    						}

    						ContourGroup cg = null;
    						float pixelDensity = (float) (paintProps
									.getCanvasBounds().width / paintProps
									.getView().getExtent().getWidth());
    						ContourSupport cntrSp = new ContourSupport(data, contourGroup.length-i-1,
    									paintProps.getView().getExtent(),
    									density, mathTransformFromGrid,
    									gridGeometry,
    									descriptor.getGridGeometry(), target,
    									descriptor, contourAttributes, name, pixelDensity,
    									contourGroup[i]);
    						cntrSp.createContours();
    						cg = cntrSp.getContours ();

    						if (cg != null) {
    							// Dispose old wireframe shapes
    							disposeContourGroup (contourGroup[i]);
                        
    							contourGroup[i] = new ContourGroup();
    							contourGroup[i].zoomLevel = cg.zoomLevel;
    							contourGroup[i].posValueShape = cg.posValueShape;
    							contourGroup[i].negValueShape = cg.negValueShape;
    							contourGroup[i].fillShapes = cg.fillShapes;
    							contourGroup[i].parent = cg.parent;
    							contourGroup[i].lastUsedPixelExtent = cg.lastUsedPixelExtent;
    							contourGroup[i].lastDensity = cg.lastDensity;
    							contourGroup[i].cvalues = new ArrayList<Double>(cg.cvalues);
    							contourGroup[i].fvalues = new ArrayList<Double>(cg.fvalues);
    							contourGroup[i].data = new HashMap< String, Geometry>(cg.data);
    							contourGroup[i].grid = cg.grid;
    							contourGroup[i].clrbar = cg.clrbar;
    							contourGroup[i].labelParms = cg.labelParms;
    							contourGroup[i].labels= cg.labels;
    							if ( contourGroup[i].posValueShape != null )
    								contourGroup[i].posValueShape.compile();
    							if ( contourGroup[i].negValueShape != null )
    								contourGroup[i].negValueShape.compile();
    							if ( contourGroup[i].fillShapes != null )
    								contourGroup[i].fillShapes.compile();
                     
 //   							  if(contourGroup[i].fvalues != null && !contourGroup[i].fvalues.isEmpty() ){
    							
    							           contourGroup[i].colorBarForGriddedFill = cg.colorBarForGriddedFill;
                                           if ( contourGroup[i].colorBarForGriddedFill != null){
                                        	   
                                        	   AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
                                        	  
                                        	   IRenderableDisplay disp = editor.getActiveDisplayPane().getRenderableDisplay();
                                        	   
                                               if (disp != null) {
	                                               
	         							           ResourcePair rp =  ResourcePair.constructSystemResourcePair( 
	         	  							              new ColorBarResourceData( contourGroup[i].colorBarForGriddedFill ) );
     	    							           cBarResource = (ColorBarResource)  rp.getResourceData().construct(rp.getLoadProperties(), disp.getDescriptor());
     	    						               if(cBarResource != null ){
     	    						     	               cBarResource.setColorBar(contourGroup[i].colorBarForGriddedFill);
     	    						     	               cBarResource.init(target);
     	    						     	
     	    						               }  
                                               }
                                           }
                                           else {
                                        	   cBarResource = null;
                                           }

//    						   }       
    						}
    						
    					}
                        break;
    				}    				

    				i--;
    				

    			}
    		}
    }   
    
    private void initContourGroup ( PaintProperties paintProps ) {
    	LineDataStringParser lineAttr = new LineDataStringParser(contourAttributes.getLine());
		this.color = GempakColor.convertToRGB(lineAttr.getInstanceOfLineBuilder().getLineColorsList().get(0));
		this.lineStyle = lineAttr.getInstanceOfLineBuilder().getLineStyleList().get(0);
		this.outlineWidth = lineAttr.getInstanceOfLineBuilder().getLineWidthList().get(0);

		if (contourGroup == null) {
			this.defaultZoomLevel = paintProps
			.getView().getExtent().getWidth()/paintProps
			.getCanvasBounds().width;
			if (contourAttributes.getCint() != null ) {
				String[] cintArray = contourAttributes.getCint().trim().split(">");
				actual_contour_level = cintArray.length;
				this.cint = contourAttributes.getCint();
			}
	
			if (actual_contour_level > NUMBER_CONTOURING_LEVELS ) 
				actual_contour_level = NUMBER_CONTOURING_LEVELS;
    
			contourGroup = new ContourGroup[actual_contour_level];
			if ( actual_contour_level > 1)
				this.zoomLevelInterval = this.defaultZoomLevel/(actual_contour_level-1);
		}
		else {
			if ( this.defaultZoomLevel == 0.0 ) {
				this.defaultZoomLevel = paintProps
				.getView().getExtent().getWidth()/paintProps
				.getCanvasBounds().width;
				if ( actual_contour_level > 1)
    				this.zoomLevelInterval = this.defaultZoomLevel/(actual_contour_level-1);
			}
		}
    }
    /**
     * Dispose the renderable
     */
    public void dispose() {
        if (contourGroup != null) {
            for (ContourGroup c : contourGroup) {
                if (c == null) {
                    continue;
                }

                if (c.posValueShape != null) {
                    c.posValueShape.dispose();
                }
                if (c.negValueShape != null) {
                    c.negValueShape.dispose();
                }
                if (c.fillShapes != null) {
                	c.fillShapes.dispose();
                }
            	if (c.labelParms != null && c.labelParms.font != null) { 
            		c.labelParms.font.dispose();
            	}
            }
        }
        /*if (font != null) {
            font.dispose();
        }*/

    }

    public void disposeInternal() {
        if (contourGroup != null) {
            for (ContourGroup c : contourGroup) {
                if (c == null) {
                    continue;
                }

                if (c.posValueShape != null) {
                    c.posValueShape.dispose();
                }
                if (c.negValueShape != null) {
                    c.negValueShape.dispose();
                }
                if (c.fillShapes != null) {
                	c.fillShapes.dispose();
                }
            	if (c.labelParms != null && c.labelParms.font != null) { 
            		c.labelParms.font.dispose();
            	}
            }
        }
    }
    /**
     * Dispose the contour group
     */
    private void disposeContourGroup (ContourGroup contourGp ) {
    	// Dispose old wireframe shapes
        if (contourGp != null ){
        	if ( contourGp.posValueShape != null)
        		contourGp.posValueShape.dispose();
        	if (contourGp.negValueShape != null) {
        		contourGp.negValueShape.dispose();
        	}
        	if (contourGp.fillShapes != null) {
        		contourGp.fillShapes.dispose();
        	}
        	if ( contourGp.cvalues != null )
        		contourGp.cvalues.clear();
        	if ( contourGp.fvalues != null )
        		contourGp.fvalues.clear();
        	if ( contourGp.data != null )
        		contourGp.data.clear();
        	if (contourGp.labels != null)
        		contourGp.labels.clear();
        	if (contourGp.labelParms != null && contourGp.labelParms.font != null) 
        		contourGp.labelParms.font.dispose();
        }
    }
    
    public ContourAttributes getContourAttributes() {
		return contourAttributes;
	}

	public void setContourAttributes(ContourAttributes contourAttributes) {
		this.contourAttributes = contourAttributes;
	}

	public void updatedContourRenderable () {
		
		if ( this.cint == null ) {
			if (contourAttributes.getCint() != null ) {
				String[] cintArray = contourAttributes.getCint().trim().split(">");
				actual_contour_level = cintArray.length;
				this.cint = contourAttributes.getCint();
			}

			if (actual_contour_level > NUMBER_CONTOURING_LEVELS ) 
				actual_contour_level = NUMBER_CONTOURING_LEVELS;

			disposeInternal();
			contourGroup = new ContourGroup[actual_contour_level];
			if ( actual_contour_level > 1)
				this.zoomLevelInterval = this.defaultZoomLevel/(actual_contour_level-1);
		}
		else {
			if (contourAttributes.getCint() == null) {
				actual_contour_level = 1;
				this.cint = null;
				disposeInternal();
				contourGroup = new ContourGroup[actual_contour_level];
			}
			else {
				if ( ! this.cint.equalsIgnoreCase(contourAttributes.getCint())){
					disposeInternal();
					String[] cintArray = contourAttributes.getCint().trim().split(">");
					actual_contour_level = cintArray.length;
					this.cint = contourAttributes.getCint();
					if (actual_contour_level > NUMBER_CONTOURING_LEVELS ) 
						actual_contour_level = NUMBER_CONTOURING_LEVELS;
					contourGroup = new ContourGroup[actual_contour_level];
					if ( actual_contour_level > 1)
						this.zoomLevelInterval = this.defaultZoomLevel/(actual_contour_level-1);
				}
			}
		}
		
	}
	
	public IDataRecord getData() {
		return data;
	}

	public void setData(IDataRecord data) {
		this.data = data;
	}

	public GeneralGridGeometry getGridGeometry() {
		return gridGeometry;
	}

	public void setGridGeometry(GeneralGridGeometry gridGeometry) {
		this.gridGeometry = gridGeometry;
	}
	
	public GridRelativeHiLoDisplay getGridRelativeHiLo () {
		return gridRelativeHiLoDisplay;
	}

	public void setGridRelativeHiLo (GridRelativeHiLoDisplay gridRelativeHiLoDisplay) {
		this.gridRelativeHiLoDisplay = gridRelativeHiLoDisplay;
	}
	
	public void setMapProject ( boolean proj){
		this.reproj = proj;
	}
	
	private boolean getMapProject () {
		return this.reproj;
	}
	public void setIMapDescriptor (IMapDescriptor descriptor) {
        this.descriptor = descriptor;
        this.defaultZoomLevel = 0.0;
    }
	public boolean isMatch ( ContourAttributes attr) {
		boolean match = false;
		
		if ( this.contourAttributes.getGlevel().equalsIgnoreCase(attr.getGlevel())&&
			 this.contourAttributes.getGvcord().equalsIgnoreCase(attr.getGvcord()) &&
			 this.contourAttributes.getScale().equalsIgnoreCase(attr.getScale()) &&
			 this.contourAttributes.getGdpfun().equalsIgnoreCase(attr.getGdpfun()) ) {
			match = true;
		}
		return match;
	}
	
}
