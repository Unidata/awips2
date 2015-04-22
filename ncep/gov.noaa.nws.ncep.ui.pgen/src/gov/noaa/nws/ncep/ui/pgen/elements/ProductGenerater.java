/*
 * ProductGenerator
 * 
 * Date created: 22 October 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.io.*;
import java.util.*;
import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IArc;
import gov.noaa.nws.ncep.ui.pgen.display.IAvnText;
import gov.noaa.nws.ncep.ui.pgen.display.ICombo;
import gov.noaa.nws.ncep.ui.pgen.display.IKink;
import gov.noaa.nws.ncep.ui.pgen.display.IMidCloudText;
import gov.noaa.nws.ncep.ui.pgen.display.ISymbolSet;
import gov.noaa.nws.ncep.ui.pgen.display.IText;
import gov.noaa.nws.ncep.ui.pgen.display.ITrack;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.display.IWatchBox;
import gov.noaa.nws.ncep.ui.pgen.display.LinePatternManager;
import gov.noaa.nws.ncep.ui.pgen.display.SymbolPatternManager;
import gov.noaa.nws.ncep.ui.pgen.display.TrackPoint;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox.WatchShape;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

/**
 * Emulate the Product Generation process.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 * 04/14/09		#72			S. Gilbert  Added IText methods
 * 04/27/09		#89			J. Wu  		Added IArc methods
 * 05/06/09     #111		J. Wu  		Added IVector methods
 * 03/10		#223		M.Laryukhin	Gfa methods added. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */

public class ProductGenerater {
	
	static ArrayList<Product>	productManager;
	
	static void start() {
       	productManager = new ArrayList<Product>();	    

        System.out.println("Started Product/Layer/Drawbale managers......\n");
	}

	
	/**
	 * generate one Product via user's input.
	 */			
	static Product generateOneProduct() {
    	
		Scanner stdin = new Scanner( System.in );
		
        System.out.println("Enter the product name:");       		
		String name = stdin.nextLine();
		
        System.out.println("Enter the product type:");       		
		String type = stdin.nextLine();
        
		System.out.println("Enter the forecaster name:");       		
		String forecaster = stdin.nextLine();				
        
        ProductInfo info = new ProductInfo();
        ProductTime time = new ProductTime();
        ArrayList<Layer> layers = new ArrayList<Layer>(0);
        
        return (new Product( name, type, forecaster, info, time, layers ) );
	}
				

	/**
	 * generate one Layer via user's input.
	 */	
	static Layer generateOneLayer() {

		Scanner stdin = new Scanner( System.in );
		
        System.out.println("Enter the layer name:");       		
		String name = stdin.nextLine();
		
        System.out.println("Enter onOff flag (0 or 1):");       		
		boolean onOff = (stdin.nextInt() != 0);		
 
	    System.out.println("Enter the color mode (0 or 1):");       		
	    boolean colorMode = (stdin.nextInt() != 0);	
	    
	    System.out.println("Enter the color:"); 
	    System.out.println("1 - Red\t2 - Green\t3 - Blue"); 
	    
		int col = stdin.nextInt();	
		Color color = Color.red;
		if ( col == 2 ) {			
			color = Color.green;
		}
		else if (col == 3 ) {
			color = Color.blue;
		}
 
		System.out.println("Enter the fill mode (0 or 1):");       		
		boolean fillMode = (stdin.nextInt() != 0);		
		            
        ArrayList<AbstractDrawableComponent> drawables = new ArrayList<AbstractDrawableComponent>();
        
        return (new Layer( name, onOff, colorMode, color, fillMode, drawables ) );
	}
	
	/**
	 * generate one Drawable via user's input.
	 */	
	static DrawableElement generateOneDrawable() {
		
		Scanner stdin = new Scanner( System.in );		
 		
        System.out.println("Please select a Drawable type:");       
		for ( DrawableType dt : DrawableType.values() ) {
		    System.out.println ( dt.ordinal() + " - " + dt + "\t");
		}
		
		int classType = stdin.nextInt();
		DrawableType dtType = DrawableType.ANY;
		for ( DrawableType dt : DrawableType.values() ) {
			if ( dt.ordinal() == classType ) {
			    dtType = dt;
			    break;
			}
		}
								
        AttributeGenerator newAttribute = new AttributeGenerator( dtType );
		DrawableElementFactory df = new DrawableElementFactory();
		
		AbstractDrawableComponent element = null;
		if (dtType == DrawableType.SYMBOL){ 
			element = df.create(dtType, newAttribute, "Symbol",
        					((AttributeGenerator)newAttribute).getType(), 
        					newAttribute.getLocation(), null);
		}
		else if (dtType == DrawableType.KINKLINE ||
			  dtType == DrawableType.LINE ){
			element = df.create(dtType, newAttribute, "Lines",
					((AttributeGenerator)newAttribute).getLinePattern(), 
					newAttribute.getLinePoints(), null);
		}
        
        return (DrawableElement)element;
	}
     

	/**
	 * Main program to emulate the product generation process.
	 * @param args
	 */
	public static void main(String[] args) throws IOException { 		
		
		Product oneProduct;
		
		System.out.println("Start Product Generation......\n");
      	
        start();
                                      
		System.out.println("\nDo you want to generate a new product (Yes/No)?");
		
		Scanner stdin = new Scanner( System.in );
		
		while ( stdin.hasNext() && stdin.next().equalsIgnoreCase("Yes") ) {		
            
			oneProduct = generateOneProduct();
		    
    	    System.out.println("Add a new Layer for this Product(Yes/No)?");
     		while ( stdin.hasNext() && stdin.next().equalsIgnoreCase("Yes") ) {
    			Layer oneLayer =  generateOneLayer(); 
    			oneProduct.addLayer( oneLayer );    			      		   			

    		    System.out.println("Add a new Drawable for this Layer(Yes/No)?");
  		
        		while ( stdin.hasNext() && stdin.next().equalsIgnoreCase("Yes") )  {		
       	            oneLayer.addElement( generateOneDrawable() );
        		    System.out.println("Add another Drawable for this Layer(Yes/No)?");       		
        		}
        		
        	    System.out.println("Add another Layer for this Product(Yes/No)?");  		
            }
    		
     		productManager.add( oneProduct );
     		    		
    		System.out.println("Do you want to generate another product (Yes/No)?");

		}			       
        
		System.out.println( "\nTotal of " + productManager.size() + " product in PGEN:" ); 
		for ( Product prd : productManager ) {
			System.out.println( prd );			
		}

		System.out.println("\nDo you want to EXIT product generation (Yes/No)?");
		if ( stdin.hasNext() && stdin.next().equalsIgnoreCase("Yes") ) {
			System.out.println("\nExited Product Generation......");  
	        stdin.close();
	    }
         	   
	}
	
	/**
	 * 
	 * 
	 */		
	
}

class AttributeGenerator implements IArc, IAvnText, ICombo, IKink, IMidCloudText, ISymbolSet,
									IText, ITrack, IVector, IWatchBox{
    
	private int 		smoothFactor, lpi;
    private boolean		clear, filled, closed;
    private float		lineWidth;
    private double		sizeScale, kinkPosition;
	private String		type, linePattern; 		
	private Color[]		colors;	
	private Color[]		colorList = new Color[]{Color.red, Color.green, Color.blue};	
		
	private Coordinate		location;		
	private Coordinate[]	linePoints;
	private ArrayList<Coordinate>	points;
	private FillPattern		fillPattern;
	private ArrowHeadType	arrowHeadType;
	
	private String 			lat, lon;
	
	public AttributeGenerator(DrawableType deType) {
		
		colors = new Color[10];
		points = new ArrayList<Coordinate>();
		
		Scanner stdin = new Scanner( System.in );
		
        
		System.out.println("Please select a" + deType + " type:");       
		for ( DrawableType dt : DrawableType.values() ) {
		    System.out.println ( dt.ordinal() + " - " + dt + "\t");
		}
		
		switch ( deType ) {
		    case LINE:
		    	
			  break;
			
		    case SYMBOL:
		    	break;
		    
		    default:
		    	break;
		
		}
		
        
		System.out.println("Please select a color:\n");       
		System.out.println("\t1 = RED\t2 = Green\t3=Blue");       
		
		int myColor = stdin.nextInt();
		if ( myColor < 0 || myColor > 2 ) {
			myColor = 0;
		}
		
		colors[0] = colorList[myColor];
		
		System.out.println("Please input lineWidth (float):");       
		lineWidth = stdin.nextFloat(); 
		System.out.println("Please input sizeScale (double):");       
		sizeScale = stdin.nextDouble(); 
        
		if ( deType == DrawableType.SYMBOL ) {
			System.out.println("Please select a SYMBOL type:"); 
			SymbolPatternManager spl = SymbolPatternManager.getInstance();
			int k = 0;
			for ( String lpName : spl.getPatternNames() ) {
				System.out.print( k + " - " + lpName + "\t\t");			    
			    if ( k > 0 && k%3 == 0 ) {
			    	System.out.println("\n");
			    }
			    k++;
			}
			
			lpi = stdin.nextInt();
			if ( lpi < 0 || lpi >= spl.getPatternNames().length ) {
				lpi = 0;
			}
			
		    type = spl.getPatternNames()[lpi];
			
			System.out.println("Please input clear flag (0 = false\t1 = true):");       
			clear = (stdin.nextInt() == 0) ? false:true; 				
			
			System.out.println("Please input location (lat, lon):");       
			System.out.println("Please input startPoint (lat, lon):");       
			lat = stdin.next(); 
			lon = stdin.next();
            location = new Coordinate( Double.parseDouble(lat), 
	       			                   Double.parseDouble(lon));
			 				
		}
		else if ( deType == DrawableType.LINE ) {
			
			System.out.println("Please select a LINE type:");
			
			LinePatternManager lpl = LinePatternManager.getInstance();
			int k = 0;
			for ( String lpName : lpl.getPatternNames() ) {
				System.out.print( k + " - " + lpName + "\t\t");			    
			    if ( k > 0 && k%3 == 0 ) {
			    	System.out.println("\n");
			    }
			    k++;
			}
			
			lpi = stdin.nextInt();
			if ( lpi < 0 || lpi >= lpl.getPatternNames().length ) {
				lpi = 0;
			}
			
		    type = lpl.getPatternNames()[lpi];
		    linePattern = lpl.getPatternNames()[lpi];
						
			System.out.println("Please input closed flag (0 = false\t1 = true):");       
			closed = (stdin.nextInt() == 0) ? false:true; 				
							
			System.out.println("Please input filled flag (0 = false\t1 = true):");       
			filled = (stdin.nextInt() == 0) ? false:true; 				
			
			if ( filled ) {
				System.out.println("Please select a FillPattern:");       
				for ( FillPattern fp : FillPattern.values() ) {
					System.out.println ( fp.ordinal() + " - " + fp + "\t");
				}
				
				int fpi = stdin.nextInt();
				fillPattern = FillPattern.SOLID;
				for (  FillPattern fp : FillPattern.values() ) {
					if ( fp.ordinal() == fpi ) {
					    fillPattern = fp;
					    break;
					}
				}   
			}
			
			System.out.println("Please input smoothFactor (0, 1, 2)");       
			smoothFactor = stdin.nextInt(); 
			if ( smoothFactor < 0 || smoothFactor > 2 ) {
				smoothFactor = 2;
			}
	        
			System.out.println("Please enter points on the LINE:");       		
	        System.out.println("One pair each line: Lat Lon");
	        System.out.println("END will end the input");       		
			
	        points.clear();
	        while ( stdin.hasNextLine() ) {
	        	if ( stdin.hasNext() ) {
	    		    lat = stdin.next();    
	    		    if ( lat.equals("END") )  break;
	    		    
	       		    lon = stdin.next();     		    
	    		    if ( lon.equals("END") )  break;
	    		    
	    	       	points.add( new Coordinate( Double.parseDouble(lat), 
	    	       			                    Double.parseDouble(lon) ));

	            }       			        	
	        }       			
		}
		else if ( deType == DrawableType.KINKLINE ) {
			
			System.out.println("Please input kinkPosition (double):");       
			kinkPosition= stdin.nextFloat(); 				
			
			points.clear();
			System.out.println("Please input startPoint (lat, lon):");       
			lat = stdin.next(); 
			lon = stdin.next();
            points.add(new Coordinate( Double.parseDouble(lat), 
	       			                   Double.parseDouble(lon)));
			
			System.out.println("Please input endPoint (lat, lon):");       
			lat = stdin.next(); 
			lon = stdin.next();
            points.add(new Coordinate( Double.parseDouble(lon), 
	       			                   Double.parseDouble(lat)));
			 				
			
			System.out.println("Please select an ArrowHeadType:");       				 				
			System.out.println("0 - Filled	1 - open");       				 				
						
			lpi = stdin.nextInt();			
		    arrowHeadType = (lpi == 0) ? (ArrowHeadType.FILLED):(ArrowHeadType.OPEN);		    
    		    
		}									

	}
	
    public String getType() {
	     return type;
	}
	
	public Color[] getColors() {
	     return colors;
	}
	
    public float getLineWidth() {		     
    	return lineWidth;
	}
    public double getSizeScale() {
	     return sizeScale;
	}
    
    public Boolean isClear() {
	     return clear;
	}   
    public Coordinate getLocation() {
	     return location;
	}
   
    public int getSmoothFactor() {
	     return smoothFactor;
	}
    
    public String getLinePattern() {
	     return linePattern;
	};
    
	public FillPattern getFillPattern() {
	     return fillPattern;
	} 
    
    public Boolean isClosedLine() {
	     return closed;
	}
    
    public Boolean isFilled() {
	     return filled;
	}
    
    public Coordinate[] getLinePoints() {
	    linePoints = new Coordinate[points.size()];
    	int k = 0;
    	for ( Coordinate coord : points ) {
    		linePoints[k++] = coord;
	    }
    	
    	return linePoints;
	}
 
	public Color getColor() {
	     return colors[0];
	}
    
	public Coordinate getStartPoint() {
	     return points.get(0);
	}	
	
	public Coordinate getEndPoint() {
	     return points.get(1);
	}		
	
	public double getKinkPosition() {
	     return kinkPosition;
	}
	
	public ArrowHeadType getArrowHeadType() {
	     return arrowHeadType;
	}

	@Override
	public String getFontName() {
		return null;
	}

	@Override
	public float getFontSize() {
		return 0;
	}

	@Override
	public TextJustification getJustification() {
		return null;
	}

	@Override
	public double getRotation() {
		return 0;
	}

	@Override
	public TextRotation getRotationRelativity() {
		return null;
	}

	@Override
	public String[] getString() {
		return null;
	}

	@Override
	public FontStyle getStyle() {
		return null;
	}

	@Override
	public int getXOffset() {
		return 0;
	}

	@Override
	public int getYOffset() {
		return 0;
	}

	@Override
	public Boolean maskText() {
		return false;
	}

	@Override
	public DisplayType getDisplayType() {
		return DisplayType.NORMAL;
	}

	@Override
	public Boolean getHide() {
		return false;
	}
	
	@Override
	public Boolean getAuto() {
		return false;
	}
	
	@Override
	public Coordinate getCenterPoint() {
		return null;
	}
	
	@Override
	public Coordinate getCircumferencePoint() {
		return null;
	}
	
	@Override
	public double getAxisRatio() {
		return 1.0;
	}	
	
	@Override
	public double getStartAngle() {
		return 0.0;
	}

	@Override
	public double getEndAngle() {
		return 360.0;
	}
	
	@Override
	public VectorType getVectorType() {
		return null;
	}
		
	@Override
	public double getSpeed() {
		return 0.0;
	}
	
	@Override	
	public double getDirection() {
		return 0.0;
	}
	
	@Override
	public double getArrowHeadSize() {
		return 1.0;
	}
	
	@Override
	public boolean hasDirectionOnly() {
		return false;
	}
	
	@Override
	public Boolean hasBackgroundMask() {
		return isClear();
	}

	@Override
	public Color getExtrapColor() {
		return null;
	}

	@Override
	public String getExtrapLinePattern() {
		return null;
	}

	@Override
	public String getExtrapMarker() {
		return null;
	}

	@Override
	public TrackPoint[] getExtrapPoints() {
		return null;
	}

	@Override
	public Color getInitialColor() {
		return null;
	}

	@Override
	public String getInitialLinePattern() {
		return null;
	}

	@Override
	public String getInitialMarker() {
		return null;
	}

	@Override
	public TrackPoint[] getInitialPoints() {
		return null;
	}
	
	public AviationTextType getAvnTextType() {
		return null;
}
	
	public String getSymbolPatternName() {
		return null;
	}
	
	public boolean hasSymbolPattern() {
		return false;
	}
	
	public String getTopValue() {
		return null;
	}
	
	public String getBottomValue() {
		return null;
	}
	
	public boolean hasBottomValue() {
		return false;
	}
	
	public boolean getFillFlag(){
		return false;
	}
	
	public Color getFillColor(){
		return null;
	}
	
	public WatchShape getWatchBoxShape(){
		return null;
	}
	
	public String getWatchSymbolType(){
		return "PLUS_SIGN";
	}
	
	public float getWatchSymbolWidth(){
		return 1.5f;
	}
	
	public double getWatchSymbolSize(){
		return 0.7;
	}
	
	public String getHazard() {
		return null;
	}

	public String getGfaDesk() {
		return null;
	}

	public String getGfaFcstHr() {
		return null;
	}

	public String getGfaHazard() {
		return null;
	}

	public String getGfaIssueType() {
		return null;
	}

	public String getGfaTag() {
		return null;
	}

	public String getGfaType() {
		return null;
	}
	
	public HashMap<String, String> getGfaValues() {
		return null;
	}

	public String getGfaArea() {
		return null;
	}

	public String getGfaBeginning() {
		return null;
	}

	public String getGfaEnding() {
		return null;
	}

	public String getGfaStates() {
		return null;
	}
	
	public int getGfaCycleDay() {
		return PgenCycleTool.getCycleDay();
	}

	public int getGfaCycleHour() {
		return PgenCycleTool.getCycleHour();
	}

	@Override
	public Station[] getAnchors() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<SPCCounty> getCountyList() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public List<SPCCounty> getOriginalCountyList() {
		// TODO Auto-generated method stub
		return null;
	} 
	
	@Override
	public int getWatchNumber() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getIssueFlag() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public FontStyle getFontStyle() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean[] getExtraPointTimeTextDisplayIndicator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExtraPointTimeDisplayOption getExtraPointTimeDisplayOption() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Coordinate getPosition() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Color getTextColor() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Symbol getSymbol() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Coordinate[] getLocations() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getCloudTypes() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getCloudAmounts() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasTurbulence() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getTurbulencePattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTurbulenceLevels() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasIcing() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getIcingPattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getIcingLevels() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasTstorm() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getTstormTypes() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTstormLevels() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isTwoColumns() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String[] getPatternNames() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Calendar getFirstTimeCalendar() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Calendar getSecondTimeCalendar() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isSetTimeButtonSelected() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public int getExtraDrawingPointNumber() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getSkipFactorText() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getFontNameComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getFontSizeComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getFontStyleComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getUnitComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getRoundComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getRoundDirComboSelectedIndex() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getIntervalTimeString() {
		// TODO Auto-generated method stub
		return null;
	}

}
