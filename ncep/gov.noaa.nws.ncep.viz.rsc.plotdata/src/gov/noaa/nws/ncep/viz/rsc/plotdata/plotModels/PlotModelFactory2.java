/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.NotDerivableException;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLatitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLongitude;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Angle;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * A singleton that will create a plot model texture based on a passed in
 * MetarRecord object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/2006              brockwoo    Initial creation.
 * 03/16/2009              jsanchez    Added processAvailDirective.
 * 06/29/2009     2538     jsanchez    Implemented pointdata.
 * 04/28/2010     #275     ghull       Incorporate to11dr3 changes
 * 06/03/2010              ghull       Incorporate to11dr11 changes
 * 07/26/2010     T285     qzhou       Modified processTextDirective and processBarbDirective for uair
 * 08/10/2010	  #291	   gzhang	   added support for Synop, Uair, etc
 * 04/08/2011     #425     ghull       use the plugin to get the plotModel and plot parameters
 * 05/07/2011     #441     ghull       add support for Derived parameters.   
 * 05/11/2011     #441     ghull       save PlotParamDefn in the PlotElements and move some code into
 *                                     the PlotElement class.    
 * 05/12/2011     #441     ghull       remove unused/untested functionality.        
 * 05/20/2011     #441     ghull       move plotFunctionTable to PlotModelGenerator
 * 06/02/2011     #441     ghull       remove class attr since it is for style sheets which we don't
 *                                     want to use. Specify symbolFont explicitly instead of setting from class.
 * 07/29/2011     #450     ghull       NcPathManager; pass PlotParameterDefns instead of the PlotParameterDefnsMngr
 * 11/11/2011              ghull       fix for wind barbs
 * 01/19/2012     #539     qzhou       Fixed southern hemisphere wind barbs
 * 02/27/2012     #694     qzhou       Fixed center symbol positions not overlap
 * 04/02/2012     #615     sgurung     Fixed a NullPointerException bug in processTableDirective()
 * 05/30/2012     #654     sgurung     Modified style in the constructor to construct FontFamily names that match SVG file names
 * 10/18/2012     #431,#896sgurung     Added method getStationPlot(... , ...)  
 * 02/26/2013     #936   asubramanian  Updated getStationPlot() to use the system font files 
 *                                     to render the text-based plot elements. 
 * </pre>
 * 
 * @author BRock97
 * @version 1.0
 */
public class PlotModelFactory2 {

	private static final File COURIER_NORMAL_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "cour.pfa");
    private static final File SERIF_NORMAL_FONT =NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "VeraSe.ttf");
    private static final File SERIF_BOLD_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "l049016t.pfa");
    private static final File SERIF_ITALIC_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "l049033t.pfa");
    private static final File SERIF_BOLD_ITALIC_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "l049036t.pfa");    
    
    private static final File SANS_SERIF_NORMAL_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisr.ttf");
    private static final File SANS_SERIF_ITALIC_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisri.ttf");
    private static final File SANS_SERIF_BOLD_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisb.ttf");
    private static final File SANS_SERIF_BOLD_ITALIC_FONT = NcPathManager.getInstance().getStaticFile (NcPathManager.NcPathConstants.FONT_FILES_DIR + "luxisbi.ttf");	
	private static final String SVG_PLOT_SYMBOL_ID = "plotData"; //  name of main svg symbol element 
	
    private Document document;

    private GraphicsNode theGraphicsNode;

    private final GVTBuilder builder;

    private final BridgeContext bridgeContext;

    private int plotModelWidth;

    private int plotModelHeight;

    private final int originalPlotModelWidth;

    private final int originalPlotModelHeight;

    private final Element svgRoot;

    private final GeodeticCalculator gc;

    private final IMapDescriptor mapDescriptor;

    private final ArrayList<PlotElement> plotElements;
    private List<PlotElement> listOfTextPlotElements = null;     
    private PlotParameterDefns plotParamDefns = null;
        
    // take this out unless we are going to use it.....
//    private StringBuffer sampleMessage = new StringBuffer();

    private boolean plotMissingData = false;

//    public TreeMap<Integer, String> rawMessageMap;

    // TODO : if we have any RANGE, ARROW or SAMPLE parameters then 
    // add these back
    public static enum DisplayMode {
        TEXT, BARB, TABLE, NULL // ARROW, AVAIL, RANGE, SAMPLE
    }

    private PlotModel plotModel = null;
    
    public PlotModel getPlotModel() {
		return plotModel;
	}

    double[] stationLoc = {0,0};
    
    public class PlotElement {    
        private DisplayMode displayMode = DisplayMode.NULL;

        // TODO : remove this from the domElement when the
        // UA processing is replaced with the VerticalSounding (ie when the PointDataView is
                // removed and the units comes from the metParameter.

        // this stores the unit, plotMode, dbParamName, metParamName.....
        private PlotParameterDefn plotParamDefn; 
        
        private String symbol = null;

        private int plotTrim = 0;

        // TODO remove this when integrating VerticalSounding (do conversion in metParameter)
        private UnitConverter converter = null;

        private Element domElement = null;

        private Node domNode = null;

        private StringLookup lookup = null;        

        private PlotWindElement winds = null;

        private boolean required = false;
        private String position = "";

		public String formattedStringToPlot = "";
        
        // mag/dir for barbs and/or arrows
//        private String vectorParm1=null, vectorParm2=null;
        
        public PlotElement( PlotParameterDefn pd, Element domElmt ) {
        	plotParamDefn = pd;
        	domElement = domElmt;
        	domNode = domElement.getChildNodes().item(0);
  
        	// plotIndex was taken out of the svg template file. We currently don't have any
        	// parameters using a plotIndex. D2d uses is in some svg files for 
        	// cloudText0 and hgtText0 with the cloud_select.txt and cloud_chars2.txt
        	// plotFunctionTable and plotLookupTables.
//        	index = plotParamDefn.getPlotIndex();
        	
        	// set the displayMode
        	if( plotParamDefn.getPlotMode() == null ) {
        		displayMode = DisplayMode.NULL;
        	}
//        	else if( plotParamDefn.getPlotMode().equals("text") ) { -T936
//        		displayMode = DisplayMode.TEXT;
//        	}
        	else if( plotParamDefn.getPlotMode().equals("barb") ) {
        		displayMode = DisplayMode.BARB;
        		
        		winds = new PlotWindElement();
        		
        		NodeList windElements = domElement.getChildNodes();
        		
        		for (int j = 0; j < windElements.getLength(); j++) {                      	
        			if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
        				Element windElement = (Element) windElements.item(j);

        				if (windElement.getAttribute("class").matches( "arrow" )) {
        					winds.arrowElement = windElement;
        					winds.arrowNode = windElement.getChildNodes().item(0);
        				} 
        				else if (windElement.getAttribute("class").matches("barb")) {
        					winds.barbElement = windElement;
        					winds.barbNode = windElement.getChildNodes().item(0);			
        				} 
        			}
        		}
        	}
        	else if( plotParamDefn.getPlotMode().equals("table") ) {
        		displayMode = DisplayMode.TABLE;
        		
        		lookup = StringLookup.readS2SFile(
						plotParamDefn.getPlotLookupTable() );
        	}
//        	else if( plotParamDefn.getPlotMode().equals("arrow") ||
//        		plotParamDefn.getPlotMode().equals("arrowuv") ) {
//        		displayMode = DisplayMode.ARROW;
//        	}
//        	else if( plotParamDefn.getPlotMode().equals("range") ) {
//        		displayMode = DisplayMode.RANGE;
        	// lookup = StringLookup.readR2SFile(domElement
//									.getAttribute(PLT_ATTRIBUTE));

//        	}
//        	else if( plotParamDefn.getPlotMode().equals("sample") ) {
//        		displayMode = DisplayMode.SAMPLE;
//        	}
        	else {
        		displayMode = DisplayMode.NULL;
        	}
        	
        	// for BARBs and ARROWs the metParamName must give 2 parameter names
        	// for the magnitude and direction separated by a comma.
        	if( plotParamDefn.isVectorParameter() ) { //|| 
        		//displayMode == DisplayMode.ARROW ) {
        		if( plotParamDefn.getMetParamNamesForVectorPlot() == null ) {
            		System.out.println("Error for BARB/ARROW Plot element: Plot Definition doesn't " +
            				"specify parameters for magnitude and direction");
            		displayMode = DisplayMode.NULL;
        		}
        	}
        	
        	// validate the units
        	if( getUnit() == null ) {        		
        		System.out.println("Error with plotElement "+plotParamDefn.getPlotParamName()+
        				" : Invalid units "+ getUnitString() );
        		displayMode = DisplayMode.NULL;
        	}
        	
        	try {
        		if( plotParamDefn.getPlotTrim() == null ) {
        			plotTrim = 0;
        		}
        		else {
        			plotTrim = Integer.parseInt( plotParamDefn.getPlotTrim() );
        		}
        	}
        	catch ( NumberFormatException nfex ) {
        		System.out.println("Error parsing trim value("+plotParamDefn.getPlotTrim()+
        				") for plotParam : " + plotParamDefn.getPlotParamName() );
        		plotTrim = 0;
        	}        	
        }
        
        //Overloaded constructor to handle only the text based plot parameters
        public PlotElement(PlotParameterDefn prmDefn) {
        	plotParamDefn = prmDefn;
        	this.displayMode = DisplayMode.TEXT;
        	if( getUnit() == null ) {        		
        		System.out.println("Error with plotElement "+plotParamDefn.getPlotParamName()+
        				" : Invalid units "+ getUnitString() );
        		displayMode = DisplayMode.NULL;
        	}
        	
        	try {
        		if( plotParamDefn.getPlotTrim() == null ) {
        			plotTrim = 0;
        		}
        		else {
        			plotTrim = Integer.parseInt( plotParamDefn.getPlotTrim() );
        		}
        	}
        	catch ( NumberFormatException nfex ) {
        		System.out.println("Error parsing trim value("+plotParamDefn.getPlotTrim()+
        				") for plotParam : " + plotParamDefn.getPlotParamName() );
        		plotTrim = 0;
        	}        	
		}

        
        public String getPlotParamName() {
        	return plotParamDefn.getPlotParamName();
        }

        public String getMetParamName() {
        	return plotParamDefn.getMetParamName();
        }

        public String getDbParamName() {
        	return plotParamDefn.getDbParamName();
        }

        public String getUnitString() {
        	return plotParamDefn.getPlotUnit();
        }
        
        public Unit<?> getUnit() {
        	if( getUnitString() == null ) {
        		return Unit.ONE;
        	}
        	try {
        		Unit<?> unit = UnitFormat.getUCUMInstance()
        					.parseProductUnit( getUnitString(),
        							new ParsePosition(0) );
        		return unit;
        	} catch (ParseException e) {
        		System.out.println("Unable parse units :"+ e.getMessage() );
        		return null;
        	}  
        }
        
        public String getPlotFormat() {
        	return plotParamDefn.getPlotFormat();
        }
        
        public DisplayMode getDisplayMode() {
        	return displayMode;
        }
        
        public int getPlotTrim() {
        	return plotTrim;
        }
        
        // for barb and arrow plot modes the 
        public  String[] getVectorParamNames( ) {
        	return plotParamDefn.getMetParamNamesForVectorPlot();
        }
    }

    public class PlotWindElement {
        Node barbNode = null;

        Element barbElement = null; // this is the svg element with a class attribute of 'barb' 

        Node arrowNode = null;

        Element arrowElement = null; // this is the svg element with a class attribute of 'arrow'

//        Node gustNode = null;

// In D2D this would print the magnitude of the barb/arrow in the plot but since we aren't doing
// this, its coming out to simplify the code.
//        Element gustElement = null;  // this is the svg element with a class attribute of 'text'
//        String gustX = null,  gustY = null;
    }

    
    class PlotImageCallback implements IRenderedImageCallback {
		private HashMap<String,AbstractMetParameter> paramsToPlot;
		private HashMap<String,AbstractMetParameter> allMetParamsMap;
		
		public PlotImageCallback(HashMap<String,AbstractMetParameter> paramsToPlot, HashMap<String,AbstractMetParameter> allMetParamsMap) {
			super();
			this.paramsToPlot = paramsToPlot;
			this.allMetParamsMap = allMetParamsMap;
		}

		@Override
		public RenderedImage getImage() throws VizException {
			return getStationPlot(paramsToPlot, allMetParamsMap);
		}
		
	}
    
    public PlotModelFactory2( IMapDescriptor mapDescriptor, PlotModel pltMdl, 
    		PlotParameterDefns plotParamDefns) {
    	
    	if( pltMdl == null ) {
    		System.out.println("PlotModelFactory: Using default PlotModel.");
    		this.plotModel = PlotModelMngr.getInstance().getDefaultPlotModel();
    	}
    	else {
    		this.plotModel = pltMdl;
    	}
    	
        this.plotElements = new ArrayList<PlotElement>();
        listOfTextPlotElements = new ArrayList<PlotElement>();//T936
        this.plotParamDefns = plotParamDefns;
        
//        this.rawMessageMap = new TreeMap<Integer, String>();

        this.gc = new GeodeticCalculator(mapDescriptor.getCRS());
        this.mapDescriptor = mapDescriptor;
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
        try {
        	File svgFile = 
        		NcPathManager.getInstance().getStaticFile( 
        				NcPathConstants.PLOT_MODELS_DIR + File.separator + plotModel.getSvgTemplate() );
            document = f.createDocument( svgFile.toURI().toString() );
        } catch( MalformedURLException e1) {
            e1.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }											
        this.svgRoot = document.getDocumentElement();
        this.originalPlotModelWidth = Integer.parseInt(svgRoot.getAttributeNS(
                null, "width"));
        this.originalPlotModelHeight = Integer.parseInt(svgRoot.getAttributeNS(
                null, "height"));
        this.plotModelWidth = this.originalPlotModelWidth;
        this.plotModelHeight = this.originalPlotModelHeight;
//        svgRoot.setAttribute( "width","300");
//        svgRoot.setAttribute( "height","300");
        
        Element svgPlotSymbol = document.getElementById( SVG_PLOT_SYMBOL_ID );
//      NodeList nodeList2 = svgPlotSymbol.getChildNodes();
		
        // get the center plot model elements  		
//		PlotModelElement mcPme = plotModel.getPlotModelElement("MC");
//		PlotModelElement scPme = plotModel.getSkyCoverageElement();
		PlotModelElement wdPme = plotModel.getWindBarbElement();
		
//		if( wdPme != null ) {
//			if(mcPme == null ) {
//				Node mcNode = null;       
//				for (int i2 = 0; i2 < nodeList2.getLength(); i2++) {
//			            if (Node.ELEMENT_NODE == nodeList2.item(i2).getNodeType()) {
//			                Element domElement2 = (Element) nodeList2.item(i2);
//			                
//			                if (domElement2.hasAttribute("position")) {
//			                	
//			                    PlotElement thisElement = new PlotElement();
//			                    thisElement.domElement = domElement2;
//			                    thisElement.domNode = domElement2.getChildNodes().item(0);
//			                    
//			                    String position = domElement2.getAttribute("position");
//			                    if( "MC".equals(position) ) { 
//			                    	mcNode = nodeList2.item(i2); 
//			                    	break;
//			                    }
//			                }
//			            }
//				}				
//			}
//		} 
		// if there is no wind symbol, remove the svg group in the template used to display it.
		if (wdPme == null) {		
			NodeList n = document.getElementsByTagName("g");
			
			if( n != null && n.getLength() > 0 ) {
				svgPlotSymbol.removeChild(n.item(0));				
			}
		}   

        NodeList nodeList = svgPlotSymbol.getChildNodes();

        // loop thru the nodes with a 'position' attribute in the svg template
        //
        for( int i = 0; i < nodeList.getLength(); i++) {
            if( Node.ELEMENT_NODE == nodeList.item(i).getNodeType()) {
                Element domElement = (Element) nodeList.item(i);
                
                if( !domElement.hasAttribute("position") ) {
                	continue;
                }

                // use the plotModel to find the plotParameter for this position
                // and create a PlotElement for it.
                //
                PlotModelElement pme = plotModel.getPlotModelElement( 
                							domElement.getAttribute("position").toUpperCase() );

                if( pme == null ) {
                	// may need to put a PlotElement in here to 'blank' out the svg element. Or blank it out here...
                	Node domNode = domElement.getChildNodes().item(0);
                	if(domNode != null)
                	domNode.setNodeValue(" ");
                	continue;
                }
//      	   	domElement.setAttribute("name", pme.getParamName());

                // get the PlotParameterDefinition and set it in the PlotElement. Later this 
                // will be used to get the DB parameter name, the plotUnits, plotMode, plotFormat....
                //
                PlotParameterDefn prmDefn = plotParamDefns.getPlotParamDefn( 
                									pme.getParamName() );

                if( prmDefn == null ) {
                	System.out.println("Unable to find PlotParameterDefn for :"+pme.getParamName() );
                	continue;
                }

//                PlotElement plotElement = new PlotElement( prmDefn, domElement );
//                thisElement.domElement = domElement;
//                thisElement.domNode = domElement.getChildNodes().item(0);

                // set any information from the plotParamDefn that is needed by the svg syntax
                // to actually create the image. (other information is no longer saved to the 
                // xml tree. It is simply referenced from this plotParamDefn.)
                	
                	
//            				if( prmDefn.getPlotMode() != null)
//            					domElement.setAttribute("plotMode", prmDefn.getPlotMode());
//            				
//            				if( prmDefn.getDbParamName() != null ){
////            					if(ParmList.isMappedParam( pme.getParamName().toLowerCase() )){//addressing NMAP2-CAVE discrepancies
////            						domElement.setAttribute("plotParam", ParmList.getMappedParam(plugin, pme.getParamName().toLowerCase()));
////            					}else{
//            						domElement.setAttribute("plotParam", 
//            								prmDefn.getDbParamName() );
////            					}
//            				}

// The units is now determined by the plotModel and set in the metParameter            				
//            				if( prmDefn.getPlotUnit() != null)
//            					domElement.setAttribute("plotUnit", prmDefn.getPlotUnit());
//            				if( prmDefn.getPlotFormat() != null)
//            					domElement.setAttribute("plotFormat", prmDefn.getPlotFormat());
//            				if( prmDefn.getPlotFunctionTable() != null)
//            					domElement.setAttribute("plotFunctionTable", prmDefn.getPlotFunctionTable());
//            				if( prmDefn.getPlotLookupTable() != null)
//            					domElement.setAttribute("plotLookupTable", prmDefn.getPlotLookupTable());
//            				if( prmDefn.getClazz() != null)
//            					domElement.setAttribute("class", prmDefn.getClazz());
                // the transform is part of the svg syntax so it is in the 
//                if( prmDefn.getTransform() != null) {
//                	domElement.setAttribute("transform", prmDefn.getTransform());
//                }
                // Set textSize, font, style, color ,etc. attributes
                //      domElement = setParamAttributes( pme, domElement);
                boolean isTextBasedPlotParameter = prmDefn.getPlotMode().equals("text");
                
                if (isTextBasedPlotParameter ) {//T936 - add the text based plot parameters in a separate list
                	
                	PlotElement plotElement = new PlotElement(prmDefn);
                	plotElement.position = new String ( pme.getPosition());

                	//add the text-based plot parameters to a separate list
                	listOfTextPlotElements.add(plotElement);
                	continue;
                
                }
                
                if (pme.getConditionalParameter() == null || "".equals(pme.getConditionalParameter()) ) {
             	   
		                String style = domElement.getAttribute("style");
		                String color = "RGB("+pme.getColor().getRed()+"," +
		               							pme.getColor().getGreen()+"," +
		               								pme.getColor().getBlue()+")";
		             
		                String fontFamily = prmDefn.getSymbolFont();
		              
		                // TODO : adjust the position of symbol parameters to align them correctly 
		                               
//		                if( prmDefn.getPlotMode().equals("text" ) ) {         
//		                	fontFamily = pme.getTextFont() + ("Standard".equals(pme.getTextFont())? "":pme.getTextStyle()) + "Font";
//		                	
//		                	style = style + "stroke: "+color+";fill: "+("Standard".equals(pme.getTextFont())? "none":color)
//		                				+";font-size: "+pme.getTextSize()
//		                					+";font-family: "+fontFamily
//		                    				+ ";letter-spacing: 1" //(fontFamily.startsWith("Courier")? "":";letter-spacing: 1")
//		                					+";font-style: "+(pme.getTextStyle().endsWith("Italic")? "italic":"normal")+";";
//		                	
//		                } 
//		                else
		                if( prmDefn.getPlotMode().equals("barb" ) ) {                
		                	style = "fill: none; "+"stroke: "+color                    		
		                			+";font-size:"+pme.getSymbolSize()+"em"
		                				+";stroke-width: 1px"
		                					+";font-family:"+fontFamily+";";
		                	
		                } 
		                else if( prmDefn.getPlotMode().equals("table" ) ){
		                	style = style + "fill: none; "+"stroke: "+color
		                			+";stroke-width: 1px"//+pme.getSymbolSize()+"px"
		                				+";font-size:"+pme.getSymbolSize()+"em"
                					+";font-family:"+fontFamily+";";
		              
		                }
		                else if( !prmDefn.getPlotMode().equals("text" ) ){
		                  	style = style + "stroke: "+color
		          				+";stroke-width: 1px"
		          				+";font-size:"+pme.getSymbolSize()+"em"
		          					+";font-family:"+fontFamily+";";
		                  	System.out.println("prmDefn missing/unrecognized plotMode: " +
		                  			prmDefn.getPlotMode() );
		                  	continue;
		                  }  
		
		               
		               domElement.setAttribute("style", style);//"stroke: "+color);
                }
                
                PlotElement plotElement = new PlotElement( prmDefn, domElement );

                // MOVED THIS CODE to the constructor of PlotElement
//                if( prmDefn.getPlotMode().equals("text" ) ) {                
//                if( domElement.getAttribute(DM_ATTRIBUTE).equals("text")) {
//                	thisElement.mode = DisplayMode.TEXT;
                	// domElement.setAttribute(CLASS_ATTRIBUTE, "text");
//                } else if (domElement.getAttribute(DM_ATTRIBUTE).equals(  "barb")) {
//                if( plotElement.getDisplayMode() == DisplayMode.BARB ) {
//                	plotElement.winds = new PlotWindElement();
//                	NodeList windElements = domElement.getChildNodes();
//                	for (int j = 0; j < windElements.getLength(); j++) {                      	
//                		if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
//                			Element windElement = (Element) windElements.item(j);
//                			
//                			if (windElement.getAttribute("class").matches( "arrow" )) {
//                				plotElement.winds.arrowElement = windElement;
//                				plotElement.winds.arrowNode = windElement.getChildNodes().item(0);
//                			} 
//                			else if (windElement.getAttribute("class").matches("barb")) {
//                				plotElement.winds.barbElement = windElement;
//                				plotElement.winds.barbNode = windElement.getChildNodes().item(0);			
//                			} 
////                			else if (windElement.getAttribute("class").matches("text")) {
////                				thisElement.winds.gustElement = windElement;
////                				thisElement.winds.gustNode = windElement
////                				.getChildNodes().item(0);
////                				thisElement.winds.gustX = windElement
////                				.getAttribute("x");
////                				thisElement.winds.gustY = windElement
////                				.getAttribute("y");
////                			}
//                		}
//                	}
//                } 
// TODO : add this back if we have any 'arrow' parameters
//                else if ( thisElement.getDisplayMode() == DisplayMode.ARROW ) {
////domElement.getAttribute(DM_ATTRIBUTE).equals( "arrowuv")) {
////                	thisElement.mode = DisplayMode.ARROW;
//                	thisElement.winds = new PlotWindElement();
//                	NodeList windElements = domElement.getChildNodes();
//                	for (int j = 0; j < windElements.getLength(); j++) {
//                		if (Node.ELEMENT_NODE == windElements.item(j)
//                				.getNodeType()) {
//                			Element windElement = (Element) windElements
//                			.item(j);
//                			if (windElement.getAttribute("class").matches(
//                					"arrow")) {
//                				thisElement.winds.arrowElement = windElement;
//                				thisElement.winds.arrowNode = windElement
//                				.getChildNodes().item(0);
//                			} else if (windElement.getAttribute("class")
//                					.matches("text")) {
//                				thisElement.winds.gustElement = windElement;
//                				thisElement.winds.gustNode = windElement
//                				.getChildNodes().item(0);
//                				thisElement.winds.gustX = windElement
//                				.getAttribute("x");
//                				thisElement.winds.gustY = windElement
//                				.getAttribute("y");
//                			}
//                		}
//                	}
//                }
//                else if( plotElement.getDisplayMode() == DisplayMode.TABLE ) {
//                		domElement.getAttribute(DM_ATTRIBUTE).equals("table")) {
//                	thisElement.mode = DisplayMode.TABLE;
//                	if (domElement.hasAttribute(PFT_ATTRIBUTE)) {
//                		thisElement.ranking = S2N.readS2NFile(domElement
//                				.getAttribute(PFT_ATTRIBUTE));
//                	}
//                	if (domElement.hasAttribute(PLT_ATTRIBUTE)) {
//                		thisElement.lookup = StringLookup.readS2SFile(
//                				prmDefn.getPlotLookupTable() );
//                	}
// TODO : Add this back.
//                    } else if (domElement.getAttribute(DM_ATTRIBUTE).equals(
//                            "arrow")) {
//                    	domElement.setAttribute("class", "text");
//                        thisElement.mode = DisplayMode.BARB;
//                } 
//                else if( thisElement.getDisplayMode() == DisplayMode.RANGE ) {
//                	if (domElement.hasAttribute(PLT_ATTRIBUTE)) {
//                		thisElement.lookup = StringLookup.readR2SFile(domElement
//                				.getAttribute(PLT_ATTRIBUTE));
//                	}
//                }
//                else if (domElement.getAttribute(DM_ATTRIBUTE).equals("null")) {
//                	thisElement.mode = DisplayMode.NULL;
//                } 
//                else  if( thisElement.getDisplayMode() == DisplayMode.SAMPLE ) {
//                	if (domElement.hasAttribute(PLT_ATTRIBUTE)) {
//                		thisElement.lookup = StringLookup.readS2SFile(domElement
//                				.getAttribute(PLT_ATTRIBUTE));
//                		thisElement.lookup2 = StringLookup
//                		.readR2SFile(domElement
//                				.getAttribute(PLT_ATTRIBUTE));
//                	}
//                }
                    
   // All this now in the plotParamDefn since it doesn't need to be part of the in-memory xml.
                    
//                    thisElement.dbParamName = domElement.getAttribute(P_ATTRIBUTE);
//                    thisElement.metParamName = domElement.getAttribute( );
                    
//                    if (domElement.hasAttribute(FMT_ATTRIBUTE)) {
//                        thisElement.format = domElement
//                                .getAttribute(FMT_ATTRIBUTE);
//                    }
//     // TODO : remove this from the domElement when the
//     // UA processing is replaced with the VerticalSounding (ie when the PointDataView is
//             // removed and the units comes from the metParameter.
//                    if (domElement.hasAttribute(UNIT_ATTRIBUTE)) {
//                        thisElement.unit = domElement
//                                .getAttribute(UNIT_ATTRIBUTE);
//                    }
//                    if (domElement.hasAttribute(SYMBOL_ATTRIBUTE)) {
//                        thisElement.symbol = domElement
//                                .getAttribute(SYMBOL_ATTRIBUTE);
//                    }
//                    if (domElement.hasAttribute(TRIM_ATTRIBUTE)) {
//                        thisElement.trim = Integer.parseInt(domElement
//                                .getAttribute(TRIM_ATTRIBUTE));
//                    }
//                    if (domElement.hasAttribute(PLT_INDEX)) {
//                        thisElement.index = Integer.parseInt(domElement
//                                .getAttribute(PLT_INDEX));
//                    }
//                    if (domElement.hasAttribute(REQUIRED)) {
//                        thisElement.required = Boolean.parseBoolean(domElement
//                                .getAttribute(REQUIRED));
//                    }
////                    if( thisElement.mode != DisplayMode.NULL  ) {
//                    	if( thisElement.dbParamName == null ||
//                    		thisElement.dbParamName.isEmpty() ) {
//                    		thisElement.mode =DisplayMode.NULL; 
//                    	}
//                    	else {
////                    		System.out.println("adding "+thisElement.parameter);
//                    	}
                plotElement.position = new String(pme.getPosition());//T-936 redundant for non-text plot parameters. But added it for consistency     	
                this.plotElements.add(plotElement);                
            }
        }

        
//        List<PlotModelElement> listOfAllPlotModelElements =  this.plotModel.getAllPlotModelElements();
//
//        if ( listOfAllPlotModelElements != null && !listOfAllPlotModelElements.isEmpty()  ){
//        	listOfTextPlotElements = new ArrayList<PlotModelFactory2.PlotElement>(0);
//        	for ( PlotModelElement pme : listOfAllPlotModelElements ){
//                    PlotParameterDefn prmDefn = plotParamDefns.getPlotParamDefn( pme.getParamName() );
//                    if( prmDefn == null ) {
//               	           System.out.println("Unable to find PlotParameterDefn for :"+pme.getParamName() );
//               	         continue;
//                     }
//                    String plotMode = prmDefn.getPlotMode();
//                    if( plotMode.compareTo("text" ) == 0 ) {   
//                    	PlotElement plotElement = new PlotElement ( prmDefn );
//                    	plotElement.position = new String(pme.getPosition());
//                    	listOfTextPlotElements.add(plotElement);
//                    }
//        	}
//        }
        
        
        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    // TODO: Do we still need this??
    public void setPlotDimensions(long x, long y) {
        this.svgRoot.setAttributeNS(null, "width", Long.toString(x));
        this.svgRoot.setAttributeNS(null, "height", Long.toString(y));
        this.plotModelWidth = (int) x;
        this.plotModelHeight = (int) y;
    }

    public int getDefinedPlotModelWidth() {
        return this.originalPlotModelWidth;
    }

    public int getDefinedPlotModelHeight() {
        return this.originalPlotModelHeight;
    }

    
    public synchronized BufferedImage getStationPlot( 
    		HashMap<String,AbstractMetParameter> metParams ) {
    	
    	//double[] stationLoc = {0,0};

    	if( !metParams.containsKey( StationLatitude.class.getSimpleName() ) ||
    			!metParams.containsKey( StationLongitude.class.getSimpleName() ) ) {
    		return null;
    	}
    	AbstractMetParameter latLonPrm = metParams.get( StationLongitude.class.getSimpleName() );    		
    	stationLoc[0] = latLonPrm.getValueAs( NonSI.DEGREE_ANGLE ).doubleValue(); 

    	latLonPrm = metParams.get( StationLatitude.class.getSimpleName() );    		
    	stationLoc[1] = latLonPrm.getValueAs( NonSI.DEGREE_ANGLE ).doubleValue(); 
    	
    	double[] stationPixelLoc = this.mapDescriptor.worldToPixel(stationLoc);

    	if (stationPixelLoc != null) {
    		stationPixelLoc[1]--;
    		double[] newWorldLoc = this.mapDescriptor.pixelToWorld(stationPixelLoc);

    		this.gc.setStartingGeographicPoint(stationLoc[0], stationLoc[1]);
    		
    		this.gc.setDestinationGeographicPoint(newWorldLoc[0], newWorldLoc[1]);
    	}
    
    	try {
    		boolean discard = false;
    		for( PlotElement pltMdlElmt : this.plotElements ) {
    			boolean valid = true;
    			boolean required = pltMdlElmt.required;

    			PlotParameterDefn prmDefn = pltMdlElmt.plotParamDefn;
    			String metParamName = prmDefn.getMetParamName();

    			switch( pltMdlElmt.getDisplayMode() ) {
    			case TEXT : case TABLE :          			
    				if( !metParams.containsKey( metParamName ) ) { //pltMdlElmt.plotParamName ) ) {
    					System.out.println("??sanity check: parameter "+metParamName +
    					                         " not found in displayParam map" );
    					continue;
    				}
    				break;
    			case BARB : // case ARROW :
    				if( !metParams.containsKey( pltMdlElmt.getVectorParamNames()[0] ) ||
    						!metParams.containsKey( pltMdlElmt.getVectorParamNames()[0] ) ) {

    					System.out.println("??sanity check: parameter "+metParamName +
    					                         " not found in displayParam map" );
    					continue;
    				}
    			}

    			switch ( pltMdlElmt.getDisplayMode() ) {
    			case TEXT:
    				this.processTextDirective(
    						metParams.get( pltMdlElmt.getMetParamName() ), pltMdlElmt,null );
    				break;
    				// the WindBarb parameter should be the only case for a 'barb' plotMode,
    				// so we will check for this here.
    			case BARB:
    				valid = this.processBarbDirective(
    						metParams.get( pltMdlElmt.getVectorParamNames()[0] ),
    						metParams.get( pltMdlElmt.getVectorParamNames()[1] ),
    						pltMdlElmt);
    				break;
    				//    				case ARROW:
    				//    					this.processArrowDirective(
    				//    							metParams.get( element.plotParamName ), element);
    				//    					break;
    			case TABLE:
    				this.processTableDirective(
    						metParams.get( pltMdlElmt.getMetParamName() ), pltMdlElmt);
    				break;
    				//    				case RANGE:
    				//    					this.processRangeDirective(
    				//    							metParams.get( element.plotParamName ), element);
    				//    					break;
    			case NULL:
    				pltMdlElmt.domNode.setNodeValue(" ");
    				break;
    				//    				case SAMPLE:
    				//    					this.processSampleDirective(stationData, element);
    				//    					break;
    			}
    			if (!valid && required) {
    				discard = true;
    			}
    		}
            
//          createMessage(id);
            
    		BufferedImage bufferedImage = new BufferedImage(
                    this.plotModelWidth, this.plotModelHeight,
                    BufferedImage.TYPE_4BYTE_ABGR );
            Graphics2D g2d = bufferedImage.createGraphics();
    
            this.theGraphicsNode = builder.build(this.bridgeContext,
                    this.document);
            this.theGraphicsNode.primitivePaint(g2d);
            theGraphicsNode.paint(g2d);
            // Cleanup and return image
            g2d.dispose();
            
            if (discard) {
                return null;
            }

            return bufferedImage;
    		
    	} catch (Exception e) {
            e.printStackTrace();
        }
        
        return null;
    }
    /**
     * Takes a collection of metParameters and produces a buffered
     * image.
     */

    public synchronized BufferedImage getStationPlot( 
    		HashMap<String,AbstractMetParameter> metParams, HashMap<String,AbstractMetParameter> allMetParamsMap) {
    	
    	if( !metParams.containsKey( StationLatitude.class.getSimpleName() ) ||
    			!metParams.containsKey( StationLongitude.class.getSimpleName() ) ) {
    		return null;
    	}
    	AbstractMetParameter latLonPrm = metParams.get( StationLongitude.class.getSimpleName() );    		
    	stationLoc[0] = latLonPrm.getValueAs( NonSI.DEGREE_ANGLE ).doubleValue(); 

    	latLonPrm = metParams.get( StationLatitude.class.getSimpleName() );    		
    	stationLoc[1] = latLonPrm.getValueAs( NonSI.DEGREE_ANGLE ).doubleValue(); 
    	
    	double[] stationPixelLoc = this.mapDescriptor.worldToPixel(stationLoc);

    	if (stationPixelLoc != null) {
    		stationPixelLoc[1]--;
    		double[] newWorldLoc = this.mapDescriptor.pixelToWorld(stationPixelLoc);

    		this.gc.setStartingGeographicPoint(stationLoc[0], stationLoc[1]);
    		
    		this.gc.setDestinationGeographicPoint(newWorldLoc[0], newWorldLoc[1]);
    	}
    
    	try {
    		boolean discard = false;
    		for( PlotElement pltMdlElmt : this.plotElements ) {
    			boolean valid = true;
    			boolean required = pltMdlElmt.required;

    			PlotParameterDefn prmDefn = pltMdlElmt.plotParamDefn;
    			
//    			if (prmDefn.getPlotMode().equals("text"))
//    				continue;
    			
    			String metParamName = prmDefn.getMetParamName();    			
    			
    			PlotModelElement pme = plotModel.getPlotModelElement(pltMdlElmt.domElement.getAttribute("position").toUpperCase() );
    			  
    			String style = pltMdlElmt.domElement.getAttribute("style");
                String color = "RGB("+pme.getColor().getRed()+"," +
                							pme.getColor().getGreen()+"," +
                								pme.getColor().getBlue()+")";  
                String fontFamily = prmDefn.getSymbolFont();    
                
                boolean applyAdvancedSettings = false;
                String condParmValue = "";
                
                if (pme.getConditionalParameter() != null && !pme.getConditionalParameter().trim().equals("")) {
                	applyAdvancedSettings = true;
                	
                	PlotParameterDefn condPrmDefn = plotParamDefns.getPlotParamDefn( pme.getConditionalParameter().trim() );
        			condParmValue = getConditionalParameterValue(condPrmDefn, allMetParamsMap.get(condPrmDefn.getMetParamName()));
                }
                               
    			switch( pltMdlElmt.getDisplayMode() ) {
//    			case TEXT : 
    				case TABLE :          			
    				if( !metParams.containsKey( metParamName ) ) { 
    					System.out.println("??sanity check: parameter "+metParamName +
    					                         " not found in displayParam map" );
    					continue;
    				}
    				break;
    			case BARB : // case ARROW :
    				if( !metParams.containsKey( pltMdlElmt.getVectorParamNames()[0] ) ||
    						!metParams.containsKey( pltMdlElmt.getVectorParamNames()[0] ) ) {

    					System.out.println("??sanity check: parameter "+metParamName +
    					                         " not found in displayParam map" );
    					continue;
    				}
    			}
    			

				if (applyAdvancedSettings) { 
					try {
						float value = Float.parseFloat(condParmValue);
    					RGB rgb = pme.getConditionalColorBar().getRGBForInterval(value);
    					color = "RGB("+rgb.red+"," +rgb.green+"," +rgb.blue+")"; 
					} 
					catch (Exception e) {
						//System.out.println(" Error while getting style settings from Advanced: " + e.getMessage() + " Error: " + e.toString());
					}
				}				
				

    			switch ( pltMdlElmt.getDisplayMode() ) {
//    			case TEXT:
//    				this.processTextDirective(
//    						metParams.get( pltMdlElmt.getMetParamName() ), pltMdlElmt );
//
//    				fontFamily = pme.getTextFont() + ("Standard".equals(pme.getTextFont())? "":pme.getTextStyle()) + "Font";
//                 	style = style + "stroke: "+color+";fill: "+("Standard".equals(pme.getTextFont())? "none":color)
//                 				+";font-size: "+pme.getTextSize()
//                 					+";font-family: "+fontFamily
//                     				+ ";letter-spacing: 1"
//                 					+";font-style: "+(pme.getTextStyle().endsWith("Italic")? "italic":"normal")+";";
//                 	    				
//    				break;
    				// the WindBarb parameter should be the only case for a 'barb' plotMode,
    				// so we will check for this here.
    			case BARB:
    				style = "fill: none; "+"stroke: "+color                    		
         				+";font-size:"+pme.getSymbolSize()+"em"
         					+";stroke-width: 1px"
         						+";font-family:"+fontFamily+";";
         			
    				valid = this.processBarbDirective(
    						metParams.get( pltMdlElmt.getVectorParamNames()[0] ),
    						metParams.get( pltMdlElmt.getVectorParamNames()[1] ),
    						pltMdlElmt);
    				break;
    				//    				case ARROW:
    				//    					this.processArrowDirective(
    				//    							metParams.get( element.plotParamName ), element);
    				//    					break;
    			case TABLE:
    				style = style + "fill: none; "+"stroke: "+color
         				+";stroke-width: 1px"
         					+";font-size:"+pme.getSymbolSize()+"em"
         						+";font-family:"+fontFamily+";";
         			
    				this.processTableDirective(
    						metParams.get( pltMdlElmt.getMetParamName() ), pltMdlElmt);
    				break;
    				//    				case RANGE:
    				//    					this.processRangeDirective(
    				//    							metParams.get( element.plotParamName ), element);
    				//    					break;
//    			case NULL:
//    				style = style + "stroke: "+color +";stroke-width: 1px"
//    					+";font-size:"+pme.getSymbolSize()+"em"
//    						+";font-family:"+fontFamily+";";
//    				
//    				pltMdlElmt.domNode.setNodeValue(" ");
//    				
//    				break;
    				//    				case SAMPLE:
    				//    					this.processSampleDirective(stationData, element);
    				//    					break;
    			}    
    			
    			if (applyAdvancedSettings) { 
    				pltMdlElmt.domElement.setAttribute("style", style);
    			}
    			
    			if (!valid && required) {
    				discard = true;
    			}
    			    			
    		}
    		

    		BufferedImage bufferedImage = new BufferedImage(
                   this.plotModelWidth, this.plotModelHeight,
                    BufferedImage.TYPE_4BYTE_ABGR ); 
            int x = bufferedImage.getWidth() /2 ;
	        int y = bufferedImage.getHeight() /2;
    		Graphics2D g2d = bufferedImage.createGraphics();
            GraphicsEnvironment graphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment();

            
    		//T936 - process the text-based plot parameters
    		if ( this.listOfTextPlotElements != null && 
    				!this.listOfTextPlotElements.isEmpty()){
    			
    	 		for ( PlotElement thisTextPlotElement : this.listOfTextPlotElements ){
        			boolean valid = true;
        			boolean required = thisTextPlotElement.required;
        			String metParamName =  thisTextPlotElement.getMetParamName();
        			if( !metParams.containsKey( metParamName ) ) { 
    					System.out.println("??sanity check: parameter "+metParamName +
    					                         " not found in displayParam map" );
    					continue;
    				}

        			        this.processTextDirective( metParams.get( metParamName ), thisTextPlotElement , metParams);

        			if (!valid && required) {
        				discard = true;
        			}
        			
                    boolean applyAdvancedSettings = false;
                    String condParmValue = "";
                    String position =  thisTextPlotElement.position.toUpperCase();
        			PlotModelElement pme = plotModel.getPlotModelElement( position );
        			
                    if (pme.getConditionalParameter() != null && !pme.getConditionalParameter().trim().equals("")) {
                    	applyAdvancedSettings = true;
                    	
                    	PlotParameterDefn condPrmDefn = plotParamDefns.getPlotParamDefn( pme.getConditionalParameter().trim() );
            			condParmValue = getConditionalParameterValue(condPrmDefn, allMetParamsMap.get(condPrmDefn.getMetParamName()));
                    }

                    Color pmeColor = null;
    				if (applyAdvancedSettings) { 
    					try {
    						float value = Float.parseFloat(condParmValue);
        					RGB rgb = pme.getConditionalColorBar().getRGBForInterval(value);
        					pmeColor = new Color(rgb.red,rgb.green,rgb.blue);
    					} 
    					catch (Exception e) {
    						//System.out.println(" Error while getting style settings from Advanced: " + e.getMessage() + " Error: " + e.toString());
    					}
    			    }
    				else{//no conditional coloring
    					int red = pme.getColor().getRed();
                        int green = pme.getColor().getGreen();
                        int blue =  pme.getColor().getBlue();
                        pmeColor = new Color(red, green, blue);
    					
    				}
                    
        			
    				g2d.setColor( pmeColor );
                    
                    String fontName = pme.getTextFont();
                    
                    float fontSize = (float)  Integer.parseInt( pme.getTextSize() );
                    String pmeFontStyle = pme.getTextStyle();
                    Font derivedFont = null;
                    Font createdFont = null;
                    int fontStyle = Font.PLAIN;
                    if ( pmeFontStyle.compareTo("Italic") == 0 )
                  	  fontStyle = Font.ITALIC;
                    else if ( pmeFontStyle.compareTo("Bold")  == 0)
                  	  fontStyle = Font.BOLD;
                    else if (pmeFontStyle.compareTo("Bold-Italic") == 0) {
                 	   fontStyle = Font.BOLD | Font.ITALIC;
                    }
                    
                    switch(fontStyle){

                              case ( Font.BOLD | Font.ITALIC ):
                            	  if ( fontName.compareTo("Courier") == 0 ){
                            		  derivedFont =Font.createFont(Font.TYPE1_FONT, COURIER_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Times") == 0 ){
                            		  derivedFont = Font.createFont(Font.TYPE1_FONT, SERIF_BOLD_ITALIC_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Helvetica") == 0 ){
                            		  derivedFont = Font.createFont(Font.TRUETYPE_FONT, SANS_SERIF_BOLD_ITALIC_FONT).deriveFont( fontStyle, fontSize);
                            	  }

                              break;

                              case Font.BOLD:

                            	  if ( fontName.compareTo("Courier") == 0 ){
                            		  derivedFont =Font.createFont(Font.TYPE1_FONT, COURIER_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Times") == 0 ){
                            		  derivedFont = Font.createFont(Font.TYPE1_FONT, SERIF_BOLD_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Helvetica") == 0 ){
                            		  derivedFont = Font.createFont(Font.TRUETYPE_FONT, SANS_SERIF_BOLD_FONT).deriveFont( fontStyle, fontSize);
                            	  }

                            	  break;

                              case Font.ITALIC:
                            	  if ( fontName.compareTo("Courier") == 0 ){
                            		  derivedFont =Font.createFont(Font.TYPE1_FONT, COURIER_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Times") == 0 ){
                            		  derivedFont = Font.createFont(Font.TYPE1_FONT, SERIF_ITALIC_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Helvetica") == 0 ){
                            		  derivedFont = Font.createFont(Font.TRUETYPE_FONT, SANS_SERIF_ITALIC_FONT).deriveFont( fontStyle, fontSize);
                            	  }                        	       
                            	  break;              

                              default: //Font.PLAIN
                            	  if ( fontName.compareTo("Courier") == 0 ){
                            		  derivedFont = Font.createFont(Font.TYPE1_FONT, COURIER_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Times") == 0 ){
                            		  derivedFont = Font.createFont(Font.TRUETYPE_FONT, SERIF_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }
                            	  else if ( fontName.compareTo("Helvetica") == 0 ){
                            		  derivedFont = Font.createFont(Font.TRUETYPE_FONT, SANS_SERIF_NORMAL_FONT).deriveFont( fontStyle, fontSize);
                            	  }    

                            	  break;
                    }
                    
                    graphicsEnvironment.registerFont(derivedFont);
                    g2d.setFont(derivedFont);
                    g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
    		        FontMetrics fm = g2d.getFontMetrics();
      		        int colSpacing = ( derivedFont.getFontName().contains("Courier") ? fm.getMaxAdvance()*2  :  fm.getMaxAdvance() );
      		        int rowSpacing = fm.getMaxAscent();
      		        
               	   int middleX = bufferedImage.getWidth()/2;
               	   int middleY = bufferedImage.getHeight()/2;
               	String strToDraw = thisTextPlotElement.formattedStringToPlot;
               	if(position.contains("MC")){
               		     x = middleX;
                         y = middleY;
               	}
               	else if (position.contains("ML")){
           		         x = middleX - colSpacing;
		        	         y = middleY;
           	     }  
               	else if (position.contains("MR")){
      		         x = middleX + colSpacing ;
	        	     y = middleY;
  	           } 
               	else if (position.contains("TC")){
   		                        x = middleX ;
        	                    y = middleY - rowSpacing*2;
   	             } 

               	else if (position.contains("BC")){
	                    x = middleX ;
	                        y = middleY + rowSpacing*2;
                      }
               	else if (position.contains("LC")){
	                    x = middleX ;
	                        y = middleY +rowSpacing;
                      }                    	

               	else if (position.contains("UC")){
	                    x = middleX ;
	                        y = middleY - rowSpacing;
                      }
               	else if (position.contains("UR")){
                         x = middleX + colSpacing;
                         y = middleY - rowSpacing;
                  }                    	
               	
               	else if (position.contains("UL")){
                          x = middleX - colSpacing;
                          y = middleY - rowSpacing;
                 }                     	
               	
               	else if (position.contains("LR")){
	                     x = middleX+colSpacing;
                         y = middleY + rowSpacing;
                  }
               	
               	else if (position.contains("LL")){
                         x = middleX-colSpacing;
                         y = middleY + rowSpacing;
              }       
                g2d.drawString(strToDraw, x, y);
                    
                    
                    
    	 		}
    		}
            
            
            
            this.theGraphicsNode = builder.build(this.bridgeContext,
                    this.document).getRoot();
            this.theGraphicsNode.primitivePaint(g2d);
          	// Cleanup and return image
            //bufferedImage.flush();
            g2d.dispose();
            
            if (discard) {
                return null;
            }
         	
            return bufferedImage;
    		
    	} catch (Exception e) {
            e.printStackTrace();
        }
        
        return null;
    }
    
    // TODO : rm PointDataView when upper air is implemented with the VerticalSounding 
    private void processTextDirective(
    		AbstractMetParameter metParam,
            PlotElement element, Map<String,AbstractMetParameter> metParamsMap) throws VizException {
    	
    	if( !metParam.hasValidValue() ) {    		
            //element.domNode.setNodeValue(  ( plotMissingData ? "m" : "" ) ); //T936 - text based plot parameters don't use the domnode now
    		element.formattedStringToPlot = ( plotMissingData ? "m" : "" );
    	}
    	else {
   			
			if(metParam.getMetParamName().compareTo("DewPointDepression") == 0 && metParamsMap != null){
				Collection<AbstractMetParameter> listOfAllDbParams = metParamsMap.values();
				if(listOfAllDbParams.size() > 0){
					try {
						metParam = metParam.derive(listOfAllDbParams);
	
					} catch (NotDerivableException e) {
					         	
						metParam.setValueToMissing();
					}
				}else
					metParam.setValueToMissing();
			}
			else    		
    		// change the units 
    		if( element.getUnit() != metParam.getUnit() ) {
    			metParam.setValue( 
    				metParam.getValueAs( element.getUnit()), element.getUnit() );
    			
    		}
    		
			if(!metParam.hasValidValue())
				return;
			
    		String formattedPlotString = metParam.getFormattedString(
    										element.getPlotFormat() ).trim();
    		
    		if( element.getPlotTrim() != 0 ) {
    			formattedPlotString = formattedPlotString.substring( element.getPlotTrim() );
    		}

//    		element.domNode.setNodeValue( formattedPlotString );//T936 - text-based plot parameters no longer use the domnode
    		element.formattedStringToPlot = new String( formattedPlotString );

    	}
//        String sValue = null;
////        String dbParam = element.dbParamName;
//        Number value = null;
//
//        if( metParam.hasStringValue() ) {
//            element.domNode.setNodeValue( metParam.getStringValue() );
//        }
//        else if( metParam.hasValidValue() ) {            
//        	value = metParam.getValueAs( element.getUnit() );
//        	
//        	double displayValue = value.doubleValue();
//
//        	if( element.getPlotFormat() != null ) {
//        		StringBuilder sb = new StringBuilder();
//        		Formatter testing = new Formatter(sb);
//        		testing.format( element.getPlotFormat(), displayValue );
//        		sValue = sb.toString();
//        	} else {
//        		sValue = Double.toString( displayValue );
//        	}
//        	sValue = sValue.substring( element.getPlotTrim() );
//
//        	//                    if("visibility".equals(dbParam)) 
//        	//                    	sValue=sValue.replace(".","");// TODO remove hard-code?
//        	element.domNode.setNodeValue(sValue);
//        }
//        else { // missing        	
//        	element.domNode.setNodeValue(  ( plotMissingData ? "m" : "" ) );        	
//        }
    }

    
    private boolean processBarbDirective( AbstractMetParameter metParam1,
    		AbstractMetParameter metParam2,
            PlotElement element) throws VizException{

    	AbstractMetParameter windSpeed, windDir;
    	
        if( metParam1 instanceof Angle ) {
        	windDir   = metParam1; 
        	windSpeed = metParam2;  
        }
        else if( metParam2 instanceof Angle ) {
        	windDir   = metParam2; 
        	windSpeed = metParam1;         	
        }
		else {
			element.displayMode = DisplayMode.NULL;
			throw new VizException("The barb plotElement must specify a direction parameter." );
		}

        if( element.winds.barbElement == null ||
        	element.winds.barbNode == null ) { // sanity check
			element.displayMode = DisplayMode.NULL;
			throw new VizException("Error with the plotModelTemplate. wind barb element is missing?" );
        }
        
        // Currently not implementing plotMissingData flag which 
        // will plot an 'm' for missing data.
        if( !windDir.hasValidValue() || 
        	!windSpeed.hasValidValue() ) {

        	element.winds.barbElement.removeAttribute("transform");
        	element.winds.barbNode.setNodeValue(" ");
            
        	if( element.winds.arrowElement != null ) {
                element.winds.arrowNode.setNodeValue(" ");
            }
            return false;
        }

        // the units in the element are for the windSpeed and not the direction.        		
        double dWindDir = windDir.getValueAs( NonSI.DEGREE_ANGLE ).doubleValue() 
        						- this.gc.getAzimuth();
        double cWindSpeed = windSpeed.getValueAs( element.getUnit() ).doubleValue();
        
        Double cWindSpeedThresh = 
        	new Amount( 3.0, NonSI.KNOT ).getValueAs( element.getUnit() ).doubleValue();

        if( cWindSpeed >= 0 && cWindSpeed < cWindSpeedThresh ) {
        	element.winds.barbElement.removeAttribute("transform");
        	element.winds.barbNode.setNodeValue("0");
        } 
        else if( cWindSpeed >= cWindSpeedThresh ) {
        	int iWindSpeed = this.windNormalizer(cWindSpeed);
        	if (stationLoc[1] < 0) 

        		element.winds.barbElement.setAttribute("transform", "rotate("
            			+ (dWindDir) + ",0,0) matrix(-1, 0, 0, 1, 0, 0)");
        	else
        		element.winds.barbElement.setAttribute("transform", "rotate(" + dWindDir + ",0,0)");

        	element.winds.barbNode.setNodeValue( Integer.toString(iWindSpeed) );
        } 

        renderArrow( windSpeed.getValueAs( element.getUnit() ),
        			 windDir.getValueAs( NonSI.DEGREE_ANGLE ),
        			 element.getUnit(), element );

        return true;
    }
    
    
    // 
    private void renderArrow( Number magnitude, Number direction,
            Unit<?> speedUnit, // units of the magnitude 
            PlotElement element) throws VizException {
    	
        double dDir = -9999.0;
        double cMag = -9999.0;
        if (element.getUnit() != null && magnitude != null) {
            if (element.converter == null) {
            	Unit<?> unit = element.getUnit();
            	element.converter = speedUnit.getConverterTo(unit);
            }
            if( isValidValue(magnitude) ) {
                cMag = element.converter.convert(magnitude.doubleValue());
            }
        } else if (magnitude != null) {
            cMag = magnitude.doubleValue();
        }
        if (direction != null) {
            dDir = direction.doubleValue() - this.gc.getAzimuth();
        }
        
        if (element.winds.arrowElement != null) {
            if (dDir != -9999.0 && cMag != -9999.0) {
            	if (stationLoc[1] < 0) 
        		element.winds.barbElement.setAttribute("transform", "rotate("
            			+ (dDir) + ",0,0) matrix(-1, 0, 0, 1, 0, 0)");
        	else
        		element.winds.barbElement.setAttribute("transform", "rotate(" + dDir + ",0,0)");
            			
            element.winds.arrowNode.setNodeValue("arrow");
            
            } else {
                element.winds.arrowElement.removeAttribute("transform");
                element.winds.arrowNode.setNodeValue(" ");
            }
        }
//        if (element.winds.gustElement != null) {
//            if (dDir != -9999.0 && cMag != -9999.0) {
//                double rWindDir = Math.toRadians(dDir + 180.0);
//                long x = Math.round(32.0 * Math.sin(rWindDir));
//                long y = Math.round(32.0 * Math.cos(rWindDir)) * -1;
//                element.winds.gustElement.setAttribute("x", Long.toString(x));
//                element.winds.gustElement.setAttribute("y", Long.toString(y));
//                element.winds.gustNode.setNodeValue(Integer
//                        .toString(((int) cMag)));
//            } else {
//                element.winds.gustNode.setNodeValue(" ");
//            }
//        }
    }

    private void processTableDirective( AbstractMetParameter metParam, 
            PlotElement element) {
//    	Number value = metParam.getValueAs( element.getUnit() );

        String displayStr = null;
        String[] fields = null;

        if( metParam.hasStringValue() ) {
            displayStr = metParam.getStringValue();        	
        }
        else {
        	Number dispValue = metParam.getValueAs( element.getUnit() );
        	if (dispValue != null) {     	       
	        	displayStr = dispValue.toString();
        	} else {
        		displayStr = "";
        	}
        	
        }
// move this functionality to the Generator where the metParameter value is set.
////        element.domNode.setNodeValue(" ");
//
//        if (element.index != -1 && element.index < fields.length) {
//            displayStr = fields[element.index];
//        } else if (element.ranking != null && fields != null) {
//            displayStr = element.ranking.getRankedField(fields);
//        } else if (fields != null) {
//            displayStr = fields[0];
//            for(int i = 1; i < fields.length; i++){
//                if(fields[i].length() > 0){
//                    displayStr = fields[i] + " " + displayStr;
//                }
//            }
//        }
        
        if (element.lookup != null) {
            displayStr = element.lookup.recursiveTranslation(displayStr);
        }
        
        if (displayStr != null) {
            element.domNode.setNodeValue( displayStr  );
        } else {
            element.domNode.setNodeValue(" ");
        }
    }
    
    
   // add this back if we actually have a parameter plotted as a range
//    private void processRangeDirective(PointDataView ob, 
//            PlotElement element) throws VizException{
//
//        String sValue = null;
//        switch (ob.getType(element.dbParamName)) {
//        case FLOAT:
//        case DOUBLE:
//        case INT:
//        case LONG:
//            Number value = ob.getNumber(element.dbParamName);
//            if (value != null && value.doubleValue() != -9999.0) {
//                if (value.doubleValue() >= lowerLimit && value.doubleValue() <= upperLimit) {
//                    double displayValue = 0.0;
//                    if (element.getUnit() != null) {
//                        if (element.converter == null) {
//                            try {
//                                Unit<?> unit = UnitFormat.getUCUMInstance().parseSingleUnit(
//                                        element.getUnit(), new ParsePosition(0));
//                                element.converter = ob.getUnit(element.dbParamName).getConverterTo(unit);
//                            } catch (ParseException e) {
//                                throw new VizException("Unable parse units ", e);
//                            }  
//                        }                     
//                        displayValue = element.converter.convert(value
//                                    .doubleValue());
//                    } else {
//                        displayValue = value.doubleValue();
//                    }
//                    if (element.getPlotFormat() != null) {
//                        StringBuilder sb = new StringBuilder();
//                        Formatter testing = new Formatter(sb);
//                        testing.format(element.getPlotFormat(), displayValue);
//                        sValue = sb.toString();
//                    } else {
//                        sValue = Double.toString(displayValue);
//                    }
//                } else {
//                    isValidSample = false;
//                }
//            } else {
//                element.domNode.setNodeValue("");
//            }
//            break;
//        case STRING:
//            sValue = ob.getString(element.dbParamName);
//            break;
//        default:
//            element.domNode.setNodeValue(" ");
//        }
//        if (sValue != null && element.lookup != null) {
//            sValue = element.lookup.determineRange(sValue);
//        }
//        if (sValue != null) {
//            element.domNode.setNodeValue(sValue.substring(element.getPlotTrim()));
//        } else if (plotMissingData) {
//            element.domNode.setNodeValue("m");
//        } else {
//            element.domNode.setNodeValue(" ");
//        }
//    }

    /**
     * Returns the corresponding windbarb character to the actual speed
     * 
     * @param windSpeed
     * @return The character that corresponds to the nearest 5 knot speed
     */
    private int windNormalizer(double dWindSpeed) {
        int windSpeed = (int) dWindSpeed;
        int major = windSpeed / 5;
        int minor = windSpeed % 5;
        if (minor >= 3) {
            major++;
        }
        return major * 5;
    }

    public synchronized List<PlotElement> getPlotElements() {
        return this.plotElements;
    }
    
//    private String[] numberToStringArray(Number[] values) {
//        ArrayList<String> list = new ArrayList<String>();
//        //String[] retVal = new String[values.length];
//        for(int i = 0; i < values.length; i++) {
//            if (!String.valueOf(values[i]).equals("")){
//                list.add(String.valueOf(values[i]));
//            }
//        }
//        
//        if (list.size() > 0) {
//            return list.toArray(new String[list.size()]);
//        } else {
//            return null;
//        }
//    }
//    
//    private Coordinate getCoordinate(PointDataView pdv) {
//        Coordinate latlon = new Coordinate();
//        try {      
//            Number lat = pdv.getNumber("latitude");
//            Number lon = pdv.getNumber("longitude");
//            if(lat != null && lon != null && lon.doubleValue() != -9999.0 
//                    && lat.doubleValue() != -9999.0) {
//                latlon.x = lon.doubleValue();
//                latlon.y = lat.doubleValue();
//            }
//        } catch (Exception e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//        return latlon;
//    }
    
//    public void setLowerLimit(double lowerLimit) {
//        this.lowerLimit = lowerLimit;
//    }
//
//    public void setUpperLimit(double upperLimit) {
//        this.upperLimit = upperLimit;
//    } 
    // TODO : remove this when Vertical Sounding is implemented. 
    // Use hasValidValue on AbstractParameter in stead.
    private boolean isValidValue(Number value) {
    	return value != null && value.doubleValue() > -9999.0;
//        return value != null && value.doubleValue() > lowerLimit
//                && value.doubleValue() <= upperLimit;
    }
    
//    private void createMessage(int id){
//        if (isValidSample) {
//            rawMessageMap.put(id, sampleMessage.toString());
//        } else {
//            rawMessageMap.put(id, "No Data");
//            isValidSample = true;
//        }
//        sampleMessage.setLength(0);
//    }
    
    public void setPlotMissingData(boolean b) {
        this.plotMissingData = b;
    }
    
    
    /*
     * ??? Migrated this but it can't be getting called...?
     * Set element attributes
     */
//	private Element setParamAttributes( PlotModelElement parm, Element element) {
//
//		// Set size, color, font, style, symbolSize ...
//		String style = element.getAttribute("style");
//		String color = "RGB("+parm.getColor().getRed()+"," +
//		  				parm.getColor().getGreen()+"," +
//		  				parm.getColor().getBlue()+")";
//		String fontFamily = null;
//		if (element.getAttribute(CLASS_ATTRIBUTE).equals("barb") || 
//				element.getAttribute(CLASS_ATTRIBUTE).equals("arrow"))
//			fontFamily = "WindSymbolFont";
//		else if (element.getAttribute(CLASS_ATTRIBUTE).equals("weather"))
//			fontFamily = "WxSymbolFont";
//		else if (element.getAttribute(CLASS_ATTRIBUTE).equals("special"))
//			fontFamily = "SpecialSymbolFont";
//		
//		if (element.getAttribute(DM_ATTRIBUTE).equals("text")) {
//             element.setAttribute(CLASS_ATTRIBUTE, "text");
//
//             style = style + "stroke: "+color+";fill:"+color
//             			+";font-size: "+parm.getTextSize()
//             			+";font-family: "+parm.getTextFont()
//             			+";font-style:"+parm.getTextStyle()+";";
//		} else {
//			style = style + "stroke: "+color
//				+";stroke-width:"+parm.getSymbolSize()+"px"
//				+";font-size:"+parm.getSymbolSize()+"em"
//				+";font-family:"+fontFamily+";";
//		}
//		
//		element.setAttribute("style", style);
//		
//		return element;
//	}
    
    private String getConditionalParameterValue(PlotParameterDefn plotPrmDefn, AbstractMetParameter metParam) throws VizException {
    	
    	String formattedPlotString = null;
		
		if (metParam == null) {
			return null;
		}
		
		try {
		if (plotPrmDefn.getPlotUnit() != null) {
			Unit<?>  pltParamUnit = new UnitAdapter().unmarshal(plotPrmDefn.getPlotUnit().toString().trim());
			
			// if the units are not same, convert value to desired unit 
    		if( pltParamUnit != metParam.getUnit()) {			    			
    			try {
    				metParam.setValue(metParam.getValueAs( pltParamUnit), pltParamUnit );
    			} catch (Exception e) {
    				metParam.setValueToMissing();
    			}
    		}
    		formattedPlotString = metParam.getFormattedString(plotPrmDefn.getPlotFormat() );    		
    		
		}	
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}   
		return formattedPlotString;		
    }

}
