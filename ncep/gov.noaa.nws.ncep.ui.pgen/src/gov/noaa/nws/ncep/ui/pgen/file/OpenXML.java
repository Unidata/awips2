/*
 * gov.noaa.nws.ncep.ui.pgen.file.OpenXML
 * 
 * Date created: 17 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.file;

import java.util.*;

/**
 * Test the file tool for reading/writing PGEN products using JAXB.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/17/09					J. Wu   	Initial Creation.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */
public class OpenXML {
    
    public static void main(String args[]) throws Exception {
    
        String inputFile = "/export/cdbsrv/jwu/workbak/PGEN/T63/xml2java/file1/myproduct.xml";
        String outputFile = "/export/cdbsrv/jwu/workbak/PGEN/T63/xml2java/file1/out_product.xml";
    	
    	Products products = FileTools.read( inputFile );
    	FileTools.write( outputFile, products);
    	
    	List<Product> listOfProducts = products.getProduct();
        List<Layer> listOfLayers;
        List<Color> listOfColors;
        DrawableElement DE;
        List<Line> listOfLines;
        List<Symbol> listOfSymbols;
        List<Point> listOfPoints;
        
	    int nn = 0, mm = 0, kk = 0;
	    for ( Product product : listOfProducts ){
            
	        System.out.println( "Product: " + (nn++) );
	        System.out.println("Name = " + product.getName() );
	        System.out.println("Forecaster = " + product.getForecaster() );
	    
	    
	        mm = 0;
            listOfLayers = product.getLayer();
            for ( Layer layer : listOfLayers ){
	    
                System.out.println("\tLayer:" + (mm++) );
                System.out.println("\t\tName:" + layer.getName() );
                
		        Color color1 = layer.getColor();
		        System.out.println("\t\tColor:");
		        System.out.println("\t\t\tRed = " + color1.getRed() );
		        System.out.println("\t\t\tGreen = " + color1.getGreen() );
		        System.out.println("\t\t\tBlue = " + color1.getBlue() );
		        System.out.println("\t\t\tAlpha = " + color1.getAlpha() );		
		
		        DE = layer.getDrawableElement();
		        kk = 0;
		        listOfLines = DE.getLine();
		        for ( Line line : listOfLines ) { 
		            System.out.println("\t\t\t\tline " + (kk++) + "Category:" + line.getPgenCategory() );		
		            System.out.println("\t\t\t\tlineWidth:" + line.getLineWidth() );		
		            System.out.println("\t\t\t\tsizeScale:" + line.getSizeScale() );		
		            System.out.println("\t\t\t\tfilled:" + line.isFilled() );		
		            System.out.println("\t\t\t\tclosed:" + line.isClosed() );		
		            System.out.println("\t\t\t\tsmoothFactor:" + line.getSmoothFactor() );		
		            System.out.println("\t\t\t\tfillPattern:" + line.getFillPattern() );		
		            System.out.println("\t\t\t\tlinePattern:" + line.getPgenType() );		
		        
		            listOfColors = line.getColor();
		            System.out.println("\t\t\t\tColor:");
		            for ( Color color : listOfColors ){
		                System.out.println("\t\t\t\tRed = " + color.getRed() );
		                System.out.println("\t\t\t\tGreen = " + color.getGreen() );
		                System.out.println("\t\t\t\tBlue = " + color.getBlue() );
		                System.out.println("\t\t\t\tAlpha = " + color.getAlpha() );		
		            }
		                
		            listOfPoints = line.getPoint();
		            System.out.println("\t\t\t\tPoints:");
		            for ( Point point : listOfPoints ){
			            System.out.println("\t\t\t\tLat = " + point.getLat() );
			            System.out.println("\t\t\t\tLon = " + point.getLon() ); 
			        }
		        }
		     
		        kk = 0;
		        listOfSymbols = DE.getSymbol();
	            for ( Symbol symbol : listOfSymbols ) {
		            System.out.println("\t\t\t\tSymbol " + (kk++) + "Type:" + symbol.getPgenType() );		
		            System.out.println("\t\t\t\tlineWidth:" + symbol.getLineWidth() );		
		            System.out.println("\t\t\t\tsizeScale:" + symbol.getSizeScale() );		
		            System.out.println("\t\t\t\tcleared:" + symbol.isClear() );		
			
		            listOfColors = symbol.getColor();
		            System.out.println("\t\t\t\tColor:");
		            for ( Color color : listOfColors ){
		                System.out.println("\t\t\t\tRed = " + color.getRed() ); 
		                System.out.println("\t\t\t\tGreen = " + color.getGreen() );
		                System.out.println("\t\t\t\tBlue = " + color.getBlue() );
		                System.out.println("\t\t\t\tAlpha = " + color.getAlpha() );		
		            }
		    
		            System.out.println("\t\t\t\tLocation:");
		            System.out.println("\t\t\t\tLat = " + symbol.getPoint().getLat() );
		            System.out.println("\t\t\t\tLon = " + symbol.getPoint().getLon() ); 
		        }
	        }
        }
    }    
}
