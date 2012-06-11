/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaGenerate
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.nvl;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

//import org.apache.log4j.Logger;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.vividsolutions.jts.geom.Geometry;

/**
 * GFA Generate functionality.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 02/11					J. Wu		Save result to Pgen working dir.
 * 04/11					J. Wu		update state list order and create additional
 * 										smear for smears with two FA areas.
 * 02/12		#672		J. Wu		Re-order states list based on FA area.
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaGenerate {

//	private final static Logger logger = Logger.getLogger(GfaGenerate.class);
	private Transformer transformer;
	private String XSLT_FILE = 	PgenStaticDataProvider.getProvider().getFileAbsolutePath(
			   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot()+ "xslt"+File.separator+
			   	"airmet"+File.separator+"gfa_product.xsl");

	/**
	 * Generates product text out of the list.
	 * 
	 * @param all
	 * @param categories
	 * @param areas
	 * @throws IOException
	 * @throws JAXBException
	 */
	public StringBuilder generate( List<Gfa> all, List<String> areas, List<String> categories )
			throws IOException, JAXBException {
		
		StringBuilder sb = new StringBuilder();
		StringBuilder temp = new StringBuilder();
		String cycle = PgenCycleTool.pad( PgenCycleTool.getCycleHour() );
		
		List<Gfa> adjusted = new ArrayList<Gfa>();
		
		/*
		 *  Adjust the state list for smears with two FA areas:
		 *  
		 *  1. the state list in the primary area should be reordered (states 
		 *     in the primary area precede states in the adjacent area).
		 *  2. an additional smear should be created - switching the order of
		 *     primary area and the adjacent area, as well as the order of the
		 *     state list.
		 * 
		 */		
		for ( Gfa g : all ) {		    	

			Gfa sg = createAdjacentGfa( g );
				
		    adjusted.add( g );
				
			if ( sg != null ) {
			    adjusted.add( sg );
			}
			
		}
		
		/*
		 * Need to reorder the state list in FA area.
		 */
		for ( Gfa gg : adjusted ) {
			String area = gg.getGfaArea();

			ArrayList<String> oldStates = new ArrayList<String>();
			ArrayList<String> oldStatesNoWater = new ArrayList<String>();
			for ( String ss : gg.getGfaStates().split(" ") ) {
				oldStates.add( ss );
				if ( ss.length() == 2 )oldStatesNoWater.add( ss );
			}
			
			String[] s = area.split( "-" );
			
			ArrayList<String> statesInArea1 = GfaInfo.getStateOrderByArea().get( s[0] );
			ArrayList<String> statesInArea2 = null;
			if ( s.length > 1 ) {
				statesInArea2 = GfaInfo.getStateOrderByArea().get( s[1] );			    
			}
			
			//Sort states in primary FA area
			StringBuilder newStates = new StringBuilder();
			for ( String st : statesInArea1 ) {
				if ( st.length() == 2 && oldStatesNoWater.contains( st ) ) {
					newStates.append( st );
					newStates.append( " " );
				}
			}
			
			//Sort and add states in second FA area, if any.
			if ( statesInArea2 != null ) {
				for ( String st : statesInArea2 ) {
					if ( st.length() == 2 && oldStatesNoWater.contains( st ) ) {
						newStates.append( st );
						newStates.append( " " );
					}
				}
			}
			
			//Add back "CSTL WTRS" or "AND CSTL WTRS".
			for ( String st : oldStates ) {
				if ( st.length() > 2 ) {
					newStates.append( st );
					newStates.append( " " );
				}
			}
						
			gg.setGfaStates( newStates.toString().trim() );
						
		}
		
		/*
		 *  Find GFA smears in each area and hazard category, generate
		 *  xml, and save to a file.
		 */		
		for ( String category : categories ) {
			for ( String area : areas ) {
				List<AbstractDrawableComponent> ret = filterSelected( adjusted, area, category );

				String fileName = "AIRMET_" + category + "_" + area + "_" + cycle + ".txt";
				fileName = PgenUtil.getWorkingDirectory() + File.separator + fileName;
                
				Product p = new Product();
				Layer l = new Layer();
				p.addLayer( l );
				l.add( ret );
				List<Product> pList = new ArrayList<Product>();
				pList.add( p );

				Products products = ProductConverter.convert( pList );

				String xml = SerializationUtil.marshalToXml( products );

				if( sb.length() > 0 && !sb.toString().endsWith("\n\n") )  sb.append( "\n\n" );
				temp.append( generateProduct( xml, category, area ).trim() );
				
				saveToFile( temp, fileName );

				sb.append( temp );
				temp.setLength( 0 ); // clear
			}
		}
		return sb;
	}

	public void saveToFile(StringBuilder sb, String fileName) throws IOException {

		File file = new File( fileName );
		BufferedWriter output = null;
		try {
			output = new BufferedWriter( new FileWriter( file ) );
			output.write( sb.toString() );
		} finally {
			try {
				if (output != null)
					output.close();
			} catch ( IOException e ) {
//				logger.error( e );
				e.printStackTrace();
			}
		}
	}

	/**
	 * Filters the list to return only gfa elements selected on Gfa Format window (per category).
	 * 
	 * @param all
	 * @param checked
	 * @param cats
	 * @return
	 */
	private static List<AbstractDrawableComponent> filterSelected( List<Gfa> all, String area,
			String category ) {

		ArrayList<AbstractDrawableComponent> ret = new ArrayList<AbstractDrawableComponent>();
		
		int ii = 0;
		for ( Gfa g : all ) {

			GfaInfo.HazardCategory c = GfaInfo.getHazardCategory( g.getGfaHazard() );			
			boolean inCategory = category.equals( c.toString() );
			
			String[] s = nvl( g.getGfaArea() ).split( "-" );
			boolean inArea = nvl( s[ 0 ] ).contains( area );		    	

			if ( inArea && inCategory ) {								
				ret.add( g );
			}
			
			ii++;
		}
		
		return ret;
	}

	public String generateProduct( String xml, String category, String area ) {
		String res = "";
		try {
			
			StreamSource xmlSource = new StreamSource( new StringReader( xml ) );
			if ( transformer == null ) {
				TransformerFactory tFactory = TransformerFactory.newInstance();
				Source xsltSource = new StreamSource( XSLT_FILE );
				transformer = tFactory.newTransformer( xsltSource );
			}
			
			StreamResult result = new StreamResult( new StringWriter() );
			transformer.reset();
			transformer.setParameter( "categories", category );
			transformer.setParameter( "areas", area );
			transformer.transform( xmlSource, result );
			
			res = result.getWriter().toString().trim();
			
		} catch ( Exception e ) {
//			logger.error( "", e );
			e.printStackTrace();
		}
		
//		logger.debug( res );
		
		if ( res.contains("FRZLVL...")){
			res = wrapFrzl(res);
		}
		
		return res;
	}
	
	/**
	 *  Adjust the state list for smears with two FA areas:
	 *  
	 *  1. the state list in the primary area should be reordered (states 
	 *     in the primary area precede states in the adjacent area).
	 *  2. an additional smear should be created - switching the order of
	 *     primary area and the adjacent area, as well as the order of the
	 *     state list.
	 * 
	 * @param g		Gfa to be processed
	 * @return
	 */
	private static Gfa createAdjacentGfa( Gfa g ) {
		
		Gfa secondg = null;
		
		String[] s = nvl( g.getGfaArea()).split( "-" );
		
		if ( s.length > 1 ) {
			
			StringBuilder sname = new StringBuilder();
			StringBuilder firstList = new StringBuilder();
			StringBuilder secondList = new StringBuilder();
			StringBuilder tailList = new StringBuilder();

			String parea = s[0].trim();
		    
			sname.append( s[ 1 ].trim() );
		    sname.append( "-" );
		    sname.append( parea );
		    			
			Geometry primaryAreaBnd = GfaClip.getInstance().getFaAreaBounds().get( parea );
			if ( primaryAreaBnd != null ) {
			    for ( String oneState : g.getGfaStates().split( " " ) ) {
			        Geometry stBnd = GfaClip.getInstance().getStateBounds().get( oneState );
			    	if ( stBnd != null ) {
			    		if ( stBnd.intersects( primaryAreaBnd ) ) {
			        	    firstList.append( oneState );
			        	    firstList.append( " " );
			            }
			            else {
			    	        secondList.append( oneState );
			    	        secondList.append( " " );
			            }			    		
			        }
			    	else {
		    	        tailList.append( oneState );
		    	        tailList.append( " " );			    		
			    	}
			    }
			    
			    //update the order of the state list
			    StringBuilder stateList1 = new StringBuilder();
			    stateList1.append( firstList.toString().trim() );
			    stateList1.append( " " );
			    stateList1.append( secondList.toString().trim() );
			    stateList1.append( " " );
			    stateList1.append( tailList.toString().trim() );
			    
			    g.setGfaStates( stateList1.toString().trim() );

			    //create an additional smear				
                secondg = g.copy();
			    secondg.setGfaArea( sname.toString() );
			    
			    StringBuilder stateList2 = new StringBuilder();
			    stateList2.append( secondList.toString().trim() );
			    stateList2.append( " " );
			    stateList2.append( firstList.toString().trim() );
			    stateList2.append( " " );
			    stateList2.append( tailList.toString().trim() );
			    
			    secondg.setGfaStates( stateList2.toString().trim() );
			    
			}
			
		}

		return secondg;
		
	}
	
	/**
	 * Wraps the text string for FRZL.
	 * @param frzl
	 * @return
	 */
	private String wrapFrzl( String frzl ){
		StringBuffer sb = new StringBuffer( frzl );
		
		int startIdx = sb.indexOf("\n", sb.indexOf( "FRZLVL..." )) + 1;
		int endIdx = sb.indexOf("....", startIdx);
		
		int ii = startIdx;
		int ii64 = 0;
		
		do {
			ii64 = ii + 64;
			int newline = sb.indexOf("\n", ii );
			
			if ( newline <= ii64 ) {  // new line less than 65 char, no wrap
				ii = newline + 1;
			}
			else {					//need wrap
				for ( int jj = ii64; jj >= ii; jj--){  // search ' ' or '-' between ii and ii64

					if ( sb.charAt( jj ) == ' ' || sb.charAt( jj ) == '-'){
						sb.insert( jj + 1, "\n      ");
						ii = jj + 2;
						break;
					}
				}
			}

		}while ( ii64 < endIdx );
		
		return new String(sb);
	}
}
