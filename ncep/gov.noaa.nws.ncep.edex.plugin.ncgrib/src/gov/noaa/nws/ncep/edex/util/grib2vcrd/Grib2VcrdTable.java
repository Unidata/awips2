package gov.noaa.nws.ncep.edex.util.grib2vcrd;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * This class reads a g2Vcrd table from an xml file and contains a list of g2Vcrds.
 * This class also provide general g2Vcrd search functions given g2Vcrd field, and
 * field value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10  		276   	   	L. Lin	   Initial Creation
 * 07/11                    X. Guo     Added getGrib2VcrdByVcrd1()
 *                       
 * </pre>
 * 
 * @author llin
 * @version 1
 */

public class Grib2VcrdTable implements IGrib2VcrdField {
	
	private final String PACKAGE = "gov.noaa.nws.ncep.edex.util.grib2vcrd";
	
	private List<Grib2Vcrd> g2VcrdList;
	
	private static Grib2VcrdField last = null;
	
	/**
	 * Constructor. 
	 * @param tableFileName - full path of the xml table file
	 */
    public Grib2VcrdTable( String tableFileName ) {
    	
    	try{
    		g2VcrdList = readGrib2VcrdTable( tableFileName );
    	}
    	catch ( JAXBException exp ){
    		g2VcrdList = null;
    		exp.printStackTrace();
    	}
    	
	}

    /**
     * Reads the contents of the input g2Vcrd table file
     * @param xmlFilename - full path of the xml table name
     * @return - a list of g2Vcrds
     * @throws JAXBException
     */
	private List<Grib2Vcrd> readGrib2VcrdTable( String xmlFilename ) throws JAXBException{

	    File xmlFile = new File(xmlFilename);
	    
        JAXBContext context = JAXBContext.newInstance(
           PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        Grib2VcrdList g2Vcrdlist = null;
        
		try {
			g2Vcrdlist = (Grib2VcrdList)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<Grib2Vcrd> listOfItems = g2Vcrdlist.getGrib2Vcrd();			
			return  listOfItems;
		    
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			
		} catch (NullPointerException e2) {
			e2.printStackTrace();		
		}	
		
		return null;
              
    }   
	
	/**
	 * Gets the list of the g2Vcrds
	 * @return - the list of g2Vcrds
	 */
	public List<Grib2Vcrd> getGrib2VcrdList(){
		
		return g2VcrdList;
		
	}
	
	/**
	 *  Search a g2Vcrd given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vcrd
	 */
	public Grib2Vcrd getGrib2Vcrd(int vcrdid1, int vcrdid2) {
		
		if (g2VcrdList == null || g2VcrdList.isEmpty()) return null;
		
		for (Grib2Vcrd g2Vcrd : g2VcrdList) {
			if (g2Vcrd.vcrdid1 == vcrdid1 && 
				g2Vcrd.vcrdid2 == vcrdid2 ) {
				return g2Vcrd;
			}		
		}
		return null;
	}
	
	/**
	 *  Search a g2Vcrd given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vcrd
	 */
	public Grib2Vcrd getGrib2VcrdByVcrd1 (int vcrdid1 ) {
		
		if (g2VcrdList == null || g2VcrdList.isEmpty()) return null;
		
		for (Grib2Vcrd g2Vcrd : g2VcrdList) {
			if (g2Vcrd.vcrdid1 == vcrdid1 ) {
				return g2Vcrd;
			}		
		}
		return null;
	}
	
	/**
	 *  Search g2Vcrd list given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vcrd
	 */
	public List<Grib2Vcrd> getGrib2Vcrds(Grib2VcrdField sf, Integer key) {
		if (g2VcrdList == null || g2VcrdList.isEmpty()) return null;
		
		Grib2VcrdComparator comparator = new Grib2VcrdComparator(sf);
		if (last == null || (last != null && last != sf )) {
			Collections.sort(g2VcrdList, comparator);
			last = sf;
		}
		
		List<Grib2Vcrd> list = new ArrayList<Grib2Vcrd>();
		
		Grib2Vcrd s = getComparedGrib2Vcrd(sf, key);
		int index;
		while ((index = Collections.binarySearch(g2VcrdList, s, comparator)) >= 0) {
			list.add(g2VcrdList.get(index));
			g2VcrdList.remove(index);
		}
		
		if (list.size() > 0) {
			for (Grib2Vcrd st : list) {
				g2VcrdList.add(st);
			}

			last = null;
			return list;
		}
		else {
			return null;
		}
	}
	
	
	private Grib2Vcrd getComparedGrib2Vcrd(Grib2VcrdField sf, Integer key){
		Grib2Vcrd g2Vcrd = new Grib2Vcrd();
		switch (sf) {
		case G2VCRDID:
			g2Vcrd.setG2Vcrdid((Integer)key);
			break;
		}
		
		return g2Vcrd;
	}
	
}