package gov.noaa.nws.ncep.edex.util.grib2vars;

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
 * This class reads a g2Vars table from an xml file and contains a list of g2Varss.
 * This class also provide general g2Vars search functions given g2Vars field, and
 * field value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10  		276   	   	L. Lin	   Initial Creation
 * 07/11                    X. Guo     Added getGrib2Vars4Grib1()
 *                       
 * </pre>
 * 
 * @author llin
 * @version 1
 */

public class Grib2VarsTable implements IGrib2VarsField {
	
	private final String PACKAGE = "gov.noaa.nws.ncep.edex.util.grib2vars";
	
	private List<Grib2Vars> g2VarsList;
	
	private static Grib2VarsField last = null;
	
	/**
	 * Constructor. 
	 * @param tableFileName - full path of the xml table file
	 */
    public Grib2VarsTable( String tableFileName ) {
    	
    	try{
    		g2VarsList = readGrib2VarsTable( tableFileName );
    	}
    	catch ( JAXBException exp ){
    		g2VarsList = null;
    		exp.printStackTrace();
    	}
    	
	}

    /**
     * Reads the contents of the input g2Vars table file
     * @param xmlFilename - full path of the xml table name
     * @return - a list of g2Varss
     * @throws JAXBException
     */
	private List<Grib2Vars> readGrib2VarsTable( String xmlFilename ) throws JAXBException{

	    File xmlFile = new File(xmlFilename);
	    
        JAXBContext context = JAXBContext.newInstance(
           PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        Grib2VarsList g2Varslist = null;
        
		try {
			g2Varslist = (Grib2VarsList)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<Grib2Vars> listOfItems = g2Varslist.getGrib2Vars();			
			return  listOfItems;
		    
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			
		} catch (NullPointerException e2) {
			e2.printStackTrace();		
		}	
		
		return null;
              
    }   
	
	/**
	 * Gets the list of the g2Varss
	 * @return - the list of g2Varss
	 */
	public List<Grib2Vars> getGrib2VarsList(){
		
		return g2VarsList;
		
	}
	
	/**
	 *  Search a g2Vars given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vars
	 */
	public Grib2Vars getGrib2Vars(int discipline, int category, int pid, int pdt) {
		
		if (g2VarsList == null || g2VarsList.isEmpty()) return null;
		
		for (Grib2Vars g2Vars : g2VarsList) {
			if (g2Vars.discipline == discipline && 
				g2Vars.category == category &&
				g2Vars.pid == pid &&
				g2Vars.pdt == pdt ) {
				return g2Vars;
			}		
		}
		return null;
	}

	public Grib2Vars getGrib2Vars4Grib1(int discipline, int category, int pid) {
		
		if (g2VarsList == null || g2VarsList.isEmpty()) return null;
		
		for (Grib2Vars g2Vars : g2VarsList) {
			if (g2Vars.discipline == discipline && 
				g2Vars.category == category &&
				g2Vars.pid == pid  ) {
				return g2Vars;
			}		
		}
		return null;
	}
	/**
	 *  Search g2Vars list given a field, and search key value.
	 *  
	 * @param sf
	 * @param key
	 * @return Grib2Vars
	 */
	public List<Grib2Vars> getGrib2Varss(Grib2VarsField sf, Integer key) {
		if (g2VarsList == null || g2VarsList.isEmpty()) return null;
		
		Grib2VarsComparator comparator = new Grib2VarsComparator(sf);
		if (last == null || (last != null && last != sf )) {
			Collections.sort(g2VarsList, comparator);
			last = sf;
		}
		
		List<Grib2Vars> list = new ArrayList<Grib2Vars>();
		
		Grib2Vars s = getComparedGrib2Vars(sf, key);
		int index;
		while ((index = Collections.binarySearch(g2VarsList, s, comparator)) >= 0) {
			list.add(g2VarsList.get(index));
			g2VarsList.remove(index);
		}
		
		if (list.size() > 0) {
			for (Grib2Vars st : list) {
				g2VarsList.add(st);
			}

			last = null;
			return list;
		}
		else {
			return null;
		}
	}
	
	
	private Grib2Vars getComparedGrib2Vars(Grib2VarsField sf, Integer key){
		Grib2Vars g2Vars = new Grib2Vars();
		switch (sf) {
		case G2VARSID:
			g2Vars.setG2Varsid((Integer)key);
			break;
		}
		
		return g2Vars;
	}
	
}