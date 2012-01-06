/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.display;


/**
 * @author sgilbert
 *
 */
public class CreatePatternFiles {

	/**
	 * Used to create an XML representation of our LinePattern and SymbolPattern
	 * objects using JAXB.
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		//createLinePatternFile("/tmp/linePatterns.xml_tmp");
		createSymbolPatternFile("/tmp/symbolPatterns.xml");
	
	}

	private static void createLinePatternFile( String fname) {
	
		LinePatternManager lpmgr = LinePatternManager.getInstance();
		System.out.println("SAG"+lpmgr.getPatternIds().length);
		
		try {
			//SerializationUtil.jaxbMarshalToXmlFile(lplist, "/tmp/sag");
			
			lpmgr.savePatternsToFile(fname);
			lpmgr.loadPatternsFromFile(fname);
			/*
			JAXBContext context = JAXBContext.newInstance(LinePatternList.class,LinePattern.class);
			Marshaller msh = context.createMarshaller();
			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

			File fileOut = new File("/tmp/sag");
			PrintWriter writer = new PrintWriter(fileOut);

			msh.marshal(lplist, writer);
			writer.close();
			*/

		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
	}

	private static void createSymbolPatternFile( String fname) {
		
		SymbolPatternManager spmgr = SymbolPatternManager.getInstance();
		System.out.println("SAG"+spmgr.getPatternIds().length);
		
		try {
			//SerializationUtil.jaxbMarshalToXmlFile(lplist, "/tmp/sag");
			
			/*
			JAXBContext context = JAXBContext.newInstance(LinePatternList.class,LinePattern.class);
			Marshaller msh = context.createMarshaller();
			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

			File fileOut = new File("/tmp/sag");
			PrintWriter writer = new PrintWriter(fileOut);

			msh.marshal(lplist, writer);
			writer.close();
			*/
			spmgr.savePatternsToFile(fname);
			spmgr.loadPatternsFromFile(fname);

		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
	}

}
