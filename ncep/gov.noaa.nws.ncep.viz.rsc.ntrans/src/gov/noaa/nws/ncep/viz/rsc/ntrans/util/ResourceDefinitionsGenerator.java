/**
 *  A standalone program to generate all the Resource Definition files
 *  for known models.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * @author bhebbard
 *
 */
public class ResourceDefinitionsGenerator {

	private final static String rootName = "/export/cdbsrv/bhebbard/";  //TODO
	
	private final static String[] models = {
			"cmc",
			"cmce",
			"cmce_avgspr",
			"cmcver",
			"cpc",
			"dgex",
			"ecens",
			"ecens_avgspr",
			"ecmwf",
			"ecmwf_hr",
			"ecmwfver",
			"ensver",
			"fnmocwave",
			"gdas",
			"gefs",
			"gefs_avgspr",
			"gfs",
			"gfsp",
			"gfsver",
			"gfsverp",
			"ghm",
			"hpcqpf",
			"hpcver",
			"hwrf",
			"iceaccr",
			"jmap",
			"medrt",
			"naefs",
			"nam",
			"nam20",
			"nam44",
			"namver",
			"nogaps",
			"nww3",
			"nww3p",
			"opc_ens",
			"rap",
			"rapp",
			"srefx",
			"sst",
			"ukmet",
			"ukmetver",
			"vaftad"
	};
	/**
	 * 
	 */
	public ResourceDefinitionsGenerator() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		for (String model : models) {
			
			//  Create folder for model
			
			String folderName = rootName + "NTRANS" + "/" + model.toUpperCase() + "_NT";
			File folder = new File(folderName);
			folder.mkdirs();
			
			//  Create Resource Definition file
			
			String resourceDefinitionFileName = model.toUpperCase() + "_NT" + ".xml";
			File resourceDefinitionFile = new File(folder, resourceDefinitionFileName);
			try {
				resourceDefinitionFile.createNewFile();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			PrintWriter out = null;
			try {
				out = new PrintWriter(resourceDefinitionFile);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			out.println("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
			out.println("<ResourceDefinition xmlns:ns2=\"group\" xmlns:ns3=\"http://www.example.org/productType\">");
			out.println("    <resourceDefnName>" + model.toUpperCase() + "_NT" + "</resourceDefnName>");
			out.println("    <inventoryEnabled>false</inventoryEnabled>");
			out.println("    <resourceCategory>NTRANS</resourceCategory>");
			out.println("	<resourceParameters>");
			out.println("pluginName=ntrans");
			out.println("modelName=" + model.toLowerCase());
			out.println("	</resourceParameters>");
			out.println("    <rscImplementation>NTRANS</rscImplementation>");
			out.println("    <subTypeGenerator>metafileName,productName</subTypeGenerator>");
			out.println("    <rscTypeGenerator></rscTypeGenerator>");
			out.println("    <timeMatchMethod>CLOSEST_BEFORE_OR_AFTER</timeMatchMethod>");
			out.println("    <frameSpan>60</frameSpan>");
			out.println("    <timelineGenMethod>USE_CYCLE_TIME_FCST_HOURS</timelineGenMethod>");
			out.println("    <dfltFrameCount>10</dfltFrameCount>");
			out.println("    <dfltTimeRange>48</dfltTimeRange>");
			out.println("    <dfltGeogArea>XY</dfltGeogArea>");
			out.println("</ResourceDefinition>");

			out.flush();
			out.close();
			
			//  Create default attributes file
			
			String defaultAttributesFileName = "default.attr";
			File defaultAttributesFile = new File(folder, defaultAttributesFileName);
			try {
				defaultAttributesFile.createNewFile();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			try {
				out = new PrintWriter(defaultAttributesFile);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			out.println("! No real attributes for NTRANS");
			out.println("color= RGB {255,255,255}");

			out.flush();
			out.close();

		}
	}

}
