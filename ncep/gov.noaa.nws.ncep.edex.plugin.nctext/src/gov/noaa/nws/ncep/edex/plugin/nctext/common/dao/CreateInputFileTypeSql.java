package gov.noaa.nws.ncep.edex.plugin.nctext.common.dao;
/*
 * Chin:
 * This program is used to create script file, "scripts/inputFileType.sql" for inserting entry to nctext_inputfile_type table.
 * Input file table and output SQL script are hard coded. Need to be changed when they are stored in different folder.
 * We only have to run this program once, as long as we have not changed inputFileTbl contents.
 * We can always edit "scripts/inputFileType.sql" directly and do not have to run this program again.
 */
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.StringTokenizer;

public class CreateInputFileTypeSql {
	private static String str;
	private static StringTokenizer st;
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
			
			 
	        try { 
	        	BufferedWriter out = new BufferedWriter(new FileWriter("/usr1/cchen/to11dr11/workspace/gov.noaa.nws.ncep.edex.plugin.nctext/res/scripts/inputFileType.sql")); 
                BufferedReader in = new BufferedReader(new FileReader("/usr1/cchen/to11dr11/workspace/gov.noaa.nws.ncep.edex.plugin.nctext/res/scripts/inputFileTbl")); 
                //String str;
                int index = 0;
                String fileType;
                String fileExtension;
                while ((str = in.readLine()) != null)
                {
               		//The following coding is based on $GEMTBL/nwx/*.bull file format and convert to
            		//XML file used by NCTEXTUI

                	
                		System.out.println (str);
                		st = new StringTokenizer(str);
                  		st.nextToken();
                  		st.nextToken();
                  		// 3rd token is file type
                  		fileType = st.nextToken();
                  		st.nextToken();
                  		//5th token is file extension
                  		fileExtension = st.nextToken();
                  		index++;
                  		out.write("INSERT INTO awips.nctext_inputfile_type VALUES ("+index+",'"+
                  				fileExtension+"','"+fileType+"');\n");                	
                		
                }
                out.close(); 
                in.close();
	        }
	       catch (Exception e) { 
	        	System.out.println("IOException");
	        }
		}

}
