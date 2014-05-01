/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.uengine.web;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Implements a web based interface to copy over test data.
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    11/08/06                  brockwoo    Initial Creation.
 *    
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class TestDataDriver {
    
	private static Log logger = LogFactory.getLog(TestDataDriver.class);
	
    private String baseDirectory;
    
    public TestDataDriver(){
        String os = System.getProperty("os.name");
        if(os.indexOf("Linux") > -1 || os.indexOf("Mac") > -1){
            baseDirectory = "../../opt/data/sbn/";
        }
        else if(os.indexOf("Windows") > -1){
            File cFolder = new File("C:/awips");
            File dFolder = new File("D:/awips");
            if(cFolder.exists()){
                baseDirectory = "../../opt/data/sbn/";
            }
            else if(dFolder.exists()){
                baseDirectory = "../../opt/data/sbn/";
            }
        }
    }
    
    public boolean ingestTestDataFiles(String dataDirectory, String decodeDirectory, String fileName) {
        boolean copySucceeded = false;
        if(fileName == null){
            return copySucceeded;
        }
        File sourceFile = new File(baseDirectory + dataDirectory + fileName);
        File destFile = new File(baseDirectory + decodeDirectory + fileName);
        if(sourceFile.isFile()){
            try {
                copyTestFile(sourceFile, destFile);
                copySucceeded = true;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return copySucceeded;
    }

    public String[] listTestDataFiles(String dataDirectory) {
        String[] files = null;
        File dir = new File(baseDirectory + dataDirectory);
        files = dir.list();
        if(files == null){
        	files = new String[1];
        	files[0] = "No Files Available";
        	return files;
        }
        Arrays.sort(files);
        return files;
    }
    
    public String viewTestDataFiles(String dataDirectory, String fileName) {
    	FileInputStream sourceFileStream = null;
    	String testFile = "";
        String theFileDirectory = baseDirectory + dataDirectory;
        String theFileLocation = theFileDirectory + fileName;
        File sourceFile = new File(theFileLocation);
        int fileSize = (int)sourceFile.length();
        if(fileSize > 30000){
            testFile = "The test file '" + fileName + "' is greater than 30K and will not be sent.";
            return testFile;
        }
        byte[] sourceFileContents = new byte[fileSize];
        try {
            sourceFileStream = new FileInputStream(sourceFile);
            sourceFileStream.read(sourceFileContents);
        } catch (FileNotFoundException e) {
            testFile = "The file '" + fileName + "' was not found in directory '" + theFileDirectory + "'.  Please try again.";
            return testFile;
        } catch (IOException e) {
            testFile = "There was an error reading the file.  Please check the file located at\n" + theFileLocation;
            return testFile;
        }
        finally{
        	if(sourceFileStream != null)
        	{
        		try {
					sourceFileStream.close();
				} catch (IOException e) {
					logger.error("Unable to close file input stream", e);
				}
        	}
        }
        
        
        testFile = new String(sourceFileContents);
        return testFile;
    }
    
    private static void copyTestFile(File source, File dest) throws IOException {
        FileChannel in = null, out = null;
        try {          
            in = new FileInputStream(source).getChannel();
            out = new FileOutputStream(dest).getChannel();
            long size = in.size();
            MappedByteBuffer buf = in.map(FileChannel.MapMode.READ_ONLY, 0, size);
            out.write(buf);
        }
        finally {
            if (in != null){
                in.close();
            }
            if (out != null){
                out.close();
            }
        }
    }
}
