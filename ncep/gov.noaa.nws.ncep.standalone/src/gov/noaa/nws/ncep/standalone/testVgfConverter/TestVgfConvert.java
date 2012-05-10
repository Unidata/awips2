package gov.noaa.nws.ncep.standalone.testVgfConverter;

import java.io.File;

import gov.noaa.nws.ncep.standalone.vgfConverter.WrapperC.VgfXml;

import com.sun.jna.Native;
/**
 * TestVgfConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/18/2010    271         Q. Zhou     Initial created
 * 6/17/2010    271         Q. Zhou     Add Gfa 
 * 9/09/2010    271         Q. Zhou     Added Main for cml.
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class TestVgfConvert {
	/**
	 * @param args
	 */
	public int testVgf(String original, String converted)  {
		
		//call c
		VgfXml wrap = (VgfXml)Native.loadLibrary( "VgfXml",  VgfXml.class); 
		int ret = wrap.compareVgf(original, converted);
		
		return ret;
	}
	
	public static void main(String[] args)  { 
		if (!new File(args[0]).exists()) {
			System.out.println("The original file does not exist.\n");
			return;
		}
		if (!new File(args[1]).exists()) {
			System.out.println("The converted file does not exist.\n");
			return;
		}
		
		new TestVgfConvert().testVgf(args[0], args[1]);
	}
}


