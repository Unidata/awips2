import gov.noaa.nws.sr.oun.edex.plugin.mping.MPingDecoder;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;


public class MpingDecoderTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			File file = new File("/home/awips/mpingtestfiles/mPingtesting.xml");
			FileInputStream fis = new FileInputStream(file);
			byte[] array = new byte[(int)file.length()];
			fis.read(array);
			fis.close();
			String input = new String(array);
			
			MPingDecoder decoder = new MPingDecoder();
			PluginDataObject[] obj = decoder.decode(input);
			
			System.out.println(obj[0].getDataTime());
		} catch(FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

}
