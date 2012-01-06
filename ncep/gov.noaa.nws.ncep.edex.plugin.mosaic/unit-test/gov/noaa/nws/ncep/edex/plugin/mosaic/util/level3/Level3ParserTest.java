/**
 * This Java class is the JUnit test for the MosaicParser.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import static org.junit.Assert.*;
import java.util.Calendar;
import org.junit.Test;
import org.junit.Before;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.*;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class Level3ParserTest {
	File afile = new File ("unit-test/gov/noaa/nws/ncep/edex/plugin/mosaic/util/level3/CREF_4.00_20100316_0900");
		
	@Before
	public void initialize () {
	}


	@Test
	public void testLevel3Parser() throws IOException {

		BufferedInputStream bufferedInput = null;
        byte[] buffer = new byte[1024];
        try {
            
            //Construct the BufferedInputStream object
            bufferedInput = new BufferedInputStream(new FileInputStream(afile));
            
            int bytesRead = 0;
            
            //Keep reading from the file while there is any content
            //when the end of the stream has been reached, -1 is returned
            bytesRead = bufferedInput.read(buffer); 
            
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            //Close the BufferedInputStream
            try {
                if (bufferedInput != null)
                    bufferedInput.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        ByteArrayInputStream theRawMosaicData = new ByteArrayInputStream(buffer);
        DataInputStream theMosaicData = new DataInputStream(theRawMosaicData);
        int theMessageCode = theMosaicData.readUnsignedShort();

        System.out.println(" theMessageCode=" + theMessageCode);

        assertEquals(30,theMessageCode);
        
        Calendar theMsgTimestamp = Calendar.getInstance();
        theMsgTimestamp.setTimeInMillis(this.createTimestamp(theMosaicData
                .readUnsignedShort(), theMosaicData.readInt()));
        
        int theMsgLength = theMosaicData.readInt();
        System.out.println(" theMsgLength=" + theMsgLength);
        assertEquals(145930,theMsgLength);

        // TODO: validate message length here and not everywhere else

        int theSourceId = theMosaicData.readShort();
        System.out.println(" theSourceId=" + theSourceId);
        assertEquals(10000,theSourceId);

        int theDestinationId = theMosaicData.readShort();
        System.out.println(" theDestinationId=" + theDestinationId);
        assertEquals(0,theDestinationId);

        int numberOfBlocks = theMosaicData.readShort();
        System.out.println(" the number of blocks=" + numberOfBlocks);
	}


	private long createTimestamp(int readUnsignedShort, int readInt) {
		// TODO Auto-generated method stub
		return 0;
	}

}

