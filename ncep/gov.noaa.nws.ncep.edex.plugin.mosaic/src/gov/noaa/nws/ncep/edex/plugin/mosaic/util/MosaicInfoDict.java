package gov.noaa.nws.ncep.edex.plugin.mosaic.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Read in mosaicInfo.txt as a dictionaray
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * 01/19/13     #               Greg hull   pass in File instead of path
 *
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicInfoDict implements Iterable<MosaicInfo>  {

    private static MosaicInfoDict instance = null;

    private Map<Integer, MosaicInfo> dict;

    private MosaicInfoDict(File midf) {

        dict = new LinkedHashMap<Integer, MosaicInfo>();
        
        try {
        	//read the mosaicInfo.txt to pointer in
            BufferedReader in = new BufferedReader(new FileReader(midf));
            
            String s = in.readLine();
            
            //read in line by line and put into mosaic information dictionary
            while (s != null) {
                if ((s.length() > 0) && (s.charAt(0) != '#')) {
                    MosaicInfo r = new MosaicInfo(s);
                    if (r != null) {
                        dict.put(r.getProductCode(), r);
                    }
                }
                s = in.readLine();
            }
            in.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Returns the MosaicInfo for the specified productCode
     * 
     * @param productCode
     * @return
     */
    public MosaicInfo getInfo(int productCode) {
    	return dict.get(productCode);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<MosaicInfo> iterator() {
        return Collections.unmodifiableCollection(dict.values()).iterator();
    }

    public static synchronized MosaicInfoDict getInstance(File midf ) {
        if (instance == null) {
            instance = new MosaicInfoDict(midf);
        }
        return instance;

    }

    @Override
    public String toString() {
        String s = "";
        for (MosaicInfo r : dict.values()) {
            s += "\n" + r;
        }
        return s;
    }

    public static void main(String[] args) {
    	File f = new File( args[0]);
        MosaicInfoDict dict = MosaicInfoDict.getInstance( f );
    }
}