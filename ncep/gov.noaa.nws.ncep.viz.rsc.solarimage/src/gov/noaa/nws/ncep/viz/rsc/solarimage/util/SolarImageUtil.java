package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Calendar;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.HDF5Util;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.Fits;
import nom.tam.fits.FitsException;
/**
 * Provides utility methods to get data from hdf5 and convert it to Fits format.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- --------------- --------------------------
 * 01/08/2013   958        sgurung, qzhou  Initial creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class SolarImageUtil {

	public static  void populateRawData(SolarImageRecord record) throws FileNotFoundException,
    StorageException {
		//System.out.println("Retrieving SolarImage data from HDF5...");

		File loc = HDF5Util.findHDF5Location(record);
		
		//Long t0 = System.currentTimeMillis();
		IDataStore dataStore = DataStoreFactory.getDataStore(loc);
		
		//System.out.println("--- solarimage: time to get datastore: " + (System.currentTimeMillis() - t0));
		//t0 = System.currentTimeMillis();
		record.retrieveFromDataStore(dataStore);
		//System.out.println("--- solarimage: time to retrieve from datastore: " + (System.currentTimeMillis() - t0));
	}	
	
	public static Fits convertToFits(SolarImageRecord record) throws Exception {
		
		populateRawData(record);
		if (record.getRawData() != null) {
        	try{
        		Fits fits = new Fits(new ByteArrayInputStream(record.getRawData()));    
        		
                return fits;
        	}
        	catch (Exception e) {
        		System.out.println("Error converting SolarImageRecord to Fits: " + e.getMessage());
        	}
        }
		
		return null;
            
	}
	
	public static BasicHDU getImageHDU(SolarImageRecord record) throws Exception {
		
		Fits fits = convertToFits(record);
		
		if (fits != null) {		
			try {
				return fits.getHDU(record.getImageHDUNum());
				
			} catch (FitsException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
				
		}
		
		return null;
	}
}
