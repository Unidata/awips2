package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.Fits;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;

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

    public static void populateRawData(SolarImageRecord record)
            throws FileNotFoundException, StorageException {

        File loc = HDF5Util.findHDF5Location(record);

        IDataStore dataStore = DataStoreFactory.getDataStore(loc);

        record.retrieveFromDataStore(dataStore);

    }

    public static Fits convertToFits(SolarImageRecord record) throws Exception {

        populateRawData(record);
        if (record.getRawData() != null) {
            try {
                Fits fits = new Fits(new ByteArrayInputStream(
                        record.getRawData()));

                return fits;
            } catch (Exception e) {
                System.out
                        .println("Error converting SolarImageRecord to Fits: "
                                + e.getMessage());
            }
        }

        return null;

    }

    public static BasicHDU getImageHDU(SolarImageRecord record)
            throws VizException {

        try {
            Fits fits = convertToFits(record);

            if (fits != null)
                return fits.getHDU(record.getImageHDUNum());

        } catch (Exception e) {
            throw new VizException("Error reading data from HDF5: "
                    + e.getMessage());
        }

        return null;
    }
}
