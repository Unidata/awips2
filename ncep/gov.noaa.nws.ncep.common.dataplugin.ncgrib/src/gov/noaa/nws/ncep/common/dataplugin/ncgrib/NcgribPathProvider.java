/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

/**
 * Path provider for storing grib data to HDF5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/24/09       1994        bphillip   Initial Creation
 * 1/12/12                   xguo       Create new HDF5 file name
 *                                      yyyy-MM-dd-HH-fhrs-ensNumber.h5
 * 3/2012					 T. Lee		changed pN to String
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgribPathProvider extends DefaultPathProvider {

    private static final SimpleDateFormat gribHdf5FileNameFormat = new SimpleDateFormat(
            "yyyy-MM-dd-HH");
    static {
        gribHdf5FileNameFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private static NcgribPathProvider instance = new NcgribPathProvider();

    public static NcgribPathProvider getInstance() {
        return instance;
    }

    protected NcgribPathProvider() {

    }

    @Override
    public String getHDFPath(String pluginName, IPersistable persistable) {
        if (persistable != null) {
            NcgribRecord record = (NcgribRecord) persistable;
            return record.getModelInfo().getModelName();
        } else {
            throw new IllegalArgumentException(
                    "Persistable object is null. Unable to generate HDF5 path");
        }
    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        Integer partition = persistable.getHdfFileId();

        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof NcgribRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + NcgribRecord.class + " but got "
                            + persistable.getClass());
        }

        if (partition == null) {
            throw new IllegalArgumentException(
                    "Expected argument hdfFileId not set on object "
                            + persistable.toString());
        } else if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        NcgribRecord pdo = (NcgribRecord) persistable;
        StringBuffer sb = new StringBuffer();

        Date refTime = pdo.getDataTime().getRefTime();
        int fhrs = pdo.getDataTime().getFcstTime()/3600;
        String refTimeString = null;
        synchronized (gribHdf5FileNameFormat) {
            refTimeString = gribHdf5FileNameFormat.format(refTime);
        }
        String pbNum = "0";
        if ( pdo.getModelInfo().getPerturbationNumber() != null){
        	pbNum = pdo.getModelInfo().getPerturbationNumber();
        }
        sb.append(refTimeString);
        sb.append(String.format("-%d", fhrs));
        sb.append(String.format("-%s", pbNum));
        sb.append(".h5");
        return sb.toString();
    }

    public String formatTime(Date date) {
        String retVal = null;
        synchronized (gribHdf5FileNameFormat) {
            retVal = gribHdf5FileNameFormat.format(date);
        }
        return retVal;
    }
}
