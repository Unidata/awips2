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

package gov.noaa.nws.ncep.common.dataplugin.ntrans;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

/**
 * Path provider for storing radar data to HDF5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/19/10     7353        rjpeter     Initial Creation (as RadarPathProvider)
 * 04/23/13     838         bhebbard    Adapted to make NtransPathProvider
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */
public class NtransPathProvider extends DefaultPathProvider {

    private static NtransPathProvider instance = new NtransPathProvider();

    public static NtransPathProvider getInstance() {
        return instance;
    }

    protected NtransPathProvider() {

    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof NtransRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + NtransRecord.class + " but got "
                            + persistable.getClass());
        }

        if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        NtransRecord pdo = (NtransRecord) persistable;
        StringBuffer sb = new StringBuffer(64);
        sb.append(pluginName);
        sb.append("-");
        sb.append(pdo.getModelName());
        sb.append(fileNameFormat.get().format(pdo.getDataTime().getRefTime()));
        sb.append(".h5");

        return sb.toString();
    }
}
