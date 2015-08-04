
package gov.noaa.nws.sr.oun.edex.plugin.mping.dao;

import gov.noaa.nws.sr.oun.dataplugin.mping.MPingReport;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
/**
 * DAO object for mPING Reports
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2015            aanderson   Initial creation of history
 * </pre>
 * 
 * @author Aaron Anderson
 * @version 1.0
 */
public class MPingReportDao extends PointDataPluginDao<MPingReport> {

    
    public MPingReportDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(MPingReport p) {
        return "mping.h5";
    }

    @Override
    public MPingReport newObject() {
        return new MPingReport();
    }

}
