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
package com.raytheon.uf.edex.plugin.modelsounding.dao;

import java.io.File;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.modelsounding.SoundingSite;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.plugin.modelsounding.common.ModelSoundingPathProvider;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Provide data access services against the SoundingSite data object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Apr 26, 2013  1861     bkowal      Added report type and forecast seconds as
 *                                    required keys for the hdf5 file name. 
 *                                    Create a new method to generate hdf5 file
 *                                    names that will use the path provider.
 * Dec 02, 2013  2537     bsteffen    Remove fcstseconds from SoundingSite.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ModelSoundingDAO extends PointDataPluginDao<SoundingSite> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ModelSoundingDAO.class);

    /**
     * Creates a new BufrMOSDao object.
     * 
     * @throws PluginException
     */
    public ModelSoundingDAO(String pluginName) throws PluginException {
        super(pluginName);
        this.pathProvider = new ModelSoundingPathProvider();
    }

    /**
     * Retrieves an Model Sounding report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SoundingSite queryByDataURI(String dataURI) {
        SoundingSite report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (SoundingSite) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the profiler
     * table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.modelsounding where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    /**
     * Attempts to find if the given WMO header exists in the database.
     * 
     * @param wmoHeader
     *            The WMO Header to query for.
     * @return Is the WMO header a potential duplicate.
     */
    public boolean isDuplicate(WMOHeader wmoHeader) {
        boolean isDup = true;

        if (wmoHeader != null) {
            StringBuilder hdr = new StringBuilder(
                    "select distinct wmoheader from awips.modelsounding where wmoheader='");
            hdr.append(wmoHeader.getWmoHeader());
            hdr.append("';");

            String sql = hdr.toString();
            Object[] results = executeSQLQuery(sql);

            isDup = ((results != null) && (results.length > 0));
        }
        return isDup;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "reportType", "dataTime.refTime",
                "dataTime.fcstTime" };
    }

    @Override
    public String getPointDataFileName(SoundingSite p) {
        return "modelsounding.h5";
    }

    @Override
    public SoundingSite newObject() {
        return new SoundingSite();
    }
    
    @Override
    protected String generatePointDataFileName(SoundingSite bean) {
        return this.pluginName
        + File.separator
        + this.pathProvider.getHDFPath(this.pluginName, bean)
        + File.separator
        + this.pathProvider.getHDFFileName(this.pluginName, bean);           
    }
}
