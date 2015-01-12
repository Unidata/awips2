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
package com.raytheon.edex.plugin.goessounding.dao;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.goessounding.GOESSounding;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Provide data access services against the SoundingSite data object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080414           1077 jkorman     Initial implementation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class GOESSoundingDAO extends PointDataPluginDao<GOESSounding> {

    /**
     * Creates a new GOESSoundingDAO object.
     * 
     * @throws PluginException
     */
    public GOESSoundingDAO(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an GOESSounding report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public GOESSounding queryByDataURI(String dataURI) {
        GOESSounding report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (GOESSounding) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the GOESSounding
     * table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.goessounding where datauri='"
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
                    "select distinct wmoheader from awips.goessounding where wmoheader='");
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
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(GOESSounding p) {
        return "goessounding.h5";
    }

    @Override
    public GOESSounding newObject() {
        return new GOESSounding();
    }

    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription.fromStream(this
                        .getClass().getResourceAsStream(
                                "/res/pointdata/goes.xml"));
            } catch (SerializationException e) {
                logger.error("Unable to load " + pluginName
                        + " Point Data Description", e);
            }
        }
        return hdf5DataDescription;
    }

}
