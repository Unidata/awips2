package com.raytheon.uf.edex.plugin.ffmp;

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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * FFMP specified data access object.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 01, 2009  2521     dhladky   Initial Creation
 * Jul 15, 2013  2184     dhladky   Remove all HUC's for storage except ALL
 * Oct 01, 2015  4756     dhladky   Custom purging for FFMP.
 * Aug 09, 2016  5728     mapeters  Improved error handling in purgeExpiredKey()
 * Jun 18, 2018  6727     njensen   Removed overridden purgeExpiredKey()
 * Jul 23, 2018  6642     randerso  Code cleanup.
 * Aug 14, 2018  6720     njensen   Use simplified enums
 * Sep 23, 2021  8608     mapeters  Add metadata id handling
 * Jun 22, 2022  8865     mapeters  Update populateDataStore to return boolean
 *
 * </pre>
 *
 * @author dhladky
 */
public class FFMPDao extends PluginDao {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDao.class);

    private FFMPTemplates template = null;

    private FFMPSourceConfigurationManager fscm = null;

    public FFMPDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public FFMPDao(String pluginName, FFMPTemplates template,
            FFMPSourceConfigurationManager fscm) throws PluginException {
        super(pluginName);
        this.template = template;
        this.fscm = fscm;
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {

        FFMPRecord record = (FFMPRecord) obj;
        boolean populated = false;
        if (fscm.getSource(record.getSourceName())
                .getSourceType() == SourceType.GAGE) {

            for (DomainXML domain : template.getDomains()) {
                String domainCwa = domain.getCwa();
                LinkedHashMap<String, FFMPVirtualGageBasinMetaData> vmap = template
                        .getVirtualGageBasins(record.getSiteKey(),
                                domain.getCwa());

                // ignore data outside of domain
                if (!vmap.isEmpty()) {

                    FFMPBasinData fbd = record.getBasinData();
                    int size = 0;

                    for (Entry<String, FFMPVirtualGageBasinMetaData> entry : vmap
                            .entrySet()) {
                        if (entry.getValue() != null) {
                            size++;
                        }
                    }

                    float[] dataRec = new float[size];
                    int i = 0;

                    for (Entry<String, FFMPVirtualGageBasinMetaData> entry : vmap
                            .entrySet()) {
                        if (entry.getValue() != null) {
                            FFMPVirtualGageBasin bd = (FFMPVirtualGageBasin) fbd
                                    .get(entry.getValue().getLookupId());
                            dataRec[i] = bd.getValue();
                            i++;
                        }
                    }

                    String dataUri = record.getDataURI();
                    // NAME | GROUP | array |Dimension | dimensions
                    IDataRecord rec = new FloatDataRecord(FFMPRecord.ALL,
                            dataUri + "/" + domainCwa, dataRec, 1,
                            new long[] { size });
                    /*
                     * TODO: This really needs a new
                     * MetadataSpecificity.MULTI_GROUP option and special
                     * handling for that, but the default should work okay for
                     * now - if any group store fails, the metadata will be
                     * deleted, and if the metadata store fails, only one group
                     * will be deleted and any others will be orphaned
                     */
                    IMetadataIdentifier metaId = new DataUriMetadataIdentifier(
                            record);
                    dataStore.addDataRecord(rec, metaId);
                    populated = true;
                } else {
                    statusHandler
                            .debug("No VGB's in domain: " + domain.getCwa());

                }
            }
        }

        else {

            if (record.getBasinData() != null) {

                for (DomainXML domain : template.getDomains()) {
                    String domainCwa = domain.getCwa();
                    LinkedHashMap<Long, ?> map = template.getMap(
                            record.getSiteKey(), domainCwa, FFMPRecord.ALL);
                    FFMPBasinData fbd = record.getBasinData();
                    // ignore data outside domain
                    if (map.size() > 0 && fbd.getBasins().size() > 0) {
                        int size = map.size();

                        float[] dataRec = new float[size];
                        int i = 0;
                        /*
                         * write individual basins, use template, preserves
                         * ordering
                         */
                        for (Long pfaf : map.keySet()) {
                            FFMPBasin bd = fbd.get(pfaf);
                            if (bd != null) {
                                dataRec[i] = bd.getValue();
                                i++;
                            }
                        }
                        // NAME | GROUP | array |Dimension | dimensions
                        if (i > 0) {
                            String dataUri = record.getDataURI();
                            IDataRecord rec = new FloatDataRecord(
                                    FFMPRecord.ALL, dataUri + "/" + domainCwa,
                                    dataRec, 1, new long[] { size });
                            /*
                             * TODO: This really needs a new
                             * MetadataSpecificity.MULTI_GROUP option and
                             * special handling for that, but the default should
                             * work okay for now - if any group store fails, the
                             * metadata will be deleted, and if the metadata
                             * store fails, only one group will be deleted and
                             * any others will be orphaned
                             */
                            IMetadataIdentifier metaId = new DataUriMetadataIdentifier(
                                    record);
                            dataStore.addDataRecord(rec, metaId);
                            populated = true;
                        }
                    } else {
                        statusHandler.debug(
                                "Data outside of domain: " + domain.getCwa());
                    }
                }
            }
        }

        statusHandler.debug("writing " + record.toString());
        return populated;
    }

    @Override
    // FIXME Need to rewrite this to get the correct set of records I think,
    // won't be URI based.
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj)
                            .retrieve(obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException("Error retrieving FFMP HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }

}
