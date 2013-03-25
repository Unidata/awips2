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
package com.raytheon.viz.mpe.ui.radartable;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasdyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.RadarBiasTableDialog;

/**
 * Get the Radar Bias table parameters for Bias table edit dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadBiasTableParam {

    private static AppsDefaults appsDefaults = AppsDefaults.getInstance();

    public static class Bias_Data {
        public float[] mem_span = new float[20];

        public float[] num_pairs = new float[20];

        public float[] sumgag = new float[20];

        public float[] sumrad = new float[20];

        public float[] bias = new float[20];
    }

    public static Bias_Data biasData = new Bias_Data();

    public void read_bias_table_param(String rid) {

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String where = "";
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        ArrayList<Rwbiasdyn> rwBiasDynHead = new ArrayList<Rwbiasdyn>();
        Rwbiasdyn rwBiasDynNode = new Rwbiasdyn();
        int irec = 0;
        double nnum_pairs;
        float[] memspan_values = new float[10];

        memspan_values[0] = RadarBiasTableDialog.rwBias.getMemSpan1();
        memspan_values[1] = RadarBiasTableDialog.rwBias.getMemSpan2();
        memspan_values[2] = RadarBiasTableDialog.rwBias.getMemSpan3();
        memspan_values[3] = RadarBiasTableDialog.rwBias.getMemSpan4();
        memspan_values[4] = RadarBiasTableDialog.rwBias.getMemSpan5();
        memspan_values[5] = RadarBiasTableDialog.rwBias.getMemSpan6();
        memspan_values[6] = RadarBiasTableDialog.rwBias.getMemSpan7();
        memspan_values[7] = RadarBiasTableDialog.rwBias.getMemSpan8();
        memspan_values[8] = RadarBiasTableDialog.rwBias.getMemSpan9();
        memspan_values[9] = RadarBiasTableDialog.rwBias.getMemSpan10();

        String fxa_local_site = appsDefaults.getToken("fxa_local_site");

        /* Build the where clause. */
        where = "WHERE radid='" + rid + "' and obstime='"
                + displayManager.getCurrentEditDate() + "' and office_id = '"
                + fxa_local_site + "' ORDER BY  memspan_ind ";

        /*-------------------------------------------------------*/
        /* Retrieve data from RWBiasDyn table for the given */
        /* radar id and time. */
        /*-------------------------------------------------------*/
        rwBiasDynHead = (ArrayList<Rwbiasdyn>) IHFSDbGenerated
                .GetRWBiasDyn(where);

        if (rwBiasDynHead.size() == 0) {
            // logMessage
            // (String.format(" Could not retrieve data from RWBiasDyn table for radar id %s and time %s.\n "
            // , rid , displayManager.getCurrentDate())) ;

            return;
        }

        else {

            ListIterator<Rwbiasdyn> li = rwBiasDynHead.listIterator();
            while (li.hasNext()) {
                rwBiasDynNode = li.next();
                biasData.mem_span[irec] = memspan_values[rwBiasDynNode.getId()
                        .getMemspanInd()];
                nnum_pairs = rwBiasDynNode.getNumpairs();
                biasData.num_pairs[irec] = (float) nnum_pairs;
                biasData.sumgag[irec] = rwBiasDynNode.getSumgag();
                biasData.sumrad[irec] = rwBiasDynNode.getSumrad();
                biasData.bias[irec] = rwBiasDynNode.getBias();
                irec++;
            }

        }

        /* Free the RWBiasDyn data. */
        if (rwBiasDynHead != null) {
            rwBiasDynHead.clear();
            rwBiasDynHead = null;
        }

        return;
    }

    /**
     * This Queries the Dpaadapt table.
     * 
     * @return ArrayList<Dpaadapt>
     */
    public static float[] getDpaadaptcoef(String rid, String obstime)
            throws VizException {
        float[] coefs = new float[2];
        String query = String
                .format("select mlt_zrcoef, pwr_zrcoef from dpaadapt where radid ='%s' and obstime='%s'",
                        rid, obstime);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        coefs = new float[2];
        for (int i = 0; i < rs.size(); i++) {
            Object[] oa = rs.get(i);
            coefs[0] = (Float) oa[0];
            coefs[1] = (Float) oa[1];
        }
        return coefs;
    }

    public static int get_rfc_bias_value(String rid, String office_id,
            float pBias) {
        String pFxaLocalSite = appsDefaults.getToken("fxa_local_site");
        String where = "";
        int bias_found = 0;
        int length;
        boolean status;
        String datime = RadarBiasTableDialog.dt;
        ArrayList<Rwbiasstat> pRWBiasStatList = new ArrayList<Rwbiasstat>();
        ArrayList<Rwbiasdyn> pRWBiasDynList = new ArrayList<Rwbiasdyn>();
        String pRadarLoc = "";
        Rwbiasstat pRWBiasStat = new Rwbiasstat();
        Rwbiasdyn pRWBiasDynNode = new Rwbiasdyn();

        length = pFxaLocalSite.length();

        if (length > 0) {

            /* Get the corresponding office id for the rid. */

            try {
                pRadarLoc = IHFSDbGenerated.GetRadarLocOffice(rid);
            } catch (VizException e) {
                e.printStackTrace();
            }

            if (pRadarLoc.length() != 0) {

                status = pFxaLocalSite.equalsIgnoreCase(pRadarLoc);

                if (status == false) {
                    where = String.format("WHERE office_id = '%s'", pRadarLoc);
                    /*
                     * Retrieve the record for this office from the RWBiasStat
                     * table.
                     */
                    pRWBiasStatList = IHFSDbGenerated.GetRWBiasstat(where);

                    if (pRWBiasStatList.size() != 0) {

                        /*
                         * Retrieve the records for this office/radar from the
                         * RWBiasDyn table.
                         */
                        where = String
                                .format("WHERE radid='%s' and obstime='%s' and office_id = '%s' ORDER BY  memspan_ind",
                                        rid, datime, pRadarLoc);
                        pRWBiasDynList = (ArrayList<Rwbiasdyn>) IHFSDbGenerated
                                .GetRWBiasDyn(where);

                        if (pRWBiasDynList.size() != 0) {
                            /*
                             * There are entries in the RWBiasDyn table for the
                             * RFC and obstime. Check for the bias which meets
                             * the number of gage/radar pairs requirement. If
                             * this does not exist, then set the bias to 1.
                             */
                            bias_found = 1;
                            pBias = 1.00f;
                            office_id = pRadarLoc;

                            ListIterator<Rwbiasdyn> li = pRWBiasDynList
                                    .listIterator();

                            while (li.hasNext()) {
                                pRWBiasDynNode = li.next();

                                if (pRWBiasDynNode.getNumpairs() >= pRWBiasStat
                                        .getNpairBiasSelect()) {
                                    pBias = pRWBiasDynNode.getBias();
                                    break;
                                }
                            }

                            pRWBiasDynList.clear();
                            pRWBiasDynList = null;
                            pRWBiasDynNode = null;
                        }

                        pRWBiasStatList.clear();
                        pRWBiasStatList = null;
                        pRWBiasStat = null;
                    }
                }

                pRadarLoc = null;
            }
        }

        return bias_found;
    }

}
