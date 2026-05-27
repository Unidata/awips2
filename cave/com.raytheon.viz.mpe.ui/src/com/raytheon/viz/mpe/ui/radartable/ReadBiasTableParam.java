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

import com.raytheon.uf.common.dataplugin.shef.tables.DAABiasDyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasdyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * Aug 11, 2015 4500       rjpeter     Fix type casts.
 * Aug 23, 2018 6953       tgurney     read_bias_table_param cleanup, change
 *                                     method signature
 * Sep 05, 2024 2037782    jsebahar    Remove DPA radar data, this includes data
 *                                     from DPAAdapt table.
 * Jan 27, 2025 2038346    mapeters    Fix get_rfc_bias_value() so that it actually uses Rwbiasstat
 *                                     value from DB,  and switch it from using Rwbiasdyn to
 *                                     DAABiasDyn for DPA data removal
 * </pre>
 *
 * @author snaples
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
        String where;
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        ArrayList<Rwbiasdyn> rwBiasDynHead = new ArrayList<>();
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

        if (rwBiasDynHead.isEmpty()) {
            // logMessage
            // (String.format(" Could not retrieve data from RWBiasDyn table for
            // radar id %s and time %s.\n "
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
    }

    /**
     * This is ported from get_rfc_bias_value() in read_bias_table_param.c, but
     * it is updated to use the DAA dynamic bias table (DAABiasDyn) instead of
     * the DPA table (RWBiasDyn). It still uses the same static bias table as
     * the legacy DPA data (RWBiasStat), which matches the logic in
     * calculateMeanBiasDP.c and this comment in there: "DAA mean-field bias
     * will use same parameters as DPA mean-field bias (for now)".
     *
     * @param rid
     * @param officeId
     * @return RFC bias value
     */
    public static Float get_rfc_bias_value(String rid, String officeId) {
        Float rval = null;
        String fxaLocalSite = appsDefaults.getToken("fxa_local_site");
        String where = "";
        String datime = RadarBiasTableDialog.dt;

        if (!fxaLocalSite.isEmpty()) {

            if (officeId != null && !officeId.isEmpty()
                    && !fxaLocalSite.equalsIgnoreCase(officeId)) {
                where = String.format("WHERE office_id = '%s'", officeId);
                /*
                 * Retrieve the static bias parameters for this office from the
                 * RWBiasStat table.
                 */
                List<Rwbiasstat> rwBiasStatList = IHFSDbGenerated
                        .GetRWBiasstat(where);

                if (!rwBiasStatList.isEmpty()) {
                    Rwbiasstat rwBiasStat = rwBiasStatList.get(0);

                    /*
                     * Retrieve the records for this office/radar from the
                     * DAABiasDyn table.
                     */
                    where = String.format(
                            "WHERE radid='%s' and obstime='%s' and office_id = '%s' ORDER BY  memspan_ind",
                            rid, datime, officeId);
                    List<DAABiasDyn> daaBiasDynList = IHFSDbGenerated
                            .getDAABiasDyn(where);

                    if (!daaBiasDynList.isEmpty()) {
                        /*
                         * There are entries in the DAABiasDyn table for the RFC
                         * and obstime. Check for the bias which meets the
                         * number of gage/radar pairs requirement. If this does
                         * not exist, then set the bias to 1.
                         */
                        rval = 1.00f;
                        for (DAABiasDyn daaBiasDynNode : daaBiasDynList) {

                            if (daaBiasDynNode.getNumpairs() >= rwBiasStat
                                    .getNpairBiasSelect()) {
                                rval = daaBiasDynNode.getBias();
                                break;
                            }
                        }
                    }
                }
            }
        }

        return rval;
    }

}
