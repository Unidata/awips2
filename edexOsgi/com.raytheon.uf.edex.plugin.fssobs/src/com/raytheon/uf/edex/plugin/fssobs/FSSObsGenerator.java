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

package com.raytheon.uf.edex.plugin.fssobs;

import java.util.ArrayList;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager.MonName;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.fssobs.common.FSSObsConfig;

/**
 * Generates a FSSObs Record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2010            skorolev     Initial creation
 * May 23, 2014 3086       skorolev     Cleaned code.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsGenerator extends CompositeProductGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsGenerator.class);

    private static final String genName = "FSSObs";

    private static final String productType = "fssobs";

    /**
     * Public construction
     */
    public FSSObsGenerator() {
        super(genName, productType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#generateProduct
     * (com.raytheon.edex.urifilter.URIGenerateMessage)
     */
    @Override
    public void generateProduct(URIGenerateMessage genMessage) {

        FSSObsConfig fss_config = null;
        try {
            fss_config = new FSSObsConfig((FSSObsURIGenrtMessage) genMessage,
                    this);
            this.setPluginDao(new FSSObsDAO(productType));
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        FSSObsRecord[] fssRecs = new FSSObsRecord[genMessage.getUris().length];
        int i = 0;
        for (String uri : genMessage.getUris()) {
            FSSObsRecord fssObsRec = new FSSObsRecord();
            fssObsRec = fss_config.getTableRow(uri);
            try {
                fssObsRec.constructDataURI();
            } catch (PluginException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            FSSObsDataTransform.buildView(fssObsRec);
            fssRecs[i] = fssObsRec;
            i++;
        }

        if (fssRecs.length > 0) {
            this.setPluginDataObjects(fssRecs);
            statusHandler.handle(Priority.INFO, "===> Successfully generated "
                    + fssRecs.length + " records.");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#createFilters()
     */
    @Override
    protected void createFilters() {
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(3);
        tmp.add(new FSSObsURIFilter(MonName.fog.name()));
        tmp.add(new FSSObsURIFilter(MonName.ss.name()));
        tmp.add(new FSSObsURIFilter(MonName.snow.name()));
        filters = tmp.toArray(new FSSObsURIFilter[tmp.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#configureFilters()
     */
    @Override
    protected void configureFilters() {
    }

    /**
     * Sets Product Time.
     * 
     * @param filter
     */
    public void setProductTime(URIFilter filter) {
        productTime = new DataTime(filter.getValidTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator#isRunning()
     */
    @Override
    public boolean isRunning() {
        return getConfigManager().getFSSState();
    }

}
