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

    @Override
    protected void createFilters() {
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(3);
        tmp.add(new FSSObsURIFilter("fog"));
        tmp.add(new FSSObsURIFilter("ss"));
        tmp.add(new FSSObsURIFilter("snow"));
        filters = tmp.toArray(new FSSObsURIFilter[tmp.size()]);
    }

    @Override
    protected void configureFilters() {
    }

    public void setProductTime(URIFilter filter) {
        productTime = new DataTime(filter.getValidTime());

    }

    @Override
    public boolean isRunning() {
        return getConfigManager().getFSSState();
    }

}
