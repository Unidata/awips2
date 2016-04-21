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
package com.raytheon.uf.viz.damagepath;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.geotools.data.simple.SimpleFeatureCollection;

import com.raytheon.uf.common.damagepath.request.ExportToLdadRequest;
import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Legend right-click action to take the current {@code DamagePathLayer} data
 * and export it to LDAD in GeoJSON format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------- 
 * Jun 08, 2015  #4355     dgilling    Initial creation
 * Jun 18, 2015  #4354     dgilling    Support FeatureCollections so each 
 *                                     polygon can have its own properties.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ExportToLdadAction extends AbstractRightClickAction {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportToLdadAction.class);

    public ExportToLdadAction() {
        super("Export to LDAD");
    }

    @Override
    public void run() {
        String siteID = LocalizationManager.getInstance().getCurrentSite();
        byte[] jsonData;

        try (ByteArrayOutputStream outStream = new ByteArrayOutputStream()) {
            DamagePathLayer<?> layer = (DamagePathLayer<?>) getSelectedRsc();
            SimpleFeatureCollection featureCollection = layer
                    .buildFeatureCollection();

            new SimpleGeoJsonService().serialize(featureCollection, outStream);
            jsonData = outStream.toByteArray();
        } catch (JsonException | IOException e) {
            statusHandler.error(
                    "Error serializing Damage Path data to GeoJSON.", e);
            return;
        }

        try {
            IServerRequest request = new ExportToLdadRequest(siteID, jsonData);
            String errorMsg = (String) ThriftClient.sendRequest(request);
            if (errorMsg != null && !errorMsg.isEmpty()) {
                statusHandler
                        .error("Could not export damage path data to LDAD: "
                                + errorMsg);
            }
        } catch (VizException e) {
            statusHandler.error("Error processing ExportToLdadRequest.", e);
        }
    }
}
