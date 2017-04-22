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
package com.raytheon.uf.viz.bufrsigwx.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Generic resourceData for SigWx data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2009 3099       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SigWxResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SigWxResourceData.class);

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof SigWxResourceData == false) {
            return false;
        }

        // SigWxCloudsResourceData other = (SigWxCloudsResourceData) obj;

        return true;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        String typeString = this.metadataMap.get("wxType").getConstraintValue();
        SigWxType type = SigWxType.valueOf(typeString);
        SigWxResource nr = null;
        if (SigWxType.CLOUD == type) {
            nr = new SigWxCloudsResource(this, loadProperties);
        } else if (SigWxType.CAT == type) {
            nr = new SigWxCatResource(this, loadProperties);
        } else if (SigWxType.VTS == type) {
            nr = new SigWxVtsResource(this, loadProperties);
        } else if (SigWxType.TROP == type) {
            nr = new SigWxTropHeightResource(this, loadProperties);
        } else if (SigWxType.JETS == type) {
            nr = new SigWxJetStreamResource(this, loadProperties);
        } else {
            throw new VizException("No Resource for SigWx Type: " + typeString);
        }
        for (PluginDataObject o : objects) {
            if (o instanceof SigWxData) {
                SigWxData rec = (SigWxData) o;
                nr.addRecord(rec);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + o.getClass()
                                + " Expected: " + SigWxData.class);
            }
        }
        return nr;
    }

}
