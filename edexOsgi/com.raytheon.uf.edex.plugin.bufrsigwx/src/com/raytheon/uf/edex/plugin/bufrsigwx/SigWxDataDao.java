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
package com.raytheon.uf.edex.plugin.bufrsigwx;

import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * DAO for bufrsigwx
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009            jkorman     Initial creation
 * Feb 04, 2016 5309       tgurney     Remove dead code
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SigWxDataDao extends PointDataPluginDao<SigWxData> {

    /**
     * Creates a new SigWxDataDao object.
     * 
     * @throws PluginException
     */
    public SigWxDataDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime", "wxType", "wxLayer" };
    }

    @Override
    public String getPointDataFileName(SigWxData p) {

        String fileName = null;

        if (SigWxType.CLOUD.equals(p.getWxType())) {
            StringBuilder sb = new StringBuilder("sigwx");
            sb.append(p.getWxType().name());
            sb.append("-");
            sb.append(p.getWxLayer().name());
            sb.append(".h5");
            fileName = sb.toString();
        } else {
            fileName = "sigwx" + p.getWxType().name() + ".h5";
        }
        return fileName;
    }

    @Override
    public SigWxData newObject() {
        return new SigWxData();
    }

    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        SigWxLayer layer = (SigWxLayer) obj.get("wxLayer");
        SigWxType type = (SigWxType) obj.get("wxType");
        String pddFile = null;
        switch (type) {
        case CAT:
            pddFile = "/res/pointdata/cat_swhshm.xml";
            break;
        case CLOUD:
            if (layer == SigWxLayer.SWH) {
                pddFile = "/res/pointdata/cloud_swh.xml";
            } else if (layer == SigWxLayer.SWM) {
                pddFile = "/res/pointdata/cloud_shm.xml";
            }
            break;
        case FRONTS:
            pddFile = "/res/pointdata/fronts_swhshm.xml";
            break;
        case JETS:
            pddFile = "/res/pointdata/jet_swhshm.xml";
            break;
        case TROP:
            pddFile = "/res/pointdata/trop_swhshm.xml";
            break;
        case VTS:
            pddFile = "/res/pointdata/vts_swhshm.xml";
            break;
        }
        if (pddFile != null) {
            try {
                return PointDataDescription.fromStream(this.getClass()
                        .getResourceAsStream(pddFile));
            } catch (SerializationException e) {
                logger.error("Unable to load " + pluginName
                        + " Point Data Description for " + type + "," + layer,
                        e);
            }
        } else {
            logger.error("Unable to load " + pluginName
                    + " Point Data Description for " + type + "," + layer);
        }
        return null;
    }

}
