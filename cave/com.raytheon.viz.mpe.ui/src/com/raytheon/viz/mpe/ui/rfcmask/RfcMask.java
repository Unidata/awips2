/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.mpe.ui.rfcmask;

import java.io.IOException;
import java.io.InputStream;

import com.raytheon.uf.common.mpe.util.RFCSiteLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;

/**
 * 
 * read RFC mask files
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2017  17911     wkwock     Initial creation
 *
 * </pre>
 *
 * @author wkwock
 */
public class RfcMask {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(RfcMask.class);

    /**
     * get this RFC mask
     * 
     * @return XMRGFile
     */
    public static XmrgFile getRFCMask(String rfcSite) throws IOException {
        String rfcName = RFCSiteLookup.RFCMAP.get(rfcSite).toLowerCase();
        XmrgFile xmrgFile = new XmrgFile();

        try (InputStream is = MPEFieldResource.class
                .getResourceAsStream("/res/RFCmask/xmrg_" + rfcName + "mask")) {

            xmrgFile.load(is);
        } catch (Exception e) {
            logger.error("Failed to read " + rfcName + "RFC mask file.", e);
        }

        return xmrgFile;
    }
}
