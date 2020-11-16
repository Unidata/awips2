/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.mpe.ui.rfcmask;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.mpe.util.RFCSiteLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.viz.mpe.ui.Activator;

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
 * Oct 30, 2017  17911     wkwock      Initial creation
 * Jun 06, 2018  20731     wkwock      Make code more robust
 * Jul 31, 2018  20677     wkwock     Use FileLocator to open input stream.
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
     * @return XMRGFile or null if it's not a RFC
     */
    public static XmrgFile getRFCMask(String rfcSite) throws IOException {
        String rfcName = RFCSiteLookup.RFCMAP.get(rfcSite);
        if (rfcName == null) {
            return null;
        }

        XmrgFile xmrgFile = new XmrgFile();
        Bundle bundle = Activator.getDefault().getBundle();
        IPath path = new Path("/res/RFCmask/xmrg_" + rfcName.toLowerCase() + "mask");
        try (InputStream is = FileLocator.openStream(bundle, path, false)) {
            xmrgFile.load(is);
        } catch (Exception e) {
            logger.error("Failed to read " + rfcName + " RFC mask file.", e);
        }

        return xmrgFile;
    }
}
