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
package com.raytheon.uf.edex.dissemination;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;
import com.raytheon.uf.edex.dissemination.transmitted.TransProdHeader;

/**
 * IRequestHandler for OUPRequests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2009            njensen     Initial creation
 * Oct 12, 2012  DR 15418  D. Friedman Use clustered TransmittedProductList
 * Jun 07, 2013   1981     mpduff      This is now a priviledged request handler.
#  Nov 20, 2013  DR 16777  D. Friedman Add a test mode.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class OUPHandler extends AbstractPrivilegedRequestHandler<OUPRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OUPHandler.class);

    /** The application for authentication */
    private static final String APPLICATION = "Official User Product";

    private OUPAckManager ackManager;

    @Override
    public OUPResponse handleRequest(OUPRequest request) throws Exception {
        return handleOUPRequest(request, false);
    }

    public OUPResponse handleOUPRequest(OUPRequest request, boolean test) throws Exception {
        OfficialUserProduct oup = request.getProduct();
        OUPResponse resp = new OUPResponse();
        boolean changedBbb = false;
        if (oupOk(oup)) {
            try {
                if (oup.isNeedsWmoHeader()) {
                    request = ModifyProduct.addWmoHeader(request);
                }
                TransProdHeader header = ModifyProduct.getProductHeader(oup);
                if (request.isCheckBBB() && ! test) {
                    changedBbb = ModifyProduct.checkBBBField(oup, header);
                    if (changedBbb) {
                        resp.setChangedBBB(request.getProduct().getWmoType());
                    }
                }

                String convertedProductText = ModifyProduct
                        .convertNewline2rrn(oup.getProductText());
                oup.setProductText(convertedProductText);

                PythonScript py = null;
                try {
                    py = initializePython();
                    Map<String, Object> args = new HashMap<String, Object>(1);
                    args.put("oup", oup);
                    args.put("afosID", header.getProductId());
                    args.put("resp", resp);
                    args.put("ackMgr", ackManager);
                    args.put("test", test);
                    resp.setAttempted(true);
                    py.execute("process", args);
                } catch (JepException e) {
                    resp.setMessage("Error executing handleOUP python");
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Error executing handleOUP python", e);
                } finally {
                    if (py != null) {
                        py.dispose();
                    }
                }
                /*
                 * TODO: Should be updating TransmittedProductList here, after
                 * success has been confirmed.
                 */
            } catch (OUPHeaderException e) {
                resp.setAttempted(false);
                resp.setMessage("Product not sent, error encountered with header.\n"
                        + e.getMessage());
            }
        } else {
            resp.setAttempted(false);
            resp.setMessage("Product not sent.  OfficialUserProduct requires an awipsWanPil, "
                    + "product text, and product filename.");
        }

        if (resp != null && resp.getMessage() == null) {
            resp.setMessage("");
        }
        return resp;
    }

    private static boolean oupOk(OfficialUserProduct oup) {
        boolean ok = false;
        if (oup != null) {
            if (oup.getAwipsWanPil() != null && oup.getFilename() != null
                    && oup.getProductText() != null) {
                ok = true;
            }
        }

        return ok;
    }

    private static PythonScript initializePython() {
        PythonScript python = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexBase = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        LocalizationContext commonBase = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        String path = pathMgr.getFile(edexBase,
                "dissemination" + File.separator + "handleOUP.py").getPath();
        String statusPath = pathMgr.getFile(commonBase, "python").getPath();
        try {
            python = new PythonScript(path,
                    PyUtil.buildJepIncludePath(statusPath),
                    OUPHandler.class.getClassLoader());
        } catch (JepException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error initializing handleOUP python", e);
        }
        return python;
    }

    public OUPAckManager getAckManager() {
        return ackManager;
    }

    public void setAckManager(OUPAckManager ackManager) {
        this.ackManager = ackManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user, OUPRequest request)
            throws AuthorizationException {
        boolean authorized = false;

        if (request.getUser().uniqueId().toString()
                .equals(OUPRequest.EDEX_ORIGINATION)) {
            authorized = true;
        } else {
            AuthManager manager = AuthManagerFactory.getInstance().getManager();
            IRoleStorage roleStorage = manager.getRoleStorage();

            authorized = roleStorage.isAuthorized((request).getRoleId(), user
                    .uniqueId().toString(), APPLICATION);
        }

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            return new AuthorizationResponse(
                    (request).getNotAuthorizedMessage());
        }
    }
}
