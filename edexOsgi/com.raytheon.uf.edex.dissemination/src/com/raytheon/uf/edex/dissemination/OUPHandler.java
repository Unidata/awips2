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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.dataplugin.text.db.MixedCaseProductSupport;
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
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.IPermissionsManager;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.dissemination.transmitted.TransProdHeader;

import jep.JepConfig;
import jep.JepException;

/**
 * IRequestHandler for OUPRequests
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Oct 22, 2009           njensen      Initial creation
 * Oct 12, 2012  15418    D. Friedman  Use clustered TransmittedProductList
 * Jun 07, 2013  1981     mpduff       This is now a privileged request handler.
 * Nov 20, 2013  16777    D. Friedman  Add a test mode.
 * May 28, 2014  3211     njensen      Use IAuthorizer instead of IRoleStorage
 * Feb 24, 2016  5411     randerso     Force product to upper case if necessary
 * Aug 01, 2016  5744     mapeters     Python script moved from edex_static to
 *                                     common_static
 * Jul 17, 2017  6288     randerso     Changed to use new Roles/Permissions
 *                                     framework
 *Jun 03, 2019   7852     dgilling     Update code for jep 3.8.
 *
 * </pre>
 *
 * @author njensen
 */

public class OUPHandler extends AbstractPrivilegedRequestHandler<OUPRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OUPHandler.class);

    private OUPAckManager ackManager;

    @Override
    public OUPResponse handleRequest(OUPRequest request) throws Exception {
        return handleOUPRequest(request, false);
    }

    public OUPResponse handleOUPRequest(OUPRequest request, boolean test)
            throws Exception {
        OfficialUserProduct oup = request.getProduct();
        OUPResponse resp = new OUPResponse();
        boolean changedBbb = false;
        if (oupOk(oup)) {
            try {
                if (oup.isNeedsWmoHeader()) {
                    request = ModifyProduct.addWmoHeader(request);
                }
                TransProdHeader header = ModifyProduct.getProductHeader(oup);
                if (request.isCheckBBB() && !test) {
                    changedBbb = ModifyProduct.checkBBBField(oup, header);
                    if (changedBbb) {
                        resp.setChangedBBB(request.getProduct().getWmoType());
                    }
                }

                String convertedProductText = ModifyProduct
                        .convertNewline2rrn(oup.getProductText());

                /*
                 * Force to upper case if product is not enabled for mixed case
                 * transmission
                 */
                String nnn = oup.getAwipsWanPil().substring(4, 7);
                convertedProductText = MixedCaseProductSupport
                        .conditionalToUpper(nnn, convertedProductText);

                oup.setProductText(convertedProductText);

                try (PythonScript py = initializePython()) {
                    Map<String, Object> args = new HashMap<>();
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
                }

                /*
                 * TODO: Should be updating TransmittedProductList here, after
                 * success has been confirmed.
                 */
            } catch (OUPHeaderException e) {
                statusHandler.error(e.getMessage(), e);
                resp.setAttempted(false);
                resp.setMessage(
                        "Product not sent, error encountered with header.\n"
                                + e.getMessage());
            }
        } else {
            resp.setAttempted(false);
            resp.setMessage(
                    "Product not sent.  OfficialUserProduct requires an awipsWanPil, "
                            + "product text, and product filename.");
        }

        if ((resp != null) && (resp.getMessage() == null)) {
            resp.setMessage("");
        }
        return resp;
    }

    private static boolean oupOk(OfficialUserProduct oup) {
        boolean ok = false;
        if (oup != null) {
            if ((oup.getAwipsWanPil() != null) && (oup.getFilename() != null)
                    && (oup.getProductText() != null)) {
                ok = true;
            }
        }

        return ok;
    }

    private static PythonScript initializePython() throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonBase = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        String path = pathMgr.getFile(commonBase,
                "dissemination" + IPathManager.SEPARATOR + "handleOUP.py")
                .getPath();
        String statusPath = pathMgr.getFile(commonBase, "python").getPath();

        PythonScript python = new PythonScript(
                new JepConfig()
                        .setIncludePath(PyUtil.buildJepIncludePath(statusPath))
                        .setClassLoader(OUPHandler.class.getClassLoader()),
                path);
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
    public AuthorizationResponse authorized(OUPRequest request)
            throws AuthorizationException {
        boolean authorized = false;

        if (request.getUser().uniqueId().toString()
                .equals(OUPRequest.EDEX_ORIGINATION)) {
            authorized = true;
        } else {
            IPermissionsManager manager = AuthManagerFactory.getInstance()
                    .getPermissionsManager();

            authorized = manager.isPermitted(request.getRoleId());
        }

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            return new AuthorizationResponse(
                    (request).getNotAuthorizedMessage());
        }
    }
}
