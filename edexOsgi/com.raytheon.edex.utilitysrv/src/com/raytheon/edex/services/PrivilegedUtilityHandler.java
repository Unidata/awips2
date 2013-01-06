package com.raytheon.edex.services;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.msgs.AbstractPrivilegedUtilityCommand;
import com.raytheon.uf.common.localization.msgs.AbstractUtilityResponse;
import com.raytheon.uf.common.localization.msgs.DeleteUtilityCommand;
import com.raytheon.uf.common.localization.msgs.PrivilegedUtilityRequestMessage;
import com.raytheon.uf.common.localization.msgs.UtilityResponseMessage;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Handle privileged requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public class PrivilegedUtilityHandler
        extends
        AbstractPrivilegedLocalizationRequestHandler<PrivilegedUtilityRequestMessage> {

    private static String UTILITY_DIR = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("DEFAULTDATADIR")
            + "/utility";

    @Override
    public UtilityResponseMessage handleRequest(
            PrivilegedUtilityRequestMessage msg) throws Exception {
        // Service each command
        List<AbstractUtilityResponse> responses = new ArrayList<AbstractUtilityResponse>();
        AbstractPrivilegedUtilityCommand[] cmds = msg.getCommands();
        for (AbstractPrivilegedUtilityCommand cmd : cmds) {
            LocalizationContext context = cmd.getContext();
            if (cmd instanceof DeleteUtilityCommand) {
                DeleteUtilityCommand castCmd = ((DeleteUtilityCommand) cmd);
                String fileName = castCmd.getFilename();
                responses.add(UtilityManager.deleteFile(UTILITY_DIR, context,
                        fileName));
            } else {
                throw new EdexException("Unsupported message type: "
                        + cmd.getClass().getName());
            }
        }
        AbstractUtilityResponse[] respArray = responses
                .toArray(new AbstractUtilityResponse[responses.size()]);

        UtilityResponseMessage response = new UtilityResponseMessage(respArray);

        return response;
    }

    @Override
    public AuthorizationResponse authorized(IUser user,
            PrivilegedUtilityRequestMessage request)
            throws AuthorizationException {

        AbstractPrivilegedUtilityCommand[] commands = request.getCommands();
        for (AbstractPrivilegedUtilityCommand abstractUtilityCommand : commands) {
            LocalizationContext context = abstractUtilityCommand.getContext();
            LocalizationLevel level = context.getLocalizationLevel();
            String filename = abstractUtilityCommand.getFilename();
            AuthorizationResponse resp = getAuthorizationResponse(user,
                    context, level, filename,
                    abstractUtilityCommand.getMyContextName());
            if (resp.isAuthorized() == false) {
                // If we are not authorized for any of the commands, break early
                return resp;
            }
        }
        return new AuthorizationResponse(true);
    }
}
