/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hrState;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagStationStateChange;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK3hrStateDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagStateDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagStationStateChangeDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.ChangeStationStateRequest;

import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for ChangeStationStateRequest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/01/2014   R4078      sgurung     Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class ChangeStationStateRequestHandler implements
        IRequestHandler<ChangeStationStateRequest> {

    private static Logger logger = Logger
            .getLogger(ChangeStationStateRequestHandler.class.toString());

    private GeoMagK3hrStateDao dao;

    @Override
    public Object handleRequest(ChangeStationStateRequest request)
            throws Exception {

        Boolean rval = Boolean.TRUE;

        logger.info("ChangeStationStateRequest for " + request.getStationCode());

        try {
            dao = new GeoMagK3hrStateDao();
            GeoMagStateDao stateDao = new GeoMagStateDao();
            GeoMagStationStateChangeDao stateChangeDao = new GeoMagStationStateChangeDao();

            Integer stateId = stateDao.getStateByProcessingState("Active")
                    .getStateId();
            Integer k3hrId = dao.getStationK3hrStateId(
                    request.getStationCode(), request.getSynopticTime(), null);

            logger.info("Inside ChangeStationStateRequest for "
                    + request.getStationCode() + " stateId = " + stateId
                    + " k3hrId = " + k3hrId);

            // Check if station is active for the Kp calculation
            // If so, delete the record from the station_indices_states table
            // if not, add the record for this period
            List<String> processingStates = dao.getKpStationStates(
                    request.getStationCode(), request.getSynopticTime());
            if (processingStates != null) {
                if (processingStates.contains("Active")) {

                    int numRowsDel = dao.delete(stateId, k3hrId);

                    if (numRowsDel > 0) {
                        // add record to table geomag_state_changes
                        GeoMagStationStateChange stateChange = new GeoMagStationStateChange();
                        stateChange.setStationCode(request.getStationCode());
                        stateChange
                                .setIssuedAction("Deactivated from Algorithm");
                        stateChange.setInsertTime(new Date());
                        stateChangeDao.persist(stateChange);
                    }
                } else {

                    // add record to table geomag_k3hr_state
                    GeoMagK3hrState k3hrState = new GeoMagK3hrState();
                    k3hrState.setK3hrId(k3hrId);
                    k3hrState.setStateId(stateId);
                    dao.persist(k3hrState);

                    // add record to table geomag_station_state_changes
                    GeoMagStationStateChange stateChange = new GeoMagStationStateChange();
                    stateChange.setStationCode(request.getStationCode());
                    stateChange.setIssuedAction("Activated in Algorithm");
                    stateChange.setInsertTime(new Date());
                    stateChangeDao.persist(stateChange);
                }
            }
        } catch (Exception e) {
            rval = Boolean.FALSE;
            logger.warning("Error while changing station state for "
                    + request.getStationCode() + " Exception = "
                    + e.getMessage() + " \n" + e.getStackTrace());
        }

        return rval;
    }
}
