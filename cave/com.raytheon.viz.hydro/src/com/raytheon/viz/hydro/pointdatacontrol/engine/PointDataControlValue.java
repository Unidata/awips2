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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;

import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.TimeModeType;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ValueType;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.util.RatingUtils;

/**
 * Handles the "Display Values As:" selection.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0 
 */

public class PointDataControlValue {
    private static final int HG = 0;
    private static final int QR = 1;
    private static final int PRIMARY = 2;

    public static ArrayList<GageData> processValueOption(ArrayList<GageData> repList, boolean forceRetrieval) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PDCDataManager dataManager = PDCDataManager.getInstance();
        double stage;
        double flow;
        int valueType = 0;
        int fldLevel = 0;
        int deriveStageFlow = 0;
        int peType;
        
        if (repList == null) {
            return repList;
        }
        
        /* Only pass through this routine if the PE starts with a 'H' or a 'Q'
           or if the PE is "PRIMARY".  That is, all of the river stage or flow
           PEs will be passed through this routine.  This filtering should be
           done here, not in the main routine. */
        if (!pcOptions.getSelectedAdHocElementString().startsWith("H") &&
                !pcOptions.getSelectedAdHocElementString().startsWith("Q") &&
                !pcOptions.getSelectedAdHocElementString().startsWith("Pr")) {
              return repList;
        }
        
        /* Set the "previous" variables to reflect what is currently being 
        requested. */
        fldLevel = pcOptions.getFloodLevel();
        deriveStageFlow = pcOptions.getDeriveStageFlow();
        valueType = pcOptions.getValueType();
        
        /* Begin testing the setting of the Value Pulldown Option Menu. */
        
        /* The first case is where the user just wants to display the
        requested value. This is the simplest case.  It corresponds
        to "Value" item on the "Value" pull down option menu. Also,
        when the user is displaying the change in a value, it does not
        make any sense to be able to display the flood stage or the
        flood departure. */
        
        if ((pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE.getTimeMode()) ||
                ((valueType == ValueType.TYPE_VALUE.getValueType()) &&
                (fldLevel == 0) &&
                (deriveStageFlow == 0))) {
            /* Not much else to do.  The value is already contained within
               the report structure. */
            return repList;
        }

        if ((valueType == ValueType.TYPE_VALUE.getValueType()) && (fldLevel == 1)) {
            /* Loop over each report in the report list and
               call get_rs_info to retrieve the flood level if
               it is available. */
            
            for (int i = 0; i < repList.size(); i++) {
                RiverStat riverStat = dataManager.getRiverStatus(repList.get(i).getLid());
                
                if ((riverStat != null) && (riverStat.getFs() != PDCConstants.MISSING_VALUE)) {
                    if (repList.get(i).getPe().equals("-")) {
                        repList.get(i).setPe(riverStat.getPe());
                    }
                    
                    if (repList.get(i).getPe().startsWith("H")) {
                        if (riverStat.getFs() > 0) {
                            repList.get(i).setValue2(riverStat.getFs());
                        } else {
                            repList.get(i).setValue2(PDCConstants.MISSING_VALUE);
                        }
                    } else {
                        if (riverStat.getFq() > 0) {
                            repList.get(i).setValue2(riverStat.getFq());
                        } else {
                            repList.get(i).setValue2(PDCConstants.MISSING_VALUE);
                        }
                    }
                }            
            }
            return repList;
        }
        
        /* Test if the option to draw the current value over the derived stage
        or flow has been selected. */
        if ((valueType == ValueType.TYPE_VALUE.getValueType()) &&
                (deriveStageFlow == 1)) {
            /* Only enter this logic if the PE is "HG", "QR" or "PRIMARY".  */
            
            if (pcOptions.getSelectedAdHocElementString().equals("HG")) {
                peType = HG;
            } else if (pcOptions.getSelectedAdHocElementString().equals("QR")) {
                peType = QR;
            } else if (pcOptions.getSelectedAdHocElementString().startsWith("Pr")) {
                peType = PRIMARY;
            } else {
                return repList;
            }
            
            /* Loop over the reports in the ReportList Linked List. */
            for (int i = 0; i < repList.size(); i++) {
                
                /* For a PE of HG:
                   If there is a corresponding rating curve then interpolate the flow
                   and set VALUE2 to it.
                   Otherwise, the VALUE2 is set to missing.
                   exit */

                switch (peType) {
                case HG:
                    /* Check to determine if there is a rating curve.
                    If there is, then a corresponding flow can
                    be derived. */
                    if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                        flow = RatingUtils.stage2discharge(repList.get(i).getLid(), repList.get(i).getValue());
                        
                        if (flow != PDCConstants.RATING_CONVERT_FAILED) {
                            repList.get(i).setValue2(flow);
                        }
                    }
                    break;
                    
                    /* For a PE of QR:
                       Loop over each report in the report list.
                       If there is a corresponding rating curve, then interpolate
                       the stage and set VALUE1 to it. 
                       Otherwise, set VALUE2 to missing.
                       exit */
                case QR:
                    /* Check to determine if there is a rating curve.
                       If there is, then a corresponding stage can
                       be derived. */
                    if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                        stage = RatingUtils.discharge2stage(repList.get(i).getLid(), repList.get(i).getValue());
                        
                        if (stage != PDCConstants.RATING_CONVERT_FAILED) {
                            repList.get(i).setValue2(stage);
                        }
                    }
                    break;
                    
                case PRIMARY:
                    if (repList.get(i).getPe().equals("HG")) {
                        /* Check to determine if there is a rating curve.
                        If there is, then it a corresponding flow can
                        be derived. */
                    if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                        flow = RatingUtils.stage2discharge(repList.get(i).getLid(), repList.get(i).getValue());

                        if (flow != PDCConstants.RATING_CONVERT_FAILED) {
                                repList.get(i).setValue2(flow);
                            }
                        }
                    } else if (repList.get(i).getPe().equals("QR")) {
                        /* Check to determine if there is a rating curve.
                           If there is, then it a corresponding stage can
                           be derived. */
                        if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                            stage = RatingUtils.discharge2stage(repList.get(i).getLid(), repList.get(i).getValue());
                            
                            if (stage != PDCConstants.RATING_CONVERT_FAILED) {
                                repList.get(i).setValue2(stage);
                            }
                        }
                    }
                    break;
                }
            }
        }
        
        /* If the value type is "departure", then find the floodlevel from the
        riverstat table and subtract it from the primary value stored in each
        node of the linked list of ReportList structures. */
        if ((valueType == ValueType.TYPE_DEPART.getValueType()) && (fldLevel == 0)) {
            /* Loop over each report in the report list and
            call get_rs_info to retrieve the flood level if
            it is available. */
            for (int i = 0; i < repList.size(); i++) {
                RiverStat rsInfo = dataManager.getRiverStatus(repList.get(i).getLid());
                
                if (rsInfo != null) {
                    if (repList.get(i).getPe().equals("-")) {
                        repList.get(i).setPe(rsInfo.getPe());
                    }

                    if (repList.get(i).getPe().startsWith("H")) {
                        if ((rsInfo.getFs() != PDCConstants.MISSING_VALUE) && (rsInfo.getFs() > 0)) {
                            if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                                repList.get(i).setValue(repList.get(i).getValue() - rsInfo.getFs());
                            }
                        } else {
                            repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                        }
                    } else {
                        if ((rsInfo.getFq() != PDCConstants.MISSING_VALUE) && (rsInfo.getFq() > 0)) {
                            if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                                repList.get(i).setValue(repList.get(i).getValue() - rsInfo.getFq());
                            }
                        } else {
                            repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                        }
                    }
                } else {
                    repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                }
            }
            return repList;
        }
        
        /* If the value type is "departure" and the user wishes
        to display both the departure and the floodlevel values, then 
        find the floodlevel from the riverstat table and subtract it from 
        the primary value stored in each node of the linked list of 
        ReportList structures. */
        if ((valueType == ValueType.TYPE_DEPART.getValueType()) && (fldLevel == 1)) {
            /* Loop over each report in the report list and
            call get_rs_info to retrieve the flood level if
            it is available. */
            for (int i = 0; i < repList.size(); i++) {
                RiverStat rsInfo = dataManager.getRiverStatus(repList.get(i).getLid());
                
                if (rsInfo != null) {
                    if (repList.get(i).getPe().equals("-")) {
                        repList.get(i).setPe(rsInfo.getPe());
                    }
                    
                    if (repList.get(i).getPe().startsWith("H")) {
                        if ((rsInfo.getFs() != PDCConstants.MISSING_VALUE) && (rsInfo.getFs() > 0)) {
                            if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                                repList.get(i).setValue(repList.get(i).getValue() - rsInfo.getFs());
                            }
                            repList.get(i).setValue2(rsInfo.getFs());
                        } else {
                            repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                            repList.get(i).setValue2(PDCConstants.MISSING_VALUE);
                        }
                    } else {
                        if ((rsInfo.getFq() != PDCConstants.MISSING_VALUE) && (rsInfo.getFq() > 0)) {
                            if (repList.get(i).getValue() != PDCConstants.MISSING_VALUE) {
                                repList.get(i).setValue(repList.get(i).getValue() - rsInfo.getFq());
                            }
                            repList.get(i).setValue2(rsInfo.getFq());
                        } else {
                            repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                            repList.get(i).setValue2(PDCConstants.MISSING_VALUE);
                        }
                    }
                } else {
                    repList.get(i).setValue(PDCConstants.MISSING_VALUE);
                    repList.get(i).setValue2(PDCConstants.MISSING_VALUE);
                }
            }
        }
        
        return repList;
    }
}