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
package com.raytheon.viz.mpe.ui.actions;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.QcPrecipOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.mpe.util.DailyQcUtils;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GageQcSelect {

    OtherPrecipOptions op = new OtherPrecipOptions();

    int dflag[] = DailyQcUtils.dflag;

    int funct[] = DailyQcUtils.funct;

    int qflag[] = DailyQcUtils.qflag;

    public void source_Select(int client_data) {
        int i;

        int maxts = DailyQcUtils.tsmax;

        dflag[client_data] = -dflag[client_data];
        // dflag[client_data] = 1;

        if (client_data == maxts + 1) {

            if (dflag[maxts + 1] == 1) {

                for (i = 1; i < maxts + 2; i++) {

                    dflag[i] = 1;
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        QcPrecipOptionsDialog.tsbuttons[i].setSelection(true);
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        QcTempOptionsDialog.tsbuttons[i].setSelection(true);
                    }
                }

            } else {

                for (i = 1; i < maxts + 2; i++) {

                    dflag[i] = -1;
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        QcPrecipOptionsDialog.tsbuttons[i].setSelection(false);
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        QcTempOptionsDialog.tsbuttons[i].setSelection(false);
                    }
                }
            }
        }
        op.refresh_exposure();
    }

    public void quality_Select(int client_data) {
        /*
         * good=0 Bad=1 Manual=2 Questionable=3 Estimate=5 Time Distributed=6
         * Missing=8 All=9
         */

        int i;

        i = funct[client_data];

        qflag[i] = -qflag[i];

        if (i == 9) {

            if (qflag[9] == 1) {

                for (i = 0; i < 9; i++) {

                    qflag[funct[i]] = 1;
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        QcPrecipOptionsDialog.qsbuttons[i].setSelection(true);
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        QcTempOptionsDialog.qsbuttons[i].setSelection(true);
                    }
                }

            }

            else {

                for (i = 0; i < 9; i++) {

                    qflag[funct[i]] = -1;
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        QcPrecipOptionsDialog.qsbuttons[i].setSelection(false);
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        QcTempOptionsDialog.qsbuttons[i].setSelection(false);
                    }

                }

            }

        }
        op.refresh_exposure();
    }

    public void qctype_select(int client_data) {

        /*
         * i=0 represents Precipitation qc type, i=1 represents Temperature qc
         * type, i=2 represents Freezing Level qc type
         */

        int i = client_data;

        SaveLevel2Data.qctype_flag[i] = -SaveLevel2Data.qctype_flag[i];
        op.send_expose();

    }

    public void quality_Select_Temperature(int client_data) {
        /*
         * good=0 Bad=1 Manual=2 Questionable=3 Estimate=5 Time Distributed=6
         * All=9
         */

        int i;

        i = funct[client_data];

        qflag[i] = -qflag[i];

        if (i == 9) {

            if (qflag[9] == 1) {

                for (i = 0; i < 9; i++) {

                    if (i == 5) {
                        continue;
                    }

                    qflag[funct[i]] = 1;
                    QcTempOptionsDialog.qsbuttons[i].setSelection(true);
                }

            }

            else {

                for (i = 0; i < 9; i++) {

                    if (i == 5) {
                        continue;
                    }

                    qflag[funct[i]] = -1;
                    QcTempOptionsDialog.qsbuttons[i].setSelection(false);
                }

            }

        }
        op.refresh_exposure();
    }

    public void change_Character(int data) {

        if (data == 2) {

            DailyQcUtils.gage_char[0] = 1;
            DailyQcUtils.gage_char[1] = 1;

        }

        else if (data == 0) {
            DailyQcUtils.gage_char[0] = 1;
            DailyQcUtils.gage_char[1] = -1;

        }

        else if (data == 1) {

            DailyQcUtils.gage_char[0] = -1;
            DailyQcUtils.gage_char[1] = 1;

        }
        op.refresh_exposure();
    }

    public void change_Plot(int i) {

        DailyQcUtils.plot_view = i;
        op.refresh_exposure();
        return;
    }
}
