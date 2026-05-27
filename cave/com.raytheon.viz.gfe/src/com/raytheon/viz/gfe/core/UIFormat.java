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
package com.raytheon.viz.gfe.core;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;

/**
 * A formatter for ui param info.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 20, 2008            Eric Babin Initial Creation
 *  Jun 09, 2009           randerso    Ported completely     
 * 
 * A port of various methods from UIFormat.C
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 * 
 */

public class UIFormat {
    public static enum FilterType {
        NONE, ALL, DISPLAYED, MUTABLE, MODIFIED, UNDISPLAYED
    };

    private int _parmNameLen;

    private int _modelLen;

    private int _levelLen;

    private int _typeLen;

    private int _siteIDLen;

    private int _dbTimeLen;

    private SimpleDateFormat sdf;

    /**
     * Constructor taking a pointer to the ParmMgr. The value of parms and
     * database determine how the sizes are determined for the strings. For
     * performance reasons, only choose the minimum set that you need.
     * 
     * Determines the longest parmName, level, model, type, time, and siteID in
     * the sequence.
     * 
     * @param parmMgr
     * @param parms
     * @param databases
     */
    public UIFormat(IParmManager parmMgr, FilterType parms, FilterType databases) {
        sdf = new SimpleDateFormat("ddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int i;
        List<DatabaseID> availDbs;
        if (databases.equals(FilterType.ALL)) {
            availDbs = parmMgr.getAvailableDbs();
        } else if (databases.equals(FilterType.DISPLAYED)) {
            availDbs = parmMgr.getDisplayedDbs();
        } else if (databases.equals(FilterType.MUTABLE)
                || databases.equals(FilterType.MODIFIED)) {
            availDbs = Arrays.asList(parmMgr.getMutableDatabase());
        } else if (databases.equals(FilterType.UNDISPLAYED)) {
            availDbs = parmMgr.getUndisplayedDbs();
        } else {
            availDbs = new ArrayList<DatabaseID>();
        }

        // find maximum lengths for the databases
        for (DatabaseID dbId : availDbs) {
            // model
            if ((dbId.getModelName().length()) > _modelLen) {
                _modelLen = dbId.getModelName().length();
            }
            // type
            if ((dbId.getDbType().length()) > _typeLen) {
                _typeLen = dbId.getDbType().length();
            }
            // siteID
            if ((dbId.getSiteId().length()) > _siteIDLen) {
                _siteIDLen = dbId.getSiteId().length();
            }
            // modelTime
            int modelTimeLen = 0;
            if (!dbId.getModelTime().equals(DatabaseID.NO_MODEL_TIME)) {
                modelTimeLen = 4;
            }
            if (modelTimeLen > _dbTimeLen) {
                _dbTimeLen = modelTimeLen;
            }
        }

        // get the longest parmName and level in the sequence for the parms
        if (parms.equals(FilterType.ALL)) {
            for (DatabaseID dbId : availDbs) {
                ParmID[] parmIDs = parmMgr.getAvailableParms(dbId);
                calcParmLengths(parmIDs);
            }
        } else if (parms.equals(FilterType.DISPLAYED)) {
            ParmID[] ids = parmMgr.getParmIDs(parmMgr.getDisplayedParms());
            calcParmLengths(ids);
        } else if (parms.equals(FilterType.MUTABLE)) {
            ParmID[] ids = parmMgr.getAvailableParms(parmMgr
                    .getMutableDatabase());
            calcParmLengths(ids);
        } else if (parms.equals(FilterType.MODIFIED)) {
            ParmID[] ids = parmMgr.getParmIDs(parmMgr.getModifiedParms());
            calcParmLengths(ids);
        } else if (parms.equals(FilterType.UNDISPLAYED)) {
            ParmID[] ids = parmMgr.getParmIDs(parmMgr.getUndisplayedParms());
            calcParmLengths(ids);
        }
    }

    /**
     * Formatting function returning the formatted ParmID.
     * 
     * @param parmID
     * @return
     */
    public String uiParmID(ParmID parmID) {
        StringBuilder str = new StringBuilder();

        str.append(StringUtils.rightPad(parmID.getParmName(), _parmNameLen));

        str.append(" ").append(
                StringUtils.rightPad(parmID.getParmLevel(), _levelLen));

        str.append(" ").append(uiDatabaseID(parmID.getDbId()));

        return str.toString();
    }

    /**
     * Formatting function returning the formatted, but collapsed ParmID.
     * 
     * @param parmID
     * @return
     */
    public String uiParmIDcollapsed(ParmID parmID) {
        StringBuilder str = new StringBuilder();

        str.append(parmID.getParmName());

        str.append(" ").append(parmID.getParmLevel());

        str.append(" ").append(uiDatabaseIDcollapsed(parmID.getDbId()));

        return str.toString();
    }

    /**
     * Formatting function returning the formatted DatabaseID.
     * 
     * @param databaseID
     * @return
     */
    public String uiDatabaseID(DatabaseID databaseID) {
        StringBuilder str = new StringBuilder();

        // model name
        str.append(StringUtils.rightPad(databaseID.getModelName(), _modelLen));

        // type field
        if (_typeLen != 0) {
            if (databaseID.getDbType().length() > 0) {
                str.append(' ').append(
                        StringUtils.rightPad(databaseID.getDbType(), _typeLen));
            } else {
                str.append(StringUtils.rightPad("", _typeLen + 1));
            }
        }

        // model time field
        if (_dbTimeLen != 0) {
            if (!databaseID.getModelTime().equals(DatabaseID.NO_MODEL_TIME)) {
                str.append(' ').append(
                        StringUtils.rightPad(
                                sdf.format(databaseID.getModelTimeAsDate()),
                                _dbTimeLen));
            } else {
                str.append(StringUtils.rightPad("", _dbTimeLen + 1));
            }
        }

        // source
        str.append(" (")
                .append(StringUtils.rightPad(databaseID.getSiteId(), _siteIDLen))
                .append(")");

        return str.toString();
    }

    /**
     * Formatting function returning the formatted, but collapsed DatabaseID.
     * 
     * @param databaseID
     * @return
     */
    public String uiDatabaseIDcollapsed(DatabaseID databaseID) {
        StringBuilder str = new StringBuilder();

        // model name
        str.append(databaseID.getModelName());

        // type field
        if (databaseID.getDbType().length() > 0) {
            str.append('_').append(databaseID.getDbType());
        }

        // model time field
        if (!databaseID.getModelTime().equals(DatabaseID.NO_MODEL_TIME)) {
            str.append('_').append(sdf.format(databaseID.getModelTimeAsDate()));
        }

        // source
        str.append(" (")
                .append(StringUtils.rightPad(databaseID.getSiteId(), _siteIDLen))
                .append(")");

        // make a String based on the char array and return it
        return str.toString();
    }

    /**
     * Calculates the longest parm (in ascii)
     * 
     * @param ids
     */
    private void calcParmLengths(ParmID[] ids) {
        for (int i = 0; i < ids.length; i++) {
            // parmName
            if ((ids[i].getParmName().length()) > _parmNameLen) {
                _parmNameLen = ids[i].getParmName().length();
            }
            if ((ids[i].getParmLevel().length()) > _levelLen) {
                _levelLen = ids[i].getParmLevel().length();
            }
        }
    }

}
