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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.lang.mutable.MutableInt;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.gfe.core.IParmManager;

/**
 * A utility class to map string expressions to ParmIDs
 * <p>
 * ABVParmID is constructed with a IParmManager pointer. It uses this object in
 * its parse() method to map strings in the form of
 * "T_SFC_BOU_GRID__Fcst_00000000_0000" to a ParmID. Fields of this string may
 * be omitted. When a field is omitted, it defaults to the value of the mutable
 * database ID. So, strings like this are valid:
 * <ul>
 * <li>"T"
 * <li>"T_SFC"
 * <li>"T_NAM12"
 * </ul>
 * 
 * In addition, if the last field is a single integer, then it will be the
 * offset in time of the available databases. 0 is the most current and 1
 * indicates one database older, etc...
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ABVParmID {

    private static final String ISDIGIT = "\\p{Digit}+";

    private enum Token {
        SID, TYPE, MODEL, TIME, OFFSET
    };

    private IParmManager parmMgr;

    public ABVParmID(IParmManager parmMgr) {
        this.parmMgr = parmMgr;
    }

    private List<Token> tokenMatch(DatabaseID dbid, List<String> lst) {
        List<Token> rval = new ArrayList<Token>(lst.size());
        for (int i = 0; i < lst.size(); i++) {
            String token = lst.get(i);
            if (dbid.getSiteId().equals(token)) {
                rval.add(Token.SID);
            } else if (dbid.getDbType().equals(token)) {
                rval.add(Token.TYPE);
            } else if (dbid.getModelName().equals(token)) {
                rval.add(Token.MODEL);
            } else if (dbid.getModelTime().equals(token)) {
                rval.add(Token.TIME);
            } else if ((i == lst.size() - 1) && offsetCheck(token)) {
                rval.add(Token.OFFSET);
            } else {
                return Collections.emptyList();
            }
        }

        return rval;
    }

    private boolean offsetCheck(String str) {
        if (!str.matches(ISDIGIT)) {
            return false;
        }

        MutableInt tmp = new MutableInt();
        return isInt(str, tmp);
    }

    private boolean isInt(String str, MutableInt i) {
        try {
            Integer is = new Integer(str);
            i.setValue(is.intValue());
        } catch (NumberFormatException e) {
            return false;
        }

        return true;
    }

    private void addDefaults(List<Token> sot, List<String> lst) {
        DatabaseID mdbid = parmMgr.getMutableDatabase();
        if (!sot.contains(Token.SID)) {
            sot.add(0, Token.SID);
            lst.add(0, mdbid.getSiteId());
        }
        if (!sot.contains(Token.TYPE)) {
            sot.add(0, Token.TYPE);
            lst.add(0, mdbid.getDbType());
        }
        if (!sot.contains(Token.MODEL)) {
            sot.add(0, Token.MODEL);
            lst.add(0, mdbid.getModelName());
        }
    }

    private boolean match(DatabaseID dbid, List<String> lst) {
        List<Token> sot = tokenMatch(dbid, lst);
        if (sot.isEmpty()) {
            return false;
        }

        // add in the defaults
        addDefaults(sot, lst);

        for (int i = 0; i < sot.size(); i++) {
            switch (sot.get(i)) {
            case SID:
                if (!dbid.getSiteId().equals(lst.get(i))) {
                    return false;
                }
                break;
            case TYPE:
                if (!dbid.getDbType().equals(lst.get(i))) {
                    return false;
                }
                break;
            case MODEL:
                if (!dbid.getModelName().equals(lst.get(i))) {
                    return false;
                }
                break;
            case TIME:
                if (!dbid.getModelTime().equals(lst.get(i))) {
                    return false;
                }
                break;
            case OFFSET:
                MutableInt offset = new MutableInt();
                isInt(lst.get(i), offset);
                lst.set(i, new Integer(offset.intValue() - 1).toString());
                if (offset.intValue() != 0) {
                    return false;
                }
                break;
            default:
                return false;
            }
        }

        return true;
    }

    private DatabaseID dbidCheck(List<String> lst) {
        if (lst.isEmpty()) {
            return parmMgr.getMutableDatabase();
        }

        List<DatabaseID> dbs = new ArrayList<DatabaseID>(
                parmMgr.getAvailableDbs());
        Collections.sort(dbs);
        for (DatabaseID dbid : dbs) {
            if (match(dbid, lst)) {
                return dbid;
            }
        }

        return new DatabaseID();
    }

    private void checkModelTime(List<String> lst) {
        // The last two tokens *might* be a model time. But
        // a model time uses '_' for its own delimiter. So,
        // check to see if the last two can make a valid time
        // if so put the tokens back together
        if (lst.size() < 2) {
            return;
        }

        if ((lst.get(lst.size() - 1).length() != 4)
                || (lst.get(lst.size() - 2).length() != 8)) {
            return;
        }

        if ((!lst.get(lst.size() - 1).matches(ISDIGIT))
                || (!lst.get(lst.size() - 2).matches(ISDIGIT))) {
            return;
        }

        // Ok. The last two tokens seem to be a model time.
        String hhmm = lst.remove(lst.size() - 1);
        String yyyymmdd = lst.remove(lst.size() - 1);
        lst.add(yyyymmdd + "_" + hhmm);
    }

    private ParmID pidCandidate(String str) {
        StringTokenizer st = new StringTokenizer(str, "_");

        // The first token is always the parmName
        // We just need to figure out what all the other bits
        // are. In the end we need a parmName, level, and dbid
        // These are the default values.
        // we use the ternary operator here to avoid try-catching for a
        // NoSuchElementException
        String parmName = st.hasMoreTokens() ? st.nextToken() : "";
        String level = "SFC";
        DatabaseID dbid = parmMgr.getMutableDatabase();

        List<String> tlist = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            tlist.add(st.nextToken());
        }

        checkModelTime(tlist);

        // The next token (if any) will either be a level or series of
        // dbid tokens.
        if (!tlist.isEmpty()) {
            DatabaseID id = dbidCheck(tlist);
            if (id.isValid()) {
                dbid = id;
            } else {
                // assume tlist[0] is a level
                level = tlist.remove(0);
                // One last chance to find the dbid
                id = dbidCheck(tlist);
                if (!id.isValid()) {
                    return new ParmID();
                }
                dbid = id;
            }
        }

        return new ParmID(parmName, dbid, level);
    }

    /**
     * Attempts to match the given expression to an available Parm.
     * 
     * @param str
     *            Expression representing the Parm to be matched against.
     * @return If the string is matched to a Parm, the ParmID is returned. Else,
     *         an invalid ParmID is returned.
     */
    public ParmID parse(String str) {
        ParmID pidc = pidCandidate(str);
        if (parmMgr.isParmInDatabase(pidc)) {
            return pidc;
        }
        return new ParmID();
    }
}
