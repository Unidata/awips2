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
package com.raytheon.uf.edex.ohd.reportalarm;


import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.AlertalarmvalId;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Self sorting data structure to cache and classify alertalarmval data into
 * buckets or groups. Sorting occurs upon row data insert.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

class AlertalarmRecord {

    private static AlertalarmRecord instance = null;

    private Map<String, List<Alertalarmval>> groups;

    private static final String tokenizer = ShefConstants.SLASH;

    private AlertalarmRecord() {
        groups = new HashMap<String, List<Alertalarmval>>();
    }

    public static AlertalarmRecord newInstance() {
        if (instance == null)
            instance = new AlertalarmRecord();
        return instance;
    }

    /**
     * Inserts this row data into the record. First, determine the row data's
     * group and if that group correspond to an existing bucket(key) then assign
     * the rowData to it, otherwise create a new bucket with the row data as its
     * first element.
     * 
     * @param rowData
     */
    public void put(Object[] rowData) {
        String group = getGroup(rowData);
        Alertalarmval value = getValue(rowData);
        if (groups.containsKey(group)) {
            put(group, value);
        } else {
            create(group, value);
        }
    }

    /**
     * Obtain the set of row data groups contained in this record
     * 
     * @return - the groups
     */
    public Set<String> getGroups() {
        return groups.keySet();
    }

    /**
     * Obtain a list of row data contained in this bucket/group.
     * 
     * @param bucket
     * @return - the list of row data
     */
    public List<Alertalarmval> getGroupData(String bucket) {
        return groups.get(bucket);
    }

    private void put(String bucket, Alertalarmval element) {
        groups.get(bucket).add(element);
    }

    private void create(String bucket, Alertalarmval first) {
        List<Alertalarmval> groupData = new ArrayList<Alertalarmval>();
        groupData.add(first);
        groups.put(bucket, groupData);
    }

    /**
     * extract the lid/pe/ts/aa_check combination as one single string for this
     * alertalarm row data
     * 
     * @param aaRow
     *            - the alertalarmval row data
     * @return - the corresponding lid/pe/ts/aa_check
     */
    private String getGroup(Object[] aaRow) {
        StringBuilder group = new StringBuilder();
        group.append((String) aaRow[0]);
        group.append(tokenizer);
        group.append((String) aaRow[1]);
        group.append(tokenizer);
        group.append((String) aaRow[3]);
        group.append(tokenizer);
        group.append((String) aaRow[9]);
        return group.toString();
    }

    /**
     * turn a row data to type Alertalarmval
     * 
     * @param aaRow
     * @return
     */
    private Alertalarmval getValue(Object[] aaRow) {

        AlertalarmvalId aaId = new AlertalarmvalId();
        aaId.setLid((String) aaRow[0]);
        aaId.setPe((String) aaRow[1]);
        aaId.setDur((Short) aaRow[2]);
        aaId.setTs((String) aaRow[3]);
        aaId.setExtremum((String) aaRow[4]);
        aaId.setProbability((Float) aaRow[5]);
        Date validtime = new Date(getMilliSeconds(aaRow[6]));
        aaId.setValidtime(validtime);
        Date basistime = new Date(getMilliSeconds(aaRow[7]));
        aaId.setBasistime(basistime);
        aaId.setAaCateg((String) aaRow[8]);
        aaId.setAaCheck((String) aaRow[9]);

        Alertalarmval aaVal = new Alertalarmval(aaId);
        aaVal.setValue((Double) aaRow[10]);
        aaVal.setSupplValue((Double) aaRow[11]);
        aaVal.setShefQualCode((String) aaRow[12]);
        aaVal.setQualityCode((Integer) aaRow[13]);
        aaVal.setRevision((Short) aaRow[14]);
        aaVal.setProductId((String) aaRow[15]);
        Date producttime = new Date(getMilliSeconds(aaRow[16]));
        aaVal.setProducttime(producttime);
        Date postingtime = new Date(getMilliSeconds(aaRow[17]));
        aaVal.setPostingtime(postingtime);
        Date actiontime = new Date(getMilliSeconds(aaRow[18]));
        aaVal.setActionTime(actiontime);

        return aaVal;
    }
    
    private long getMilliSeconds(Object o) {
        if(o instanceof java.sql.Timestamp){
            java.sql.Timestamp t = (java.sql.Timestamp) o;
            return t.getTime() + (t.getNanos() / 1000000);
        }
        return 0;
    }
    
    
    /**
     * @return - a print ready string for
     * the list of alertalarmval row data
     * contained in this record.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Set<String> grps = getGroups();
        for(String grp : grps) 
            sb.append(grpDataToString(grp));
        return sb.toString();
    }

    private String grpDataToString(String grp){
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT);      
        StringBuilder sb = new StringBuilder("!");
        List<Alertalarmval> grpData = getGroupData(grp);
        for(Alertalarmval aav : grpData){
            sb.append("|");
            sb.append(aav.getId().getLid());
            sb.append("|");
            sb.append(aav.getId().getPe());
            sb.append("|");
            sb.append(aav.getId().getDur());
            sb.append("|");
            sb.append(aav.getId().getTs());
            sb.append("|");
            sb.append(aav.getId().getExtremum());
            sb.append("|");
            sb.append(aav.getId().getProbability());
            sb.append("|");
            sb.append(df.format(aav.getId().getValidtime()));
            sb.append("|");
            sb.append(df.format(aav.getId().getBasistime()));
            sb.append("|");
            sb.append(aav.getValue());
            sb.append("|");
            sb.append(aav.getSupplValue());
            sb.append("|");
            sb.append(aav.getShefQualCode());
            sb.append("|");
            sb.append(aav.getQualityCode());
            sb.append("|");
            sb.append(aav.getRevision());
            sb.append("|");
            sb.append(aav.getProductId());
            sb.append("|");
            sb.append(df.format(aav.getProducttime()));
            sb.append("|");
            sb.append(df.format(aav.getPostingtime()));
            sb.append("|");
            sb.append(df.format(aav.getActionTime()));
            sb.append("|");
            sb.append(aav.getId().getAaCateg());
            sb.append("|");
            sb.append(aav.getId().getAaCheck());
            sb.append("|");
            sb.append(Constants.EOL);
        }    
        
        return sb.toString();
    }
    
}