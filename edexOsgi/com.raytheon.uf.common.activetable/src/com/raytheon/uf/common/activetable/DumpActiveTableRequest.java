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
package com.raytheon.uf.common.activetable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A request to dump the active table, from Thrift.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class DumpActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private String[] actions;

    @DynamicSerializeElement
    private String[] etns;

    @DynamicSerializeElement
    private String fileContent;

    @DynamicSerializeElement
    private String fileName;

    @DynamicSerializeElement
    private String fromSite;

    @DynamicSerializeElement
    private String[] ids;

    @DynamicSerializeElement
    private String mode;

    @DynamicSerializeElement
    private String[] phens;

    @DynamicSerializeElement
    private String[] pils;

    @DynamicSerializeElement
    private String[] sigs;

    @DynamicSerializeElement
    private String[] sites;

    /**
     * Get the actions to include in the dump. If this is null or empty, include
     * ALL actions.
     * 
     * @return the actions
     */
    public String[] getActions() {
        return actions;
    }

    /**
     * @return the etns
     */
    public String[] getEtns() {
        return etns;
    }

    /**
     * @return the fileContent
     */
    public String getFileContent() {
        return fileContent;
    }

    /**
     * @return the fileName
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @return the fromSite
     */
    public String getFromSite() {
        return fromSite;
    }

    /**
     * Get the ids to include in the dump. If this is null or empty, include ALL
     * ids.
     * 
     * @return the ids
     */
    public String[] getIds() {
        return ids;
    }

    /**
     * @return the mode
     */
    public String getMode() {
        return mode;
    }

    /**
     * Get the phens to include in the dump. If this is null or empty, include
     * ALL phens.
     * 
     * @return the phens
     */
    public String[] getPhens() {
        return phens;
    }

    /**
     * Get the pils to include in the dump. If this is null or empty, include
     * ALL pils.
     * 
     * @return the pils
     */
    public String[] getPils() {
        return pils;
    }

    /**
     * Get the sigs to include in the dump. If this is null or empty, include
     * ALL sigs.
     * 
     * @return the sigs
     */
    public String[] getSigs() {
        return sigs;
    }

    /**
     * Get the site codes to include in the dump. This should never be null or
     * empty. When no sites are given by the user, it is the client's
     * responsibility to choose a default set of site IDs.
     * 
     * @return the sites
     */
    public String[] getSites() {
        return sites;
    }

    /**
     * @param actions
     *            the actions to set
     */
    public void setActions(String[] actions) {
        this.actions = actions;
    }

    /**
     * @param etns
     *            the etns to set
     */
    public void setEtns(String[] etns) {
        this.etns = etns;
    }

    /**
     * @param fileContent
     *            the fileContent to set
     */
    public void setFileContent(String fileContent) {
        this.fileContent = fileContent;
    }

    /**
     * @param fileName
     *            the fileName to set
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @param fromSite
     *            the fromSite to set
     */
    public void setFromSite(String fromSite) {
        this.fromSite = fromSite;
    }

    /**
     * @param ids
     *            the ids to set
     */
    public void setIds(String[] ids) {
        this.ids = ids;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(String mode) {
        this.mode = mode;
    }

    /**
     * @param phens
     *            the phens to set
     */
    public void setPhens(String[] phens) {
        this.phens = phens;
    }

    /**
     * @param pils
     *            the pils to set
     */
    public void setPils(String[] pils) {
        this.pils = pils;
    }

    /**
     * @param sigs
     *            the sigs to set
     */
    public void setSigs(String[] sigs) {
        this.sigs = sigs;
    }

    /**
     * @param sites
     *            the sites to set
     */
    public void setSites(String[] sites) {
        this.sites = sites;
    }

}
