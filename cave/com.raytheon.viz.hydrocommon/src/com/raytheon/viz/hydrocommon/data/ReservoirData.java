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
package com.raytheon.viz.hydrocommon.data;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class contains the reservoir data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2008 1782       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ReservoirData extends HydroDBData implements IHydroDBData {

    /**
     * lid - Location/Station ID
     */
    private String lid;

    /**
     * name - Name
     */
    private String name;

    /**
     * type - Type
     */
    private String type;

    /**
     * owner - Owner
     */
    private String owner;

    /**
     * deadpool - Dead pool
     */
    private double deadpool;

    /**
     * conserpool - Conservation pool
     */
    private double conserpool;

    /**
     * floodpool - Flood pool
     */
    private double floodpool;

    /**
     * spillway - spillway
     */
    private double spillway;

    /**
     * sill - sill
     */
    private double sill;

    /**
     * top - top
     */
    private double top;

    /**
     * surchg - surcharge
     */
    private double surchg;

    /**
     * elev - elevation
     */
    private double elev;

    /**
     * gates - gates
     */
    private int gates;

    /**
     * impounded - impounded
     */
    private Date impounded;

    /**
     * uses - uses
     */
    private String uses;

    /**
     * damids - state abbreviation in national inventory of dams
     */
    private String damids;

    /**
     * damidn - numeric identifier in national inventory of dams
     */
    private String damidn;

    /**
     * Formats the date for the DB.
     */
    private SimpleDateFormat dateFormat;

    /**
     * Constructor.
     */
    public ReservoirData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call.
     * 
     * @param data
     *            The raw data from the database.
     */
    public ReservoirData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setName(getDBValue("name", data, dataMap, ""));
        setType(getDBValue("type", data, dataMap, ""));
        setOwner(getDBValue("owner", data, dataMap, ""));
        setDeadpool(getDBValue("deadpool", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setConserpool(getDBValue("conserpool", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setFloodpool(getDBValue("floodpool", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setSpillway(getDBValue("spillway", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setSill(getDBValue("sill", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setTop(getDBValue("top", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setSurchg(getDBValue("surchg", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setElev(getDBValue("elev", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setGates(getDBValue("gates", data, dataMap, Integer
                .valueOf(HydroConstants.MISSING_VALUE)));
        setImpounded(getDBValue("impounded", data, dataMap, (Date) null));
        setUses(getDBValue("uses", data, dataMap, ""));
        setDamids(getDBValue("damids", data, dataMap, ""));
        setDamidn(getDBValue("damidn", data, dataMap, ""));
    }

    /**
     * Method to format the date.
     */
    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Get the lid.
     * 
     * @return The lid.
     */
    public String getLid() {
        return lid;
    }

    /**
     * Set the lid.
     * 
     * @param lid
     *            The lid.
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * Get the name.
     * 
     * @return The name.
     */
    public String getName() {
        return name;
    }

    /**
     * Set the name.
     * 
     * @param name
     *            The name.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get the type.
     * 
     * @return The type.
     */
    public String getType() {
        return type;
    }

    /**
     * Set the type.
     * 
     * @param type
     *            The type.
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Get the owner.
     * 
     * @return The owner.
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Set the owner.
     * 
     * @param owner
     *            The owner.
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * Get the deadpool.
     * 
     * @return The deadpool.
     */
    public double getDeadpool() {
        return deadpool;
    }

    /**
     * Set the dead pool.
     * 
     * @param deadpool
     *            The dead pool.
     */
    public void setDeadpool(double deadpool) {
        this.deadpool = deadpool;
    }

    /**
     * Get the conserpool.
     * 
     * @return The conserpool.
     */
    public double getConserpool() {
        return conserpool;
    }

    /**
     * Set the conservation pool.
     * 
     * @param conserpool
     *            The conservation pool.
     */
    public void setConserpool(double conserpool) {
        this.conserpool = conserpool;
    }

    /**
     * Get the flood.
     * 
     * @return The flood.
     */
    public double getFloodpool() {
        return floodpool;
    }

    /**
     * Set the flood.
     * 
     * @param flood
     *            The flood.
     */
    public void setFloodpool(double floodpool) {
        this.floodpool = floodpool;
    }

    /**
     * Get the spillway.
     * 
     * @return The spillway.
     */
    public double getSpillway() {
        return spillway;
    }

    /**
     * Set the spillway.
     * 
     * @param spillway
     *            The spillway.
     */
    public void setSpillway(double spillway) {
        this.spillway = spillway;
    }

    /**
     * Get the sill.
     * 
     * @return The sill.
     */
    public double getSill() {
        return sill;
    }

    /**
     * Set the sill.
     * 
     * @param sill
     *            The sill.
     */
    public void setSill(double sill) {
        this.sill = sill;
    }

    /**
     * Get the top.
     * 
     * @return The top.
     */
    public double getTop() {
        return top;
    }

    /**
     * Set the top.
     * 
     * @param top
     *            The top.
     */
    public void setTop(double top) {
        this.top = top;
    }

    /**
     * Get the surchg.
     * 
     * @return The surchg.
     */
    public double getSurchg() {
        return surchg;
    }

    /**
     * Set the surcharge.
     * 
     * @param surch
     *            The surcharge.
     */
    public void setSurchg(double surchg) {
        this.surchg = surchg;
    }

    /**
     * Get the elev.
     * 
     * @return The elev.
     */
    public double getElev() {
        return elev;
    }

    /**
     * Set the elevation.
     * 
     * @param elev
     *            The elevation.
     */
    public void setElev(double elev) {
        this.elev = elev;
    }

    /**
     * Get the gates.
     * 
     * @return The gates.
     */
    public int getGates() {
        return gates;
    }

    /**
     * Set the gates.
     * 
     * @param gates
     *            The gates.
     */
    public void setGates(int gates) {
        this.gates = gates;
    }

    /**
     * Get the impounded date.
     * 
     * @return The impounded date.
     */
    public Date getImpounded() {
        return impounded;
    }

    /**
     * Set the impound date.
     * 
     * @param impounded
     *            The impound date.
     */
    public void setImpounded(Date impounded) {
        this.impounded = impounded;
    }

    /**
     * Get the uses.
     * 
     * @return The uses.
     */
    public String getUses() {
        return uses;
    }

    /**
     * Set the uses.
     * 
     * @param uses
     *            The uses.
     */
    public void setUses(String uses) {
        this.uses = uses;
    }

    /**
     * Get the state identifier.
     * 
     * @return The state identifier.
     */
    public String getDamids() {
        return damids;
    }

    /**
     * Set the state identifier.
     * 
     * @param damids
     *            The state identifier.
     */
    public void setDamids(String damids) {
        this.damids = damids;
    }

    /**
     * Get the numeric identifier.
     * 
     * @return The numeric identifier.
     */
    public String getDamidn() {
        return damidn;
    }

    /**
     * Set the numeric identifier.
     * 
     * @param damidn
     *            The numeric identifier.
     */
    public void setDamidn(String damidn) {
        this.damidn = damidn;
    }

    @Override
    public String getPKStatement() {
        String pkString = "lid=%s";
        return String.format(pkString, getDBString(lid));
    }

    @Override
    public String getSelectStatement() {
        return "SELECT lid, name, type, owner, deadpool, conserpool, floodpool, spillway, sill, top, surchg, elev, gates, impounded, uses, damids, damidn FROM reservoir";
    }

    @Override
    public String getExistsStatement() {
        return getSelectStatement() + " WHERE " + getPKStatement();
    }

    @Override
    public String getConstrainedSelectStatement() {
        return getSelectStatement() + " WHERE lid='" + lid + "'";
    }

    @Override
    public String getInsertStatement() {
        String rval = "INSERT INTO reservoir ( lid, name, type, owner, deadpool, conserpool, floodpool, spillway, sill, top, surchg, elev, gates, impounded, uses, damids, damidn ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String.format(rval, getDBString(lid), getDBString(name),
                getDBString(type), getDBString(owner), getDBString(deadpool),
                getDBString(conserpool), getDBString(floodpool),
                getDBString(spillway), getDBString(sill), getDBString(top),
                getDBString(surchg), getDBString(elev), getDBString(gates),
                getDBString(impounded, dateFormat), getDBString(uses),
                getDBString(damids), getDBString(damidn));

        return rval;
    }

    @Override
    public String getUpdateStatement() {
        String rval = "UPDATE reservoir SET lid=%s, name=%s, type=%s, owner=%s, deadpool=%s, conserpool=%s, floodpool=%s, spillway=%s, sill=%s, top=%s, surchg=%s, elev=%s, gates=%s, impounded=%s, uses=%s, damids=%s, damidn=%s WHERE %s";

        rval = String.format(rval, getDBString(lid), getDBString(name),
                getDBString(type), getDBString(owner), getDBString(deadpool),
                getDBString(conserpool), getDBString(floodpool),
                getDBString(spillway), getDBString(sill), getDBString(top),
                getDBString(surchg), getDBString(elev), getDBString(gates),
                getDBString(impounded, dateFormat), getDBString(uses),
                getDBString(damids), getDBString(damidn), getPKStatement());

        return rval;
    }

    @Override
    public String getDeleteStatement() {
        return String
                .format("DELETE FROM reservoir WHERE %s", getPKStatement());
    }

}
