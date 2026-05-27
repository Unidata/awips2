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
 * this class contains the Observer data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 9, 2008	1802    	askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class ObserverData extends HydroDBData implements IHydroDBData {

    /**
     * lid character varying(8) NOT NULL,
     */
    private String lid;

    /**
     * a1 character varying(30),
     */
    private String addressLine1;

    /**
     * a2 character varying(30),
     */
    private String addressLine2;

    /**
     * a3 character varying(30),
     */
    private String addressLine3;

    /**
     * city character varying(30),
     */
    private String city;

    /**
     * state character varying(2) NOT NULL,
     */
    private String state;

    /**
     * zip character varying(10),
     */
    private String zipCode;

    /**
     * comm character varying(10) NOT NULL,
     */
    private String comms;

    /**
     * dos date,
     */
    private Date dateOfService;

    /**
     * gn character varying(1),
     */
    private String gender;

    /**
     * hphone character varying(18),
     */
    private String homePhone;

    /**
     * firstname character varying(12),
     */
    private String firstName;

    /**
     * lastname character varying(28),
     */
    private String lastName;

    /**
     * phone character varying(18),
     */
    private String workPhone;

    /**
     * email character varying(60),
     */
    private String email;

    /**
     * ornr character varying(4),
     */
    private String cd404;

    /**
     * rate double precision,
     */
    private double rate;

    /**
     * recip character varying(15) NOT NULL,
     */
    private String recipient;

    /**
     * rprt character varying(60),
     */
    private String report;

    /**
     * spons character varying(7) NOT NULL,
     */
    private String sponsor;

    /**
     * ssn character varying(11),
     */
    private String ssn;

    /**
     * tsk character varying(13),
     */
    private String taskNumber;

    /**
     * Date format
     */
    private SimpleDateFormat dateFormat;

    /**
     * Database table name
     */
    private String dbTable = "observer";

    /**
     * Constructor
     */
    public ObserverData() {
        initDateFormat();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public ObserverData(QueryResultRow data, Map<String, Integer> dataMap) {
        initDateFormat();

        setLid(getDBValue("lid", data, dataMap, ""));
        setAddressLine1(getDBValue("a1", data, dataMap, ""));
        setAddressLine2(getDBValue("a2", data, dataMap, ""));
        setAddressLine3(getDBValue("a3", data, dataMap, ""));
        setCity(getDBValue("city", data, dataMap, ""));
        setState(getDBValue("state", data, dataMap, ""));
        setZipCode(getDBValue("zip", data, dataMap, ""));
        setComms(getDBValue("comm", data, dataMap, ""));
        setDateOfService(getDBValue("dos", data, dataMap, (Date) null));
        setGender(getDBValue("gn", data, dataMap, ""));
        setHomePhone(getDBValue("hphone", data, dataMap, ""));
        setFirstName(getDBValue("firstname", data, dataMap, ""));
        setLastName(getDBValue("lastname", data, dataMap, ""));
        setWorkPhone(getDBValue("phone", data, dataMap, ""));
        setEmail(getDBValue("email", data, dataMap, ""));
        setCd404(getDBValue("ornr", data, dataMap, ""));
        setRate(getDBValue("rate", data, dataMap, Double
                .valueOf(HydroConstants.MISSING_VALUE)));
        setRecipient(getDBValue("recip", data, dataMap, ""));
        setReport(getDBValue("rprt", data, dataMap, ""));
        setSponsor(getDBValue("spons", data, dataMap, ""));
        setSsn(getDBValue("ssn", data, dataMap, ""));
        setTaskNumber(getDBValue("tsk", data, dataMap, ""));
    }

    /**
     * Initialize the date format
     */
    private void initDateFormat() {
        dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the addressLine1
     */
    public String getAddressLine1() {
        return addressLine1;
    }

    /**
     * @param addressLine1
     *            the addressLine1 to set
     */
    public void setAddressLine1(String addressLine1) {
        this.addressLine1 = addressLine1;
    }

    /**
     * @return the addressLine2
     */
    public String getAddressLine2() {
        return addressLine2;
    }

    /**
     * @param addressLine2
     *            the addressLine2 to set
     */
    public void setAddressLine2(String addressLine2) {
        this.addressLine2 = addressLine2;
    }

    /**
     * @return the addressLine3
     */
    public String getAddressLine3() {
        return addressLine3;
    }

    /**
     * @param addressLine3
     *            the addressLine3 to set
     */
    public void setAddressLine3(String addressLine3) {
        this.addressLine3 = addressLine3;
    }

    /**
     * @return the city
     */
    public String getCity() {
        return city;
    }

    /**
     * @param city
     *            the city to set
     */
    public void setCity(String city) {
        this.city = city;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the zipCode
     */
    public String getZipCode() {
        return zipCode;
    }

    /**
     * @param zipCode
     *            the zipCode to set
     */
    public void setZipCode(String zipCode) {
        this.zipCode = zipCode;
    }

    /**
     * @return the comms
     */
    public String getComms() {
        return comms;
    }

    /**
     * @param comms
     *            the comms to set
     */
    public void setComms(String comms) {
        this.comms = comms;
    }

    /**
     * @return the dateOfService
     */
    public Date getDateOfService() {
        return dateOfService;
    }

    /**
     * @param dateOfService
     *            the dateOfService to set
     */
    public void setDateOfService(Date dateOfService) {
        this.dateOfService = dateOfService;
    }

    /**
     * @return the gender
     */
    public String getGender() {
        return gender;
    }

    /**
     * @param gender
     *            the gender to set
     */
    public void setGender(String gender) {
        this.gender = gender;
    }

    /**
     * @return the homePhone
     */
    public String getHomePhone() {
        return homePhone;
    }

    /**
     * @param homePhone
     *            the homePhone to set
     */
    public void setHomePhone(String homePhone) {
        this.homePhone = homePhone;
    }

    /**
     * @return the firstName
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName
     *            the firstName to set
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return the lastName
     */
    public String getLastName() {
        return lastName;
    }

    /**
     * @param lastName
     *            the lastName to set
     */
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    /**
     * @return the workPhone
     */
    public String getWorkPhone() {
        return workPhone;
    }

    /**
     * @param workPhone
     *            the workPhone to set
     */
    public void setWorkPhone(String workPhone) {
        this.workPhone = workPhone;
    }

    /**
     * @return the email
     */
    public String getEmail() {
        return email;
    }

    /**
     * @param email
     *            the email to set
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * @return the cd404
     */
    public String getCd404() {
        return cd404;
    }

    /**
     * @param cd404
     *            the cd404 to set
     */
    public void setCd404(String cd404) {
        this.cd404 = cd404;
    }

    /**
     * @return the rate
     */
    public double getRate() {
        return rate;
    }

    /**
     * @param rate
     *            the rate to set
     */
    public void setRate(double rate) {
        this.rate = rate;
    }

    /**
     * @return the recipient
     */
    public String getRecipient() {
        return recipient;
    }

    /**
     * @param recipient
     *            the recipient to set
     */
    public void setRecipient(String recipient) {
        this.recipient = recipient;
    }

    /**
     * @return the report
     */
    public String getReport() {
        return report;
    }

    /**
     * @param report
     *            the report to set
     */
    public void setReport(String report) {
        this.report = report;
    }

    /**
     * @return the sponsor
     */
    public String getSponsor() {
        return sponsor;
    }

    /**
     * @param sponsor
     *            the sponsor to set
     */
    public void setSponsor(String sponsor) {
        this.sponsor = sponsor;
    }

    /**
     * @return the ssn
     */
    public String getSsn() {
        return ssn;
    }

    /**
     * @param ssn
     *            the ssn to set
     */
    public void setSsn(String ssn) {
        this.ssn = ssn;
    }

    /**
     * @return the taskNumber
     */
    public String getTaskNumber() {
        return taskNumber;
    }

    /**
     * @param taskNumber
     *            the taskNumber to set
     */
    public void setTaskNumber(String taskNumber) {
        this.taskNumber = taskNumber;
    }

    @Override
    public String getConstrainedSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, a1, a2, a3, city, state, zip, comm, dos, gn, hphone, firstname, "
                + "lastname, phone, email, ornr, rate, recip, rprt, spons, ssn, tsk";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM ");
        rval.append(dbTable);
        rval.append(" WHERE lid=");
        rval.append(getDBString(lid));

        return rval.toString();
    }

    @Override
    public String getDeleteStatement() {
        return "DELETE FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getExistsStatement() {
        return "SELECT lid FROM " + dbTable + " WHERE " + getPKStatement();
    }

    @Override
    public String getInsertStatement() {
        String columns = "lid, a1, a2, a3, city, state, zip, comm, dos, gn, hphone, firstname, "
                + "lastname, phone, email, ornr, rate, recip, rprt, spons, ssn, tsk";

        String rval = "INSERT INTO "
                + dbTable
                + " ( "
                + columns
                + " ) VALUES ( %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )";

        rval = String
                .format(rval, getDBString(lid), getDBString(addressLine1),
                        getDBString(addressLine2), getDBString(addressLine3),
                        getDBString(city), getDBString(state),
                        getDBString(zipCode), getDBString(comms), getDBString(
                                dateOfService, dateFormat),
                        getDBString(gender), getDBString(homePhone),
                        getDBString(firstName), getDBString(lastName),
                        getDBString(workPhone), getDBString(email),
                        getDBString(cd404), getDBString(rate),
                        getDBString(recipient), getDBString(report),
                        getDBString(sponsor), getDBString(ssn),
                        getDBString(taskNumber));

        return rval;
    }

    @Override
    public String getPKStatement() {
        return "lid=" + getDBString(lid);
    }

    @Override
    public String getSelectStatement() {
        StringBuffer rval = new StringBuffer();

        String columns = "lid, a1, a2, a3, city, state, zip, comm, dos, gn, hphone, firstname, "
                + "lastname, phone, email, ornr, rate, recip, rprt, spons, ssn, tsk";

        rval.append("SELECT ");
        rval.append(columns);
        rval.append(" FROM ");
        rval.append(dbTable);

        return rval.toString();
    }

    @Override
    public String getUpdateStatement() {
        // Set the basic update statement
        String rval = "UPDATE "
                + dbTable
                + " SET lid=%s, a1=%s, a2=%s, a3=%s, city=%s, state=%s, zip=%s, comm=%s, dos=%s, gn=%s, hphone=%s, firstname=%s, "
                + "lastname=%s, phone=%s, email=%s, ornr=%s, rate=%s, recip=%s, rprt=%s, spons=%s, ssn=%s, tsk=%s WHERE %s";

        // Populate the values
        rval = String.format(rval, getDBString(lid), getDBString(addressLine1),
                getDBString(addressLine2), getDBString(addressLine3),
                getDBString(city), getDBString(state), getDBString(zipCode),
                getDBString(comms), getDBString(dateOfService, dateFormat),
                getDBString(gender), getDBString(homePhone),
                getDBString(firstName), getDBString(lastName),
                getDBString(workPhone), getDBString(email), getDBString(cd404),
                getDBString(rate), getDBString(recipient), getDBString(report),
                getDBString(sponsor), getDBString(ssn),
                getDBString(taskNumber), getPKStatement());

        return rval;
    }
}
