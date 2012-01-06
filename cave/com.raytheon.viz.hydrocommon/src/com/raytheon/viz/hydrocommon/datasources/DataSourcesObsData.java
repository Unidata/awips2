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
package com.raytheon.viz.hydrocommon.datasources;

import java.util.Date;

/**
 * This class contains data sources OBS data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2008  1555       grichard     Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class DataSourcesObsData {

    /**
     * Location (Station) ID.
     */
    private String lid;

    /**
     * Address Line 1.
     */
    private String a1;

    /**
     * Address Line 2.
     */
    private String a2;

    /**
     * Address Line 3.
     */
    private String a3;

    /**
     * City.
     */
    private String city;

    /**
     * State.
     */
    private String state;

    /**
     * Zip.
     */
    private String zip;

    /**
     * Comm.
     */
    private String comm;

    /**
     * Dos.
     */
    private Date dos;

    /**
     * Gender.
     */
    private String gn;

    /**
     * Home Phone.
     */
    private String hphone;

    /**
     * First Name.
     */
    private String firstName;

    /**
     * Last Name.
     */
    private String lastName;

    /**
     * Phone.
     */
    private String phone;

    /**
     * E-mail.
     */
    private String email;

    /**
     * ORNR.
     */
    private String ornr;

    /**
     * Rate.
     */
    private double rate;

    /**
     * Recip.
     */
    private String recip;

    /**
     * Report.
     */
    private String rprt;

    /**
     * Sponsor.
     */
    private String spons;

    /**
     * SSN.
     */
    private String ssn;

    /**
     * Task.
     */
    private String task;

    public DataSourcesObsData(String lid, String a1, String a2, String a3,
            String city, String zip, Date dos, String gn, String hphone,
            String firstName, String lastName, String phone, String email,
            String ornr, double rate, String rprt, String ssn, String task) {
        this.lid = lid;
        this.a1 = a1;
        this.a2 = a2;
        this.a3 = a3;
        this.city = city;
        this.zip = zip;
        this.dos = dos;
        this.gn = gn;
        this.hphone = hphone;
        this.firstName = firstName;
        this.lastName = lastName;
        this.phone = phone;
        this.email = email;
        this.ornr = ornr;
        this.rate = rate;
        this.rprt = rprt;
        this.ssn = ssn;
        this.task = task;

    }

    /**
     * Getter for LID.
     * 
     * @return lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * Getter for Address Line 1.
     * 
     * @return a1
     */
    public String getA1() {
        return a1;
    }

    /**
     * Getter for Address Line 2.
     * 
     * @return a2
     */
    public String getA2() {
        return a2;
    }

    /**
     * Getter for Address Line 3.
     * 
     * @return a3
     */
    public String getA3() {
        return a3;
    }

    /**
     * Getter for city.
     * 
     * @return city
     */
    public String getCity() {
        return city;
    }

    /**
     * Getter for state.
     * 
     * @return state
     */
    public String getState() {
        return state;
    }

    /**
     * Getter for zip.
     * 
     * @return zip
     */
    public String getZip() {
        return zip;
    }

    /**
     * Getter for comm.
     * 
     * @return comm
     */
    public String getComm() {
        return comm;
    }

    /**
     * Getter for dos.
     * 
     * @return dos
     */
    public Date getDos() {
        return dos;
    }

    /**
     * Getter for gender.
     * 
     * @return gn
     */
    public String getGn() {
        return gn;
    }

    /**
     * Getter for home phone.
     * 
     * @return hphone
     */
    public String getHphone() {
        return hphone;
    }

    /**
     * Getter for first name.
     * 
     * @return firstName
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * Getter for last name.
     * 
     * @return lastName
     */
    public String getLastName() {
        return lastName;
    }

    /**
     * Getter for phone.
     * 
     * @return phone
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Getter for e-mail.
     * 
     * @return email
     */
    public String getEmail() {
        return email;
    }

    /**
     * Getter for ornr.
     * 
     * @return ornr
     */
    public String getOrnr() {
        return ornr;
    }

    /**
     * Getter for rate.
     * 
     * @return rate
     */
    public double getRate() {
        return rate;
    }

    /**
     * Getter for recip.
     * 
     * @return recip
     */
    public String getRecip() {
        return recip;
    }

    /**
     * Getter for report.
     * 
     * @return rprt
     */
    public String getRprt() {
        return rprt;
    }

    /**
     * Getter for spons.
     * 
     * @return spons
     */
    public String getSpons() {
        return spons;
    }

    /**
     * Getter for ssn.
     * 
     * @return ssn
     */
    public String getSsn() {
        return ssn;
    }

    /**
     * Getter for task.
     * 
     * @return task
     */
    public String getTask() {
        return task;
    }

}
