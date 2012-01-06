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
package com.raytheon.viz.avnconfig;

import java.util.ArrayList;
import java.util.Iterator;

import com.raytheon.viz.avnconfig.AvnConfigConstants.RuleType;

/**
 * A class containing available method data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class MethodData implements Comparable<MethodData> {
    /**
     * Method name.
     */
    private String methodName = "";

    /**
     * Rule/Method comment.
     */
    private String comment = "";

    private String message = "";

    private RuleType type;

    private boolean unique = false;

    private int severity;

    private boolean msgFromFile = false;

    /**
     * Array of MethodArgData.
     */
    private ArrayList<MethodArgData> methodArgsArray;

    /**
     * Constructor.
     */
    public MethodData() {
        init();
    }

    /**
     * Constructor.
     * 
     * @param methodName
     *            Method name.
     * @param comment
     *            Comment.
     */
    public MethodData(String methodName, String comment, String msg,
            RuleType type, boolean unique) {
        this.methodName = methodName;
        this.comment = comment;
        this.message = msg;
        this.type = type;
        this.unique = unique;
        init();
    }

    /**
     * Constructor.
     * 
     * @param methodName
     *            Method name.
     * @param comment
     *            Comment.
     * @param methodArgsArray
     *            Array of MethodArgData.
     */
    public MethodData(String methodName, String comment,
            ArrayList<MethodArgData> methodArgsArray) {
        this.methodName = methodName;
        this.comment = comment;
        this.methodArgsArray = methodArgsArray;
    }

    /**
     * Initialize method.
     */
    private void init() {
        methodArgsArray = new ArrayList<MethodArgData>();
    }

    /**
     * Add MethodArgData to the array.
     * 
     * @param methodArgData
     */
    public void addMethodArgData(MethodArgData methodArgData) {
        methodArgsArray.add(methodArgData);
    }

    /**
     * Get the method name.
     * 
     * @return The method name.
     */
    public String getMethodName() {
        return methodName;
    }

    /**
     * Set the method name.
     * 
     * @param methodName
     *            The method name.
     */
    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    /**
     * Get the comment.
     * 
     * @return The comment.
     */
    public String getComment() {
        return comment;
    }

    /**
     * Set the comment.
     * 
     * @param comment
     *            The comment.
     */
    public void setComment(String comment) {
        this.comment = comment;
    }

    /**
     * Get the array of MethodArgData.
     * 
     * @return Array of MethodArgData.
     */
    public ArrayList<MethodArgData> getMethodArgsArray() {
        return methodArgsArray;
    }

    /**
     * Get the array of argument names from methodArgData.
     * 
     * @return Array of argument names.
     */
    public ArrayList<String> getArgs() {
        ArrayList<String> args = new ArrayList<String>();
        Iterator<MethodArgData> itr = methodArgsArray.iterator();

        while (itr.hasNext()) {
            args.add(itr.next().getArgName());
        }

        return args;
    }

    /**
     * Get a list of argument values from methodArgData.
     * 
     * @return ArrayList of argument values.
     */
    public ArrayList<String> getArgValues() {
        ArrayList<String> args = new ArrayList<String>();
        Iterator<MethodArgData> itr = methodArgsArray.iterator();

        while (itr.hasNext()) {
            args.add(itr.next().getArgValue());
        }

        return args;
    }

    /**
     * Set the array of MethodArgData.
     * 
     * @param methodArgsArray
     *            Array of MethodArgData.
     */
    public void setMethodArgsArray(ArrayList<MethodArgData> methodArgsArray) {
        this.methodArgsArray = methodArgsArray;
    }

    /**
     * Add an argument to the MethodArgData array.
     * 
     * @param arg
     *            MethodArgData argument
     */
    public void addArgument(MethodArgData arg) {
        this.methodArgsArray.add(arg);
    }

    /**
     * Set the Unique flag.
     * 
     * @param unique
     *            Unique flag boolean value.
     */
    public void setUnique(boolean unique) {
        this.unique = unique;
    }

    /**
     * Get the value of the Unique flag.
     * 
     * @return Unique flag boolean value
     */
    public boolean getUnique() {
        return unique;
    }

    /**
     * Set the RuleType
     * 
     * @param type
     *            RuleType
     */
    public void setType(RuleType type) {
        this.type = type;
    }

    /**
     * Get the RuleType.
     * 
     * @return RuleType.
     */
    public RuleType getType() {
        return type;
    }

    /**
     * Set the Message String.
     * 
     * @param message
     *            String
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Get the Message String.
     * 
     * @return String message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Set the Severity value with an int.
     * 
     * @param severity
     *            int severity
     */
    public void setSeverity(int severity) {
        this.severity = severity;
    }

    /**
     * Get the Severity value.
     * 
     * @return severityColorEnum severity
     */
    public int getSeverity() {
        return severity;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(MethodData o) {
        if (o.getSeverity() < this.getSeverity()) {
            return -1;
        } else if (o.getSeverity() > this.getSeverity()) {
            return 1;
        } else {
            return 0;
        }
    }

    public void setMsgFromFile(boolean value) {
        msgFromFile = value;
    }

    public boolean getMsgFromFile() {
        return msgFromFile;
    }
}
