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
/**
 * 
 */
package com.raytheon.viz.gfe.core.script;

/**
 * An enumeration for describing the relationship between object names supplied
 * by the user and system objects (usually files) with those names. NONE
 * indicates that any name is OK, regardless of whether or not a corresponding
 * object exists. ERR_EXISTS indicates that it is an error for the user to
 * supply the name of an object that already exists (i.e., when creating a new
 * script); ERR_NOTEXISTS indicates that it is an error for the user to supply
 * the name of an object that does not exist (i.e., when modifying or deleting a
 * script).
 * 
 * @author wldougher
 * 
 */
public enum ExistMode {
    NONE, ERR_EXISTS, ERR_NOTEXISTS
}
