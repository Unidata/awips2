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
package com.raytheon.uf.common.nc.bufr;

import java.util.Iterator;
import java.util.List;

import ucar.ma2.ArraySequence;
import ucar.ma2.StructureData;
import ucar.ma2.StructureMembers;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.Variable;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Represents a level of a NetCDF structure variable during processing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2014 2905       bclement     Initial creation
 * Apr 01, 2014 2905       bclement     added references to member and variable lists
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class StructureLevel {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(StructureLevel.class);

    private final StructureData structData;

    private final List<Member> memberList;

    private final Iterator<Member> memberIter;

    private final List<Variable> memberVarList;

    private final Iterator<Variable> memberVarIter;

    private Member currentMember;

    private Variable currentMemberVar;

    /**
     * @param structData
     * @param memberVarIter
     */
    public StructureLevel(StructureData structData,
            List<Variable> memberVariables) {
        this(structData, structData.getStructureMembers(), memberVariables);
    }

    /**
     * @param structData
     * @param members
     * @param memberVarIter
     */
    public StructureLevel(StructureData structData, StructureMembers members,
            List<Variable> memberVariables) {
        this.structData = structData;
        this.memberList = members.getMembers();
        this.memberIter = memberList.iterator();
        this.memberVarList = memberVariables;
        this.memberVarIter = memberVarList.iterator();
    }

    /**
     * @return true if this level has more members to process
     */
    public boolean hasNext() {
        if (memberIter.hasNext() && memberVarIter.hasNext()) {
            return true;
        } else if (memberIter.hasNext() || memberVarIter.hasNext()) {
            log.error("Structure iteration out of sync");
            throw new IllegalStateException("Structure iteration out of sync");
        }
        return false;
    }

    /**
     * move on to the next member to process. This updates the fields
     * {@link #currentMember} and {@link #currentMemberVar}
     */
    public void next() {
        currentMember = memberIter.next();
        currentMemberVar = memberVarIter.next();
    }

    /**
     * @return
     */
    public StructureData getSubStructure() {
        return (StructureData) structData.getScalarObject(currentMember);
    }

    /**
     * @return
     */
    public ArraySequence getSubSequence() {
        return structData.getArraySequence(currentMember);
    }

    /**
     * @return the structData
     */
    public StructureData getStructData() {
        return structData;
    }

    /**
     * @return the memberList
     */
    public List<Member> getMemberList() {
        return memberList;
    }

    /**
     * @return the memberVarList
     */
    public List<Variable> getMemberVarList() {
        return memberVarList;
    }

    /**
     * @return the currentMember
     */
    public Member getCurrentMember() {
        return currentMember;
    }

    /**
     * @return the currentMemberVar
     */
    public Variable getCurrentMemberVar() {
        return currentMemberVar;
    }

}
